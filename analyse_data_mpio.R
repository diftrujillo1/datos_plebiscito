library(tidyverse)
library(stringr)

df_plebiscito <- read_csv("data/votacion_plebiscito_2016_mpios.csv", col_names = TRUE)
df_pres <- read_csv("data/votacion_2nda_vuelta_2014_presidencial_mpios.csv", col_names = TRUE)
df_mpios <- read_csv("data/alcaldes_2015.csv", col_names = TRUE)

df_plebiscito<- df_plebiscito %>% filter(Names_depto != "CONSULADOS")
#df_plebiscito <- df_plebiscito[c(-1,-6,-15),]
#df_pres <- df_pres[c(-1,-6,-35),]

df_plebiscito <- df_plebiscito %>% mutate(Ganador_Pleb = ifelse(Votos_SI > Votos_NO, "Sí", "No"))
df_pres <- df_pres %>% mutate(Ganador_Pres = ifelse(Votos_Santos > Votos_Zuluaga, "Santos", "Zuluaga"))

df_cruce_pleb <- df_plebiscito %>% select(Names_mpio, Ganador_Pleb)
df_cruce_pres <- df_pres %>% select(Names_mpio, Ganador_Pres)
df_cruce <- left_join(df_cruce_pleb, df_cruce_pres, by = c("Names_mpio"))

df_santos_si <- df_cruce %>% filter(Ganador_Pleb == "Sí" & Ganador_Pres == "Santos") %>% summarise(count=n())
df_santos_no <- df_cruce %>% filter(Ganador_Pleb == "No" & Ganador_Pres == "Santos") %>% summarise(count=n())
df_zul_no <- df_cruce %>% filter(Ganador_Pleb == "No" & Ganador_Pres == "Zuluaga") %>% summarise(count=n())
df_zul_si <- df_cruce %>% filter(Ganador_Pleb == "Sí" & Ganador_Pres == "Zuluaga") %>% summarise(count=n())

df_contingencia <- data.frame(Sí = c(df_santos_si$count, df_zul_si$count), 
                              No = c(df_santos_no$count, df_zul_no$count), 
                              row.names = c("Santos", "Zuluaga"))

df_part_pol <- df_mpios %>% group_by(`Partido Político`) %>% summarise(count = n()) %>% arrange(desc(count))

df_part_pol <- df_part_pol[1:8,]
part_pol <- df_part_pol$`Partido Político`

list_df <- lapply(part_pol, function(p){
  df_mpios_part_pol <- df_mpios %>% filter(`Partido Político` == p) %>% select(Names_mpio, Names_depto, `Partido Político`)
  return(df_mpios_part_pol)
})
df_mpios_part_pol <- bind_rows(list_df)
df_pleb <- df_plebiscito %>% select(Names_mpio, Names_depto, Votos_SI, Votos_NO)
df_cruce <- left_join(df_pleb, df_mpios_part_pol, by = c("Names_mpio", "Names_depto"))
df_cruce <- df_cruce %>% mutate(Ganador_Pleb = ifelse(Votos_SI > Votos_NO, "Sí", "No"))
df_cruce_ <- df_cruce %>% filter(!is.na(`Partido Político`)) %>%
  group_by(Ganador_Pleb, `Partido Político`) %>% summarise(count=n()) %>%
  tidyr::spread(`Partido Político`, count) %>% tidyr::gather(`Partido Político`, count, -Ganador_Pleb)

df_cruce_show <- df_cruce_ %>% group_by(`Partido Político`) %>% summarise(Votación = sum()) %>% 
  mutate(`Porcentaje Votación` = count / Votación) %>% arrange(desc(`Partido Político`)) 

df_cruce_$`Partido Político` <- gsub("PARTIDO|PARTIDO DE","", df_cruce_$`Partido Político`)
titleLabel <- "Análisis de las Votaciones del Plebiscito vs. \n Partidos Políticos en las elecciones \n de la alcaldía"
xlab <- "Partidos Políticos"
yLabel <- "Conteo"
flabel <- "Votación Plebiscito"
leg_pos <- "right"
ggplot(df_cruce_, aes(`Partido Político`, weight = count, fill = Ganador_Pleb)) + 
  geom_bar(position = "dodge") + geom_text(aes(label = count, y = count + 0.5), size=3,position = position_dodge(0.9), vjust = 0.5, hjust=1.5) +  
  labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel) + theme_minimal() + 
  theme(legend.position=leg_pos) + theme(axis.text.y = element_text(size=6)) + coord_flip()

df_pres_ <- df_pres %>% select(Names_mpio, Names_depto, Votos_Santos, Votos_Zuluaga)
df_cruce2 <- left_join(df_pres_, df_mpios_part_pol, by = c("Names_mpio", "Names_depto"))
df_cruce2_ <- df_cruce2 %>% mutate(Ganador_Pres = ifelse(Votos_Santos > Votos_Zuluaga, "Santos", "Zuluaga"))
df_cruce2_ <- df_cruce2_ %>% filter(!is.na(`Partido Político`)) %>%
  group_by(Ganador_Pres, `Partido Político`) %>% dplyr::summarise(count=n()) %>% arrange(desc(count))

df_cruce2_show_ <- df_cruce2_ %>% dplyr::group_by(`Partido Político`) %>% dplyr::summarise(Votación = sum(count))
df_cruce2_show_ <- left_join(df_cruce2_, df_cruce2_show_, by = "Partido Político") 
df_cruce2_show_ <- df_cruce2_show_ %>% 
  dplyr::mutate(`Porcentaje Votación` = round(count/Votación,4)*100) %>% 
  dplyr::arrange(desc(`Partido Político`))

pos_santos <- df_cruce2_show_ %>% filter(Ganador_Pres == 'Santos') %>%
  mutate(pos = `Porcentaje Votación`/1000*7) %>% select(Ganador_Pres,`Partido Político`, pos)
pos_zul <- df_cruce2_show_ %>% filter(Ganador_Pres == 'Zuluaga') %>%
  mutate(pos = 1-`Porcentaje Votación`/1000*8) %>% select(Ganador_Pres,`Partido Político`, pos)
pos <- bind_rows(list(pos_santos,pos_zul))
df_cruce2_show_ <- left_join(df_cruce2_show_, pos, by = c("Ganador_Pres", "Partido Político"))
  #select(Ganador_Pres, `Partido Político`, `Porcentaje Votación`)
df_cruce2_show_$`Partido Político` <- gsub("PARTIDO ", "", df_cruce2_show_$`Partido Político`)
titleLabel <- "Análisis de las Votaciones 2nda Vuelta Presidencial vs. \n Partidos Políticos en las elecciones \n de la alcaldía"
xlab <- "Partidos Políticos"
yLabel <- "Proporción Votación"
flabel <- "Candidatos \n Presidenciales"
ggplot(df_cruce2_show_, aes(`Partido Político`, weight = `Porcentaje Votación`, fill = Ganador_Pres)) + 
  geom_bar(position = "fill")  +  
  labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel) +
  scale_y_continuous(labels = scales::percent) + theme_minimal() + 
  geom_text(aes(label = paste0(`Porcentaje Votación`,"%"), y = pos), size=2.5) +
  theme(legend.position=leg_pos) + theme(axis.text.y = element_text(size=6)) + coord_flip()

df_santos_si
df_contingencia_ <- data.frame(Ganador_Pres = c("Santos", "Santos", "Zuluaga", "Zuluaga"),
                              Ganador_Pleb = c("Sí", "No", "Sí", "No"),
                              count = c(df_santos_si$count, df_santos_no$count, df_zul_si$count, df_zul_no$count))
df_contingencia_ <- df_contingencia_ %>% mutate(Color = ifelse(Ganador_Pleb=="Sí", "#7fcdbb", "#f03b20"))

treemap(df_contingencia_, #Your data frame object
        index=c("Ganador_Pres","Ganador_Pleb"),  #A list of your categorical variables
        vSize = "count",  #This is your quantitative variable
        vColor = "Color",
        type = "color", #Type sets the organization and color scheme of your treemap
        palette = "Reds",
        title = "", #Customize your title
        #fontsize.title = 10, #Change the font size of the title
        overlap.labels = 1
)

df_cruce_ <- df_cruce %>% filter(!is.na(`Partido Político`)) %>%
  group_by(Ganador_Pleb, `Partido Político`) %>% summarise(count=n()) %>%
  dplyr::arrange(desc(count))

titleLabel <- ""
xlab <- ""
yLabel <- ""
flabel <- "Votación Plebiscito"
ggplot(df_cruce_, aes(Ganador_Pleb, fill = `Partido Político`, weight = count)) + geom_bar(width = 1) +
  coord_polar() + labs(title = titleLabel, x = xlab, y = yLabel, fill=flabel) + 
  theme_minimal() + theme(legend.position=leg_pos)



