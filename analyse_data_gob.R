library(tidyverse)
library(stringi)
library(stringr)
library(treemap)

df_plebiscito <- read_csv("data/votacion_plebiscito_2016_deptos.csv", col_names = TRUE)
df_pres <- read_csv("data/votacion_2nda_vuelta_2014_presidencial_deptos.csv", col_names = TRUE)
df_gobernadores <- read_csv("data/gobernadores_colombia.csv", col_names = TRUE)

df_gobernadores$`Partido Político`[7] <- gsub("Cambio", "", df_gobernadores$`Partido Político`[7])
df_plebiscito<- df_plebiscito %>% filter(Names_depto != "CONSULADOS" & Names_depto != "Todos")

df_plebiscito <- df_plebiscito %>% mutate(Ganador_Pleb = ifelse(Votos_SI > Votos_NO, "Sí", "No"))
df_pres <- df_pres %>% mutate(Ganador_Pres = ifelse(Votos_Santos > Votos_Zuluaga, "Santos", "Zuluaga"))

df_cruce_pleb <- df_plebiscito %>% select(Names_depto, Ganador_Pleb)
df_cruce_pres <- df_pres %>% mutate(Names_depto = Name_Depto) %>% select(Names_depto, Ganador_Pres)
df_cruce <- left_join(df_cruce_pleb, df_cruce_pres, by = c("Names_depto"))

df_santos_si <- df_cruce %>% filter(Ganador_Pleb == "Sí" & Ganador_Pres == "Santos") %>% summarise(count=n())
df_santos_no <- df_cruce %>% filter(Ganador_Pleb == "No" & Ganador_Pres == "Santos") %>% summarise(count=n())
df_zul_no <- df_cruce %>% filter(Ganador_Pleb == "No" & Ganador_Pres == "Zuluaga") %>% summarise(count=n())
df_zul_si <- df_cruce %>% filter(Ganador_Pleb == "Sí" & Ganador_Pres == "Zuluaga") %>% summarise(count=n())

df_contingencia <- data.frame(Sí = c(df_santos_si$count, df_zul_si$count), 
                              No = c(df_santos_no$count, df_zul_no$count), 
                              row.names = c("Santos", "Zuluaga"))

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
df_contingencia_ <- df_cruce %>% filter(!is.na(Ganador_Pres)) %>%
  group_by(Ganador_Pleb,Ganador_Pres) %>% summarise(count=n())
treemap(df_contingencia_, #Your data frame object
        index=c("Ganador_Pleb","Ganador_Pres"),  #A list of your categorical variables
        vSize = "count",  #This is your quantitative variable
        type="index", #Type sets the organization and color scheme of your treemap
        palette = "Reds",  #Select your color palette from the RColorBrewer presets or make your own.
        title="Análisis Plebiscito", #Customize your title
        fontsize.title = 14, #Change the font size of the title
        overlap.labels = 0
)

df_plebiscito <- df_plebiscito[c(-1,-6,-15),]
df_pres <- df_pres[c(-1,-6,-35),]

df_gobernadores$Depto <- gsub("á|Á","a", df_gobernadores$Depto)
df_gobernadores$Depto <- gsub("é|É","e", df_gobernadores$Depto)
df_gobernadores$Depto <- gsub("í|Í","i", df_gobernadores$Depto)
df_gobernadores$Depto <- gsub("ó|Ó","o", df_gobernadores$Depto)
df_gobernadores$Depto <- gsub("ú|Ú","u", df_gobernadores$Depto)
df_gobernadores$Depto <- gsub("\\s+"," ", df_gobernadores$Depto)
df_gobernadores$Depto <- stri_trim(df_gobernadores$Depto)
df_gobernadores$Depto <- gsub("N. de Santander","NORTE DE SAN", df_gobernadores$Depto)
df_gobernadores$Depto <- gsub("Valle del Cauca","Valle", df_gobernadores$Depto)
df_gobernadores$Depto <- toupper(df_gobernadores$Depto)


df_gobernadores <- df_gobernadores %>% mutate(Ganador_Gob = ifelse(grepl("U-Cambio|La U - Cambio Radical|U. CR|Cambio-U",
                                                                         df_gobernadores$`Partido Político`), "La U - Cambio Radical",
                                                                   ifelse(grepl("C.R|CR|Cambio Radical|Cambio",
                                                                                df_gobernadores$`Partido Político`), "Cambio Radical",
                                                                          ifelse(grepl("Centro Democrático|Centro",
                                                                                       df_gobernadores$`Partido Político`), "Centro Democrático",
                                                                                 ifelse(grepl("La U|U.|U", df_gobernadores$`Partido Político`),
                                                                                        "La U", NA)))))

df_cruce_gob <- df_gobernadores %>% mutate(Names_depto = Depto) %>% select(Names_depto, Ganador_Gob) 
df_cruce <- left_join(df_cruce, df_cruce_gob, by = c("Names_depto"))
df_cruce_ <- df_cruce %>% filter(!is.na(Ganador_Pres) & !is.na(Ganador_Gob)) %>%
  group_by(Ganador_Pleb,Ganador_Pres,Ganador_Gob) %>% summarise(count=n()) 
df_cruce_ <- df_cruce %>% filter(!is.na(Ganador_Pres) & !is.na(Ganador_Gob)) %>%
  group_by(Ganador_Pleb,Ganador_Pres,Ganador_Gob) %>% summarise(count=n()) %>% mutate(Color = ifelse(Ganador_Pleb == "Sí", "#7fcdbb", "#f03b20"))
df_cruce_ <- table(df_cruce)
library(treemap)
treemap(df_cruce_, #Your data frame object
        index=c("Ganador_Gob","Ganador_Pres","Ganador_Pleb"),  #A list of your categorical variables
        vSize = "count",  #This is your quantitative variable
        vColor = "Color",
        type = "color", #Type sets the organization and color scheme of your treemap
        palette = c("#7fcdbb", "#f03b20"),
        title="Análisis Plebiscito", #Customize your title
        fontsize.title = 15, #Change the font size of the title
        overlap.labels = 0.5
)


df_cruce_tidy <- df_cruce %>% filter(!is.na(Ganador_Pres) & !is.na(Ganador_Gob)) %>%
  dplyr::group_by(Ganador_Pleb, Ganador_Pres, Ganador_Gob) %>% dplyr::summarise(count=n())  %>%
  tidyr::spread(Ganador_Gob, count) %>% tidyr::gather(Ganador_Gob, count, -Ganador_Pleb, -Ganador_Pres)

titleLabel <- "Análisis de las Votaciones en el Plebiscito"
xlab <- "Candidatos Presidenciales 2014"
yLabel <- "Conteo"
flabel <- "Partidos Políticos"
leg_pos <- "right"
ggplot(df_cruce_tidy, aes(Ganador_Pres, weight = count, fill = Ganador_Gob)) + 
  geom_bar(position = "dodge") + geom_text(aes(label = count, y = count + 0.05), position = position_dodge(0.9), vjust = 0) +  
  labs(title = titleLabel, x = xlab, y = yLabel, fill = flabel) +
  theme(legend.position=leg_pos) + facet_grid(.~Ganador_Pleb)


df_cruce_tidy2 <- df_cruce %>% filter(!is.na(Ganador_Pres) & !is.na(Ganador_Gob)) %>%
  dplyr::group_by(Ganador_Pleb, Ganador_Pres, Ganador_Gob) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::arrange(desc(count))

titleLabel <- "Análisis de las Votaciones en el Plebiscito"
xlab <- "Candidatos Presidenciales 2014"
yLabel <- "Partidos Políticos"
ggplot(df_cruce_tidy2, aes(x = Ganador_Pres, y = Ganador_Gob, size = count)) +
  geom_point() + labs(title = titleLabel, x = xlab, y = yLabel) + 
  theme(legend.position="none") + facet_grid(.~Ganador_Pleb)


abstencionismo <- df_plebiscito$Porc_Abstensionismo
abstencionismo <- abstencionismo[c(-1,-6,-15)]
deptos <- df_plebiscito$Names_depto
deptos <- deptos[c(-1,-6,-15)]

df_abs <- data.frame(Abstencionismo = abstencionismo, x = deptos)
df_abs <- df_abs %>% filter(Abstencionismo <= 0.6)

ggplot(data = df_abs, aes(x = factor(""), y= Abstencionismo)) + 
  geom_boxplot() + geom_point() + labs(title = titleLabel, x = xlab, y = yLabel)


