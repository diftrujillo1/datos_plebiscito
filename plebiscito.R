library(rvest)
library(tidyverse)
library(stringr)
library(curl)


url <- read_html("http://plebiscito.registraduria.gov.co/99PL/DPLZZZZZZZZZZZZZZZZZ_L1.htm")
urls_depto <- url %>% html_nodes('#combo2 > option') %>% html_attr("value")
names_depto <- url %>% html_nodes('#combo2 > option') %>% html_text()

urls_depto <- urls_depto[-1]
names_depto <- names_depto[-1]
urls_depto <- gsub("\\.\\.", "http://plebiscito.registraduria.gov.co", urls_depto)
urls_depto[1]
list_df_depto <- lapply(urls_depto, function(url_depto){
  
  h <- read_html(url_depto)
  urls_mpio <- h %>% html_nodes("#combo3 > option") %>% html_attr("value")
  names_mpio <- h %>% html_nodes('#combo3 > option') %>% html_text()
  
  urls_mpio <- urls_mpio[-1]
  names_mpio <- names_mpio[-1]
  
  urls_mpio <- gsub("\\.\\.", "http://plebiscito.registraduria.gov.co", urls_mpio)
  name_depto <- names_depto[which(urls_depto==url_depto)]
  df <- data.frame(Names_mpio = names_mpio, 
                   Names_depto = name_depto,
                   URL_mpio = urls_mpio,
                   URL_depto = url_depto)
  gc()
  return(df)
  
})

df <- bind_rows(list_df)

urls_mpio <- df$URL_mpio
list_df <- lapply(urls_mpio, function(url_mpio){
  
  h <- read_html(url_mpio)
  info_votos <- h %>% html_nodes(".skill-bar-percent, .contenido, .cajaDatosBody span") %>%
    html_text()
  info_votos <- gsub("Votos válidos|Votos no marcados|Votos nulos| mesas instaladas| personas habilitadas", 
                     "", info_votos)
  df <- data.frame(t(info_votos))
  names(df) <- c("Porc_escrutado", "Mesas_instaladas", "Votacion", 
                 "Votos/Personas_habilitadas", "Porc_SI", "Votos_SI", 
                 "Porc_NO","Votos_NO", "Votos_Válidos",
                 "Votos_no_marcados", "Votos_nulos")
  gc()
  return(df)
})

df_info <- bind_rows(list_df)  

pleb <- bind_cols(list(df,df_info))
pleb$Votos_SI <- gsub("\\(|\\)| Votos|\\.","",pleb$Votos_SI)
pleb$Votos_NO <- gsub("\\(|\\)| Votos|\\.","",pleb$Votos_NO)
pleb$Porc_escrutado <- gsub("\\%","",pleb$Porc_escrutado)
pleb$Porc_escrutado <- gsub("\\,","\\.", pleb$Porc_escrutado)
pleb$Votacion <- gsub("\\%","", pleb$Votacion)
pleb$Votacion <- gsub("\\,","\\.", pleb$Votacion)
pleb$Porc_SI <- gsub("\\%","", pleb$Porc_SI)
pleb$Porc_SI <- gsub("\\,","\\.", pleb$Porc_SI)
pleb$Porc_NO <- gsub("\\%","", pleb$Porc_NO)
pleb$Porc_NO <- gsub("\\,","\\.", pleb$Porc_NO)
pleb$`Votos/Personas_habilitadas` <- gsub("\\.","", pleb$`Votos/Personas_habilitadas`)
pleb$Mesas_instaladas <-gsub("\\.","", pleb$Mesas_instaladas)
pleb$Votos_Válidos <- gsub("\\.","", pleb$Votos_Válidos)
pleb$Votos_no_marcados <- gsub("\\.","", pleb$Votos_no_marcados)
pleb$Votos_nulos <- gsub("Votos nulos|\\.","", pleb$Votos_nulos)
pleb <- pleb %>% separate(`Votos/Personas_habilitadas`, c("Num_Votos", "Num_Personas_Habilitadas"), sep=" de ")
pleb <- pleb %>% separate(Mesas_instaladas, c("Num_mesas", "Total_mesas"), sep=" de ")
pleb[,c(-1,-2,-3,-4)] <- sapply(pleb[,c(-1,-2,-3,-4)], as.numeric)
pleb <- pleb %>% dplyr::mutate(Porc_Mesas_Inscritas = Num_mesas / Total_mesas,
                               Porc_Abstensionismo = 1-(Num_Votos/Num_Personas_Habilitadas))

write_csv(pleb, "data/votacion_plebiscito_2016_mpios.csv", col_names = TRUE)

######################################################


url <- read_html("http://plebiscito.registraduria.gov.co/99PL/DPLZZZZZZZZZZZZZZZZZ_L1.htm")
urls_depto <- url %>% html_nodes('#combo2 > option') %>% html_attr("value")
names_depto <- url %>% html_nodes('#combo2 > option') %>% html_text()
urls_depto <- gsub("\\.\\.", "http://plebiscito.registraduria.gov.co", urls_depto)

list_df_depto <- lapply(urls_depto, function(url_depto){
  h <- read_html(url_depto)
  info_votos <- h %>% html_nodes(".skill-bar-percent , .contenido , .descripcionCaja , .porcentajesCajas") %>%
    html_text()
  info_votos <- gsub(" mesas instaladas|\\%|Votos válidos|Votos nulos| personas habilitadas |Votos no marcados",
                     "", info_votos)
  info_votos <- gsub("\\.|\\(|\\)| Votos","", info_votos)
  info_votos <- gsub("\\,","\\.", info_votos)
  name_depto <- names_depto[which(urls_depto==url_depto)]
  df <- data.frame(Names_depto = name_depto,
                   URL_depto = url_depto)
  df_info <- data.frame(t(info_votos))
  names(df_info) <- c("Porc_escrutado", "Mesas_instaladas", "Votacion", 
                 "Votos/Personas_habilitadas", "Porc_SI", "Votos_SI", 
                 "Porc_NO","Votos_NO", "Votos_Válidos",
                 "Votos_no_marcados", "Votos_nulos")
  df <- bind_cols(list(df, df_info))
  
  gc()
  return(df)
})

df_depto <- bind_rows(list_df_depto)
df_depto <- df_depto %>% separate(`Votos/Personas_habilitadas`, c("Num_Votos", "Num_Personas_Habilitadas"), sep=" de ")
df_depto <- df_depto %>% separate(Mesas_instaladas, c("Num_mesas", "Total_mesas"), sep=" de ")
df_depto[,c(-1,-2)] <- sapply(df_depto[,c(-1,-2)], as.numeric)
df_depto <- df_depto %>% dplyr::mutate(Porc_Mesas_Inscritas = Num_mesas / Total_mesas,
                               Porc_Abstensionismo = 1-(Num_Votos/Num_Personas_Habilitadas))


write_csv(df_depto, "data/votacion_plebiscito_2016_deptos.csv", col_names = TRUE)
