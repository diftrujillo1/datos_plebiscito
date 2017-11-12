library(rvest)
library(tidyverse)
library(stringr)
library(curl)

url <- read_html("http://www3.registraduria.gov.co/elecciones/elecciones2014/presidente/2v/99PR2/DPR0119299_L1.htm")
urls_depto <- url %>% html_nodes("#idMenu1 a") %>% html_attr("href")
urls_depto <- gsub("\\.\\.", "http://www3.registraduria.gov.co/elecciones/elecciones2014/presidente/2v", urls_depto)
names_depto <- url %>% html_nodes("#idMenu1 a") %>% html_text()

list_df_depto <- lapply(urls_depto, function(url_depto){
  h <- read_html(url_depto)
  info1 <- h %>% html_nodes(".tizda .item , .tizda .dato") %>%
    html_text()
  info1 <- gsub("\\.|\\%", "", info1)
  info1 <- gsub("\\,", "\\.", info1)
  info1 <- split(info1, ceiling(seq_along(info1)/2))
  info1 <- bind_rows(info1)
  names(info1) <- info1[1,]
  info1 <- info1[-1,]
  
  info2 <- h %>% html_nodes(".tder .dato , .tder .item") %>%
    html_text()
  info2 <- gsub("\\.|\\%", "", info2)
  info2 <- gsub("\\,", "\\.", info2)
  info2 <- split(info2, ceiling(seq_along(info2)/2))
  info2 <- bind_rows(info2)
  names(info2) <- info2[1,]
  info2 <- info2[-1,]
  
  info3 <- h %>% html_nodes(".prc , .abs , #infocandidato0 div") %>%
    html_text()
  info3 <- gsub("\\.|\\%", "", info3)
  info3 <- gsub("\\,", "\\.", info3)
  info3 <- info3[info3 != ""]
  info3 <- split(info3, ceiling(seq_along(info3)/4))
  info3 <- bind_rows(info3)
  if(grepl("Santos", info3$`1`[1])){
    votos_santos <- info3$`1`[3]
    porc_votos_santos <- info3$`1`[4]
    votos_zul <- info3$`2`[3]
    porc_votos_zul <- info3$`2`[4]
  }else{
    votos_santos <- info3$`2`[3]
    porc_votos_santos <- info3$`2`[4]
    votos_zul <- info3$`1`[3]
    porc_votos_zul <- info3$`1`[4]
  }
  info3 <- data.frame(Votos_Santos = votos_santos,
                      Porc_Votos_Santos = porc_votos_santos,
                      Votos_Zuluaga = votos_zul,
                      Porc_Votos_Zuluaga = porc_votos_zul)
  
  name_depto <- names_depto[which(urls_depto == url_depto)]
  info <- data.frame(Name_Depto = name_depto, URL_Depto = url_depto)
  df <- bind_cols(list(info, info1, info2, info3))

  gc()
  return(df)
})

df_depto <- bind_rows(list_df_depto)
df_depto[,c(-1,-2)] <- sapply(df_depto[,c(-1,-2)], as.numeric)
write_csv(df_depto, "votacion_2nda_vuelta_2014_presidencial_deptos.csv", col_names = TRUE)

####################################################################

urls_depto <- urls_depto[-1]
names_depto <- names_depto[-1]

list_df_mpios <- lapply(urls_depto, function(url_depto){
  try({
    name_depto <- names_depto[which(urls_depto==url_depto)]
    if(name_depto != "BOGOTA D.C."){
      h <- read_html(url_depto)
      urls_mpio <- h %>% html_nodes("#combmunidesp a") %>% html_attr("href")
      names_mpio <- h %>% html_nodes('#combmunidesp a') %>% html_text()
      
      urls_mpio <- urls_mpio[-1]
      names_mpio <- names_mpio[-1]
      
      urls_mpio <- gsub("\\.\\.", "http://www3.registraduria.gov.co/elecciones/elecciones2014/presidente/2v", urls_mpio)
      
      df <- data.frame(Names_mpio = names_mpio, 
                       Names_depto = name_depto,
                       URL_mpio = urls_mpio,
                       URL_depto = url_depto)
      gc()
      return(df)
    }else{
      
      df <- data.frame(Names_mpio = name_depto, 
                       Names_depto = name_depto,
                       URL_mpio = url_depto,
                       URL_depto = url_depto)
      
      return(df)
    }
  })
  message("URL: ", url_depto)
})

df_mpios <- bind_rows(list_df_mpios)
urls_mpio <- df_mpios$URL_mpio

list_df_mpio <- lapply(urls_mpio, function(url_mpio){
  h <- read_html(url_mpio)
  info1 <- h %>% html_nodes(".tizda .item , .tizda .dato") %>%
    html_text()
  info1 <- gsub("\\.|\\%", "", info1)
  info1 <- gsub("\\,", "\\.", info1)
  info1 <- split(info1, ceiling(seq_along(info1)/2))
  info1 <- bind_rows(info1)
  names(info1) <- info1[1,]
  info1 <- info1[-1,]
  
  info2 <- h %>% html_nodes(".tder .dato , .tder .item") %>%
    html_text()
  info2 <- gsub("\\.|\\%", "", info2)
  info2 <- gsub("\\,", "\\.", info2)
  info2 <- split(info2, ceiling(seq_along(info2)/2))
  info2 <- bind_rows(info2)
  names(info2) <- info2[1,]
  info2 <- info2[-1,]
  
  info3 <- h %>% html_nodes(".prc , .abs , #infocandidato0 div") %>%
    html_text()
  info3 <- gsub("\\.|\\%", "", info3)
  info3 <- gsub("\\,", "\\.", info3)
  info3 <- info3[info3 != ""]
  info3 <- split(info3, ceiling(seq_along(info3)/4))
  info3 <- bind_rows(info3)
  if(grepl("Santos", info3$`1`[1])){
    votos_santos <- info3$`1`[3]
    porc_votos_santos <- info3$`1`[4]
    votos_zul <- info3$`2`[3]
    porc_votos_zul <- info3$`2`[4]
  }else{
    votos_santos <- info3$`2`[3]
    porc_votos_santos <- info3$`2`[4]
    votos_zul <- info3$`1`[3]
    porc_votos_zul <- info3$`1`[4]
  }
  info3 <- data.frame(Votos_Santos = votos_santos,
                      Porc_Votos_Santos = porc_votos_santos,
                      Votos_Zuluaga = votos_zul,
                      Porc_Votos_Zuluaga = porc_votos_zul)
  
  df <- bind_cols(list(info1, info2, info3))
  
  gc()
  return(df)
})

df_info <- bind_rows(list_df_mpio)  

df_mpios <- bind_cols(list(df_mpios, df_info))
write_csv(df_mpios, "votacion_2nda_vuelta_2014_presidencial_mpios.csv", col_names = TRUE)

######################################################################

url <- read_html("http://hsbnoticias.com/noticias/politica/lista-de-los-nuevos-alcaldes-y-gobernadores-en-las-eleccione-165500")
table <- url %>% html_nodes("#block-system-main table") %>% html_table(fill=TRUE)
table <- as.data.frame(table) 
table <- table[-6,]
table$X1 <- gsub("^\\s+|\\s+$", "", table$X1)
table$X2 <- gsub("^\\s+|\\s+$", "", table$X2)
table$X2 <- gsub("Gobernador: ", "", table$X2)
table$X2 <- gsub("Alcalde (.*)", "", table$X2)
table$X2 <- gsub("^\\s+|\\s+$", "", table$X2)
table <- table[,-3]
table <- table[-1,]
names(table) <- c("Depto", "Gobernador")

part_pol <- url %>% html_nodes("tr+ tr td~ td+ td p:nth-child(1)") %>% html_text()
part_pol <- part_pol[-6]
part_pol <- gsub("Gobernador: ", "", part_pol)
table$`Partido Político` <- part_pol

write_csv(table, "gobernadores_colombia.csv", col_names = TRUE)
