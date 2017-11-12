url <- read_html("http://elecciones.registraduria.gov.co:81/esc_elec_2015/99AL/DAL60ZZZZZZZZZZZZZZZ_L1.htm")

urls_depto <- url %>% html_nodes('#combo2 > option') %>% html_attr("value")
names_depto <- url %>% html_nodes('#combo2 > option') %>% html_text()

urls_depto <- urls_depto[-1]
names_depto <- names_depto[-1]
urls_depto <- gsub("\\.\\.", "http://elecciones.registraduria.gov.co:81/esc_elec_2015", urls_depto)
url_depto <- urls_depto[1]
list_df_depto <- lapply(urls_depto, function(url_depto){
  
  h <- read_html(url_depto)
  urls_mpio <- h %>% html_nodes("#combo3 > option") %>% html_attr("value")
  names_mpio <- h %>% html_nodes('#combo3 > option') %>% html_text()
  
  urls_mpio <- urls_mpio[-1]
  names_mpio <- names_mpio[-1]
  
  urls_mpio <- gsub("\\.\\.", "http://elecciones.registraduria.gov.co:81/esc_elec_2015", urls_mpio)
  name_depto <- names_depto[which(urls_depto==url_depto)]
  df <- data.frame(Names_mpio = names_mpio, 
                   Names_depto = name_depto,
                   URL_mpio = urls_mpio,
                   URL_depto = url_depto)
  gc()
  return(df)
  
})

df <- bind_rows(list_df_depto)
dim(df)
urls_mpio <- df$URL_mpio
url_mpio <- "http://elecciones.registraduria.gov.co:81/esc_elec_2015/99AL/DAL01007ZZZZZZZZZZZZ_L1.htm"
list_df <- lapply(urls_mpio, function(url_mpio){
  print(url_mpio)
  h <- read_html(url_mpio)
  try({
    info_votos <- h %>% html_nodes("#infocandidato0 div") %>%
      html_text()
    info_votos <- info_votos[c(2,3)]
    df <- data.frame(t(info_votos))
    names(df) <- c("Alcalde", "Partido Político")
    gc()
    return(df)
  })
})

df_info <- bind_rows(list_df)  

alcaldia <- bind_cols(list(df,df_info))

write_csv(pleb, "alcaldes_2015.csv", col_names = TRUE)