#instalo las librerias 
install.packages("httr2")
install.packages("tidytext")
install.packages("udpipe")
install.packages("robotstxt")
install.packages("xml2")
#abro las librerias necesaria

library(tidyverse)  # Manipulación de datos
library(rvest)      # Web scraping
library(httr2)      # Requests HTTP
library(tidytext)   # Análisis de texto
library(udpipe)     # Lematización
library(robotstxt)  # Verificar permisos de scraping
library(here)       # Manejo de rutas de archivos
library(xml2)       # Manejo de HTML (guardar la página completa x ej)


#armo la carpeta de "data" 

data_dir <- here("tp2", "data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}


# URL de la página de noticias sobre la OEA
url_noticias <- "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp"

# Leemos el HTML de la página
pagina_html <- read_html(url_noticias)

# Inspeccionamos la estructura
pagina_html

# Guardamos en formato rds para uso interno y registro de fecha de descarga
attr(pagina_html, "fecha_descarga") <- Sys.time()

# Guardamos en html para abrir en el navegador si queremos. 
write_html(pagina_html, file = file.path(data_dir, "pagina_oea.html"))

#Guardamos en formato RDS 

saveRDS(pagina_html, file = file.path(data_dir, "pagina_oea.rds"))


#EMPEZAMOS EL WEB SCRAPPING: 

# Función para scrapear una página de noticias, de forma sistematizada. 

scrapear_oea<- function(url, first_page=FALSE) {
  
  # Pausa para no sobrecargar el servidor (3 segundos) 
  Sys.sleep(3)
  
  # Leemos la página
  html <- read_html(url)
  
  # Extraemos los elementos que identificamos con el Selector o el Inspector
  titulos_nodos <- html |>
    html_elements(".itemmenulink")
  
  titulos <- titulos_nodos |>
    html_text2() |>
    str_trim()
  
  urls <- titulos_nodos |>
    html_attr("href")
  
  cuerpo <- html |>
    html_elements("p") |>
    html_text2() |>
    str_trim()
  
  fechas <- html |>
    html_elements(".headlinelink") |>
    html_text2() |>
    str_trim()
  
  # Retornamos un tibble
  
  tibble(
    titulo = titulos,
    url = urls,
    fecha_raw = fechas
  )
}


scrapear_oea("https://www.oas.org/es/centro_noticias/comunicados_prensa.asp", first_page=TRUE)
 

scrapear_meses_oea <- function(meses, anio){
  
  noticias_oea <- tibble()
  
  for(i in meses){
    
    Sys.sleep(3)
    
    url <- paste0(
      "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=",
      i,
      "&nAnio=",
      anio
    )
    
    noticias_mes <- scrapear_oea(url)
    
    noticias_oea <- bind_rows(noticias_oea, noticias_mes)
    
    message(
      paste(
        "\nMes", i,
        "de", anio,
        "scrapeado correctamente.",
        "Total noticias:", nrow(noticias_oea)
      )
    )
  }
  
  noticias_oea <- noticias_oea |>
    mutate(id = row_number())
  
  return(noticias_oea)
}



noticias_oea = scrapear_meses_oea(meses =1:4, anio = 2026)


#Extraemos el cuerpo de las noticias, 

#Definimos una función que extrae el texto de cada noticia haciendo request al url
# extraer_cuerpo_noticia = function(url) {
#   Sys.sleep(1) # Pausa para no sobrecargar el servidor
#   html_noticia = read_html(url)
#   
#   # El selector para el texto completo de la noticia puede variar según la estructura del sitio
#   cuerpo_noticia = html_noticia |>
#     html_elements("#rightmaincol") |> # Selector para el contenido de la noticia
#     html_elements("p" ) |> # Extraemos los párrafos para obtener el texto completo (sin headers ni partes que puedan dificultar mucho el análisis)
#     html_text2() |>
#     str_trim()
#   


extraer_cuerpo_noticia = function(url) {
  
  Sys.sleep(3)
  
  #antes de leer el HTML
  if (!grepl("^http", url)) {
    url <- paste0("https://www.oas.org/es/centro_noticias/", url)
  }
  
  html_noticia = read_html(url)
  
  cuerpo_noticia = html_noticia |>
    html_elements("#rightmaincol") |>
    html_elements("p") |>
    html_text2() |>
    str_trim()

  
  # Concatenamos todos los párrafos
  cuerpo_noticia = str_c(cuerpo_noticia, collapse = " ")
  
  # Eliminamos caracteres 
  cuerpo_noticia = str_replace_all(cuerpo_noticia, "[\\r\\n\\t]+", " ")
  
  # Eliminamos todo tipo de comillas (estándar, tipográficas, simples, dobles, backticks y porcentajes)
  cuerpo_noticia = str_replace_all(cuerpo_noticia, "[\\\"'“”‘’«»`´%()]", "")
  
  # Eliminamos espacios múltiples
  cuerpo_noticia = str_squish(cuerpo_noticia)  # parecido a trim, pero además saca dos o más espacios seguidos
  
  return(cuerpo_noticia)
}

# La probamos con una noticia cualquiera
extraer_cuerpo_noticia(
  "https://www.oas.org/es/centro_noticias/comunicado_prensa.asp?sCodigo=C-047/26"
)


# Creamos otra base de datos para almacenar los cuerpos
cuerpos_noticias_oea <- noticias_oea |>
  select(id, url) |> # mismo ID que el dataset original para hacer un join más tarde
  
  mutate(
    url = paste0(
      "https://www.oas.org/es/centro_noticias/",
      url
    ),
    cuerpo = map_chr(url, extraer_cuerpo_noticia)
  ) |>
  select(id, cuerpo) # tiene id y cuerpo nada más

# Guardamos esta tabla por separado para ahorrar memoria y no tener que volver a hacer scraping cada vez que queramos analizar el texto

# Le asignamos la misma fecha de descarga que el dataset original por consistencia
attr(cuerpos_noticias_oea, "fecha_descarga") <- pagina_html |> attr("fecha_descarga")

cuerpos_noticias_oea |> write_rds(
  file.path(data_dir, "cuerpos_noticias_oea")
)


#unimos ambas tablas: 

noticias_oea <- noticias_oea |>
  left_join(cuerpos_noticias_oea, by = "id", suffix = c("", "_nuevo")) |>
  select(id, titulo, cuerpo)





 