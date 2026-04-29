
install.packages("stopwords")

library(tidytext)


# Definimos rutas de entrada y salida, y verificamos su existencia

input_dir = here("TP2/data")
output_dir = here("TP2/output")

# Verificamos si existe; si no, la creamos incluyendo carpetas padres si faltan
if (!dir.exists(output_dir)) {
  message("Creando el directorio: ", output_dir)
  dir.create(output_dir, recursive = TRUE) 
}
if (!dir.exists(input_dir)) {
  stop("Error: El directorio de entrada no existe: ", input_dir)
}

noticias_oea <- read_rds(
  file.path(input_dir, "noticias_oea.rds")
)

#me fijo si me abrio el script. 
glimpse(noticias_oea)


#Limpiamos las noticias antes de lematizar. 

noticias_limpio <- noticias_oea |>
  mutate(
    texto_completo = str_c(titulo, ". ", cuerpo),
    # Limpiamos caracteres extraños y normalizamos
    texto_completo = str_replace_all(texto_completo, "[\\r\\n\\t]+", " "),
    texto_completo = str_replace_all(texto_completo, "[[:punct:]]",  " "),
    texto_completo = str_replace_all(texto_completo, "[[:digit:]]",  " "),
    texto_completo = str_squish(texto_completo)  # Elimina espacios múltiples
  ) |>
  select(-cuerpo) # Eliminamos la columna original de cuerpo para ahorrar espacio



# Verificamos el resultado
noticias_limpio |>
  select(id, titulo, texto_completo) |>
  head(2)


# Cargar modelo de udpipe 
m_es <- udpipe_download_model(language = "spanish",overwrite=FALSE)
modelo_es <- udpipe_load_model(m_es$file_model)


# Lematiza el texto completo
noticias_lemas <- udpipe_annotate(
  modelo_es, 
  x = noticias_limpio$texto_completo, 
  doc_id = noticias_limpio$id
) |> # devuelve un objeto de clase "udpipe_annotation"
  as.data.frame() |>
  mutate(id=as.integer(doc_id)) |> 
  select(id, lemma, upos) 

# Agregamos los titulares y las fuentes
noticias_lemas <- noticias_lemas |> 
  left_join(
    noticias_limpio |> select(id, titulo, texto_completo), 
    by = "id"
  )


# Cargamos las stop words en español del paquete tidytext
data("stopwords", package = "stopwords")


stop_es <- stopwords::stopwords("es")
stop_en <- stopwords::stopwords("en")
stop_words <- tibble(lemma = c(stop_es, stop_en))

# Convertir a tibble
noticias_lemas <- as_tibble(noticias_lemas) |>
  # Filtrar solo palabras (no puntuación)
  filter(upos %in% c("NOUN", "ADJ", "VERB")) |>
  # Convertir los lemmas a minúsculas
  mutate(lemma = str_to_lower(lemma)) |>
  # Eliminar stopwords y palabras muy cortas
  anti_join(stop_words, by = "lemma") |>
  filter(str_length(lemma) > 2)m


#Guardado del frame final en capreta output
write_rds(
  noticias_lemas,
  file.path(output_dir, "processed_text.rds")
)




