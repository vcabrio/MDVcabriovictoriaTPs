

install.packages("tm")
library(ggplot2)


output_dir <- here("TP2/output")

# Cargo del archivo generado por processing.R 
noticias_lemas <- read_rds(
  file.path(output_dir, "processed_text.rds")
)

glimpse(noticias_lemas)


# Conteo de frecuencia por documento y término

frecuencia_noticias_oea <- noticias_lemas |> 
  count(id, lemma, name = "n")  |>  
  arrange(id)

# Convertimos el data frame a una Document-Term Matrix (formato ancho)
matriz_dtm <- frecuencia_noticias_oea|> 
  cast_dtm( document = id, term = lemma, value = n )


# Filtramos palabras clave de la OEA
terminos_de_interes <- c( "democracia", "derechos", "seguridad", "paz", "desarrollo")
matriz_dtm_de_interes <- matriz_dtm[, colnames(matriz_dtm) %in% terminos_de_interes]
matriz_dtm_de_interes


# Mostramos unicamente las primeras 20 lineas de la matriz
as.matrix(matriz_dtm_de_interes)[1:20, ]


# Convertimos la DTM a un data frame para ggplot
dtm_df <- as.data.frame(as.matrix(matriz_dtm_de_interes)) |>
  rownames_to_column(var = "id") |>
  pivot_longer(-id, names_to = "lemma", values_to = "n") |> 
  group_by(lemma) |> # Agrupamos por palabra
  summarise(frecuencia_total = sum(n)) # Sumamos todas las apariciones

# Graficamos la frecuencia de los términos de interés
grafico_final = ggplot(dtm_df, aes(x = lemma, y = frecuencia_total)) +
  geom_col(fill = "pink") +
  labs(
    title = "Frecuencia de términos de interés en la OEA",
    x = "Término",
    y = "Frecuencia",
    caption = "Fuente: Sitio oficial de la OEA"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


ggsave(
  filename = here("TP2", "output", "frecuencia_terminos.png"),
  plot = grafico_final,
  width = 10,
  height = 9,
  dpi = 300
)




