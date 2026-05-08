# Prueba

# carga un formulario, rubrica y catálogo

library(readr)
df_raw <- read_csv("data/respuestas_LCA.csv")
rubrica <- read_excel("data/Rubricas_2027-1.xlsx", sheet = "compilada")
catalogo_perfiles <- read_excel("data/CATALOGO_LCA.xlsx", sheet = "perfiles")

