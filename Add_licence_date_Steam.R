# Instala si no lo tienes
if (!require(stringdist)) install.packages("stringdist")
library(stringdist)
# Cargar librería
if (!require(stringdist)) install.packages("stringdist")
library(stringdist)

# Función de limpieza (quita paréntesis y palabras extra)
limpiar_nombre <- function(nombre) {
  nombre %>%
    tolower() %>%
    gsub("\\(.*?\\)", "", .) %>%
    gsub("deluxe|pre[- ]?purchase|edition|ultimate|goty|game of the year|\\:", "", .) %>%
    trimws()
}

# Data de ejemplo
df_original <- read.delim("jojo.txt")
df_mapeo <- read.delim("jeje.txt")

# Aplicar limpieza
nombres_limpios_original <- sapply(df_original$Name, limpiar_nombre)
nombres_limpios_mapeo <- sapply(df_mapeo$Artículo, limpiar_nombre)

# Función para encontrar el más parecido con limpieza
encontrar_mas_parecido <- function(nombre_limpio, nombres_limpios_mapeo, nombres_originales_mapeo) {
  distancias <- stringdist(nombre_limpio, nombres_limpios_mapeo, method = "cosine")
  mejor_indice <- which.min(distancias)
  similitud <- (1 - distancias[mejor_indice]) * 100
  return(c(nombres_originales_mapeo[mejor_indice], round(similitud, 1)))
}

# Aplicar fila a fila
resultados <- t(mapply(encontrar_mas_parecido, nombres_limpios_original, 
                       MoreArgs = list(nombres_limpios_mapeo = nombres_limpios_mapeo,
                                       nombres_originales_mapeo = df_mapeo$Artículo)))

# Añadir columnas al original
df_resultado <- df_original
df_resultado$Nombre_mas_parecido <- resultados[, 1]
df_resultado$Similitud <- as.numeric(resultados[, 2])
# Añadir columna ID del mapeo usando match
df_resultado$ID_mapeo <- df_mapeo$Fecha[match(df_resultado$Nombre_mas_parecido, df_mapeo$Artículo)]

# Subsamplear: quedarse solo con los de 100% similitud
df_similitud_100 <- subset(df_resultado, Similitud == 100)

write.table(df_similitud_100,"eaaa.txt",sep = "\t")
# Ver resultado
print(df_resultado)
