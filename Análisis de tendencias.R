## Se instalan paquetes necesarios  (en caso de no tenerlos)
install.packages("tm")
install.packages("slam")
install.packages("text2vec")
install.packages("dplyr")
install.packages("readxl")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("here")
### En caso de que el paquete "text2vec" no funcione realizar estos pasos:
install.packages("remotes")
remotes::install_cran("text2vec")

## Se procede a cargar los paquetes
library(tm)
library(slam)
library(text2vec)
library(dplyr)
library(readxl)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(tidyr)
library(ggplot2)

## Se cargan los datos originarios de Excel y se les asigna una etiqueta
hombres_europa <- read_excel("data/Europa Sur hombres.xlsx")
mujeres_europa <- read_excel("data/Europa Sur mujeres.xlsx")
hombres_hispano <- read_excel("data/Hispanoamérica hombres.xlsx")
mujeres_hispano <- read_excel("data/Hispanoamérica mujeres.xlsx")

## Se agregan columnas por género y región
hombres_europa <- hombres_europa %>%
  mutate(genero = "Hombre", region = "Europa Sur")

mujeres_europa <- mujeres_europa %>%
  mutate(genero = "Mujer", region = "Europa Sur")

hombres_hisp <- hombres_hispano %>%
  mutate(genero = "Hombre", region = "Hispanoamérica")

mujeres_hisp <- mujeres_hispano %>%
  mutate(genero = "Mujer", region = "Hispanoamérica")

## Se unen todos los datos en un único dataframe
datos <- bind_rows(hombres_europa, mujeres_europa, hombres_hisp, mujeres_hisp)

## Paso previo a averiguar las palabras más repetidas: con el stopwords eliminamos posibles palabras que sirva de nexo entre unas y otras o bien palabras que puedan cruzarse pero no pertenezcan a las búsquedas originales
stopwords_personalizadas <- c(stopwords("spanish"), "hombre", "mujer", "de", "la", 
                              "el", "en", "los", "las", "un", "una", "y", "con", "para")

## Se realiza también una limpieza de texto
palabras_filtradas <- datos %>%
  unnest_tokens(palabra, Tendencia) %>%
  filter(!palabra %in% stopwords_personalizadas)

## Se crea texto concatenado por género y región
textos_por_region <- datos %>%
  group_by(region) %>%
  summarise(texto = paste(Tendencia, collapse = " "), .groups = "drop")

textos_por_genero <- datos %>%
  group_by(genero) %>%
  summarise(texto = paste(Tendencia, collapse = " "), .groups = "drop")

## Ahora, se hace una función para crear nubes de palabras
crear_nube <- function(texto, nombre) {
  corpus <- Corpus(VectorSource(texto))
  corpus <- tm_map(corpus, content_transformer(tolower))       
  corpus <- tm_map(corpus, removePunctuation)                  
  corpus <- tm_map(corpus, removeNumbers)                      
  corpus <- tm_map(corpus, removeWords, stopwords_personalizadas)  
  corpus <- tm_map(corpus, stripWhitespace)                    
  
  dtm <- TermDocumentMatrix(corpus)
  matriz <- as.matrix(dtm)
  frecuencias <- sort(rowSums(matriz), decreasing = TRUE)
  
  wordcloud(
    words = names(frecuencias),
    freq = frecuencias,
    min.freq = 2,
    max.words = 100,
    colors = brewer.pal(8, "Dark2"),
    random.order = FALSE
  )
}

### Nubes de palabras por región
par(mfrow = c(1, nrow(textos_por_region)))
for (i in 1:nrow(textos_por_region)) {
  crear_nube(textos_por_region$texto[i], textos_por_region$region[i])
  title(main = textos_por_region$region[i], cex.main = 1.5)
}

### Nubes de palabras por género
par(mfrow = c(1, nrow(textos_por_genero)))
for (i in 1:nrow(textos_por_genero)) {
  crear_nube(textos_por_genero$texto[i], textos_por_genero$genero[i])
  title(main = textos_por_genero$genero[i], cex.main = 1.5)
}

## A continuación, se obtienen las palabras más frecuentes para género y región
### Palabras más frecuentes por género
frecuencias_genero <- palabras_filtradas %>%
  count(genero, palabra, sort = TRUE) %>%
  group_by(genero) %>%
  top_n(10, n) %>%
  ungroup() %>%
  arrange(genero, desc(n))

print(frecuencias_genero, n = Inf)

### Gráfico comparativo por género
frecuencias_genero %>%
  ggplot(aes(x = reorder(palabra, n), y = n, fill = genero)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~genero, scales = "free_y") +
  labs(title = "Palabras más frecuentes por género",
       x = NULL, y = "Frecuencia")

### Palabras más frecuentes por región
frecuencias_region <- palabras_filtradas %>%
  count(region, palabra, sort = TRUE) %>%
  group_by(region) %>%
  top_n(10, n) %>%
  ungroup() %>%
  arrange(region, desc(n))

print(frecuencias_region, n = Inf)

### Gráfico comparativo por región
frecuencias_region %>%
  ggplot(aes(x = reorder(palabra, n), y = n, fill = region)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~region, scales = "free_y") +
  labs(title = "Palabras más frecuentes por región",
       x = NULL, y = "Frecuencia")

## Análisis secundario: con este análisis que se realiza a parte se eliminan aquellas palabras comunes entre grupos para detectar posibles diferencias
### Por género
palabras_comunes_genero <- palabras_filtradas %>%
  group_by(palabra) %>%
  summarise(grupos = n_distinct(genero)) %>%
  filter(grupos > 1) %>%
  pull(palabra)

frecuencias_genero_sin_comunes <- palabras_filtradas %>%
  filter(!palabra %in% palabras_comunes_genero) %>%
  count(genero, palabra, sort = TRUE) %>%
  group_by(genero) %>%
  top_n(10, n) %>%
  ungroup()

frecuencias_genero_sin_comunes %>%
  ggplot(aes(x = reorder(palabra, n), y = n, fill = genero)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~genero, scales = "free_y") +
  labs(title = "Palabras más distintivas por género",
       x = NULL, y = "Frecuencia")

### Por región
palabras_comunes_region <- palabras_filtradas %>%
  group_by(palabra) %>%
  summarise(grupos = n_distinct(region)) %>%
  filter(grupos > 1) %>%
  pull(palabra)

frecuencias_region_sin_comunes <- palabras_filtradas %>%
  filter(!palabra %in% palabras_comunes_region) %>%
  count(region, palabra, sort = TRUE) %>%
  group_by(region) %>%
  slice_max(order_by = n, n = 10) %>%
  ungroup()

frecuencias_region_sin_comunes %>%
  ggplot(aes(x = reorder(palabra, n), y = n, fill = region)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~region, scales = "free_y") +
  labs(title = "Palabras más distintivas por región",
       x = NULL, y = "Frecuencia")

# A continuación se buscan los cambios a lo largo del tiempo de las tendencias
## Se detectan las columnas cuyo nombre parece una fecha
fechas <- names(datos)[grepl("^\\d{4}-\\d{2}-\\d{2}$", names(datos))]

# Se convierten las fechas en un formato largo
fechas <- names(datos)[grepl("^\\d{4}-\\d{2}-\\d{2}$", names(datos))]

datos_largos <- datos %>%
  pivot_longer(
    cols = all_of(fechas),
    names_to = "Fecha",
    values_to = "Valor"
  ) %>%
  mutate(
    Fecha = as.Date(Fecha),
    Valor = as.numeric(Valor)
  )

# Se agrupan las 5 tendencias más comunes tendencia y género, y se calcula la suma total del valor
top5_por_genero <- datos_largos %>%
  group_by(genero, Tendencia) %>%
  summarise(total = sum(Valor, na.rm = TRUE), .groups = "drop") %>%
  group_by(genero) %>%
  slice_max(order_by = total, n = 5) %>%
  pull(Tendencia) %>%
  unique()  # Extrae nombres únicos de tendencias

# Se filtran datos solo para esas tendencias
datos_filtrados <- datos_largos %>%
  filter(Tendencia %in% top5_por_genero)

# Ahora se hacen los gráficos con los cambios en las tendencias más comunes, según la región y el género
## Gráfico de líneas
ggplot(datos_filtrados, aes(x = Fecha, y = Valor, color = Tendencia)) +
  geom_line(size = 1) +
  facet_grid(genero ~ region) +
  labs(title = "Evolución de las Top 5 tendencias por género y región",
       x = "Fecha", y = "Nivel de tendencia") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = "Set1")

----------

# Para abrir la presentación!!!
list.files("data", pattern = "\\.xlsx$")
getwd()
list.files("data")

system("git log --oneline")
system("git status")
system("git push")
