rm(list = ls())
#Librerías
library(rtweet)
library(readr)
library(tidytext)
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
library(tidyverse)
library(twitteR)
library(syuzhet)
library(quantmod)
library(TTR)
library(zoo)
library(randomForest)
library(lubridate)
library(widyr)

# Empresa elegida para el Análisis. TESLA
# 1- Descargamos la Información financiera de TESLA para los últimos 15 días
TSLA = getSymbols("TSLA", auto.assign = FALSE , from = '2020-04-24', to = '2020-05-05')
#Gráfico evolución del Indice
plot(TSLA[,4] , main = "Evolucion del Indice")

# 2- Descargamos los Twitters relacionados con Tesla para el Período de Estudio del Indice.
# Con esto nos permite Descargar Tweets Solo Una semana para atrás
# TESLA_tweets <- search_tweets(q = "tesla market", n = 5000,
#                               lang = "en",
#                               include_rts = FALSE,
#                               until = "2020-05-03",
#                               #geocode = lookup_coords("usa"),
#                               retryonratelimit = TRUE,
#                               type = "mixed")
#
# Musk_tweets <- search_tweets(q = "Elon Musk", n = 1000,
#                               lang = "en",
#                               include_rts = FALSE,
#                               until = "2020-05-03",
#                               #geocode = lookup_coords("usa"),
#                               retryonratelimit = FALSE,
#                               type = "mixed")
#
# save(TESLA_tweets,file="C:/Users/emili/Desktop/ORT Analitica/Big data en inversiones/Obligatorio/TESLA_tweets.RData")
#
# save(Musk_tweets,file="C:/Users/emili/Desktop/ORT Analitica/Big data en inversiones/Obligatorio/Musk_tweets.RData")
# 
# TESLA_tweetsNEW <- search_tweets(q = "tesla market", n = 4000,
#                               lang = "en",
#                               include_rts = FALSE,
#                               since = "2020-05-03",
#                               until = "2020-05-05",
#                               #geocode = lookup_coords("usa"),
#                               retryonratelimit = TRUE,
#                               type = "mixed")
# 
# Musk_tweetsNEW <- search_tweets(q = "Elon Musk", n = 1000,
#                               lang = "en",
#                               include_rts = FALSE,
#                               since = "2020-05-03",
#                               until = "2020-05-05",
#                               #geocode = lookup_coords("usa"),
#                               retryonratelimit = FALSE,
#                               type = "mixed")

# save(TESLA_tweetsNEW,file="C:/Users/emili/Desktop/ORT Analitica/Big data en inversiones/Obligatorio/TESLA_tweetsNEW.RData")
# 
# save(Musk_tweetsNEW,file="C:/Users/emili/Desktop/ORT Analitica/Big data en inversiones/Obligatorio/Musk_tweetsNEW.RData")

load("C:/Users/emili/Desktop/ORT Analitica/Big data en inversiones/Obligatorio/Musk_tweets.RData")
load("C:/Users/emili/Desktop/ORT Analitica/Big data en inversiones/Obligatorio/TESLA_tweets.RData")
load("C:/Users/emili/Desktop/ORT Analitica/Big data en inversiones/Obligatorio/TESLA_tweetsNEW.RData")
load("C:/Users/emili/Desktop/ORT Analitica/Big data en inversiones/Obligatorio/Musk_tweetsNEW.RData")
# Unir las Bases 
Bdtweet <- rbind(TESLA_tweets, Musk_tweets, TESLA_tweetsNEW, Musk_tweetsNEW)

#Selecciono las Columnas para el análisis
Bdtweet <- Bdtweet %>% select(2,3,5,13,14)

# Balanceo los tweets por dia
Bdtweet %>% mutate(diatw = format(created_at, "%m-%d")) %>% group_by(diatw) %>% summarise(n = n())
set.seed(100)
Bdtweet <- Bdtweet %>% mutate(diatw = format(created_at, "%m-%d")) %>% group_by(diatw) %>%  sample_n(100)

## Limpiea de Texto
Bdtweet$cleantext <- gsub("^[[:space:]]*","",Bdtweet$text) # Remove leading whitespaces
Bdtweet$cleantext <- gsub("[[:space:]]*$","",Bdtweet$cleantext) # Remove trailing whitespaces
Bdtweet$cleantext <- gsub(" +"," ",Bdtweet$cleantext) #Remove extra whitespaces
Bdtweet$cleantext <- gsub("'", "%%", Bdtweet$cleantext) #Replace apostrophes with %%
Bdtweet$cleantext <- iconv(Bdtweet$cleantext, "latin1", "ASCII", sub="") # Remove emojis
Bdtweet$cleantext <- gsub("<(.*)>", "", Bdtweet$cleantext) #Remove Unicodes like <U+A>
Bdtweet$cleantext <- gsub("\\ \\. ", " ", Bdtweet$cleantext) #Replace orphaned fullstops with space
Bdtweet$cleantext <- gsub("  ", " ", Bdtweet$cleantext) #Replace double space with single space
Bdtweet$cleantext <- gsub("%%", "\'", Bdtweet$cleantext) #Change %% back to apostrophes
Bdtweet$cleantext <- gsub("https(.*)*$", "", Bdtweet$cleantext) #Remove tweet URL
Bdtweet$cleantext <- gsub("\\n", "-", Bdtweet$cleantext) #Replace line breaks with "-"
Bdtweet$cleantext <- gsub("--", "-", Bdtweet$cleantext) #Remove double "-" from double line breaks
Bdtweet$cleantext <- gsub("&amp;", "&", Bdtweet$cleantext) #Fix ampersand &
Bdtweet$cleantext[Bdtweet$cleantext == " "] <- "<no text>"

# Tokenizar el texto Limpio

limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

# Se aplica la función de limpieza y tokenización a cada tweet
Bdtweet <- Bdtweet %>% mutate(texto_tokenizado = map(.x = cleantext,
                                                   .f = limpiar_tokenizar))

# Generar la base con columna de palabras por tweet
bd_tidy <- Bdtweet %>% select(-c(3,6,7)) %>% unnest()
bd_tidy <- bd_tidy %>% rename(token = texto_tokenizado)
head(bd_tidy) 

# Palabras más usadas por día
bd_tidy %>% group_by(diatw, token) %>% count(token) %>% group_by(diatw) %>% top_n(10, n) %>% arrange(diatw, desc(n)) %>% print(n=100)

# Se filtran las stopwords
bd_tidy_stop <- bd_tidy %>% filter(!(token %in% stop_words$word))
# Se repien pralabras más usadas por día
bd_tidy_stop %>% group_by(diatw, token) %>% count(token) %>% group_by(diatw) %>% top_n(10, n) %>% arrange(diatw, desc(n)) %>% print(n=100)

#Representación gráfica de las frcuencias. 
bd_tidy_stop %>% group_by(diatw, token) %>% count(token) %>% group_by(diatw) %>%
  top_n(10, n) %>% arrange(diatw, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = diatw)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~diatw,scales = "free", ncol = 3, drop = TRUE)

# Analisis de sentimiento
# Diccionario Bing. Clasificación binaria
sentimientos <- get_sentiments(lexicon = "bing")
head(sentimientos)

#Recodificción
sentimientos <- sentimientos %>%
  mutate(valor = if_else(sentiment == "negative", -1, 1))

#Aignación de senimiento a cada Token
tweets_sent <- inner_join(x = bd_tidy_stop, y = sentimientos,
                          by = c("token" = "word"))

#Se suman los sentimientos de las palabras que forman cada tweet.
#Porcentaje de tweets Negativos positivos y neutros por día

tweets_sent %>% group_by(status_id, diatw) %>%
  summarise(sentimiento_promedio = sum(valor)) %>%
  group_by(diatw) %>%
  summarise(positivos = 100 * sum(sentimiento_promedio > 0) / n(),
            neutros = 100 * sum(sentimiento_promedio == 0) / n(),
            negativos = 100 * sum(sentimiento_promedio  < 0) / n())

#Graficamente

tweets_sent %>% group_by(status_id, diatw) %>%
  summarise(sentimiento_promedio = sum(valor)) %>%
  group_by(diatw) %>%
  summarise(positivos = 100 * sum(sentimiento_promedio > 0) / n(),
            neutros = 100 * sum(sentimiento_promedio == 0) / n(),
            negativos = 100 * sum(sentimiento_promedio  < 0) / n()) %>% 
  ungroup() %>%
  gather(key = "sentimiento", value = "valor", -diatw) %>%
  ggplot(aes(x = diatw, y = valor, fill = sentimiento)) + 
  geom_col(position = "dodge", color = "black")  +
  theme_bw()

# Tweets ponderados
TweetsPonderados <- tweets_sent %>% ungroup() %>%  select(-3) %>% mutate(ValorPonderado = valor*(1+(0.01*(tweets_sent$favorite_count)+0.05*tweets_sent$retweet_count)))

#Se suman los sentimientos de las palabras que forman cada tweet.
#Porcentaje de tweets Negativos positivos y neutros por día

TweetsPonderados %>% group_by(status_id, diatw, sentiment) %>%
  summarise(sentimiento_promedio = sum(ValorPonderado)) %>%
  group_by(diatw, sentiment) %>%
  summarise(suma = sum(sentimiento_promedio)) %>% spread(key = sentiment, value = suma) %>% 
  mutate(Indice = positive/(positive+abs(negative))) %>%  
  ungroup() %>%
  ggplot(aes(x = diatw, y = Indice,  group = 1)) +
  geom_point() + 
  geom_line() + 
  labs(x = "fecha de publicación") +
  theme_bw() +
  theme(legend.position = "none")

BaseGrafIndice <- TweetsPonderados %>% group_by(status_id, diatw, sentiment) %>%
  summarise(sentimiento_promedio = sum(ValorPonderado)) %>%
  group_by(diatw, sentiment) %>%
  summarise(suma = sum(sentimiento_promedio)) %>% spread(key = sentiment, value = suma) %>% 
  mutate(Indice = positive/(positive+abs(negative)))  


