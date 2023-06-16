install.packages("academictwitteR")

library(academictwitteR)
library(ggplot2)
library(tidyverse)

library(data.table)
library(textclean)
library(tm)
library(stringr)
library(dplyr)
library(rtweet)
set_bearer()

library(academictwitteR)

# obtener tweets relacionados con Mercadona durante un período de tiempo específico, 
# filtrados por país.
  get_all_tweets(
    query = c("mercadona"),
    start_tweets = "2017-01-01T00:00:00Z",
    end_tweets = "2022-11-22T00:00:00Z",
    bearer_token = get_bearer(),
    bind_tweets=FALSE,
    country = "ES", 
    file = "tweets_mercadona",
    data_path="data_complete_mercadona/",
    n = 1000000,
  )

# Los tweets se almacenan en archivos y luego se pueden cargar yutilizar para análisis adicionales.
tweets_mercadona <- bind_tweets(data_path = "data_complete_mercadona/")


# Se crea un dataframe a partir de los datos de los tweets relacionados con Mercadona. 
df<-data.frame(author_id=tweets_mercadona$author_id, text=tweets_mercadona$text, fecha=tweets_mercadona$created_at, lang=tweets_mercadona$lang, retweet_number=tweets_mercadona$public_metrics$retweet_count, reply_number=tweets_mercadona$public_metrics$reply_count,like_number=tweets_mercadona$public_metrics$like_count)
df$hashtags=lapply(tweets_mercadona$entities$hashtags, function(x) if (length(x) == 0) NA else  toString(unlist(`[`(x, c('tag')))) )
df$users=lapply(tweets_mercadona$entities$mentions, function(x) if (length(x) == 0) NA else  array(unlist(`[`(x, c('username')))) )
df$fecha <- as.Date(df$fecha, format = "%Y-%m-%d")
df$year <- format(df$fecha, "%Y")
df$month <- format(df$fecha, "%m")
df$day <- format(df$fecha, "%d")


# creamos varios csv con los datos que queremos graficar en flourish
number_tweets_bynumberretweets<-df %>% 
  group_by(retweet_number) %>% 
  summarise(count = n())

write.csv(number_tweets_bynumberretweets, file = "tweets_by_rt.csv", row.names = FALSE)

number_tweets_bydate<-df %>% 
  group_by(fecha) %>% 
  summarise(count = n())

write.csv(number_tweets_bydate, file = "tweets_by_date.csv", row.names = FALSE)

number_tweets_bylang <- df %>% 
  group_by(lang) %>% 
  summarise(count = n())

write.csv(number_tweets_bylang, file = "tweets_by_lang.csv", row.names = FALSE)

# obtenemos un listado de los hashtags mas utilizados
all_hashtags <- unlist( 
  regmatches(df$hashtags,  gregexpr('\\w+', df$hashtags)))

freq_count <- as.data.frame(table(all_hashtags))

freq_count<-freq_count[order(freq_count$Freq, decreasing = TRUE),]

write.csv(freq_count, file = "hashtags_freq.csv", row.names = FALSE)

# Frecuencias de emojis
devtools::install_github("hadley/emo")

library(emo)
emojis<-df %>%
  mutate(emoji = ji_extract_all(text)) %>%
  unnest(cols = c(emoji)) %>%
  count(emoji, sort = TRUE) %>%
  top_n(20)

write.csv(emojis, file = "emojis_freq.csv", row.names = FALSE)

# Para el preprocesamiento creamos unas reglas personalizadas

custom_rules <- "ñ > \\~;
                 Ñ > \\^;
           ::Latin-ASCII;
                 \\~ > ñ;
                 \\^ > Ñ"

# se realiza el preprocesamiento

#gsub es para eliminar
stopw<- stopwords("spanish")
 stop<- c(stopw,"mercadona","el") # añadimos otras stopwords personalizadas
clean <- function(x){
  if (!(is.na(x))){x <- gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", x)} #Eliminar URLs
  if (!(is.na(x))){x <- gsub("@\\w+", "", x)} #Eliminar menciones
  if (!(is.na(x))){x <- gsub("#\\w+", "", x)} #Eliminar hashtags
  if (!(is.na(x))){x <- gsub("\\d+\\w*\\d*", "", x)} #Elimina números y cualquier combinación de números y letras
  
  if (!(is.na(x))){x<- stringi::stri_trans_general(x, id = custom_rules, rules = TRUE)} #Normalizar caracteres especiales o acentos.
  #Se eliminan estas lineas para el análisis de sentimientos
  if (!(is.na(x))){x <- gsub("[[:punct:]]", " ", x) } #Remove punctuation
  #Se eliminan estas lineas para el análisis de sentimientos
  if (!(is.na(x))){x <-tolower(x)} #Convertir a minúsculas
  if (!(is.na(x))){x <-iconv(x, "latin1", "ASCII", sub="")}#Quitar acentos
  
  # Se eliminan estas lineas para el análisis de sentimientos
  if (!(is.na(x))){x <-removeNumbers(x)} #Eliminar numeros
  # Se eliminan estas lineas para el análisis de sentimientos
  if (!(is.na(x))){ x <-removeWords(x,stop)} #Eliminar palabras que no aportan información
  if (!(is.na(x))){x <-gsub('\\b+RT', '', x)} #Eliminar RT
  
  if (!(is.na(x))){x <- str_replace(gsub("\\s+", " ", str_trim(x)), "B", "b")} #Eliminar mas de 1 espacios en blanco y cambiar "B" por "b"
  return(x)
}
#se aplica la función del preprocesado y se crea el nuevo df limpio
df$cleaned_text<-apply(df[c('text')],MARGIN = 1, FUN = clean)
df_filtered<-df[!(is.na(df$cleaned_text) | df$cleaned_text==""), ]
 
#se eliminan varias columnas que no necesitamos para el análisis
drop <- c("hashtags","users","text")
df_filtered = df_filtered[,!(names(df_filtered) %in% drop)]

# Se cambia el nombre a la columna del texto limpio para el análisis de sentimientos
# df_filtered <- df_filtered %>%
# rename(cleaned_text_sent = cleaned_text)
 
#se guarda el csv para despues usarlo en python
write.csv2(df_filtered,"df_filtered_minorista.csv", row.names = FALSE)
 