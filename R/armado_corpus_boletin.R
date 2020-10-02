# armado corpus con las sentencias aludidas en los boletines de jurisprudencia
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tidyr)


corpus_boletin <- read_csv("corpus_boletin.csv")
df <- corpus_boletin %>% 
  rename(indice = X1, textos_raw = "0")

df <- df %>% 
  mutate(indice = 1:length(indice))

corpus_clean <- df %>% 
  mutate(textos_r2 = str_split(textos_raw, "\n")) %>% 
  mutate(textos_raw_len = lengths(textos_r2)) %>% 
  select(indice, textos_raw, textos_raw_len, textos_r2) 

# Empiezo Limpieza separando vector de textos
# Elimino el pie de pagina web de todos los textos
pie <- corpus_clean$textos_r2[[1]][89:168]

lista <- list()
for(i in seq_along(corpus_clean$textos_r2)){
  lista[[i]] <- corpus_clean$textos_r2[[i]][!corpus_clean$textos_r2[[i]] %in% pie]
}

# Separo datos de Fallos y Textos de Resoluciones
datos_fallos <- vector()
textos_completos <- vector()

for(i in seq_along(lista)){
  
  text <- str_c(as_vector(lista[[i]], "character"), collapse = "#")
  text <- str_split(text, pattern = "Imprimir#|#Descargar")
  
  datos_fallos[[i]] <- text[[1]][1]
  textos_completos[[i]] <-  text[[1]][3]
  
}

df_2 <- tibble(datos_fallos, textos_completos)

# Control de no-disponibles
any(!is.na(df_2$datos_fallos))
any(!is.na(df_2$textos_completos))

# Descomposición del extracto de datos de fallos
# 1: resultado no satisfactorio
# df_datf <- df_2 %>% 
#   mutate(indice = 1:length(datos_fallos)) %>% 
#   mutate(datos_fallos = str_remove_all(datos_fallos, "SCBA - JUBA#Inicio#VISUALIZACION DEL TEXTO COMPLETO#DATOS DEL FALLO#")) %>% 
#   select(indice, datos_fallos) %>% 
#   separate_rows(datos_fallos, sep = ":")

#2: Creo Modelo de Procesamiento con regex
df_2$datos_fallos[1]
#materia
str_match(df_2$datos_fallos[1], "Materia:(.*?)#Tipo")
#tipo_de_fallo
str_match(df_2$datos_fallos[1], "Tipo de Fallo:(.*?)#Tribunal")
#tribunal_emisor
str_match(df_2$datos_fallos[1], "Tribunal Emisor:(.*?)#Causa")
#causa
str_match(df_2$datos_fallos[1], "Causa:(.*?)#Fecha")
#fecha
str_match(df_2$datos_fallos[1], "Fecha:(.*?)#Nro")
#nro_reg_int
str_match(df_2$datos_fallos[1], "Interno:(.*?)#Caratula")
#caratula
str_match(df_2$datos_fallos[1], "Caratula:(.*?)#Caratula Publica")
#caratula_publica
str_match(df_2$datos_fallos[1], "Caratula Publica:(.*?)#Magistrad")
#magistrados_votantes
str_match(df_2$datos_fallos[1], "Magistrados Votantes:(.*?)#Tribunal")
#tribunal_origen
str_match(df_2$datos_fallos[1], "Origen:(.*?)#NNF")
#nnf
str_match(df_2$datos_fallos[1], "NNF:(.*?)#Observación:")
#observ
str_match(df_2$datos_fallos[1], "Observación:(.*?)#Sentencias")
#sent_anulada
str_match(df_2$datos_fallos[1], "Anuladas:(.*?)#Alcance")
#alcance
str_match(df_2$datos_fallos[1], "Alcance:(.*?)#Público")
#publico
str_match(df_2$datos_fallos[1], "Público(.*?)#Iniciales")
#iniciales
str_match(df_2$datos_fallos[1], "Iniciales(.*?)#Reserva")
#identidad
str_match(df_2$datos_fallos[1], "identidad(.*?)#Observaciones")
#observaciones
str_match(df_2$datos_fallos[1], "Observaciones:(.*?)#")

# Procesamiento de vector texto para descomposición por campo
# eliminar asterisco al principio
df_2 <- df_2 %>% 
  mutate(materia = str_remove(str_match(datos_fallos, "Materia:(.*?)#Tipo")[,2], "#")) %>% 
  mutate(fecha = str_remove(str_match(datos_fallos, "Fecha:(.*?)#Nro")[,2], "#")) %>%  
  mutate(tipo_fallo  = str_remove(str_match(datos_fallos, "Tipo de Fallo:(.*?)#Tribunal")[,2], "#")) %>%
  mutate(tribunal_emisor  = str_remove(str_match(datos_fallos, "Tribunal Emisor:(.*?)#Causa")[,2], "#")) %>%
  mutate(id_causa  = str_remove(str_match(datos_fallos, "Causa:(.*?)#Fecha")[,2], "#")) %>%
  mutate(fecha_res  = str_remove(str_match(datos_fallos, "Fecha:(.*?)#Nro")[,2], "#")) %>%
  mutate(nro_reg_int  = str_remove(str_match(datos_fallos, "Interno:(.*?)#Caratula")[,2], "#")) %>%
  mutate(caratula  = str_remove(str_match(datos_fallos, "Caratula:(.*?)#Caratula Publica")[,2], "#")) %>%
  mutate(caratula_pub = str_remove(str_match(datos_fallos, "Caratula Publica:(.*?)#Magistrad")[,2], "#")) %>%
  mutate(magistrados_votantes = str_remove(str_match(datos_fallos, "Magistrados Votantes:(.*?)#Tribunal")[,2], "#")) %>%
  mutate(tribunal_origen = str_remove(str_match(datos_fallos, "Origen:(.*?)#NNF")[,2], "#")) %>%
  mutate(nnf  = str_remove(str_match(datos_fallos, "NNF:(.*?)#Observación:")[,2], "#")) %>%
  mutate(observ  = str_remove(str_match(datos_fallos, "Observación:(.*?)#Sentencias")[,2], "#")) %>%
  mutate(sent_anulada  = str_remove(str_match(datos_fallos, "Anuladas:(.*?)#Alcance")[,2], "#")) %>%
  mutate(alcance  = str_remove(str_match(datos_fallos, "Alcance:(.*?)#Público")[,2], "#")) %>%
  mutate(publico = str_remove(str_match(datos_fallos, "Público(.*?)#Iniciales")[,2], "#")) %>%
  mutate(iniciales  = str_remove(str_match(datos_fallos, "Iniciales(.*?)#Reserva")[,2], "#")) %>%
  mutate(reserva_identidad  = str_remove(str_match(datos_fallos, "identidad(.*?)#Observaciones")[,2], "#")) %>%
  mutate(observaciones  = str_remove(str_match(datos_fallos, "Observaciones:(.*?)#")[,2], "#")) 

head(df_2, 10) %>% View()

corpus_clean <- corpus_clean %>% 
  select(-textos_r2) %>% 
  bind_cols(df_2 %>% select(2:21)) 

corpus_clean <- corpus_clean %>% 
  rename(textos_fallo = textos_completos)

corpus_clean <- corpus_clean %>% 
  select(-textos_raw)

colnames(corpus_clean)

# Guardar data frame corpus
write.table(corpus_clean, "~/NLU_legal_domain/data/corpus_clean.csv", 
            col.names = T, row.names = F, sep = "\t")

corpus_textosfallo <- corpus_clean %>% select(indice, textos_fallo)
corpus_fallosmetdat <- corpus_clean %>% select(-textos_fallo)

# Guardar json

write_json(corpus_fallosmetdat, "~/NLU_legal_domain/data/corpus_bo_fallosmetdat.json")
write_json(corpus_textosfallo, "~/NLU_legal_domain/data/corpus_bo_textosfallos.json")