
# Armando DFs

# Librearías
source("~/estudios-misc/R/informe.R")
library(DataExplorer)
library(qdapRegex)


boletin_infojuba_N50_RAW <- read_csv("data/boletin_infojuba_N50_RAW.csv")

colnames(boletin_infojuba_N50_RAW) <- c("file", "boletin")
boletines <- boletin_infojuba_N50_RAW

# Encabezado
temp <- str_extract(boletines$boletin, "\\(\\s*(.*?)\\s*\\)")
bnro <- str_sub(temp, 6,8)
bfecha <- str_sub(temp, 10, -3)

# Creo df
dfBoletines <- tibble(bnro, bfecha)

# En la exploración de la tarea de segmentación de los textos (para determinar
# dato básico de materia se ecuentra variabilidad
# por lo tanto los falsos negativos son muy elevados. Descarto ese camino, por ahora

# Extracción de sumarios. Eficacia detección de sumarios 100% 
sumario_extract <- function(boletin_nro) {
  
  boletines$boletin[[boletin_nro]]
  indice_sumarios_xbol[[boletin_nro]]
  
  marca_inic = indice_sumarios_xbol[[boletin_nro]][,2] + 1
  marca_fin = indice_sumarios_xbol[[boletin_nro]][,1] - 1
  marca_fin = marca_fin[-1]
  marca_fin = c(marca_fin, nchar(boletines$boletin[boletin_nro]))
  sumarios = data_frame(marca_inic, marca_fin)
  
  bd <- vector()
  
  for (i in seq_along(sumarios$marca_inic)) {
    
    bd[i] <- str_sub(boletines$boletin[[boletin_nro]], sumarios$marca_inic[i], sumarios$marca_fin[i])
    
   }
  
  bd
}

# creando df de sumarios de causa
dfSumarios <- data_frame()

for (i in seq_along(boletines$boletin)) {
  
  dfSumarios <- dfSumarios %>% 
    bind_rows(tibble(sumario_extract(i)) %>% tibble() %>% 
                rename(sumarios = 1) %>% 
                mutate(bnro = dfBoletines$bnro[i]) %>% 
                rowwise() %>% 
                mutate(nro_fres_caratula = str_sub(sumarios, 0, str_locate(sumarios, '\nMagistrados')[1]-2),
                       caratula = str_remove_all(str_split(nro_fres_caratula,'“|”', simplify = T)[2], "\n|”"),
                       fres = unlist(ex_date(nro_fres_caratula, trim = T))[1],
                       nro_causa = as_numeric2(rm_number(str_remove_all(nro_fres_caratula, ',|\\.'), extract=TRUE)[[1]][1]), 
                       magistrados = str_match(sumarios, "\nMagistrados votantes:\\s*(.*?)\\s*\n")[, 2]))
    dfSumarios
}


# Exploracion de Datos
library(DataExplorer)
plot_missing(dfSumarios)
plot_str(dfSumarios)
plot_bar(dfSumarios)


# Borrar todo registro con al menos un NA
dfSumarios <- dfSumarios %>% drop_na()

# Extrayendo texto sumarios

vec_sumario <- vector()

for (i in seq_along(dfSumarios$sumarios)) {
  
  magistrados = str_match(dfSumarios$sumarios[i], "\nMagistrados votantes:\\s*(.*?)\\s*\n")[, 2]
  
  vec_sumario[i] <- str_sub(dfSumarios$sumarios[i],
                            str_locate(dfSumarios$sumarios[i], magistrados)[2]+1,
                            str_locate(dfSumarios$sumarios[i], "\\(Texto")[1]-1)
  
}


dfSumText <- tibble(vec_sumario)
plot_missing(dfSumText)

# Creando df 
dfSumarios <- dfSumarios %>% 
  mutate(sumario_txtcorto = dfSumText$vec_sumario) 


# creando df de extractos de doctrina

vec_doctrina <- vector()

for (i in seq_along(dfSumarios$sumarios)) {
  
  vec_doctrina[i] <- str_sub(dfSumarios$sumarios[i],
                         str_locate(dfSumarios$sumarios[[i]], "DOCTRINA")[2]+1,
                         str_locate(dfSumarios$sumarios[[i]], "<<")[1]-1)
  
}


dfDoctrina <- data_frame(vec_doctrina)


# voces 
vec_voces <- vector()

for (i in seq_along(dfDoctrina$vec_doctrina)) {
  
  voces <- unique(unlist(rm_caps_phrase(iconv(dfDoctrina$vec_doctrina[i], to='ASCII//TRANSLIT'), extract=TRUE)))
  #voces <- str_remove_all(voces, "-[IVXLCDM]+")
  #voces <- voces[!(voces  %in% c("", "-"))]
  
  voces <- str_c(voces, collapse = ",")
  
  vec_voces[i] <- voces
  
}

dfDoctrina$voces <- vec_voces
plot_missing(dfDoctrina)


# Unificar
dfSumarios <- dfSumarios %>% 
  bind_cols(dfDoctrina)

# Creando tabla final 1: causa-sumario-voces
tabla_causa_sumarios_voces <- dfSumarios %>% 
  select(-sumarios, -nro_fres_caratula, -vec_doctrina) %>% 
  select(bnro, nro_causa, fres, caratula, magistrados, sumario = sumario_txtcorto, voces)

# Creando tabla final 2: bo, causa y textos de doctrina

# sumarios de doctrina
text_doctrina <- vector()

for (i in seq_along(dfSumarios$vec_doctrina)) {
  
  temp <- dfSumarios$vec_doctrina[i]
  nro_causa <- dfSumarios$nro_causa[i]
  bnro <- dfSumarios$bnro[i]
    
  marca_inic <- str_locate_all(temp, "\n[:digit:]+\\. ")[[1]][,1]
  marca_fin <- marca_inic[-1]
  marca_fin <- c(marca_fin, nchar(temp))
  
  temp2 <- vector()
  
  for(j in seq_along(marca_inic)) {
  
    temp2[j] <- rm_caps_phrase(str_sub(temp, marca_inic[j], marca_fin[j]))
    
  }
  
  temp2 <- str_c(bnro,'&', nro_causa, "#", temp2)
  text_doctrina <- c(text_doctrina, temp2)
  
}

text_doctrina <- tibble(text_doctrina)


text_doctrina <- text_doctrina %>% separate(text_doctrina, into = c("bnro", "text"), sep = "&") 
text_doctrina <- text_doctrina %>% separate(text, into = c("nro_causa", "doctrina"), sep = "#") 

tabla_bo_causa_textdoctrina <- text_doctrina %>% 
  mutate(doctrina_nro = str_remove(str_sub(doctrina, 1,2), "\\.")) %>% 
  mutate(doctrina = str_trim(str_sub(doctrina, 3))) %>% 
  mutate(doctrina = str_remove(doctrina, "^\\. "))
  select(bnro, nro_causa, doctrina_nro, doctrina)


# formateo de tablas
tabla_bo_causa_textdoctrina <- tabla_bo_causa_textdoctrina %>% 
  mutate(bnro = as.integer(bnro), 
         nro_causa = as.integer(nro_causa), 
         doctrina_nro = as.integer(doctrina_nro)) 

tabla_causa_sumarios_voces <- tabla_causa_sumarios_voces %>% 
  mutate(bnro = as.integer(bnro), 
         fres = dmy(fres))


tabla_causa_sumarios_voces <- na.omit(tabla_causa_sumarios_voces)
tabla_bo_causa_textdoctrina <- na.omit(tabla_bo_causa_textdoctrina)


# write.table(tabla_bo_causa_textdoctrina, "tabla_bo_causa_textdoctrina.csv", sep = ",", col.names = T, 
#             row.names = F)
# write.table(tabla_causa_sumarios_voces, "tabla_causa_sumarios_voces.csv", sep = ",", col.names = T, 
#             row.names = F)


# Exportar como json

library(jsonlite)

tbsumarios_json <- toJSON(tabla_causa_sumarios_voces)
df <- fromJSON(tbsumarios_json)
write(tbsumarios_json, "tbsumarios.json")
df <- fromJSON(txt = "~/NLU_legal_domain/data/tbsumarios.json")
dplyr::setequal(tabla_causa_sumarios_voces, df)

colnames(df)
colnames(tabla_causa_sumarios_voces)
str(df)
str(tabla_causa_sumarios_voces)

tbdoctrina <- toJSON(tabla_bo_causa_textdoctrina)
write(tbdoctrina, "tbdoctrina.json")

# formateo corpus
corpus_textosfallos <- fromJSON(txt = "~/NLU_legal_domain/data/corpus_textosfallos.json")
corpus_fallosmetdat <- fromJSON(txt = "~/NLU_legal_domain/data/corpus_fallosmetdat.json")

corpus_textosfallos <- corpus_textosfallos %>% 
  filter(!str_detect(corpus_textosfallos$textos_fallo, "TEXTO COMPLETO PRIVADO"))

corpus_fallosmetdat <- corpus_fallosmetdat %>% 
  mutate(nro_causa = rm_number(id_causa, extract = T), 
         fres = dmy(corpus_fallosmetdat$fecha_res))

sum(corpus_fallosmetdat$nro_causa %in% tbsumarios$nro_causa)



# Tools------------------------------------------------------------------------

# # Descomposición de sumarios---------------------------------------------------
# 
# # nro_causa, fecha de resolución, caratula: OK
# 
# nro_fres_caratula <- str_sub(bd$`sumario_extract(30)`[1],0, str_locate(bd$`sumario_extract(30)`[1], '”'))[2]
# nro_fres_caratula
# caratula <- str_split(nro_fres_caratula,'“', simplify = T)[2] %>% str_remove_all("\n") %>% str_remove_all("”")
# fres <- unlist(ex_date(nro_fres_caratula, trim = T))
# nro <- as_numeric2(rm_number(str_remove_all(nro_fres_caratula, ',|\\.'), extract=TRUE))
# 
# # Magistrados Votantes: OK (puede haber falso negativo )
# 
# magistrados <- str_match(bd$`sumario_extract(30)`, "\nMagistrados votantes:\\s*(.*?)\\s*\n")[, 2]
# 
# # Voces-Palabras clavas: se repite con los encabezados de doctrina. La descarto
# 
# # Sumario: OK (metodo eficaz con algo de basura)
# 
# text_sumario2 <- vector()
# 
# for (i in seq_along(magistrados)) {
#     
#   firstmark <- magistrados[i]
#   lastmark <- "\\(Texto"
#     
#   temp <- str_sub(bd$`sumario_extract(30)`[i], 
#                                str_locate(bd$`sumario_extract(30)`[i], firstmark)[2]+2, 
#                                str_locate(bd$`sumario_extract(30)`[i], lastmark)[1]-2)
#   
#   text_sumario2[i] <- str_sub(temp, str_locate_all(temp, '\n')[[1]][2])
#   
# 
# }
# 
# text_sumario2 <- tibble(text_sumario2)
#  
# 
# 
# # extraccion de sumarios de doctrina y voces-----------------------------------
# # la cantidad de sumario es un dato valioso
# bd$`sumario_extract(30)`[3]
# 
# # desde doctrina a menu
# temp <- str_sub(bd$`sumario_extract(30)`[3], 
#         str_locate(bd$`sumario_extract(30)`[3], "DOCTRINA")[2]+1,
#         str_locate(bd$`sumario_extract(30)`[3], "<<")[1]-1)
# 
# # voces 
# voces <- unique(unlist(rm_caps_phrase(temp, extract=TRUE)))
# voces <- str_remove_all(voces, "-[IVXLCDM]+")
# voces <- voces[!(voces  %in% c("", "-"))]
# 
# 
# # sumarios de doctrina
# text_doctrina <- vector()
# 
# marca_inic <- str_locate_all(temp, "\n \n[:digit:]+\\. ")[[1]][,1]
# marca_fin <- marca_inic[-1]
# marca_fin <- c(marca_fin, nchar(temp))
# 
# for (i in seq_along(marca_inic)) {
# 
#   text_doctrina[i] <- rm_caps_phrase(str_sub(temp, marca_inic[i], marca_fin[i]))
#   
# }
# 
# text_doctrina <- tibble(text_doctrina)
# 
# text_doctrina %>% View()
# 
# 
# 
