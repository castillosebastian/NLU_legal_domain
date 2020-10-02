# Abrir tablas de sumarios y fallos de jurisprudencia
source("~/estudios-misc/R/informe.R")
library(jsonlite)
library(DataExplorer)
library(qdapRegex)

tbsumarios <- fromJSON(txt = "~/NLU_legal_domain/data/tbsumarios.json") %>% 
  mutate(nro_causa = as.integer(nro_causa)) %>% 
  select(-c(fres, caratula))
tbdoctrina <- fromJSON(txt = "~/NLU_legal_domain/data/tbdoctrina.json") %>% 
  select(-c(bnro))

corpus_fallostext <- fromJSON(txt = "~/NLU_legal_domain/data/corpus_bo_textosfallos.json")
corpus_fallosmetdat <- fromJSON(txt = "~/NLU_legal_domain/data/corpus_bo_fallosmetdat.json")

corpus_fallosmetdat <- corpus_fallosmetdat %>% 
  mutate(nro_causa = rm_number(id_causa, extract = T), fres = dmy(fecha_res)) %>% 
  mutate(nro_causa = as.integer(nro_causa)) %>% 
  select(-c(publico, alcance, reserva_identidad, iniciales,nnf, observ, id_causa,
            sent_anulada, observaciones, fecha, fecha_res, nro_reg_int)) %>% 
  select(indice, nro_causa, fres, everything()) %>% 
  drop_na() %>% 
  left_join(tbsumarios, by = "nro_causa")

corpus_fallostext <- corpus_fallostext %>% 
  drop_na() %>% 
  left_join(corpus_fallosmetdat %>% select(indice, nro_causa), by = "indice") %>% 
  filter(nro_causa > 10) %>% 
  select(-indice)

# Consolido tablas
corpus_fallosmetdat <- corpus_fallosmetdat %>% 
  semi_join(corpus_fallostext, by = "nro_causa") %>% 
  select(-indice)

corpus_fallosdoctrina <- tbdoctrina %>% 
  semi_join(corpus_fallostext, by = "nro_causa")


# guardo resultados
tabladoctrina <- toJSON(corpus_fallosdoctrina)
tablametadat <- toJSON(corpus_fallosmetdat)
tablafallos <- toJSON(corpus_fallostext)

write(tabladoctrina, "tbdoctrina.json")
write(tablametadat, "tbmetdat.json")
write(tablafallos, "tbfallos.json")




