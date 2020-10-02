# Informe 
library(dplyr)
library(knitr)
library(apgyeDSL)
library(apgyeJusEROrganization)
library(apgyeOperationsJusER)
library(stringr)
library(janitor)
library(kableExtra)
library(tidyr)
library(tibble)
library(ggplot2)
library(RColorBrewer)
library(colorRamps)
library(lubridate)
library(scales)
library(apgyeProcesamiento)
library(gghighlight)

# Listado de Magistrados y Funcionarios
magistrados_y_secretarios <- function(dbprod) {
  mag <- dbprod %>%
    apgyeDSL::apgyeTableData('personal_planta_ocupada') %>%
    left_join(dbprod %>% apgyeDSL::apgyeTableData('personal_personas') %>%
                select(idagente, categoria, apellido, nombres), by=c("idagente")
    ) %>%
    filter(grepl("JUEZ|SECRETARIO|VOCAL|FISCAL|DEFENSOR", categoria)) %>%
    collect()
  
  magist_func_id_agentes <- mag$idagente %>% unique()
  # add "0" ti IDs for External Agent
  magist_func_id_agentes[length(magist_func_id_agentes)+1] <- "0"
  
  magist_func_id_agentes 

} 

magist_func_id_agentes <- magistrados_y_secretarios(DB_PROD())

outputTable <- function (table, caption, row_group_label_position = "identity") {
  if(row_group_label_position == "identity") {
    total_rows <-  c(which(table[, 1] == "Total"), which(table[, 2] == "Total"), which(table[, 3] == "Total"))
  } else {
    total_rows <-  c(which(table[, 2] == "Total"), which(table[, 3] == "Total"))
  }
  table %>% rename_all(.funs = stringr::str_replace_all, pattern = "_", replacement=" ") %>% 
    kable("latex", caption = caption,
          align = 'c', longtable = TRUE, booktabs = T ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), latex_options = c("repeat_header"),
                  full_width = F, font_size = 10) %>% 
    row_spec(total_rows, bold = T) %>% 
    collapse_rows(columns = 1:2, row_group_label_position = row_group_label_position)
  
}
