library(tidyverse)
library(dplyr)

nascimentos <- read_delim("dados/g_robson_nascimentos.csv",
                     delim = ";",
                     locale = locale(decimal_mark = ",")) %>%
  filter(Estabelecimento %in% c("0000000 PÚBLICO", "0000000 NÃO PÚBLICO")) %>%
  mutate(Estabelecimento = recode(Estabelecimento,
                                  "0000000 PÚBLICO" = "público",
                                  "0000000 NÃO PÚBLICO" = "não_público")) %>%
  rename_with(~ str_replace_all(., "Grupo (\\d+)", "grupo_\\1"), 
              starts_with("Grupo ")) %>%
  pivot_longer(cols = c(paste0("grupo_", 1:10), "Branco/Ignorado", "Todos"),
               names_to = "grupo_de_Robson",
               values_to = "Nascimentos") %>%
  filter(grupo_de_Robson != "Todos")

cesareas <- read_delim("dados/g_robson_cesareas.csv",
                       delim = ";",
                       locale = locale(decimal_mark = ",")) %>%
  filter(Estabelecimento %in% c("0000000 PÚBLICO", "0000000 NÃO PÚBLICO")) %>%
  mutate(Estabelecimento = recode(Estabelecimento,
                                  "0000000 PÚBLICO" = "público",
                                  "0000000 NÃO PÚBLICO" = "não_público")) %>%
  rename_with(~ str_replace_all(., "Grupo (\\d+)", "grupo_\\1"), 
              starts_with("Grupo ")) %>%
  pivot_longer(cols = c(paste0("grupo_", 1:10), "Branco/Ignorado", "Todos"),
               names_to = "grupo_de_Robson",
               values_to = "Cesareas")

partos <- inner_join(nascimentos, cesareas, by = c("Estabelecimento", "grupo_de_Robson"))
rm(nascimentos, cesareas)

partos <- partos %>%
  filter(grupo_de_Robson != "Branco/Ignorado")