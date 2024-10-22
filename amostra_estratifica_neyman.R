library(tidyverse)
library(dplyr)

partos <- read_delim("dados/partos_instrucao.csv",
                     delim = ";",
                     locale = locale(encoding = "ISO-8859-1")) %>%
  filter(Total >= 500, `Instruçao da mae` != "Total") %>%
  select(!Ignorado) %>%
  filter(complete.cases(.)) %>%
  mutate(prop = Cesario / Total,
         desvio = sqrt(prop * (1 - prop)))

normalizador <- sum(partos$desvio * partos$Total)

partos <- partos %>%
  mutate(tamanho_estrato = round(2000 * desvio * Total / normalizador))

n_simulations <- 10000
distr_parametro <- numeric(n_simulations)

set.seed(1001)

for (i in 1:n_simulations) {
  
  partos$observado <- mapply(function(v, c, size) {
    rhyper(nn = 1,
           m = c,
           n = v,
           k = size)
  }, partos$Vaginal, partos$Cesario, partos$tamanho_estrato)
  
  total_observado <- sum(partos$observado)
  total_estrato <- sum(partos$tamanho_estrato)
  estatistica <- total_observado / total_estrato
  
  distr_parametro[i] <- estatistica
}
