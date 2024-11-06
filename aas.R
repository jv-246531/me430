#source("leitura_dados.R")

set.seed(57)

amostrador_aas <- function(n_, tamanho_amostra) {
  
  amostras <- numeric()
  
  amostra <- partos %>%
      summarise(nascimentos = sum(Nascimentos),
                cesareas = sum(Cesareas)) %>%
      as.list() %>% unlist %>%
      {rhyper(nn = n_,
              m = .[2],
              n = .[1] - .[2],
              k = tamanho_amostral)}
    
    amostras <- append(amostras, amostra)
  
  return(amostras)
}

tamanho_amostral <- 1035
tamanho_populacional <- partos %>%
  summarise(nasc = sum(Nascimentos)) %>%
  pull
quantidade_cesarias <- partos %>%
  summarise(ces = sum(Cesareas)) %>%
  pull

p_real <- quantidade_cesarias/tamanho_populacional

amostra_aas <- amostrador_aas(1, tamanho_amostral)/tamanho_amostral

iteracoes <- 50000


amostras_aas <- data.frame(amostra = amostrador_aas(iteracoes, tamanho_amostral)/tamanho_amostral)

if (!file.exists("bancos/amostras_aas.rds")) {
  saveRDS(object = amostras_aas, file = "bancos/amostras_aas.rds")
}


