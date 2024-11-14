source("leitura_dados.R")

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

tamanho_amostral <- 1035
tamanho_populacional <- partos %>%
  summarise(nasc = sum(Nascimentos)) %>%
  pull
quantidade_cesarias <- partos %>%
  summarise(ces = sum(Cesareas)) %>%
  pull

p_real <- quantidade_cesarias/tamanho_populacional
quantilestimado <- quantile(amostras_aas$amostra, probs = c(.025,.975))

var_aas <- (tamanho_populacional-tamanho_amostral)*p_real*(1-p_real)/
  ((tamanho_populacional-1)*tamanho_amostral)

grafico <- ggplot(amostras_aas) +
  geom_histogram(aes(x = amostra, y = ..density.., fill = "Empírica"),
                 binwidth = 3/tamanho_amostral,
                 color = "#68353c",
                 alpha = .65) +
  stat_function(aes(color = "Assintótica"),
                fun = dnorm,
                size = 1,
                alpha = .85,
                args = list(mean = p_real,
                            sd = sqrt(
                              var_aas
                            ))) +
  geom_vline(aes(xintercept = p_real, color = "Proporção de\ncesáreas\n(populacional)"),
             size = .71,
             alpha = .8) +
  geom_vline(aes(color = "Quantil 2.5%",
                 xintercept = p_real - qnorm(.975)*sqrt(var_aas)),
             size = .71,
             alpha = .8) +
  geom_vline(aes(color = "Quantil 97.5%",
                 xintercept = p_real + qnorm(.975)*sqrt(var_aas)),
             size = .71,
             alpha = .8) +
  labs(title = paste0("Distribuições empírica e assintótica ",
                      "para estimador da proporção ",
                      "de cesáreas na RMC\n(Utilizando AAS)"),
       x = "Proporção",
       y = "Densidade de probabilidade",
       color = NULL, fill = NULL) +
  scale_fill_manual(values = c("Empírica" = "#e8e51c")) +
  scale_color_manual(values = c("Assintótica" = "#6e4619",
                                "Proporção de\ncesáreas\n(populacional)" = "#1e3e4e",
                                "Quantil 2.5%" = "#d02020",
                                "Quantil 97.5%" = "#802090")) +
  scale_x_continuous(breaks = seq(0.610, 0.720, by = 0.010), 
                     #labels = scales::number_format(accuracy = 0.001)
  )+
  theme_classic()

ggsave(filename = "graficos/distr_aas.png",
       plot = grafico,
       width = 8,
       height = 5)
