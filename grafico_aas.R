amostras_aas <- readRDS("bancos/amostras_aas.rds")

source("leitura_dados.R")

tamanho_amostral <- 1035
tamanho_populacional <- partos %>%
  summarise(nasc = sum(Nascimentos)) %>%
  pull
quantidade_cesarias <- partos %>%
  summarise(ces = sum(Cesareas)) %>%
  pull

p_real <- quantidade_cesarias/tamanho_populacional
quantilestimado <- quantile(amostras_aas$amostra, probs = c(.025,.975))

grafico <- ggplot(amostras_aas) +
  geom_histogram(aes(x = amostra, y = ..density.., fill = "Empírica"),
                 binwidth = 2/tamanho_amostral,
                 color = "#68353c",
                 alpha = .65) +
  stat_function(aes(color = "Assintótica"),
                fun = dnorm,
                size = 1,
                alpha = .85,
                args = list(mean = p_real,
                            sd = sqrt(
                              (1-(tamanho_amostral/tamanho_populacional))*((amostra_aas*(1-amostra_aas))/(tamanho_amostral-1))
                            ))) +
  geom_vline(aes(xintercept = p_real, color = "Proporção de\ncesáreas\n(populacional)"),
             size = .71,
             alpha = .8) +
  geom_vline(aes(color = "Quantil 2.5%",
                 xintercept = quantilestimado[1]),
             size = .71,
             alpha = .8) +
  geom_vline(aes(color = "Quantil 97.5%",
                 xintercept = quantilestimado[2]),
             size = .71,
             alpha = .8) +
  labs(title = "Distribuições empírica e assintótica para estimador da proporção de cesáreas na RMC",
       x = "Proporção",
       y = "Densidade de probabilidade",
       color = NULL, fill = NULL) +
  scale_fill_manual(values = c("Empírica" = "#e8e51c")) +
  scale_color_manual(values = c("Assintótica" = "#6e4619",
                                "Proporção de\ncesáreas\n(populacional)" = "#1e3e4e",
                                "Quantil 2.5%" = "#d02020",
                                "Quantil 97.5%" = "#d02020")) +
  theme_minimal()

ggsave(filename = "graficos/distr_aas.png",
       plot = grafico,
       width = 8,
       height = 5)