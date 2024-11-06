source("leitura_dados.R")
source("aas.R")

tamanho_amostral <- 1035

tamanho_populacional <- partos %>%
  summarise(nasc = sum(Nascimentos)) %>%
  pull

dados <- partos %>%
  group_by(grupo_de_Robson) %>%
  summarise(nasc = sum(Nascimentos),
            ces = sum(Cesareas)) %>%
  mutate(prop = ces/nasc) %>%
  mutate(sigma = sqrt(prop*(1-prop))) %>%
  mutate(nasc_sigma = nasc*sigma) %>%
  mutate(tamanho = round((tamanho_amostral*nasc_sigma/sum(nasc_sigma))-.0185)) %>%
  mutate(peso = nasc/tamanho_populacional) 

sum(dados$tamanho)

constante_grupo9 <- dados$peso[10]

amostrador_aeot <- function(n_, tamanho_amostral, total_estratos = FALSE) {
  
  amostras <- numeric(n_) + constante_grupo9
  estratos <- numeric()
  prop <- numeric(constante_grupo9)
  
  for(i in c(1,3:10,2)) {
    
    ces <- dados[i, "ces"] %>% pull()
    nasc <- dados[i, "nasc"] %>% pull()
    tamanho <- dados[i, "tamanho"] %>% pull()
    peso <- dados[i, "peso"] %>% pull()
    
    amostra <- rhyper(nn = n_,
                      m = ces,
                      n = nasc - ces,
                      k = tamanho)
    
    estratos <- append(estratos, amostra)
    prop <- append(prop, amostra/tamanho)
    if(tamanho > 0) {
      amostras <- amostras + amostra*peso/tamanho
    }
  }
  
  if (total_estratos) {
    return(list(prop = amostras,
                grupo = estratos,
                prop_grupo = prop))
  }
  return(amostras)
    
  }

#######################

set.seed(57)



tamanho_amostral <- 1035
tamanho_populacional <- partos %>%
  summarise(nasc = sum(Nascimentos)) %>%
  pull
quantidade_cesarias <- partos %>%
  summarise(ces = sum(Cesareas)) %>%
  pull



amostra_aeot <- amostrador_aeot(1, tamanho_amostral, total_estratos = TRUE)

iteracoes <- 50000

amostras_aeot <- data.frame(amostra = amostrador_aeot(iteracoes, tamanho_amostral))

var_aeot <- sum((dados[-10,]$nasc^2/(33964^2))*(dados[-10,]$nasc - dados[-10,]$tamanho)*dados[-10,]$sigma^2/(
  (dados[-10,]$nasc-1)*dados[-10,]$tamanho
  ))

p_real <- quantidade_cesarias/tamanho_populacional




grafico <- ggplot(amostras_aeot) +
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
                              var_aeot
                            ))) +
  geom_vline(aes(xintercept = p_real, color = "Proporção de\ncesáreas\n(populacional)"),
             size = .71,
             alpha = .8) +
  geom_vline(aes(color = "Quantil 2.5%",
                 xintercept = p_real - qnorm(.975)*sqrt(var_aeot)),
             size = .71,
             alpha = .8) +
  geom_vline(aes(color = "Quantil 97.5%",
                 xintercept = p_real + qnorm(.975)*sqrt(var_aeot)),
             size = .71,
             alpha = .8) +
  labs(title = paste0("Distribuições empírica e assintótica ",
                      "para estimador da proporção ",
                      "de cesáreas na RMC\n(Utilizando AEot)"),
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
                     ) +
  theme_classic()

ggsave(filename = "graficos/distr_aeot.png",
       plot = grafico,
       width = 8,
       height = 5)
