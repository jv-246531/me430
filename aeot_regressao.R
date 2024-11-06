source("aas_regressao.R")

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

dados_por_estabelecimento <- partos %>%
  group_by(Estabelecimento, grupo_de_Robson) %>%
  summarise(vaginais = sum(Nascimentos)-sum(Cesareas),
            cesareas = sum(Cesareas)) 
parametros_amostra <- dados_por_estabelecimento %>%
  as.list() %>% unlist() %>% {.[3:6]} %>% as.numeric()

#############

amostrador_aeot_regressao <- function(n_) {
  
  sums_x <- numeric()
  sums_y <- numeric()
  thetas <- numeric()
  many_p <- numeric()
  many_p_reg <- numeric()
  many_b <- numeric()
  
  for (grupo in c("grupo_1", "grupo_2", "grupo_3", "grupo_4", "grupo_5",
                  "grupo_6", "grupo_7", "grupo_8", "grupo_9", "grupo_10" )) {
    
    tamanho_estrato <- dados %>%
      filter(grupo_de_Robson == grupo) %>%
      select(tamanho) %>%
      pull
    
    parametros_amostra <- partos %>%
      filter(grupo_de_Robson == grupo) %>%
      mutate(vaginais = Nascimentos - Cesareas) %>%
      arrange(Estabelecimento) %>%
      select(Cesareas, vaginais) %>%
      as.list() %>% unlist()
    
    amostras <- extraDistr::rmvhyper(n = parametros_amostra,
                                     k = tamanho_estrato,
                                     nn = n_)
    
    sum_x <- amostras[,1] + amostras[,3]
    sum_y <- amostras[,1] + amostras[,2]
    
    theta <- sum_x/tamanho_estrato
    p <- sum_y/tamanho_estrato
    
    p_reg <- p + analise_regressao(grupo)$b*(analise_regressao(grupo)$theta - theta)
    
    b_estimado <- (amostras[,1] - p*sum_x -theta*sum_y + tamanho_estrato*p*theta)/(sum_x - 2*sum_x*theta + tamanho_estrato*theta^2)
    
    sums_x <- cbind(sums_x, sum_x)
    sums_y <- cbind(sums_y, sum_y)
    thetas <- cbind(thetas, theta)
    many_p <- cbind(many_p, p)
    many_p_reg <- cbind(many_p_reg, p_reg)
    many_b <- cbind(many_b, b_estimado)
  }
  
  pesos <-dados$peso[c(1,3:10,2)]
  
  final_p <- apply(many_p_reg, 1, function(x) {
    sum(pesos * x, na.rm = TRUE)
  }) + pesos[9]
  
  return(list(sums_x = sums_x,
              sums_y = sums_y,
              thetas = thetas,
              many_p = many_p,
              many_p_reg = many_p_reg,
              many_b = many_b,
              final_p = final_p))
}

#############

set.seed(518)

amostra <- amostrador_aeot_regressao(1)

var_grupo <- numeric()

for (grupo in c("grupo_1", "grupo_2", "grupo_3", "grupo_4", "grupo_5",
                "grupo_6", "grupo_7", "grupo_8", "grupo_10" )) {
  
  var_grupo <- append(var_grupo,
                      (1-(tamanho_amostral/tamanho_populacional))*(dados %>%
                         filter(grupo_de_Robson == grupo)%>%
                         summarise(const = (nasc-tamanho)*(peso^2)*prop*(1-prop)/(nasc*tamanho)) %>%
                         pull)*(1-((analise_regressao(grupo)$sigma_xy)^2/
                                     (analise_regressao(grupo)$sigma_x2*analise_regressao(grupo)$sigma_y2))))
}

var_aeot_regressao <- sum(var_grupo)

#############

iteracoes <- 50000

amostras_aeot_regressao <- data.frame(amostra = amostrador_aeot_regressao(iteracoes)$final_p)

grafico <- ggplot(amostras_aeot_regressao) +
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
                              var_aeot_regressao
                            ))) +
  geom_vline(aes(xintercept = p_real, color = "Proporção de\ncesáreas\n(populacional)"),
             size = .71,
             alpha = .8) +
  geom_vline(aes(color = "Quantil 2.5%",
                 xintercept = p_real - qnorm(.975)*sqrt(var_aeot_regressao)),
             size = .71,
             alpha = .8) +
  geom_vline(aes(color = "Quantil 97.5%",
                 xintercept = p_real + qnorm(.975)*sqrt(var_aeot_regressao)),
             size = .71,
             alpha = .8) +
  labs(title = paste0("Distribuições empírica e assintótica ",
                      "para estimador da proporção ",
                      "de cesáreas na RMC\n(Utilizando AEot e Estimador Regressão)"),
       x = "Proporção",
       y = "Densidade de probabilidade",
       color = NULL, fill = NULL) +
  scale_fill_manual(values = c("Empírica" = "#e8e51c")) +
  scale_color_manual(values = c("Assintótica" = "#6e4619",
                                "Proporção de\ncesáreas\n(populacional)" = "#1e3e4e",
                                "Quantil 2.5% (assintótico)" = "#d02020",
                                "Quantil 97.5% (assintótico)" = "#802090")) +
  scale_x_continuous(breaks = seq(0.61, 0.72, by = 0.01),
                     #labels = scales::number_format(accuracy = 0.001)
  ) +
  theme_classic()

ggsave(filename = "graficos/distr_aeot_regressao.png",
       plot = grafico,
       width = 8,
       height = 5)
