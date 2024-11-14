source("leitura_dados.R")
source("aas.R")

dados_por_estabelecimento <- partos %>%
  group_by(Estabelecimento) %>%
  summarise(vaginais = sum(Nascimentos)-sum(Cesareas),
            cesareas = sum(Cesareas)) 
parametros_amostra <- dados_por_estabelecimento %>%
  as.list() %>% unlist() %>% {.[3:6]} %>% as.numeric()

#############

analise_regressao <- function(grupo) {
  
  if (grupo == "geral") {
    
    cada_grupo <- partos %>%
      mutate(vag = Nascimentos - Cesareas) %>%
      group_by(Estabelecimento) %>%
      summarise(Nascimentos = sum(Nascimentos),
                Cesareas = sum(Cesareas),
                vag = sum(vag)) %>%
      arrange(Estabelecimento) %>%
      select(Cesareas, vag) %>%
      as.list() %>% unlist()
  }
  
  if (grupo %in% c("grupo_1", "grupo_2", "grupo_3", "grupo_4", "grupo_5",
                   "grupo_6", "grupo_7", "grupo_8", "grupo_9", "grupo_10" )) {
    
    cada_grupo <- partos %>%
      filter(grupo_de_Robson == grupo) %>%
      mutate(vag = Nascimentos - Cesareas) %>%
      arrange(Estabelecimento) %>%
      select(Cesareas, vag) %>%
      as.list() %>% unlist()
  }
  
  
  tamanho <- sum(cada_grupo)
  
  sum_y <- cada_grupo[1]+cada_grupo[2]
  p <- sum_y/tamanho
  
  sum_x <- cada_grupo[1]+cada_grupo[3]
  theta <- sum_x/tamanho
  
  sigma_xy <- cada_grupo[1] - p*sum_x -theta*sum_y + tamanho*p*theta
  sigma_x2 <- sum_x - 2*sum_x*theta + tamanho*theta^2
  sigma_y2 <- sum_y - 2*sum_y*p + tamanho*p^2
  
  b <- sigma_xy/sigma_x2
  
  #print(
  #  paste(tamanho, sum_x, theta %>% round(4), sum_y, p %>% round(4), sigma_xy %>% round(2), sigma_x2 %>% round(2), b%>%round(4), sep = " & ")
  #)
  
  return(list(tamanho = tamanho,
              sum_x = sum_x, theta=theta ,
              sum_y = sum_y, p=p ,
              sigma_xy=sigma_xy,
              sigma_x2 = sigma_x2,
              sigma_y2 = sigma_y2,
              b = b))
}

#############

melhor_b <- analise_regressao("geral")$b
theta_total <- analise_regressao("geral")$theta

amostrador_aas_regressao <- function(n_, tamanho_amostra) {
  
  amostras <- extraDistr::rmvhyper(n = parametros_amostra,
                                   k = tamanho_amostra,
                                   nn = n_)
  
  sum_x <- amostras[,1] + amostras[,3]
  sum_y <- amostras[,3] + amostras[,4]
  ambos <- amostras[,3]
  
  theta <- sum_x/tamanho_amostra
  p <- sum_y/tamanho_amostra
  
  p_reg <- p + melhor_b*(theta_total - theta)
  
  b_estimado <- (ambos - p*sum_x -theta*sum_y + tamanho_amostra*p*theta)/(sum_x - 2*sum_x*theta + tamanho_amostra*theta^2)
  
  p_reg_b_est <- p + b_estimado*(theta_total - theta)
  
  return(list(sum_x = sum_x,
              sum_y = sum_y,
              interação = ambos,
              theta = theta,
              p = p,
              p_reg = p_reg,
              b_estimado = b_estimado,
              p_reg_b_est = p_reg_b_est))
}

#############

set.seed(-1201)

tamanho_amostral <- 1035

amostra <- amostrador_aas_regressao(1, tamanho_amostral)

p_real <- analise_regressao("geral")$p
var_aas_regressao <- (1-(tamanho_amostral/tamanho_populacional))*((tamanho_populacional/(tamanho_populacional-1))*p_real*(1-p_real)/1035)*(1-((analise_regressao("geral")$sigma_xy)^2/
                                                          (analise_regressao("geral")$sigma_x2*analise_regressao("geral")$sigma_y2)
))

#############

iteracoes <- 50000

amostras_aas_regressao <- data.frame(amostra = amostrador_aas_regressao(iteracoes, tamanho_amostral)$p_reg_b_est)


grafico <- ggplot(amostras_aas_regressao) +
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
                              var_aas_regressao
                            ))) +
  geom_vline(aes(xintercept = p_real, color = "Proporção de\ncesáreas\n(populacional)"),
             size = .71,
             alpha = .8) +
  geom_vline(aes(color = "Quantil 2.5%",
                 xintercept = p_real - qnorm(.975)*sqrt(var_aas_regressao)),
             size = .71,
             alpha = .8) +
  geom_vline(aes(color = "Quantil 97.5%",
                 xintercept = p_real + qnorm(.975)*sqrt(var_aas_regressao)),
             size = .71,
             alpha = .8) +
  labs(title = paste0("Distribuições empírica e assintótica ",
                      "para estimador da proporção ",
                      "de cesáreas na RMC\n(Utilizando AAS e Estimador Regressão)"),
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

ggsave(filename = "graficos/distr_aas_regressao.png",
       plot = grafico,
       width = 8,
       height = 5)
