source("leitura_dados.R")
source("aas.R")

dados_por_estabelecimento <- partos %>%
  group_by(Estabelecimento) %>%
  summarise(vaginais = sum(Nascimentos)-sum(Cesareas),
            cesareas = sum(Cesareas)) 
parametros_amostra <- dados_por_estabelecimento %>%
  as.list() %>% unlist() %>% {.[3:6]} %>% as.numeric()

#############

analise_razao <- function(grupo) {
  
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
  
  sum_xy <- cada_grupo[1]
  
  r <- p/theta
  
  sigma <- (cada_grupo[1]*(1-r)^2 + cada_grupo[2] + cada_grupo[3]*r^2)/tamanho
  
  #print(
  #  paste(tamanho, sum_x, theta %>% round(4), sum_y, p %>% round(4), r %>% round(4), sigma %>% round(4), sep = " & ")
  #)
  
  return(list(tamanho = tamanho,
              sum_x = sum_x, theta=theta ,
              sum_y = sum_y, p=p,
              sum_xy = sum_xy,
              r=r ,
              sigma=sigma ))
           
           
  
}

theta_total <- analise_razao("geral")$theta

#############

amostrador_aas_razao <- function(n_, tamanho_amostra) {
  
  amostras <- extraDistr::rmvhyper(n = parametros_amostra,
                                   k = tamanho_amostra,
                                   nn = n_)
  
  sum_x <- amostras[,1] + amostras[,3]
  sum_y <- amostras[,3] + amostras[,4]
  ambos <- amostras[,3]
  
  theta <- sum_x/tamanho_amostra
  p <- sum_y/tamanho_amostra
  
  r <- p/theta
  prop <- r*theta_total
    
    return(list(sum_x = sum_x,
                sum_y = sum_y,
                interação = ambos,
                theta = theta,
                p = p,
                r = r,
                prop = prop))
}

#############

tamanho_amostral <- 1035

set.seed(67)

amostra <- amostrador_aas_razao(1, tamanho_amostral)

var_aas_razao <- (1-(tamanho_amostral/tamanho_populacional))*(tamanho_populacional/(tamanho_populacional-1))*analise_razao("geral")$sigma/tamanho_amostral

vies <- function(r, am, pop, th, p_, interacao) {
  return(
    (1/th)*((r*(1-(am/pop))*th*(1-th)*(pop/(pop-1))/am) + (pop-am)*(pop*th*p_ - interacao)/((pop-1)*pop*am))
  )
}

vies_aas_razao <- vies(r = analise_razao("geral")$r,
                      am = tamanho_amostral,
                      pop = tamanho_populacional,
                      p = analise_razao("geral")$p,
                      th = analise_razao("geral")$theta,
                      interacao = parametros_amostra[3])

#############

iteracoes <- 50000

amostras_aes_razao <- data.frame(amostra = amostrador_aas_razao(iteracoes, tamanho_amostral)$prop)


p_real <- analise_razao("geral")$p


grafico <- ggplot(amostras_aes_razao) +
  geom_histogram(aes(x = amostra, y = ..density.., fill = "Empírica"),
                 binwidth = 2/tamanho_amostral,
                 color = "#68353c",
                 alpha = .65) +
  stat_function(aes(color = "Assintótica"),
                fun = dnorm,
                size = 1,
                alpha = .85,
                args = list(mean = p_real + vies_aas_razao,
                            sd = sqrt(
                              var_aas_razao
                            ))) +
  geom_vline(aes(xintercept = p_real, color = "Proporção de\ncesáreas\n(populacional)"),
             size = .71,
             alpha = .8) +
  geom_vline(aes(color = "Quantil 2.5%",
                 xintercept = p_real - qnorm(.975)*sqrt(var_aas_razao)),
             size = .71,
             alpha = .8) +
  geom_vline(aes(color = "Quantil 97.5%",
                 xintercept = p_real + qnorm(.975)*sqrt(var_aas_razao)),
             size = .71,
             alpha = .8) +
  labs(title = paste0("Distribuições empírica e assintótica ",
                      "para estimador da proporção ",
                      "de cesáreas na RMC\n(Utilizando AAS e Estimador Razão)"),
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

ggsave(filename = "graficos/distr_aas_razao.png",
       plot = grafico,
       width = 8,
       height = 5)
