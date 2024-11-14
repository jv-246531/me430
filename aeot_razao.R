source("aas_razao.R")

tamanho_amostral <- 1035

tamanho_por_estrato <- numeric()

for (grupo in c("grupo_1", "grupo_2", "grupo_3", "grupo_4", "grupo_5",
                "grupo_6", "grupo_7", "grupo_8", "grupo_10" )) {
  
  tamanho_por_estrato <- append(tamanho_por_estrato,
                                analise_razao(grupo)$tamanho*sqrt(analise_razao(grupo)$sigma))
}

tamanho_por_estrato <- ((tamanho_por_estrato*tamanho_amostral/sum(tamanho_por_estrato)) +
                          0.05) %>%
  round()

tamanho_por_estrato[10] <- tamanho_por_estrato[9]
tamanho_por_estrato[9] <- 0

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
  mutate(tamanho = tamanho_por_estrato[c(1,10,2:9)]) %>%
  mutate(peso = nasc/tamanho_populacional)

#############

amostrador_aeot_razao <- function(n_) {
  
  sums_x <- numeric()
  sums_y <- numeric()
  sums_xy <- numeric()
  thetas <- numeric()
  many_p <- numeric()
  many_r <- numeric()
  props <- numeric()
  
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
    sum_xy <- amostras[,1]
    
    theta <- sum_x/tamanho_estrato
    p <- sum_y/tamanho_estrato
    
    r <- p/theta
    prop <- r*analise_razao(grupo)$theta
    
    sums_x <- cbind(sums_x, sum_x)
    sums_y <- cbind(sums_y, sum_y)
    sums_xy <- cbind(sums_xy, sum_xy)
    thetas <- cbind(thetas, theta)
    many_p <- cbind(many_p, p)
    many_r <- cbind(many_r, r)
    props <- cbind(props, prop)
  }
  
  pesos <- dados$peso[c(1,3:10,2)]
  
  final_p <- apply(props, 1, function(x) {
    sum(pesos * x, na.rm = TRUE)
  }) + pesos[9]
  
  return(list(sums_x = sums_x,
              sums_y = sums_y,
              sums_xy = sums_xy,
              thetas = thetas,
              many_p = many_p,
              many_r = many_r,
              props = props,
              final_p = final_p))
}

#############
  
set.seed(98)

amostra <- amostrador_aeot_razao(1)

var_grupo <- numeric()

for (grupo in c("grupo_1", "grupo_2", "grupo_3", "grupo_4", "grupo_5",
                "grupo_6", "grupo_7", "grupo_8", "grupo_10" )) {
  
  var_grupo <- append(var_grupo,
                      analise_razao(grupo)$sigma * (dados %>%
                                                      filter(grupo_de_Robson == grupo)%>%
                                                      summarise(const = (nasc/(nasc-1))*(nasc-tamanho)*peso^2/(tamanho*nasc)) %>%
                                                      pull))
}

var_aeot_razao <- sum(var_grupo)

vies_grupo <- numeric()

for (grupo in c("grupo_1", "grupo_2", "grupo_3", "grupo_4", "grupo_5",
                "grupo_6", "grupo_7", "grupo_8", "grupo_10" )) {
  
  vies_grupo <- append(vies_grupo,
                      vies(r = analise_razao(grupo)$r,
                           am = (dados %>% filter(grupo_de_Robson == grupo) %>% select(tamanho) %>% pull),
                           pop = analise_razao(grupo)$tamanho,
                           th = analise_razao(grupo)$theta,
                           p_ = analise_razao(grupo)$p,
                           interacao = analise_razao(grupo)$sum_xy)*(dados %>%
                            filter(grupo_de_Robson == grupo)%>%
                            select(peso) %>%
                            pull))
}

vies_aeot_razao <- sum(vies_grupo)

#############

iteracoes <- 50000

amostras_aeot_razao <- data.frame(amostra = amostrador_aeot_razao(iteracoes)$final_p)

grafico <- ggplot(amostras_aeot_razao) +
  geom_histogram(aes(x = amostra, y = ..density.., fill = "Empírica"),
                 binwidth = 2/tamanho_amostral,
                 color = "#68353c",
                 alpha = .65) +
  stat_function(aes(color = "Assintótica"),
                fun = dnorm,
                size = 1,
                alpha = .85,
                args = list(mean = p_real + vies_aeot_razao,
                            sd = sqrt(
                              var_aeot_razao
                            ))) +
  geom_vline(aes(xintercept = p_real + vies_aeot_razao, color = "Proporção de\ncesáreas\n(populacional) + viés"),
             size = .71,
             alpha = .8) +
  geom_vline(aes(color = "Quantil 2.5%",
                 xintercept = p_real + vies_aeot_razao - qnorm(.975)*sqrt(var_aeot_razao)),
             size = .71,
             alpha = .8) +
  geom_vline(aes(color = "Quantil 97.5%",
                 xintercept = p_real + vies_aeot_razao + qnorm(.975)*sqrt(var_aeot_razao)),
             size = .71,
             alpha = .8) +
  labs(title = paste0("Distribuições empírica e assintótica ",
                      "para estimador da proporção ",
                      "de cesáreas na RMC\n(Utilizando AEot e Estimador Razão)"),
       x = "Proporção",
       y = "Densidade de probabilidade",
       color = NULL, fill = NULL) +
  scale_fill_manual(values = c("Empírica" = "#e8e51c")) +
  scale_color_manual(values = c("Assintótica" = "#6e4619",
                                "Proporção de\ncesáreas\n(populacional) + viés" = "#1e3e4e",
                                "Quantil 2.5%" = "#d02020",
                                "Quantil 97.5%" = "#802090")) +
  scale_x_continuous(breaks = seq(0.58, 0.78, by = 0.01)) +
  theme_classic()

ggsave(filename = "graficos/distr_aeot_razao.png",
       plot = grafico,
       width = 8,
       height = 5)
