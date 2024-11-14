source("aas_razao.R")
source("aas_regressao.R")

dados <- partos %>%
  group_by(grupo_de_Robson) %>%
  summarise(nasc = sum(Nascimentos),
            ces = sum(Cesareas)) %>%
  mutate(prop = ces/nasc) %>%
  mutate(sigma = sqrt(prop*(1-prop))) %>%
  mutate(nasc_sigma = nasc*sigma) %>%
  mutate(peso = nasc/tamanho_populacional)

margem_esperada <- .03

margem_aas <- function(n_amostral) {
  return(
    qnorm(.975)*sqrt((tamanho_populacional-n_amostral)*p_real*(1-p_real)/
    ((tamanho_populacional-1)*n_amostral)
    ))
}

tamanho_amostral_aas <- uniroot(function(x) {margem_aas(x) - margem_esperada},
                interval = c(1, tamanho_amostral))$root 

######################

margem_aeot <- function(n_amostral) {
  variancia <- partos %>%
    group_by(grupo_de_Robson) %>%
    summarise(Nascimentos = sum(Nascimentos),
              Cesareas = sum(Cesareas)) %>%
    mutate(prop = Cesareas/Nascimentos,
           sigma = prop*(1-prop),
           peso = Nascimentos/sum(Nascimentos),
           tamanho = n_amostral*peso*sigma/sum(peso*sigma)) %>%
    summarise(variancia = sum(
      (peso^2)*(1-(tamanho/Nascimentos))*(sigma*(Nascimentos/(Nascimentos-1)))/tamanho, na.rm = TRUE
    )) %>% pull(variancia)
  
  return(qnorm(.975)*sqrt(variancia))
}

tamanho_amostral_aeot <- uniroot(function(x) {margem_aeot(x) - margem_esperada},
                                interval = c(1, tamanho_amostral))$root 

######################

margem_aas_razao <- function(n_amostral) {
  variancia <- (1 - (n_amostral/tamanho_populacional))*
    analise_razao("geral")$sigma*
    (tamanho_populacional/(tamanho_populacional-1))/n_amostral
  
  return(qnorm(.975)*sqrt(variancia))
}

tamanho_amostral_aas_razao <- uniroot(function(x) {margem_aas_razao(x) - margem_esperada},
                                 interval = c(1, tamanho_populacional))$root 

######################

margem_aeot_razao <- function(n_amostral) {
  
  tamanho_por_estrato <- numeric()
  
  for (grupo in c("grupo_1", "grupo_2", "grupo_3", "grupo_4", "grupo_5",
                  "grupo_6", "grupo_7", "grupo_8", "grupo_10" )) {
    
    tamanho_por_estrato <- append(tamanho_por_estrato,
                                  analise_razao(grupo)$tamanho*sqrt(analise_razao(grupo)$sigma))
  }
  
  tamanho_por_estrato <- tamanho_por_estrato*n_amostral/sum(tamanho_por_estrato)
  tamanho_por_estrato[10] <- tamanho_por_estrato[9]
  tamanho_por_estrato[9] <- 0
  
  dados[,"tamanho"] <- tamanho_por_estrato[c(1,10,2:9)]
  
  variancia_por_estrato <- numeric()
  
  for (grupo in c("grupo_1", "grupo_2", "grupo_3", "grupo_4", "grupo_5",
                  "grupo_6", "grupo_7", "grupo_8", "grupo_10" )) {
    
    variancia_por_estrato <- append(variancia_por_estrato,
                                    analise_razao(grupo)$sigma * (dados %>%
                                                                    filter(grupo_de_Robson == grupo) %>%
                                                                    summarise(const = (nasc/(nasc-1))*(nasc-tamanho)*peso^2/(tamanho*nasc)) %>%
                                                                    pull
                                    )
                                    )}
  variancia <- sum(variancia_por_estrato)
  return(qnorm(.975)*sqrt(variancia))
}

tamanho_amostral_aeot_razao <- uniroot(function(x) {margem_aeot_razao(x) - margem_esperada},
                                      interval = c(1, 2*tamanho_amostral))$root 

######################

margem_aas_regressao <- function(n_amostral) {
  variancia <- (1 - (n_amostral/tamanho_populacional))*p_real*(1-p_real)*
    ((tamanho_populacional/(tamanho_populacional-1))/n_amostral)*
    (1 - ((analise_regressao("geral")$sigma_xy)^2)/(analise_regressao("geral")$sigma_x2*analise_regressao("geral")$sigma_y2))
  
  return(qnorm(.975)*sqrt(variancia))
}

tamanho_amostral_aas_regressao <- uniroot(function(x) {margem_aas_regressao(x) - margem_esperada},
                                      interval = c(1, tamanho_populacional))$root 

######################

margem_aeot_regressao <- function(n_amostral) {
  
  tamanho_por_estrato <- numeric()
  
  for (grupo in c("grupo_1", "grupo_2", "grupo_3", "grupo_4", "grupo_5",
                  "grupo_6", "grupo_7", "grupo_8", "grupo_10" )) {
    
    tamanho_por_estrato <- append(tamanho_por_estrato,
                                  sqrt(analise_regressao(grupo)$tamanho*analise_regressao(grupo)$sigma_y2*
                                         (1 - ((analise_regressao(grupo)$sigma_xy)^2)/
                                            (analise_regressao(grupo)$sigma_y2*analise_regressao(grupo)$sigma_x2)))
                                  )
  }
  
  tamanho_por_estrato <- tamanho_por_estrato*n_amostral/sum(tamanho_por_estrato)
  tamanho_por_estrato[10] <- tamanho_por_estrato[9]
  tamanho_por_estrato[9] <- 0
  
  dados[,"tamanho"] <- tamanho_por_estrato[c(1,10,2:9)]
  
  variancia_por_estrato <- numeric()
  
  for (grupo in c("grupo_1", "grupo_2", "grupo_3", "grupo_4", "grupo_5",
                  "grupo_6", "grupo_7", "grupo_8", "grupo_10" )) {
    
    variancia_por_estrato <- append(variancia_por_estrato,
                                    (1-(n_amostral/tamanho_populacional))*(dados %>%
                                                                             filter(grupo_de_Robson == grupo)%>%
                                                                             summarise(const = (nasc-tamanho)*(peso^2)*prop*(1-prop)/(nasc*tamanho)) %>%
                                                                             pull)*(1-((analise_regressao(grupo)$sigma_xy)^2/
                                                                                         (analise_regressao(grupo)$sigma_x2*analise_regressao(grupo)$sigma_y2))))
    }
  variancia <- sum(variancia_por_estrato)
  return(qnorm(.975)*sqrt(variancia))
}

tamanho_amostral_aeot_regressao <- uniroot(function(x) {margem_aeot_regressao(x) - margem_esperada},
                                       interval = c(1, 2*tamanho_amostral))$root 