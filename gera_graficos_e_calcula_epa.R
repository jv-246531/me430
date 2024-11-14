variancias <- numeric()

source("aas.R")

variancias <- append(variancias, var_aas)
rm(list = setdiff(ls(), "variancias"))

source("aeot.R")

variancias <- append(variancias, var_aeot)
rm(list = setdiff(ls(), "variancias"))

source("aeot_razao.R")

variancias <- append(variancias, c(var_aas_razao,
                                   var_aeot_razao))
rm(list = setdiff(ls(), "variancias"))

source("aeot_regressao.R")

variancias <- append(variancias, c(var_aas_regressao,
                                   var_aeot_regressao))
rm(list = setdiff(ls(), "variancias"))

names(variancias) <- c("aas", "aeot", "aas_r", "aeot_r", "aas_reg", "aeot_reg")
outer(variancias, variancias, "/") %>% round(4)
