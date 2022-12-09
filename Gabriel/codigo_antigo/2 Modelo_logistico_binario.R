
## 80% of the sample size
smp_size <- floor(0.8 * nrow(CEIS))

## set the seed to make your partition reproducible
set.seed(12345)
train_ind <- sample(seq_len(nrow(CEIS)), size = smp_size)

train <- CEIS[train_ind, ]
test  <- CEIS[-train_ind, ]
remove(train_ind,smp_size)

modelo  <- glm(tipo_sancao_3 ~ tipo_orgao+tipo_de_pessoa+abragencia, family = binomial(link = "logit"), data = train)
uf_orgao_sancionador
summary(modelo)

library(sjPlot)
plot_model(modelo)
