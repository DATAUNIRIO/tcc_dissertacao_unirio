#CONFIGURA O DIRETORIO DE TRABALHO
setwd("C:/Users/rianm/Documents/Rian/Mestrado_UFF/Artigos/Atropelamento_pedestres")

unique(unlist(strsplit(as.character(pessoas$tipo_envolvido), ",")))
unique(unlist(strsplit(as.character(pessoas$tipo_veiculo), ",")))

# SELECIONA OS ATROPELAMMENTOS DE PEDESTRES
peds <- subset(pessoas, (tipo_envolvido == "Pedestre" | tipo_envolvido == "Pedestre            ") & ano > 2016)
remove(pessoas)

# VERIFICACAO DE PESSOAS DUPLICADAS
a <- peds[duplicated(peds$pesid), ]
remove(a)

#PADRAO DADOS FALTANTES
library(visdat)
a <- subset(peds, select = -c(data_inversa, tipo_envolvido, id, br, tipo_acidente, causa_acidente, classificacao_acidente, id_PRF, pesid, horario, km, municipio, id_veiculo, sentido_via, marca))
a <- a[, c(6:14, 5, 3, 2, 1, 4)]
tiff("test.tiff", units="in", width=6, height=3, res=300)
vis_miss(a, warn_large_data = FALSE)
dev.off()

#EXCLUI AS VARIAVEIS DA BASE PEDS
a <- a[,-c(1, 3, 6, 8)]
peds <- a[complete.cases(a),]
remove(a)

#INSERE A COLUNA REGIAO NA BASE PEDS
peds$regiao <- NA
norte <- c("AC", "AP", "AM", "PA", "RO", "RR", "TO")
nordeste <- c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")
sul <- c("PR", "SC", "RS")
sudeste <- c("RJ", "SP", "MG", "ES")
centro <- c("GO", "MS", "MT", "DF")

peds$id <- 1:nrow(peds)
rownames(peds) <- 1:nrow(peds)
for (id in peds$id) {
  if (peds[id, "uf"] %in% centro){
    peds[id, "regiao"] <- "Centro-Oeste"
  } else if (peds[id, "uf"] %in% sul){
    peds[id, "regiao"] <- "Sul"
  } else if (peds[id, "uf"] %in% sudeste){
    peds[id, "regiao"] <- "Sudeste"
  } else if (peds[id, "uf"] %in% norte){
    peds[id, "regiao"] <- "Norte"
  } else if (peds[id, "uf"] %in% nordeste) {
    peds[id, "regiao"] <- "Nordeste"
  } else {
  }
}
remove(id, centro, norte, nordeste, sul, sudeste)

#INCLUI A COLUNA ID NA BASE PEDS
peds$id <- 1:nrow(peds)
rownames(peds) <- 1:nrow(peds)
peds <- peds[, c(12, 1:11)]

#SELECIONA OS MODELOS PELO METODO STEPWISE
install.packages("nnet")
install.packages("Hmisc")
install.packages("MASS")
library(nnet)
library(Hmisc)
library(MASS)

#TRANSFORMA BICICLETA EM OUTROS
peds[peds$tipo_veiculo == "Bicicleta", "tipo_veiculo"] <- "Outros"

#POSICIONA A REGIAO SUDESTE COMO REFERENCIA
peds[peds$regiao == "Sudeste", "regiao"] <- "1.Sudeste"
peds[peds$regiao == "Sul", "regiao"] <- "2.Sul"
peds[peds$regiao == "Nordeste", "regiao"] <- "3.Nordeste"
peds[peds$regiao == "Norte", "regiao"] <- "4.Norte"
peds[peds$regiao == "Centro-Oeste", "regiao"] <- "5.Centro-Oeste"

#POSICIONA A FASE PLENO_DIA COMO REFERENCIA
peds[peds$fase_dia == "Pleno Dia", "fase_dia"] <- "1.Pleno Dia"
peds[peds$fase_dia == "Amanhecer", "fase_dia"] <- "2.Amanhecer"
peds[peds$fase_dia == "Anoitecer", "fase_dia"] <- "3.Anoitecer"
peds[peds$fase_dia == "Plena Noite", "fase_dia"] <- "4.Plena Noite"

# INSERE AS COLUNAS TIPO_DIA E ESTACAO
peds$tipo_dia <- NA
peds[peds$dia_semana %in% c("segunda-feira", "terça-feira", "quarta-feira", "quinta-feira", "sexta-feira"), "tipo_dia"] <- "Dia_util"
peds[peds$dia_semana %in% c("sábado", "domingo"), "tipo_dia"] <- "Fim_semana"

peds$estacao <- NA
peds[peds$mes %in% c("janeiro", "fevereiro", "março"), "estacao"] <- "Verao"
peds[peds$mes %in% c("abril", "maio", "junho"), "estacao"] <- "Outono"
peds[peds$mes %in% c("julho", "agosto", "setembro"), "estacao"] <- "Inverno"
peds[peds$mes %in% c("outubro", "novembro", "dezembro"), "estacao"] <- "Primavera"

##FORWARD
step(multinom(estado_fisico ~ 1, data = peds), direction = "forward", scope =~ tipo_pista + uso_solo + tipo_veiculo +
       sexo + fase_dia + dia_semana + mes + ano + regiao,)

step(multinom(estado_fisico ~ 1, data = peds), direction = "forward", scope =~ tipo_pista + uso_solo + tipo_veiculo +
       sexo + fase_dia + tipo_dia + estacao + ano + regiao,)

#SALVA O MODELO
modelo <- multinom(formula = estado_fisico ~ tipo_veiculo + fase_dia + 
                     uso_solo + sexo + tipo_pista + ano + regiao, data = peds)

modelo <- multinom(formula = estado_fisico ~ tipo_veiculo + fase_dia + uso_solo + regiao + 
                     sexo + tipo_pista + tipo_dia + ano, data = peds)


#RESUMO DOS COEFICIENTES
install.packages("lmtest")
install.packages("tidyverse")
install.packages("AER")
library(lmtest)
library(tidyverse)
library(AER)
wald <- coeftest(modelo)

#INTERCEPTOS DO MODELO NULO
summary(multinom(formula = estado_fisico ~ 1, data = peds))

#VALIDACAO DO MODELO

##LIKELIHOOD RATIO TEST
modelo_saturado <- multinom(formula = estado_fisico ~ tipo_pista + uso_solo + tipo_veiculo +
                              sexo + fase_dia + dia_semana + mes + ano + regiao, data = peds)

modelo_nulo <- multinom(formula = estado_fisico ~ 1, data = peds)

verossim <- lrtest(modelo, modelo_nulo)

##HOSMER-LEMESHOW
install.packages("generalhoslem")
library(generalhoslem)
hosmer_leme <- logitgof(peds$estado_fisico, fitted(modelo), g = 4)

#EXPORTA A TABELA DO TESTE WALD COMO CSV
stars <- symnum(wald[, ncol(wald)], corr = FALSE, na = FALSE, 
                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                symbols = c("***", "**", "*", ".", " "))

out <- cbind(as.data.frame.matrix(wald), Stars = format(stars))
write.csv(out, "test2.csv")
