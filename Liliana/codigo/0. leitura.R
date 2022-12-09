#--------------------------------------------------------------
# FASE 1 - IMPORTACAO
#--------------------------------------------------------------

library(readr)
library(magrittr)
temp <- list.files(path ='C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Liliana/sdg-resumo/series/' , pattern="*.csv")
temp2 = paste0('C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Liliana/sdg-resumo/series/',temp)
myfiles = lapply(temp2, read_csv)
remove(temp2)
temp<-gsub("\\.csv","",temp)
temp
#--------------------------------------------------------------

#--------------------------------------------------------------
# FASE 2 - JUNTAR AS 45 BASES DE DADOS
#--------------------------------------------------------------

# remover - unidade de analise nao e pais (eh continente)
myfiles[[1]] |> View() # remover

myfiles[[2]] |> View() # selecionar
str(myfiles[[2]])
table(myfiles[[2]]$`Food Waste Sector`)

myfiles[[3]] |> View() # selecionar
table(myfiles[[3]]$`Food Waste Sector`)

myfiles[[4]] |> View()
library(dplyr)
dim(myfiles[[4]])
myfiles[[4]] <- myfiles[[4]] %>% filter(Sex=='BOTHSEX')
dim(myfiles[[4]])

myfiles[[5]] |> View()
dim(myfiles[[5]])
myfiles[[5]] <- myfiles[[5]] %>% filter(Sex=='BOTHSEX')
dim(myfiles[[5]])

myfiles[[6]] |> View()
myfiles[[7]] |> View()
myfiles[[8]] |> View()
myfiles[[9]] |> View()
myfiles[[10]] |> View()

View(myfiles[[11]])
View(myfiles[[12]])
View(myfiles[[13]])
View(myfiles[[14]])
table(myfiles[[14]]$`Type of waste treatment`)
dim(myfiles[[14]])
myfiles[[14]] <- myfiles[[14]] %>% filter(`Type of waste treatment`=='RECYCL')
dim(myfiles[[14]])

View(myfiles[[15]]) # Desconsiderar
table(myfiles[[15]]$Cities)

myfiles[[16]] |> View()
dim(myfiles[[16]])
myfiles[[16]] <- myfiles[[16]] %>% filter(Activity=='TOTAL')
dim(myfiles[[16]])

View(myfiles[[17]])
View(myfiles[[18]])
View(myfiles[[19]]) # Desconsiderar
table(myfiles[[19]]$`Level/Status`)

View(myfiles[[20]])
View(myfiles[[21]])
temp[21]
dim(myfiles[[21]])
myfiles[[21]] <- myfiles[[21]] %>% filter(Sex=='BOTHSEX' & Location=='ALLAREA' & Age=='ALLAGE')
dim(myfiles[[21]])
View(myfiles[[22]])
View(myfiles[[23]])
table(myfiles[[23]]$`Education level`=='UPPSEC')
dim(myfiles[[23]])
myfiles[[23]] <- myfiles[[23]] %>% filter(`Education level`=='UPPSEC')
dim(myfiles[[23]])

View(myfiles[[24]])
table(myfiles[[24]]$`Education level`)
myfiles[[24]] <- myfiles[[24]] %>% filter(`Education level`=='UPPSEC')

View(myfiles[[25]])
table(myfiles[[25]]$`Education level`)
myfiles[[25]] <- myfiles[[25]] %>% filter(`Education level`=='UPPSEC')

View(myfiles[[26]])
table(myfiles[[26]]$`Education level`)
myfiles[[26]] <- myfiles[[26]] %>% filter(`Education level`=='UPPSEC')

View(myfiles[[27]])
table(myfiles[[27]]$`Education level`)
myfiles[[27]] <- myfiles[[27]] %>% filter(`Education level`=='UPPSEC')

View(myfiles[[28]])
table(myfiles[[28]]$`Education level`)
myfiles[[28]] <- myfiles[[28]] %>% filter(`Education level`=='UPPSEC')

View(myfiles[[29]])
dim(myfiles[[29]])
myfiles[[29]] <- myfiles[[29]] %>% filter(Sex=='BOTHSEX')
dim(myfiles[[29]])

View(myfiles[[30]])
dim(myfiles[[30]])
myfiles[[30]] <- myfiles[[30]] %>% filter(`Education level`=='UPPSEC' & Sex=='BOTHSEX')
dim(myfiles[[30]])

View(myfiles[[31]])
dim(myfiles[[31]])
myfiles[[31]] <- myfiles[[31]] %>% filter(Location=='ALLAREA')
dim(myfiles[[31]])

View(myfiles[[32]])
dim(myfiles[[32]])
myfiles[[32]] <- myfiles[[32]] %>% filter(Location=='ALLAREA')
dim(myfiles[[32]])

View(myfiles[[33]])
dim(myfiles[[33]])
myfiles[[33]] <- myfiles[[33]] %>% filter(Location=='ALLAREA')
dim(myfiles[[33]])

View(myfiles[[34]])
dim(myfiles[[34]])
myfiles[[34]] <- myfiles[[34]] %>% filter(Location=='ALLAREA')
dim(myfiles[[34]])

View(myfiles[[35]])

View(myfiles[[36]])
dim(myfiles[[36]])
myfiles[[36]] <- myfiles[[36]] %>% filter(Sex=='BOTHSEX'& Age=='15+')
dim(myfiles[[36]])

View(myfiles[[37]])
dim(myfiles[[37]])
myfiles[[37]] <- myfiles[[37]] %>% filter(Location=='ALLAREA')
dim(myfiles[[37]])

View(myfiles[[38]])
View(myfiles[[39]])

dim(myfiles[[39]])
myfiles[[39]] <- myfiles[[39]] %>% filter(Sex=='BOTHSEX')
dim(myfiles[[39]])

View(myfiles[[40]])
dim(myfiles[[40]])
myfiles[[40]] <- myfiles[[40]] %>% filter(Sex=='BOTHSEX')
dim(myfiles[[40]])

View(myfiles[[41]])
dim(myfiles[[41]])
myfiles[[41]] <- myfiles[[41]] %>% filter(Sex=='BOTHSEX'& Age=='15+')
dim(myfiles[[41]])

View(myfiles[[42]])
View(myfiles[[43]])
View(myfiles[[44]])
dim(myfiles[[44]])
myfiles[[44]] <- myfiles[[44]] %>% filter(Location=='ALLAREA')
dim(myfiles[[44]])

View(myfiles[[45]])
dim(myfiles[[45]])
myfiles[[45]] <- myfiles[[45]] %>% filter(Location=='ALLAREA')
dim(myfiles[[45]])

#-------------------------------------------------------
library(janitor)
library(dplyr)

AG_PRD_FIESS <- myfiles[[4]] %>% clean_names()
AG_PRD_FIESSN <- myfiles[[5]] %>% clean_names()

AG_PRD_FIESS <- AG_PRD_FIESS %>% select("codigo_pais","pais","valor")
AG_PRD_FIESSN <- AG_PRD_FIESSN %>% select("codigo_pais","pais","valor")

AG_PRD_FIESS <-  AG_PRD_FIESS  %>% rename(AG_PRD_FIESS=valor)
AG_PRD_FIESSN <- AG_PRD_FIESSN %>% rename(AG_PRD_FIESSN=valor)
names(AG_PRD_FIESS)
names(AG_PRD_FIESSN)
#dados <- AG_PRD_FIESS %>% left_join(AG_PRD_FIESSN,by = 'codigo_pais')
dados <- AG_PRD_FIESS %>% left_join(AG_PRD_FIESSN)
remove(AG_PRD_FIESS,AG_PRD_FIESSN)

temp[6]
EG_EGY_CLEAN <- myfiles[[6]] %>% clean_names()
names(EG_EGY_CLEAN)
EG_EGY_CLEAN <- EG_EGY_CLEAN %>% select("codigo_pais","pais","valor")
EG_EGY_CLEAN <-  EG_EGY_CLEAN  %>% rename(EG_EGY_CLEAN=valor)
dados <- dados %>% left_join(EG_EGY_CLEAN)
remove(EG_EGY_CLEAN)

temp[7]
EG_EGY_PRIM <- myfiles[[7]] %>% clean_names()
EG_EGY_PRIM <- EG_EGY_PRIM %>% select("codigo_pais","pais","valor")
EG_EGY_PRIM <- EG_EGY_PRIM  %>% rename(EG_EGY_PRIM=valor)
dados <- dados %>% left_join(EG_EGY_PRIM)
remove(EG_EGY_PRIM)

temp[8]
EG_EGY_RNEW <- myfiles[[8]] %>% clean_names()
EG_EGY_RNEW <- EG_EGY_RNEW %>% select("codigo_pais","pais","valor")
EG_EGY_RNEW <- EG_EGY_RNEW  %>% rename(EG_EGY_RNEW=valor)
dados <- dados %>% left_join(EG_EGY_RNEW)
remove(EG_EGY_RNEW)

temp[9]
EN_H2O_GRAMBQ <- myfiles[[9]] %>% clean_names()
EN_H2O_GRAMBQ <- EN_H2O_GRAMBQ %>% select("codigo_pais","pais","valor")
EN_H2O_GRAMBQ <- EN_H2O_GRAMBQ  %>% rename(EN_H2O_GRAMBQ=valor)
dados <- dados %>% left_join(EN_H2O_GRAMBQ)
remove(EN_H2O_GRAMBQ)

temp[10]
EN_H2O_OPAMBQ <- myfiles[[10]] %>% clean_names()
EN_H2O_OPAMBQ <- EN_H2O_OPAMBQ %>% select("codigo_pais","pais","valor")
EN_H2O_OPAMBQ <- EN_H2O_OPAMBQ  %>% rename(EN_H2O_OPAMBQ=valor)
dados <- dados %>% left_join(EN_H2O_OPAMBQ)
remove(EN_H2O_OPAMBQ)

temp[11]
EN_H2O_RVAMBQ <- myfiles[[11]] %>% clean_names()
EN_H2O_RVAMBQ <- EN_H2O_RVAMBQ %>% select("codigo_pais","pais","valor")
EN_H2O_RVAMBQ <- EN_H2O_RVAMBQ  %>% rename(EN_H2O_RVAMBQ=valor)
dados <- dados %>% left_join(EN_H2O_RVAMBQ)
remove(EN_H2O_RVAMBQ)

temp[12]
EN_H2O_WBAMBQ <- myfiles[[12]] %>% clean_names()
EN_H2O_WBAMBQ <- EN_H2O_WBAMBQ %>% select("codigo_pais","pais","valor")
EN_H2O_WBAMBQ <- EN_H2O_WBAMBQ  %>% rename(EN_H2O_WBAMBQ=valor)
dados <- dados %>% left_join(EN_H2O_WBAMBQ)
remove(EN_H2O_WBAMBQ)

temp[13]
EN_LND_SLUM <- myfiles[[12]] %>% clean_names()
EN_LND_SLUM <- EN_LND_SLUM %>% select("codigo_pais","pais","valor")
EN_LND_SLUM <- EN_LND_SLUM  %>% rename(EN_LND_SLUM=valor)
dados <- dados %>% left_join(EN_LND_SLUM)
remove(EN_LND_SLUM)

temp[14]
EN_MWT_TREATR <- myfiles[[14]] %>% clean_names()
EN_MWT_TREATR <- EN_MWT_TREATR %>% select("codigo_pais","pais","valor")
EN_MWT_TREATR <- EN_MWT_TREATR  %>% rename(EN_MWT_TREATR=valor)
dados <- dados %>% left_join(EN_MWT_TREATR)
remove(EN_MWT_TREATR)

# Pular o 15 (cidades)
temp[16]
EN_WWT_TREATR <- myfiles[[16]] %>% clean_names()
EN_WWT_TREATR <- EN_WWT_TREATR %>% select("codigo_pais","pais","valor")
EN_WWT_TREATR <- EN_WWT_TREATR  %>% rename(EN_WWT_TREATR=valor)
dados <- dados %>% left_join(EN_WWT_TREATR)
remove(EN_WWT_TREATR)

temp[17]
EN_WWT_WWDS <- myfiles[[17]] %>% clean_names()
EN_WWT_WWDS <- EN_WWT_WWDS %>% select("codigo_pais","pais","valor")
EN_WWT_WWDS <- EN_WWT_WWDS  %>% rename(EN_WWT_WWDS=valor)
dados <- dados %>% left_join(EN_WWT_WWDS)
remove(EN_WWT_WWDS)

temp[18]
ER_H2O_IWRMD <- myfiles[[18]] %>% clean_names()
ER_H2O_IWRMD <- ER_H2O_IWRMD %>% select("codigo_pais","pais","valor")
ER_H2O_IWRMD <- ER_H2O_IWRMD  %>% rename(ER_H2O_IWRMD=valor)
dados <- dados %>% left_join(ER_H2O_IWRMD)
remove(ER_H2O_IWRMD)

temp[19] # Desconsiderar

temp[20] 
ER_H2O_STRESS <- myfiles[[20]] %>% clean_names()
ER_H2O_STRESS <- ER_H2O_STRESS %>% select("codigo_pais","pais","valor")
ER_H2O_STRESS <- ER_H2O_STRESS  %>% rename(ER_H2O_STRESS=valor)
dados <- dados %>% left_join(ER_H2O_STRESS)
remove(ER_H2O_STRESS)


temp[21] 
SD_MDP_MUHC <- myfiles[[21]] %>% clean_names()
SD_MDP_MUHC <- SD_MDP_MUHC %>% select("codigo_pais","pais","valor")
SD_MDP_MUHC <- SD_MDP_MUHC  %>% rename(SD_MDP_MUHC=valor)
dados <- dados %>% left_join(SD_MDP_MUHC)
remove(SD_MDP_MUHC)

temp[22] 
SD_XPD_ESED <- myfiles[[22]] %>% clean_names()
SD_XPD_ESED <- SD_XPD_ESED %>% select("codigo_pais","pais","valor")
SD_XPD_ESED <- SD_XPD_ESED  %>% rename(SD_XPD_ESED=valor)
dados <- dados %>% left_join(SD_XPD_ESED)
remove(SD_XPD_ESED)

temp[23] 
SE_ACC_HNDWSH <- myfiles[[23]] %>% clean_names()
SE_ACC_HNDWSH <- SE_ACC_HNDWSH %>% select("codigo_pais","pais","valor")
SE_ACC_HNDWSH <- SE_ACC_HNDWSH  %>% rename(SE_ACC_HNDWSH=valor)
dados <- dados %>% left_join(SE_ACC_HNDWSH)
remove(SE_ACC_HNDWSH)

temp[24] 
SE_ACS_CMPTR <- myfiles[[24]] %>% clean_names()
SE_ACS_CMPTR <- SE_ACS_CMPTR %>% select("codigo_pais","pais","valor")
SE_ACS_CMPTR <- SE_ACS_CMPTR  %>% rename(SE_ACS_CMPTR=valor)
dados <- dados %>% left_join(SE_ACS_CMPTR)
remove(SE_ACS_CMPTR)

temp[25] 
SE_ACS_ELECT <- myfiles[[25]] %>% clean_names()
SE_ACS_ELECT <- SE_ACS_ELECT %>% select("codigo_pais","pais","valor")
SE_ACS_ELECT <- SE_ACS_ELECT  %>% rename(SE_ACS_ELECT=valor)
dados <- dados %>% left_join(SE_ACS_ELECT)
remove(SE_ACS_ELECT)

temp[26] 
temporario <- myfiles[[26]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SE_ACS_H2O=valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[27] 
temporario <- myfiles[[27]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SE_ACS_INTNT=valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[28] 
temporario <- myfiles[[28]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SE_ACS_SANIT=valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[29] 
temporario <- myfiles[[29]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SE_ADT_EDUCTRN=valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[30] 
temporario <- myfiles[[30]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SE_TRA_GRDL =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[31] 
temporario <- myfiles[[31]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SH_H2O_SAFE =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[32] 
temporario <- myfiles[[32]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SH_SAN_DEFECT =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[33] 
temporario <- myfiles[[33]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SH_SAN_HNDWSH =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[34] 
temporario <- myfiles[[34]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SH_SAN_SAFE =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[35] 
temporario <- myfiles[[35]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SI_POV_DAY1 =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[36] 
temporario <- myfiles[[36]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SI_POV_EMP1 =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[37] 
temporario <- myfiles[[37]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SI_POV_NAHC =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[38] 
temporario <- myfiles[[38]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SL_EMP_PCAP =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[39] 
temporario <- myfiles[[39]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SL_TLF_CHLDEA =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[40] 
temporario <- myfiles[[40]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SL_TLF_NEET =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[41] 
temporario <- myfiles[[41]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SL_TLF_UEM =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[42] 
temporario <- myfiles[[42]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SN_ITK_DEFC =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[43] 
temporario <- myfiles[[43]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SN_ITK_DEFCN =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[44] 
temporario <- myfiles[[44]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SP_ACS_BSRVH2O =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

temp[45] 
temporario <- myfiles[[45]] %>% clean_names()
temporario <- temporario %>% select("codigo_pais","pais","valor")
temporario <- temporario  %>% rename(SP_ACS_BSRVSAN =valor)
dados <- dados %>% left_join(temporario)
remove(temporario)

dados <- unique(dados)
dados$EG_EGY_CLEAN <- gsub(">","",dados$EG_EGY_CLEAN)
dados$EG_EGY_CLEAN <- gsub("<","",dados$EG_EGY_CLEAN)

dados$EG_EGY_CLEAN<-as.numeric(dados$EG_EGY_CLEAN)
dados$EG_EGY_RNEW <-as.numeric(dados$EG_EGY_RNEW)
dados$SN_ITK_DEFC <-as.numeric(dados$SN_ITK_DEFC)
dados$SN_ITK_DEFCN<-as.numeric(dados$SN_ITK_DEFCN)

dados<-dados %>% select(-codigo_pais)
nomes <-dados$pais
dados<-dados %>% select(-pais)

saveRDS(dados, file="C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Liliana/dados/dados_Liliana.RDS")
