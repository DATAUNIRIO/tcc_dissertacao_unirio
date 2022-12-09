
dados <- readRDS("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Liliana/dados/dados_Liliana_com_imputacao_17_05_2022.RDS")

#----------------------------------------------
# Hierarquico
#----------------------------------------------
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

# To standarize the variables
dados_2 <- scale(dados)  
# United Kingdom of Great Britain and Northern Ireland
# virou Great Britain
rownames(dados_2)[174] <- 'Great Britain'

# Dissimilarity matrix
d <- dist(dados_2, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )
plot(hc1, cex = 0.5, hang = -1)
abline(h = 10, lty = 2,col = "red")


library("ggplot2")
library("ggdendro")
ggdendrogram(hc1)+ geom_hline(yintercept=10.5, linetype="dashed", color = "red")
ggsave('liliana.png',width = 50, height = 30, units = "cm",dpi=600)

grupos <- cutree(hc1, k = 15) 
dados$grupos<-grupos

ggdendrogram(hc1, rotate = TRUE, theme_dendro = FALSE)

grupos <- data.frame(grupos) 
nome <- rownames(dados)
grupos$pais<-nome
dados$pais<-nome



prox_palavra<-list(grupos,dados)
writexl::write_xlsx(prox_palavra,path = "cluster_liliana_17_05_2022.xlsx")



library(dplyr)

medias <- dados %>% group_by(grupos) %>%
  summarise(
    media_AG_PRD_FIESS=mean(AG_PRD_FIESS),   
media_AG_PRD_FIESSN=mean(AG_PRD_FIESSN),  
media_EG_EGY_CLEAN=mean(EG_EGY_CLEAN),
media_EG_EGY_PRIM=mean(EG_EGY_PRIM),
media_EG_EGY_RNEW=mean(EG_EGY_RNEW),
media_EN_H2O_GRAMBQ=mean(EN_H2O_GRAMBQ),
media_EN_H2O_OPAMBQ=mean(EN_H2O_OPAMBQ),
media_EN_H2O_RVAMBQ=mean(EN_H2O_RVAMBQ),
media_EN_H2O_WBAMBQ=mean(EN_H2O_WBAMBQ), 
media_EN_LND_SLUM=mean(EN_LND_SLUM), 
media_EN_WWT_WWDS=mean(EN_WWT_WWDS), 
media_ER_H2O_IWRMD=mean(ER_H2O_IWRMD),
media_ER_H2O_STRESS=mean(ER_H2O_STRESS), 
media_SD_MDP_MUHC=mean(SD_MDP_MUHC), 
media_SD_XPD_ESED=mean(SD_XPD_ESED), 
media_SE_ACC_HNDWSH=mean(SE_ACC_HNDWSH),
media_SE_ACS_CMPTR=mean(SE_ACS_CMPTR), 
media_SE_ACS_ELECT=mean(SE_ACS_ELECT), 
media_SE_ACS_H2O=mean(SE_ACS_H2O), 
media_SE_ACS_INTNT=mean(SE_ACS_INTNT),
media_SE_ACS_SANIT=mean(SE_ACS_SANIT), 
media_SE_ADT_EDUCTRN=mean(SE_ADT_EDUCTRN),
media_SE_TRA_GRDL=mean(SE_TRA_GRDL),
media_SH_SAN_DEFECT=mean(SH_SAN_DEFECT),
media_SH_SAN_HNDWSH=mean(SH_SAN_HNDWSH),
media_SI_POV_DAY1=mean(SI_POV_DAY1),
media_SI_POV_EMP1=mean(SI_POV_EMP1),
media_SI_POV_NAHC=mean(SI_POV_NAHC),
media_SL_EMP_PCAP=mean(SL_EMP_PCAP),
media_SL_TLF_CHLDEA=mean(SL_TLF_CHLDEA),
media_SL_TLF_NEET=mean(SL_TLF_NEET),
media_SL_TLF_UEM=mean(SL_TLF_UEM),
media_SN_ITK_DEFC=mean(SN_ITK_DEFC),
media_SN_ITK_DEFCN=mean(SN_ITK_DEFCN),
media_SP_ACS_BSRVH2O=mean(SP_ACS_BSRVH2O),
media_SP_ACS_BSRVSAN=mean(SP_ACS_BSRVSAN))


writexl::write_xlsx(medias,path = "C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Liliana/dados/medias_liliana_31_05_2022.xlsx")
