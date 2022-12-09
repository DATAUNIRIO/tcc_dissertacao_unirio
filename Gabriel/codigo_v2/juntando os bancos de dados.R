
load("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Gabriel/banco_gabriel_parte0.RData")
banco_completo <- dados2
remove(dados2)

load("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Gabriel/banco_gabriel_parte1.RData")
banco_completo <- rbind(banco_completo,banco1)
remove(banco1)

load("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Gabriel/banco_gabriel_parte2.RData")
banco_completo <- rbind(banco_completo,banco1)
remove(banco1)

load("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Gabriel/banco_gabriel_parte3.RData")
banco_completo <- rbind(banco_completo,banco1)
remove(banco1)

load("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Gabriel/banco_gabriel_parte4.RData")
banco_completo <- rbind(banco_completo,banco1)
remove(banco1)

load("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Gabriel/banco_gabriel_parte5.RData")
banco_completo <- rbind(banco_completo,banco1)
remove(banco1)

saveRDS(banco_completo,file = "C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Gabriel/banco_completo.Rds")


