
dados<-readRDS("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/TCC UNIRIO/Liliana/dados/dados_Liliana.RDS")

nao_pais<-c('Africa','Americas',
'British Indian Ocean Territory',
'Central African Republic',
'Central America',
'Central and Southern Asia',
'Central Asia',
'Eastern Africa',
'Eastern and South-Eastern Asia',
'Eastern Asia',
'Eastern Europe',
'Europe',
'Europe and Northern America',
'French Southern Territories',
'Landlocked developing countries (LLDCs)',
'Latin America and the Caribbean',
'Least Developed Countries (LDCs)',
'Landlocked developing countries (LLDCs)',
'Middle Africa',
'Northern Africa',
'Northern Africa (exc. Sudan)',
'Northern Africa and Western Asia',
'Northern America',
'Northern Europe',
'Oceania',
'Oceania (exc. Australia and New Zealand)',
'Small island developing States (SIDS)',
'South-Eastern Asia',
'Southern Africa',
'Southern Asia',
'Southern Europe',
'Sub-Saharan Africa',
'Sub-Saharan Africa (inc. Sudan)',
'United States Minor Outlying Islands',
'Western Africa',
'Western Asia',
'Western Asia (exc. Armenia, Azerbaijan, Cyprus, Israel and Georgia)',
'Western Europe',
'Western Sahara',
'World')

`%!in%` = Negate(`%in%`)

dados <-dados[dados$pais%!in%nao_pais,] 

library(Amelia)
#missmap(dados, main = "Mapa de Dados Faltantes")


nao_pais2<- c("Åland Islands",                                 
"Bonaire, Sint Eustatius and Saba",              
"Caucasus and Central Asia",                     
"China, Hong Kong Special Administrative Region",
"China, Macao Special Administrative Region",    
"Christmas Island",                              
"Cocos (Keeling) Islands",                       
"Faroe Islands",                                 
"Guernsey",                                      
"Heard Island and McDonald Islands",             
"Holy See",                                      
"Jersey",                                        
"Norfolk Island",                                
"Pitcairn",                                      
"South Georgia and the South Sandwich Islands",  
"Svalbard and Jan Mayen Islands") 


dados <-dados[dados$pais%!in%nao_pais2,] 

# missmap(dados, main = "Mapa de Dados Faltantes 2")

nao_pais3<-c('Aruba',
'Asia',
'Australia and New Zealand',
'Bermuda',
'British Virgin Islands',
'Caribbean',
'Cayman Islands',
'Channel Islands',
'Cook Islands',
'Curaçao',
'Falkland Islands (Malvinas)',
'French Guiana',
'French Polynesia',
'Gibraltar',
'Guadeloupe',
'Guam',
'Isle of Man',
'Martinique',
'Mayotte',
'Melanesia',
'Nauru',
'New Caledonia',
'Niue',
'Northern Mariana Islands',
'Palau',
'Réunion',
'Saint Barthélemy',
'Saint Martin (French Part)',
'Saint Pierre and Miquelon',
'Saint Helena',
'Saint Kitts and Nevis',
'Sint Maarten (Dutch part)',
'South America',
'Tokelau',
'Turks and Caicos Islands',
'Tuvalu',
'United States Virgin Islands',
'Wallis and Futuna Islands')


dados <-dados[dados$pais%!in%nao_pais3,] 

# missmap(dados, main = "Mapa de Dados Faltantes")

nao_pais4<-c('Anguilla','Marshall Islands','American Samoa','Montserrat','Micronesia','Micronesia (Federated States of)','Bahamas','Dominica','Greenland','Grenada','Liechtenstein','Polynesia')

dados <-dados[dados$pais%!in%nao_pais4,] 

library(dplyr)
dados <-dados %>% select(-EN_MWT_TREATR)
dados <-dados %>% select(-EN_WWT_TREATR)

# missmap(dados, main = "Mapa de Dados Faltantes")

names(dados)
remove(nao_pais,nao_pais2,nao_pais3,nao_pais4)

# writexl::write_xlsx(dados,path = "Dados_Liliana.xlsx")
 
