data<-read_delim("Presidencia 1a vuelta 2022.csv","\t",
                 escape_double = FALSE, trim_ws = TRUE,locale = locale(encoding = "LATIN1"))
data$code<-ifelse(nchar(data$codigo_municipio)==4,paste("0",as.character(data$codigo_municipio),sep=""),data$codigo_municipio)
str(data$codigo_municipio)
str(data$code)
municipios<-read_xlsx("./voto_muncipio_elpaisV3.xlsx")
municipios$dif_part<-municipios$part-municipios$part_18
str(municipios$code)

porcentajes<-read_xlsx("./porcentajes.xlsx")
names(porcentajes)[2]<-"code"

data2<-left_join(municipios, porcentajes,by="code")


data2<-left_join(data2,data,by="code")
names(data2)
data2<-data2[,c(-36,-37,-38,-39)]
names(data2)

data2$max_pc_18<-pmax(data2$`Iván Duque_18_pc`,data2$`Gustavo Petro_18_pc`,data2$`Sergio Fajardo_18_pc`,data2$`Viviane Morales_18_pc`,data2$`Germán Vargas Lleras_18_pc`,data2$`Humberto De La Calle_18_pc`,data2$`Jorge Antonio Trujillo_18_pc`)
data2$gana_petro18<-ifelse(data2$`Gustavo Petro_18_pc`==data2$max_pc_18,1,0)
data2$max_pc_22<-pmax(data2$`Luis Pérez_22_pc`,data2$`Gustavo Petro_22_pc`,data2$`Sergio Fajardo_22_pc`,data2$`Rodolfo Hernández_22_pc`,data2$`Federico Gutiérrez_22_pc`,data2$`John Milton Rodríguez_22_pc`,data2$`Enrique Gómez Martínez_22_pc`)
data2$gana_petro22<-ifelse(data2$`Gustavo Petro_22_pc`==data2$max_pc_22,1,0)
data2$ganaPetro2<-ifelse(data2$gana_petro18+data2$gana_petro22==2,1,0)
table(data2$ganaPetro2)
data2$part_crecio_above_average<-ifelse(data2$dif_part>mean(data2$dif_part),1,0)

table(data2$ganaPetro2,data2$part_crecio_above_average)

mun_subir_part<-filter(data2,ganaPetro2==1&part_crecio_above_average==0)
mun_subir_part<-mun_subir_part[order(mun_subir_part$pob_total_2018,decreasing = TRUE),] 

library("xlsx")
write.xlsx(mun_subir_part,"./mun_subir_part.xlsx")

data2$diff_petro_rodolfo<-data2$`Gustavo Petro_22_pc`-data2$`Rodolfo Hernández_22_pc`
data2$diff_petro_rodolfo_abs<-abs(data2$diff_petro_rodolfo)
summary(data2$diff_petro_rodolfo)

mun_apretaditos<-filter(data2,diff_petro_rodolfo_abs<=5)
mun_apretaditos<-mun_apretaditos[order(mun_apretaditos$pob_total_2018,decreasing = TRUE),]

write.xlsx(mun_apretaditos,"./mun_apretaditos.xlsx")

table(mun_apretaditos$gana_petro22)
summary(municipios$dif_part)

