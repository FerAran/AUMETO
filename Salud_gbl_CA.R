library(readxl)
Salud_CA <- read_excel("IDH.xlsx", sheet = "Salud")
View(Salud_CA)

#Extraer variables de IDH por año

Aux_Salud_CA <- Salud_CA[5:33]

View(Aux_Salud_CA)

#Rowmeans Calcula la media de cada fila del objeto indicado

Mean_anual_SALUD_CA <- rowMeans(Aux_Salud_CA)


#Separación de la columna log_IDH-1990 COMO PIB BASE

sALUD_CA__base <- Salud_CA[4]

#Graficar el IDH BASE, contra el promedio anual

plot(x=sALUD_CA__base$LOG_IDH_1990,y=Mean_anual_SALUD_CA ,pch=1,main = "Convergencia Absoluta \n Salud Global",xlab = "Año base 1990", ylab = "Tasa crec. IDH", col="blue")
#Seleccionar los países a estudiar con: col=ifelse(CA$`Country Code` %in% c("Bra","Mex","Chi"), 'red', 'purple')
text(sALUD_CA__base$LOG_IDH_1990,Mean_anual_SALUD_CA ,Salud_CA$CODIGO, col=ifelse(Salud_CA$CODIGO %in% c("Bra","Mex","Chi"), 'red', 'purple'),cex =ifelse(Salud_CA$CODIGO %in% c("Bra","Mex","Chi"), 1, 0.6) ,pos = 4)
abline(lm(Mean_anual_SALUD_CA ~sALUD_CA__base$LOG_IDH_1990), col = "green")

