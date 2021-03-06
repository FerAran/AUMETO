library(readxl)
IDH <- read_excel("IDH.xlsx")
View(IDH)

#Extraer variables de IDH por año

aux_IDH <- IDH[5:33]

View(aux_IDH)

#Rowmeans Calcula la media de cada fila del objeto indicado

Mean_anual_IDH <- rowMeans(aux_IDH)


#Separación de la columna log_IDH-1990 COMO PIB BASE

IDH_base <- IDH[4]

#Graficar el IDH BASE, contra el promedio anual

plot(x=IDH_base$LOG_IDH_1990,y=Mean_anual_IDH,pch=1,main = "Convergencia Absoluta \n IDH Global",xlab = "Año base 1990", ylab = "Tasa crec. IDH", col="blue")
#Seleccionar los países a estudiar con: col=ifelse(CA$`Country Code` %in% c("Bra","Mex","Chi"), 'red', 'purple')
text(IDH_base$LOG_IDH_1990,Mean_anual_IDH,IDH$CODIGO, col=ifelse(IDH$CODIGO %in% c("Bra","Mex","Chi"), 'red', 'purple'),cex =ifelse(IDH$CODIGO %in% c("Bra","Mex","Chi"), 1, 0.6) ,pos = 4)
abline(lm(Mean_anual_IDH~IDH_base$LOG_IDH_1990), col = "green")