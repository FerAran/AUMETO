library(readxl)
IDH_mex <- read_excel("IDH_mex.xlsx", sheet = "IDH_C")
View(IDH_mex)

#Extraer variables de IDH por año

aux_IDH <- IDH_mex[5:32]

View(aux_IDH)

#Rowmeans Calcula la media de cada fila del objeto indicado

Mean_anual_IDH <- rowMeans(aux_IDH)


#Separación de la columna log_IDH-1990 COMO PIB BASE

IDH_base <- IDH_mex[4]

#Graficar el IDH BASE, contra el promedio anual

plot(x=IDH_base$LOG_IDH_1990,y=Mean_anual_IDH,pch=1,main = "Convergencia Absoluta \n IDH estatal ",xlab = "Año base 1990", ylab = "Tasa crec. IDH", col="blue")
#Seleccionar los países a estudiar con: col=ifelse(CA$`Country Code` %in% c("Chia","Dis","Yuc"), 'red', 'purple')
text(IDH_base$LOG_IDH_1990,Mean_anual_IDH,IDH_mex$CODIGO, col=ifelse(IDH_mex$CODIGO %in% c("Chia","Dis","Yuc"), 'red', 'purple'),cex =ifelse(IDH_mex$CODIGO %in% c("Chia","Mex","Dis"), 1, 0.6) ,pos = 4)
abline(lm(Mean_anual_IDH~IDH_base$LOG_IDH_1990), col = "green")

