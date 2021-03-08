library(readxl)
salud_mex <- read_excel("IDH_mex.xlsx", sheet = "salud_c")
View(salud_mex)
#Extraer variables de IDH por año

aux_salud <- salud_mex[5:32]

View(aux_salud)

#Rowmeans Calcula la media de cada fila del objeto indicado

Mean_anual_salud <- rowMeans(aux_salud)


#Separación de la columna log_IDH-1990 COMO PIB BASE

salud_base <- salud_mex[4]

#Graficar el IDH BASE, contra el promedio anual

plot(x=salud_base$LOG_IDH_1990,y=Mean_anual_salud ,pch=1,main = "Convergencia Absoluta \n Salud estatal",xlab = "Año base 1990", ylab = "Tasa crec. Salud", col="blue")
#Seleccionar los países a estudiar con: col=ifelse(CA$`Country Code` %in% c("Chia","Dis","Yuc"), 'red', 'purple')
text(salud_base$LOG_IDH_1990,Mean_anual_salud ,salud_mex$CODIGO, col=ifelse(salud_mex$CODIGO %in% c("Chia","Dis","Yuc"), 'red', 'purple'),cex =ifelse(salud_mex$CODIGO %in% c("Chia","Mex","Dis"), 1, 0.6) ,pos = 4)
abline(lm(Mean_anual_salud ~salud_base$LOG_IDH_1990), col = "green")

