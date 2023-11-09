
### TP FINAL ###
 ## Analisis de las concentraciones de CO2 en Hawaii ##

#Leo el archivo ascii y cargo los datos
archivo <- "~/Documentos/Labo_Luca/TP_Final/co2_daily_mlo.csv"
datos <- read.csv(archivo, skip = 32)
head(datos)
tail(datos) #Datos diarios desde Mayo de 1974 hasta Septiembre de 2023

#Le pongo nombre a las columnas del data frame
colnames(datos) = c("Anio","Mes","Dia","Decimal","CO2(ppm)")

datos1 <- read.csv("~/Documentos/Labo_Luca/TP_Final/co2_mm_mlo.csv",skip = 40)
head(datos1)
tail(datos1) #Datos mensuales desde Marzo de 1958 hasta Septiembre de 2023

#Quiero aprovechar los datos desestacionalizados pero solamente estan en el
 #archivo con datos mensuales, entonces voy a seleccionar el periodo 
 #Enero 1975 - Diciembre 2022, recorto esos datos que me interesan y los pego
 #en los diarios

#Recorto mis datos
datos <- datos[datos$Anio %in% 1975:2022,]
datos1 <- datos1[datos1$year %in% 1975:2022,]

#Relleno los dias que no hay datos con el valor mensual de ese mes
anios <- c(1975:2022)
meses <- c(1:12)
for (i in 1:length(anios)) {
  anio <- anios[i]
  datos_anio <- datos[datos$Anio == anio,]
  for (j in 1:length(meses)) {
    datos_mes <- datos_anio[datos_anio$Mes == j,]
    if (j==1 | j==3 | j==5 | j==7 | j==8 | j==10 | j==12) {
      for (k in 1:31) {
        datos_dias <- datos_mes[datos_mes$Dia == k,]
        if (is.na(datos_dias$Dia)) {
          
        }
      }
  }
}



