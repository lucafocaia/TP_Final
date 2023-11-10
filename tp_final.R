
### TP FINAL ###
 ## Analisis de las concentraciones de CO2 en Hawaii ##

#Leo el archivo ascii y cargo los datos
archivo <- "~/PracticasLabo/TP_Final/co2_daily_mlo.csv" 
archivo1 <- "~/PracticasLabo/TP_Final/co2_mm_mlo.csv"
#Direcciones de los archivos a usar

datos <- read.csv(archivo, skip = 32)
head(datos)
tail(datos) #Datos diarios desde Mayo de 1974 hasta Septiembre de 2023

#Le pongo nombre a las columnas del data frame
colnames(datos) = c("Anio","Mes","Dia","Dia_decimal","CO2_ppm")

datos1 <- read.csv(archivo1, skip = 40)
head(datos1)
tail(datos1) #Datos mensuales desde Marzo de 1958 hasta Septiembre de 2023

#Quiero aprovechar los datos desestacionalizados pero solamente estan en el
 #archivo con datos mensuales, entonces voy a seleccionar el periodo 
 #Enero 1975 - Diciembre 2022, recorto esos datos que me interesan y los pego
 #en los diarios

#Recorto mis datos
datos <- datos[datos$Anio %in% 1975:2022,]
datos1 <- datos1[datos1$year %in% 1975:2022,]

#########################
#Relleno los dias que no hay datos con el valor mensual de ese mes
anios <- c(1975:2022)
meses <- c(1:12)
for (i in 1:length(anios)) {
  anio <- anios[i]
  datos_anio <- datos[datos$Anio == anio,]
  for (j in 1:length(meses)) {
    datos_mes <- datos_anio[datos_anio$Mes == j,]
    media_mes = round(mean(datos_mes$CO2_ppm),2)
    if (j==1 | j==3 | j==5 | j==7 | j==8 | j==10 | j==12) {
      
    }
  }
}  
#Dejo esto en stand by, por ahora trabajo sin los datos que no estan
#########################

#Agrego en "datos" una columna con los datos desestacionalizados
anios <- c(1975:2022)
meses <- c(1:12)
Desestacionalizado = c()
for (i in 1:length(anios)) {
  anio = anios[i]
  datos_anio = datos[datos$Anio == anio,]
  datos_anio1 = datos1[datos1$year == anio,]
  for (j in 1:length(meses)) {
    datos_mes = datos_anio[datos_anio$Mes == j,]
    datos_mes1 = datos_anio1[datos_anio1$month == j,]
    desestacionalizado_mes = seq(datos_mes1$deseasonalized,
                             datos_mes1$deseasonalized,
                             length.out = length(datos_mes$CO2_ppm))
    Desestacionalizado = c(Desestacionalizado, desestacionalizado_mes)
  }
} #Creo un vector con los datos desestacionalizados pero repetidos la cantidad
   #necesario para cada mes porque estan dados como datos mensuales

#Agrego a mis datos la columna con los datos desestacionalizados
datos = cbind(datos, Desestacionalizado)

#Calculo los promedios mensual y semanal
 #PROMEDIO MENSUAL
media_mensual_xanio = c()
for (i in 1:length(anios)) {
  for (j in 1:length(meses)) {
    anio = anios[i]
    datos_anio = datos[datos$Anio == anio,]
    datos_mes = datos_anio[datos_anio$Mes == j,]
    media_mes = round(mean(datos_mes$CO2_ppm, na.rm = T),2)
    media_mensual_xanio = c(media_mensual_xanio, media_mes)
  }
}

 #PROMEDIO SEMANAL
media_semanal_xanio = c()
for (i in 1:length(anios)) {
  for (j in 1:length(meses)) {
    anio = anios[i]
    datos_anio = datos[datos$Anio == anio,]
    datos_mes = datos_anio[datos_anio$Mes == j,]
    for (k in seq(1,31,8)) {
      semana = datos_mes[datos_mes$Dia %in% k:(k+7),]
      media_semanal = round(mean(semana$CO2_ppm, na.rm = T),2)
      media_semanal_xanio = c(media_semanal_xanio, media_semanal)
    }
  }
}

#Armo el grafico pedido con ggplot
require(ggplot2)
grafico = ggplot(data = datos, mapping = aes(x = Anio, y = CO2_ppm)) + 
  geom_point()











  