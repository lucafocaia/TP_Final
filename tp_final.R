
### TP FINAL ###
 ## Analisis de las concentraciones de CO2 en Hawaii ##

#Leo el archivo ascii y cargo los datos
archivo <- "~/Documentos/Labo_Luca/TP_Final/co2_daily_mlo.csv" 
archivo1 <- "~/Documentos/Labo_Luca/TP_Final/co2_mm_mlo.csv"
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
datos <- datos[datos$Anio %in% 1984:2022,]
datos1 <- datos1[datos1$year %in% 1984:2022,]

#########################
#Relleno los dias que no hay datos con el valor mensual de ese mes

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


#Armo un nuevo df y lo voy llenando
 #Anios
anios <- rep(1984:2022, each = 365)
 
 #Meses
ene <- rep(1, 31)
feb <- rep(2, 28)
mar <- rep(3, 31)
abr <- rep(4, 30)
may <- rep(5, 31)
jun <- rep(6, 30)
jul <- rep(7, 31)
ago <- rep(8, 31)
sep <- rep(9, 30)
oct <- rep(10, 31)
nov <- rep(11, 30)
dic <- rep(12, 31)
meses_un_anio <- c(ene, feb, mar, abr, may, jun, jul, ago, sep, oct, nov, dic)
meses <- rep(meses_un_anio, 39)

 #Dias
dias_31 <- c(1:31)
dias_30 <- c(1:30)
dias_28 <- c(1:28)
dias_un_anio <- c(dias_31, dias_28, dias_31, dias_30, dias_31, dias_30, dias_31,
                  dias_31, dias_30, dias_31, dias_30, dias_31)
dias <- rep(dias_un_anio, 39)

#Defino un nuevo df
datos_nuevo <- data.frame("Anio" = anios, "Mes" = meses, "Dia" = dias)

#Agrego una columna con las fechas
require(lubridate)
Fecha <- c()
for (i in 1:length(datos1$Anio)) {
  anio <- datos_nuevo$Anio[i]
  mes <- datos_nuevo$Mes[i]
  dia <- datos_nuevo$Dia[i]
  fecha <- make_date(year = anio, month = mes, day = dia)
  fecha <- format(fecha, tz="")
  Fecha[i] <- fecha 
}
Fecha <- ymd(Fecha)
datos_nuevo$Fecha <- Fecha

#Agrego una columna vacia para los datos
datos_nuevo$CO2_ppm <- 0

#Veo los datos que faltan
anios1 <- c(1984:2022)
meses1 <- c(1:12)
for (i in 1:length(anios1)) {
  anio = anios1[i]
  for (j in 1:length(meses1)) {
    datos_anio_mes <- datos[datos$Anio == anio & datos$Mes == j,]
    datos_nuevo_anio_mes <- datos_nuevo[datos_nuevo$Anio == anio & datos_nuevo$Mes == j,]
    faltantes <- setdiff(datos_nuevo_anio_mes$Dia, datos_anio_mes$Dia)
    #pos_faltantes <- which(datos_nuevo$Dia[datos_nuevo$Anio == anio & datos_nuevo$Mes == j] %in% faltantes)
    media_mes <- datos1$average[datos1$year == anio & datos1$month == j]
    datos_nuevo$CO2_ppm[datos_nuevo$Anio == anio & datos_nuevo$Mes == j &
                      datos_nuevo$Dia %in% faltantes] <- media_mes
  }
}








#########################

#Agrego en "datos" una columna con los datos desestacionalizados
anios <- c(1984:2022)
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

#Calculo los promedios mensuales y semanales
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
g = ggplot(data = datos, mapping = aes(x = Fecha, y = CO2_ppm)) 
g = g + geom_point() 
g = g + scale_x_date(date_breaks = "4 years", date_labels = "%Y-%m")
g








  