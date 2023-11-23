
### TP FINAL ###
 ## Analisis de las concentraciones de CO2 en Hawaii ##

#Leo el archivo ascii y cargo los datos
 #CASA
archivo <- "~/PracticasLabo/TP_Final/co2_daily_mlo.csv" 
archivo1 <- "~/PracticasLabo/TP_Final/co2_mm_mlo.csv"
 #FACU
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

#Recorto mis datos
datos <- datos[datos$Anio %in% 1984:2022,]
datos1 <- datos1[datos1$year %in% 1984:2022,]

#Relleno los dias que no hay datos con el valor mensual de ese mes
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

#Agrego una columna con las fechas a ambos data frames
require(lubridate)
Fecha <- c()
for (i in 1:length(datos$Anio)) {
  anio <- datos$Anio[i]
  mes <- datos$Mes[i]
  dia <- datos$Dia[i]
  fecha <- make_date(year = anio, month = mes, day = dia)
  fecha <- format(fecha, tz="")
  Fecha[i] <- fecha 
}
Fecha <- ymd(Fecha)
datos$Fecha <- Fecha

Fecha <- c()
for (i in 1:length(datos_nuevo$Anio)) {
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

#Completo los datos que faltan con las medias de su respectivo mes
anios1 <- c(1984:2022)
meses1 <- c(1:12)
for (i in 1:length(anios1)) {
  anio = anios1[i]
  for (j in 1:length(meses1)) {
    datos_anio_mes <- datos[datos$Anio == anio & datos$Mes == j,]
    datos_nuevo_anio_mes <- datos_nuevo[datos_nuevo$Anio == anio &
                                          datos_nuevo$Mes == j,]
    faltantes <- setdiff(datos_nuevo_anio_mes$Dia, datos_anio_mes$Dia)
    media_mes <- datos1$average[datos1$year == anio & datos1$month == j]
    datos_nuevo$CO2_ppm[datos_nuevo$Anio == anio & datos_nuevo$Mes == j &
                      datos_nuevo$Dia %in% faltantes] <- media_mes
  }
}

#Completo  los dias del nuevo df con los datos de los dias que si tengo
for (i in 1:length(datos$Fecha)) {
  fecha <- datos$Fecha[i]
  datos_nuevo$CO2_ppm[datos_nuevo$Fecha == fecha] <- 
    datos$CO2_ppm[datos$Fecha == fecha]
}

#Quiero aprovechar los datos desestacionalizados pero solamente estan en el
#archivo con datos mensuales, entonces voy a seleccionar el periodo 
#Enero 1984 - Diciembre 2022, recorto esos datos que me interesan y los pego
#en los diarios
#Agrego en "datos" una columna con los datos desestacionalizados
Desestacionalizado = c()
for (i in 1:length(anios1)) {
  anio = anios1[i]
  datos_anio = datos_nuevo[datos_nuevo$Anio == anio,]
  datos_anio1 = datos1[datos1$year == anio,]
  for (j in 1:length(meses1)) {
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
datos_nuevo = cbind(datos_nuevo, Desestacionalizado)

#Selecciono los datos de 2020 a 2021 para armar el grafico solicitado
datos_periodo <- datos_nuevo[datos_nuevo$Anio %in% 2020:2021,]

#Calculo los promedios mensuales y semanales
 #PROMEDIO MENSUAL
anios_periodo <- c(2020:2021)
media_mensual_periodo = c()
for (i in 1:length(anios_periodo)) {
  for (j in 1:length(meses1)) {
    anio <- anios_periodo[i]
    datos_anio <- datos_periodo[datos_periodo$Anio == anio,]
    datos_mes <- datos_anio[datos_anio$Mes == j,]
    media_mes <- round(mean(datos_mes$CO2_ppm, na.rm = T),2)
    media_mensual_periodo <- c(media_mensual_periodo, media_mes)
  }
}

 #PROMEDIO SEMANAL
media_semanal_periodo = c()
for (i in 1:length(anios_periodo)) {
  for (j in 1:length(meses1)) {
    anio <- anios_periodo[i]
    datos_anio <- datos_periodo[datos_periodo$Anio == anio,]
    datos_mes <- datos_anio[datos_anio$Mes == j,]
    for (k in seq(1,31,8)) {
      semana <- datos_mes[datos_mes$Dia %in% k:(k+7),]
      media_semanal <- round(mean(semana$CO2_ppm, na.rm = T), 2)
      media_semanal_periodo <- c(media_semanal_periodo, media_semanal)
    }
  }
}

#Agrego las medias al data frame
datos_periodo$Media_mensual <- 0
datos_periodo$Media_semanal <- 0

k <- 0
for (i in 1:length(anios_periodo)) {
 anio <- anios_periodo[i]
 for (j in 1:length(meses1)) {
   datos_periodo$Media_mensual[
    datos_periodo$Anio == anio & datos_periodo$Mes == j] <-
    media_mensual_periodo[j+k*12]
 }
 k <- k + 1
}

sec <- seq(1,31,8)
h <- 0
for (i in 1:length(anios_periodo)) {
  anio <- anios_periodo[i]
  for (j in 1:length(meses1)) {
    for (k in 1:length(sec)) {
      dia <- sec[k]
      datos_periodo$Media_semanal[
       datos_periodo$Anio == anio & datos_periodo$Mes == j &
        datos_periodo$Dia %in% dia:(dia+7)] <- media_semanal_periodo[k+h*4]
    }
    h <- h + 1
  }
}

#Armo el grafico pedido con ggplot #####geom_ribbon()
require(ggplot2)
datos_periodo$Etiqueta <- month(datos_periodo$Mes,label = T)
datos_periodo$Etiqueta[which(datos_periodo$Dia != 15)] <- NA
datos_periodo$Media_corrida <- datos_periodo$Media_mensual + 0.3

g = ggplot(data = datos_periodo, mapping = aes(x = Fecha, y = CO2_ppm)) +
  geom_point(color = "dark grey") +
  scale_x_date(date_breaks = "2 month", date_labels = "%Y-%m") + 
  geom_point(mapping = aes(x = Fecha, y = Media_mensual),
                   shape = 0, color = "dark blue", size = 0.4) +
  geom_text(datos_periodo,
            mapping = aes(label = Etiqueta,
                          x = Fecha, y = Media_corrida), color = "blue") +
  geom_point(mapping = aes(x = Fecha, y = Media_semanal), shape = 0,
             color = "violet", size = 0.3)
g




  