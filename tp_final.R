
### TP FINAL ###
 ## Analisis de las concentraciones de CO2 en Hawaii ##

#Setear el directorio de trabajo
setwd()

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
datos_covid <- datos_nuevo[datos_nuevo$Anio %in% 2020:2021,]

#Calculo los promedios mensuales y semanales
 #PROMEDIO MENSUAL
anios_covid <- c(2020:2021)
media_mensual_covid = c()
for (i in 1:length(anios_covid)) {
  for (j in 1:length(meses1)) {
    anio <- anios_covid[i]
    datos_anio <- datos_covid[datos_covid$Anio == anio,]
    datos_mes <- datos_anio[datos_anio$Mes == j,]
    media_mes <- round(mean(datos_mes$CO2_ppm, na.rm = T),2)
    media_mensual_covid <- c(media_mensual_covid, media_mes)
  }
}

 #PROMEDIO SEMANAL
media_semanal_covid = c()
for (i in 1:length(anios_covid)) {
  for (j in 1:length(meses1)) {
    anio <- anios_covid[i]
    datos_anio <- datos_covid[datos_covid$Anio == anio,]
    datos_mes <- datos_anio[datos_anio$Mes == j,]
    for (k in seq(1,31,8)) {
      semana <- datos_mes[datos_mes$Dia %in% k:(k+7),]
      media_semanal <- round(mean(semana$CO2_ppm, na.rm = T), 2)
      media_semanal_covid <- c(media_semanal_covid, media_semanal)
    }
  }
}

#Agrego las medias al data frame
datos_covid$Media_mensual <- 0
datos_covid$Media_semanal <- 0

k <- 0
for (i in 1:length(anios_covid)) {
 anio <- anios_covid[i]
 for (j in 1:length(meses1)) {
   datos_covid$Media_mensual[
    datos_covid$Anio == anio & datos_covid$Mes == j] <- 
     media_mensual_covid[j+k*12]
 }
 k <- k + 1
}

sec <- seq(1,31,8)
h <- 0
for (i in 1:length(anios_covid)) {
  anio <- anios_covid[i]
  for (j in 1:length(meses1)) {
    for (k in 1:length(sec)) {
      dia <- sec[k]
      datos_covid$Media_semanal[
        datos_covid$Anio == anio & datos_covid$Mes == j &
          datos_covid$Dia %in% dia:(dia+7)] <- media_semanal_covid[k+h*4]
    }
    h <- h + 1
  }
}

#Armo el grafico pedido con ggplot 
require(ggplot2)
datos_covid$Etiqueta <- month(datos_covid$Mes,label = T)
datos_covid$Etiqueta[which(datos_covid$Dia != 15)] <- NA
datos_covid$Media_corrida <- datos_covid$Media_mensual + 0.2

g <- ggplot(data = datos_covid, mapping = aes(x = Fecha, y = CO2_ppm)) +
  geom_point(color = "grey45") +
  scale_x_date(date_breaks = "2 month", date_labels = "%Y-%m") + 
  geom_point(mapping = aes(x = Fecha, y = Media_semanal), shape = 0,
             fill = "darkviolet", color = "darkviolet", size = 0.8) + 
  geom_point(mapping = aes(x = Fecha, y = Media_mensual),
             shape = 0, fill = "darkblue", color = "darkblue", size = 0.8) +
  geom_text(datos_covid,
            mapping = aes(label = Etiqueta, x = Fecha, y = Media_corrida),
            color = "#191970", cex = 6) +
  labs(title = "Mediciones de CO2 durante la pandemia en Mauna Loa",
       x = "Tiempo", y = "CO2 (ppm)",
       subtitle = "Con promedios mensuales (azul) y semanales (violeta)")
g


#Armo un nuevo df solo con los promedios para guardar en una tabla ascii
anios_tabla <- rep(2020:2021, each = 48)
meses_tabla <- rep(1:12, each = 4, times = 2)
semanas_tabla <- rep(1:4, times = 24) 
df_tabla <- data.frame("Anio" = anios_tabla, "Mes" = meses_tabla, 
                       "Semana" = semanas_tabla, "Media mensual" = 0,
                       "Media semanal" = 0)

#Lleno las columnas de las medias
df_tabla$Media.semanal <- media_semanal_covid

k <- 0
for (i in 1:length(anios_covid)) {
  for (j in 1:length(meses1)) {
    anio <- anios_covid[i]
    df_tabla$Media.mensual[df_tabla$Anio == anio & df_tabla$Mes == j] <-
      media_mensual_covid[j+k*12]
  }
  k <- k + 1
}

#Guardo los datos en una tabla
write.table(df_tabla, file = "Medias mensuales y semanales de CO2 en pandemia",
            col.names = T, row.names = F, quote = F)


#Voy a comparar la serie temporal completa (1984-2022) con y sin la
 #estacionalidad, ambos graficos en el mismo pero en distintos paneles
require(ggplot2)

serie <- ggplot(data = datos_nuevo, mapping = aes(x = Fecha, y = CO2_ppm)) +
  geom_line(color = "black") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Serie temporal de CO2 (Periodo 1984-2021)", x = "Tiempo", 
       y = "CO2 (ppm)")
serie2 <- ggplot(data = datos_nuevo, mapping = aes(x = Fecha,
                                                   y = Desestacionalizado)) +
  geom_line(color = "black") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Datos desestacionalizados de CO2", x = "Tiempo", 
       y = "CO2 (ppm)")
serie
serie2

#Junto los graficos, uno abajo del otro para que se vea mejor
serie / serie2


#Armo una columna con la diferencia entre los datos medidos de CO2 y los
 #desestacionalizados, luego los promedio por mes sobre todos los anios
datos_nuevo$Dif <- datos_nuevo$CO2_ppm - datos_nuevo$Desestacionalizado

#Tambien quiero agregar el desvio estandar
datos_nuevo$Sd <- sd(datos_nuevo$CO2_ppm)
datos_nuevo$Sd_desestacionalizado <- sd(datos_nuevo$Desestacionalizado)

#Armo una columna con los datos + el desvio y otra con los datos menos el 
 #desvio
datos_nuevo$Datos_mas_sd <- datos_nuevo$CO2_ppm + datos_nuevo$Sd
datos_nuevo$Datos_menos_sd <- datos_nuevo$CO2_ppm - datos_nuevo$Sd
datos_nuevo$Datos_mas_sd_des <- datos_nuevo$Desestacionalizado + 
  datos_nuevo$Sd_desestacionalizado
datos_nuevo$Datos_menos_sd_des <- datos_nuevo$Desestacionalizado - 
  datos_nuevo$Sd_desestacionalizado

#Armo unas dos ultimas columnas con las diferencias entre los desvios
datos_nuevo$Dif_max <- datos_nuevo$Datos_mas_sd - datos_nuevo$Datos_mas_sd_des
datos_nuevo$Dif_min <- datos_nuevo$Datos_menos_sd -
  datos_nuevo$Datos_menos_sd_des

#Calculo la media de las diferencias
medias_dif <- c()
medias_difmax <- c()
medias_difmin <- c()
for (i in 1:length(meses1)) {
  datos_mes <- datos_nuevo[datos_nuevo$Mes == i,]
  media_mes <- round(mean(datos_mes$Dif), 2)
  medias_dif[i] <- media_mes
  media_mes_max <- round(mean(datos_mes$Dif_max), 2)
  medias_difmax[i] <- media_mes_max
  media_mes_min <- round(mean(datos_mes$Dif_min), 2)
  medias_difmin[i] <- media_mes_min
}

#Armo un data frame con los valores obtenidos
df_estacionalidad <- data.frame("Mes" = meses1, "Media_dif" = medias_dif,
                                "Media_difmax" = medias_difmax,
                                "Media_difmin" = medias_difmin)

#Grafico lo obtenido 
p <- ggplot(data = df_estacionalidad, mapping = aes(x = Mes, y = Media_dif)) +
  scale_x_continuous(breaks = seq(min(df_estacionalidad$Mes),
                                  max(df_estacionalidad$Mes), by = 1)) +
  geom_ribbon(aes(ymin = medias_difmin, ymax = medias_difmax),
              fill = "palegreen") +
  geom_line(color = "black", linewidth = 0.9) +
  geom_point(color = "black") +
  geom_line(data = df_estacionalidad, mapping = aes(x = Mes, y = Media_difmax),
            color = "#757575", linewidth = 0.8) +
  geom_line(data = df_estacionalidad, mapping = aes(x = Mes, y = Media_difmin),
            color = "#757575", linewidth = 0.8) +
  labs(title = "Variabilidad estacional del CO2",
       subtitle = "Periodo 1984-2022", x = "Meses", y = "")
p
#Sí, hay variabilidad estacional, si no hubiese, el grafico tomaria un valor en
 #y constante, por lo tanto las concentraciones de CO2 en la atmosfera varian
 #segun la estacion o la epoca del anio.


#Para ver si hay un cambio en el registro del CO2 debido al COVID, comparo el
 #anio de pandemia con encierro mas estricto (2020) con el promedio por mes
 #sobre todos los anios

#Calculo la media, esta vez del dato puro
media_climatologica <- c()
for (i in 1:length(meses1)) {
  datos_mes <- datos_nuevo[datos_nuevo$Mes == i,]
  media_mes <- round(mean(datos_mes$CO2_ppm), 2)
  media_climatologica[i] <- media_mes
}

#Selecciono los datos del 2020
datos_2020 <- datos_nuevo[datos_nuevo$Anio == 2020,]

#Calculo el promedio mensual del 2020
media_2020 <- c()
for (i in 1:length(meses1)) {
  datos_mes_2020 <- datos_2020[datos_2020$Mes == i,]
  media_mes_2020 <- round(mean(datos_mes_2020$CO2_ppm), 2)
  media_2020[i] <- media_mes_2020
}

#Armo un df con la media climatologica y las medias mensuales del 2020
df_comparacion <- data.frame("Mes" = meses1,
                             "Media climatologica" = media_climatologica, 
                             "Media mensual 2020" = media_2020)

#Armo el grafico
comp <- ggplot(data = df_comparacion,
               mapping = aes(x = Mes, y = media_climatologica)) +
  geom_line(color = "black", linewidth = 1) + 
  geom_line(data = df_comparacion, mapping = aes(x = Mes, y = media_2020), 
            color = "darkred", size = 1)
comp


media_2020_des <- c()
for (i in 1:length(meses1)) {
  datos_mes_2020 <- datos_2020[datos_2020$Mes == i,]
  media_mes_2020 <- round(mean(datos_mes_2020$Dif), 2)
  media_2020_des[i] <- media_mes_2020
}

df_comp <- data.frame("Mes" = meses1,
                      "Media.climatologica" = medias_dif, 
                      "Media.mensual.2020" = media_2020_des)

comp2 <- ggplot(data = df_comp,
               mapping = aes(x = Mes, y = Media.climatologica)) +
  geom_line(color = "black", linewidth = 1) + 
  geom_line(data = df_comp, mapping = aes(x = Mes, y = media_2020_des), 
            color = "darkred", size = 1)
comp2






  