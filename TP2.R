#INSTRUMENTOS_DE_ANALISIS_URBANO_TP2

#1. Para este TP se creó un nuevo repositorio en mi cuenta personal de GitHub: jimebere/TP2_BEREZOVSKY_MEU, con versión de control y se instalaron las librerias necesarias.

install.packages("tidyverse")
library(tidyverse)

#3. Como volví a trabajar con los datos del TP1, comenzé importando la base de datos correspondiente a las Obras Registradas en la Ciudad de Buenos Aires, obtenida de la pagina: https://data.buenosaires.gob.ar/dataset/obras-registradas

obras <- read.csv2("obrasregistradas-acumulado.csv",
                 stringsAsFactors = TRUE,
                 encoding = "UTF-8")

#PREGUNTA: CUANTAS DEMOLICIONES O PERMISOS DE DEMOLICION HAY REGISTRADOS EN LOS BARRIOS DE PALERMO Y BELGRANO? CUAL ES EL PROMEDIO REGISTRADO POR AÑO EN ESOS BARRIOS?

#4.TRANSFORMACIONES

### FILTRAR
#Se filtró la información referente a obras detectadas en los barrios: Palermo y Belgrano

palermo_belgrano <- obras %>% 
  filter(barrio %in% c("PALERMO", "BELGRANO"))

### Por último chequeo la selección
unique(palermo_belgrano$barrio)

#### SELECCIÓN de columnas
#Se corroboró el nombre de cada columna y se seleccionó de la tabla solo la columna tipos de obras y nombre de barrios

colnames(palermo_belgrano)

palermo_belgrano_sel1 <- palermo_belgrano %>% 
  select(3, 5, 11)

#Se seleccionaron de esos datos, solo los registros de demolición y permiso de demolición
palermo_belgrano_sel2 <- palermo_belgrano_sel1 %>% 
  filter(tipo_obra %in% c("DEMOLICION", "PERMISO DE DEMOLICION"))

install.packages("plyr")
library(plyr)
count(palermo_belgrano_sel2, "tipo_obra")

#Aqui, tal como lo indica el resultado obtenido, podemos observar que en los barrios de Palermo y Belgrano hubo, según estos registros, 17 demoliciones y 230 permisos de demolición.


##### Cuántos por año?
library(lubridate)

palermo_belgrano_anios <- palermo_belgrano_sel2 %>%
  mutate(mes = month(fecha),
         dia = day(fecha),
         anio = year(fecha)) 
count (palermo_belgrano_anios, "anio")

#Por ultimo según el resultado arrojado, la mayoria de las demoliciones y permisos de deomilicion se dieron en el año 2022

#### TP2
#Ahora, es cuando continúo con lo solicitado para el TP2 con el fin de ver estos resultados de forma gráfica.
#En primer lugar vinculo mi tabla obtenida con los datos de las comunas que estan georeferenciadas.

#1. Se instalaron las librerias necesarias

library(ggplot2)
library (sf)

#2. Cargue el dataset de las comunas georeferenciadas, que esta en formato shape y verifique con que datos cuento.

comunas<-st_read("comunas_wgs84.shp",
                 stringsAsFactors =TRUE,
                 options ="ENCODING=UTF-8")

# Primero se creó un nuevo dataset geográfico que contenga solo los datos de Comunas: Palermo y Belgrano que son los que me interesan

comunas2 <- comunas %>% filter(COMUNAS == 13 | COMUNAS == 14)

# Luego, tomando como base la tabla del TP1, se creó una nueva tabla sólo con el tipo de obra y la comuna

resumen <- palermo_belgrano %>%
  select(tipo_obra, comuna)

# Se contó el número de obras por tipo y comuna

resumen <- resumen %>%
  group_by(tipo_obra, comuna) %>%
  count()

# Se agregó una nueva columna solo con el número de las Comunas, para poder vincular ambas tablas a través de ese valor.

resumen_comuna <- resumen %>% mutate(COMUNAS = ifelse(comuna == "Comuna 13",13,14))

# Finalmente, se unieron las tablas.

tabla_grafico <- left_join (resumen_comuna, comunas, by="COMUNAS")

# Observando esta tabla, se creó un nuevo dataset que contenga solo los datos de Demoliciones:

grafico<- tabla_grafico %>% filter(tipo_obra == "DEMOLICION" | tipo_obra == "OBRA NUEVA" | tipo_obra == "PERMISO DE DEMOLICION" | tipo_obra == "REGISTRO-PERMISO OBRA" | tipo_obra == "REGISTRO DE PLANO DE DEMOLICIÓN")

# El paso siguiente es generar el gráficos sobre la respuesta del TP1, en este caso, un gráfico de barras:

install.packages("geofacet")
library(geofacet)
  
ggplot(grafico) +
  geom_bar(aes(x = tipo_obra, weight = freq, fill = BARRIOS), width = 0.5) +
  labs(
    title = "Total de permisos y demoliciones por comuna",
    x = "Tipos de Registros",
    y = "Registros"
  ) +
  facet_wrap(~comuna, scales = "free_x") +
  theme(
    legend.position = 'none',
    axis.text.x = element_text(colour = "gray25", size = 6),
    axis.text.y = element_text(colour = "gray25", size = 6)
  ) +
  scale_y_continuous(
    labels = scales::comma, 
    breaks = scales::pretty_breaks(n = 2))


# En este gráfico, se puede observar cuantas demoliciones y permisos de obra se registraron en Belgrano y cuantos en Palermo.

# Por último, y contestando a la pregunta que me hice en el TP1, generé una tabla de registros de Demoliciones en cada Comuna:
  
grafico2<- obras %>% filter(tipo_obra == "DEMOLICION")

# Hice el conteo de demoliciones por comunas: 

library(dplyr)
library(stringr)

grafico2_resumen <- grafico2 %>%
  group_by(comuna) %>%
  summarise(count(comuna)) 

comunas_todas<-st_read("comunas.shp",
                 stringsAsFactors =TRUE,
                 options ="ENCODING=UTF-8")

# Segun los resultados arrojados en la tabla "grafico2_resumen" generé este nuevo dataset, con los nombres que necesitaba. (Cabe alcarar en este punto, que probe muchas veces usar la funcion RENAME y me dio siempre eror, por eso, a los fines del ejercicio, decidi crearlo de esta forma)

df <- data.frame (COMUNAS = c ('Comuna 1', 'Comuna 2', 'Comuna 3', 'Comuna 4', 'Comuna 5', 'Comuna 6','Comuna 7', 'Comuna 8', 'Comuna 9', 'Comuna 10', 'Comuna 11', 'Comuna 12','Comuna 13','Comuna 14','Comuna 15'),
                  DEMOLICIONES = c ('3', '5', '6', '11', '4','11', '20', '6', '10', '27','33', '35', '17', '12', '15'))

df <- df %>%
  mutate(DEMOLICIONES = as.numeric(DEMOLICIONES))

# Uni esta tabla con la que contiene los datos geográficos

tabla_grafico2 <- left_join (df, comunas_todas, by="COMUNAS")

tabla_grafico2 <- st_as_sf(tabla_grafico2)

# Realice el mapa coroplético asociado a la geometría de las comunas

ggplot(tabla_grafico2) +
  geom_sf(aes(fill = DEMOLICIONES)) +
  labs(title = "Cantidad de Demoliciones por Comunas", fill = "DEMOLICIONES") +
  scale_fill_gradient(low = "darkgoldenrod1", high = "darkred") +
  theme_minimal()

# Como conclusión observamos en un color mas oscuro las comunas que registraron mayor cantidad de demoliciones, y en colores más claros las de menor registro. 