# Lectura de los datos
## Carga de las bibliotecas
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(scales)
library(grid)
library(gridExtra)

# Carga de los datos Liberia
clm <- read.csv("liberia_datos_climaticos.csv", sep = ",", na.strings = "", dec =",")

# Carga de un segundo dato, para trabajar con él
clm1 <- read.csv("liberia_datos_climaticos.csv", sep = ",", na.strings = "", dec =",")

View(clm1)
View(clm)
head(clm)
dim(clm)

#Eliminar los N/A

clm1[!complete.cases(clm1),]
clm1 <- na.omit(clm1)

# El archivo ya no tiene N/A y es un data frame con el cual se puede trabajar
## Observación de la estructura del dato
str(clm1)

## Cambio del nombre de las columnas y formato de la fecha, para un trabajo más ordenado y sencillo
clm1 <- clm1 %>%
  rename(Temperatura =Temperatura..Celsius.,
         Fecha = Date,
         Humedad = HumedadRelativa....,
         Velocidad = VelocidadViento..m.s.,
         Lluvia = Lluvia..mm.,
         Irradiacion = Irradiacion..W.m2.,
         Evaporacion = EvapoTranspiracion..mm.
         )
clm1 <- 
  clm1 %>%
  mutate(Fecha = as.Date(Fecha, format = "%d/%m/%Y"))


## Creación de los histogramas  
k<- ggplot(clm1,aes(y=Lluvia,group = 1))+
  geom_histogram(
    col="#1E90FF",
    fill="#87CEEB"
  ) +
  ggtitle("Lluvia")+
  xlab("Dato") +
  ylab("Lluvia en mm") +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
l<- ggplot(clm1, aes(y= Humedad, group = 1, colour = Humedad)) +
  geom_histogram(
    col="#98FB98",
    fill="#3CB371"
  ) +
  ggtitle("Humedad")+
  xlab("Dato") +
  ylab("Humedad Relativa %") +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
m<- ggplot(clm1, aes(y= Temperatura, group = 1, colour = Temperatura)) +
  geom_histogram(
    col="#FA8072",
    fill="#CD5C5C"
  ) +
  ggtitle("Temperatura")+
  xlab("Dato") +
  ylab("Temperatura en ???") +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
n<- ggplot(clm1, aes(y= Velocidad, group = 1, colour = Velocidad)) +
  geom_histogram(
    col="#40E0D0",
    fill="#AFEEEE"
  ) +
  ggtitle("Velocidad")+
  xlab("Dato") +
  ylab("Velocidad en m/s") +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
o<- ggplot(clm1, aes(y= Evaporacion, group = 1, colour = Evaporacion)) +
  geom_histogram(
    col="#BC8F8F",
    fill="#FFDEAD"
  ) +
  ggtitle("Evaporación")+
  xlab("Dato") +
  ylab("Evaporación en mm") +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
p<- ggplot(clm1, aes(y= Irradiacion, group = 1, colour = Irradiacion)) +
  geom_histogram(
    col="#DDA0DD",
    fill="#D8BFD8"
  ) +
  ggtitle("Irradiación")+
  xlab("Dato") +
  ylab("Irradiación en Wm2") +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )

### Arrange para mostrar los histogramas en paneles
grid.arrange(k,l,m,n,o,p, nrow = 2, ncol = 3)

## Carga de los datos en un formato promediado
Data_prom <-
  clm1 %>%
  select(Fecha, Temperatura, Humedad, Velocidad, Lluvia, Irradiacion, Evaporacion)%>%
  mutate(Fecha = as.Date(Fecha, format = "%d/%m/%Y"))%>%
  group_by (Fecha = format(Fecha,"%Y")) %>%
  summarise(Lluvia = sum(Lluvia),Evaporacion = sum(Evaporacion),
            Temperatura = mean(Temperatura),Velocidad = mean(Velocidad),
            Irradiacion = mean(Irradiacion), Humedad = mean(Humedad))


## Creación de los gráficos lineales promediados en paneles
a<- ggplot(Data_prom,aes(x=Fecha,y=Lluvia,group = 1, colour=Lluvia,)) + 
  geom_line() +
  ylab("\nLluvia en mm") +
  geom_point() +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
b<- ggplot(Data_prom, aes(x= Fecha, y= Humedad, group = 1, colour = Humedad,)) +
  geom_line() +
  ylab("\nHumedad Relativa %") +
  geom_point() +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
c<- ggplot(Data_prom, aes(x= Fecha, y= Temperatura, group = 1, colour = Temperatura)) +
  geom_line() +
  ylab("\nTemperatura en °C") +
  geom_point() +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
d<- ggplot(Data_prom, aes(x= Fecha, y= Velocidad, group = 1, colour = Velocidad)) +
  geom_line() +
  ylab("\nVelocidad en m/s") +
  geom_point() +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
e<- ggplot(Data_prom, aes(x= Fecha, y= Evaporacion, group = 1, colour = Evaporacion)) +
  geom_line() +
  ylab("\nEvaporación en mm") +
  geom_point() +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
f<- ggplot(Data_prom, aes(x= Fecha, y= Irradiacion, group = 1, colour = Irradiacion)) +
  geom_line() +
  ylab("\nIrradiacion en Wm2") +
  geom_point() +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )

## Arrange para mostrar los datos lineales promediados en paneles
grid.arrange(a,b,c,d,e,f, nrow =6, ncol =1)


## Gráfico de la nube de puntos

g<- ggplot(clm1,aes(x=Fecha, y=Lluvia,group = 1))+
  geom_point(
    col="#1E90FF",
    fill="#87CEEB"
  ) +
  ggtitle("Lluvia")+
  xlab("Dato") +
  ylab("Lluvia en mm") +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
h<- ggplot(clm1, aes(x=Fecha, y= Humedad, group = 1, colour = Humedad)) +
  geom_point(
    col="#98FB98",
    fill="#3CB371"
  ) +
  ggtitle("Humedad")+
  xlab("Dato") +
  ylab("Humedad Relativa %") +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
i<- ggplot(clm1, aes(x=Fecha, y= Temperatura, group = 1, colour = Temperatura)) +
  geom_point(
    col="#FA8072",
    fill="#CD5C5C"
  ) +
  ggtitle("Temperatura")+
  xlab("Dato") +
  ylab("Temperatura en ???") +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
j<- ggplot(clm1, aes(x=Fecha, y= Velocidad, group = 1, colour = Velocidad)) +
  geom_point(
    col="#40E0D0",
    fill="#AFEEEE"
  ) +
  ggtitle("Velocidad")+
  xlab("Dato") +
  ylab("Velocidad en m/s") +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
w<- ggplot(clm1, aes(x=Fecha, y= Evaporacion, group = 1, colour = Evaporacion)) +
  geom_point(
    col="#BC8F8F",
    fill="#FFDEAD"
  ) +
  ggtitle("Evaporación")+
  xlab("Dato") +
  ylab("Evaporación en mm") +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )
z<- ggplot(clm1, aes(x=Fecha, y= Irradiacion, group = 1, colour = Irradiacion)) +
  geom_point(
    col="#DDA0DD",
    fill="#D8BFD8"
  ) +
  ggtitle("Irradiación")+
  xlab("Dato") +
  ylab("Irradiación en Wm2") +
  theme_ipsum(
    axis_text_size = 9,
    ticks = TRUE,
    axis = "y",
    grid = "Y,y"
  )

grid.arrange(g,h,i,j,w,z, nrow =2, ncol =3)