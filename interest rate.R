# Este es un comando para eliminar todo lo que se ha realizado
remove(list = ls())

# Paqueterias que necesitamos 
library(ggplot2) #funciones para graficas
library(dplyr) #transformaciones a la base de datos
library(ggthemes) #diseno de grafica

# Definimos el area de trabajo mediante una ruta con la direccion de donde esta guardada la base de datos
path <- paste("G:/Calderon's work/Interest rate/", sep = "") # Aqui solo se cambia la dirección que esta entre comillas " 

# Cargamos la base de datos 
data <- read.csv(paste(path,"irates.csv", sep = ""))
str(data) #estructura de la base
head(data) #los primeros datos de la base
range(as.character(data$DATE))
#Variables
#DS10: 10-Year Treasury Constant Maturity Rate
#DS1: 1-Year Treasury Constant Maturity Rate
#DS3: 3-Year Treasury Constant Maturity Rate
#DS2: 2-Year Treasury Constant Maturity Rate
#DS3M: 3-Month Treasury Constant Maturity Rate

# Creamos los diferenciales que nos interesa analizar
data <- data %>%
  mutate(d3m = GS10 - GS3M, # 10 años menos 3 meses
         d1y = GS10 - GS1, # 10 años menos 1 año
         d2y = GS10 - GS2, # 10 años menos 2 años
         d3y = GS10 - GS3) # 10 años menos 3 años

# Queremos ver que tiene el dataframe de data; el comando head()
# nos muestra las primeras seis observaciones de todas las vari
# ables que contenga. 
head(data)
str(data)
# Como la base de datos tiene informacion desde abril de 1953
range(as.character(data$DATE))


# Lo que queremos es analizar la informacion a partir del 2000, 
# seleccionamos la informacion que queremos. 
rates <- data[562:805,]
head(rates)

# El periodo de la informacion de rates es el siguiente:
range(as.character(rates$DATE))

# Queremos visualizar los periodos de crisis, se debe crear un objeto que incorpore las fechas, se toma como referencia  el US Business Cycle Expansions and Contractions
recess <- data.frame(inicio=as.Date(c("2001-03-01","2007-12-01")),
                     fin=as.Date(c("2001-11-01","2009-06-01")))

# Proceso de grafica
graph <- ggplot() +
  geom_line(data = rates, aes(x = as.Date(DATE), y = d3m, colour = "Diferencial 10 años - 3 meses")) +
  geom_line(data = rates, aes(x = as.Date(DATE), y = d1y, colour = "Diferencial 10 años - 1 año")) +
  geom_line(data = rates, aes(x = as.Date(DATE), y = d2y, colour = "Diferencial 10 años - 2 años")) +
  geom_line(data = rates, aes(x = as.Date(DATE), y = d3y, colour = "Diferencial 10 años - 3 años")) +
  geom_rect(data = recess,
            aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = +Inf),
            inherit.aes = FALSE, fill = "gray", alpha = 0.2) +
  geom_line() +
  geom_hline(yintercept = 0, colour = "black", linetype = "longdash") + 
  theme_classic()

graph_1 <- graph + scale_colour_manual("", 
                    breaks = c("Diferencial 10 años - 3 meses", "Diferencial 10 años - 1 año", "Diferencial 10 años - 2 años", "Diferencial 10 años - 3 años"),
                    values = c("Diferencial 10 años - 3 meses" = "red", "Diferencial 10 años - 1 año" = "blue", 
                               "Diferencial 10 años - 2 años" = "purple", "Diferencial 10 años - 3 años" = "orange"))

graph_2 <- graph_1 + labs(x = "", y = "Tasa de interés", title = "Gráfica 1.Tasa de reversión del rendimiento a 10 años",
     subtitle = "",
     caption = "Fuente: Elaboración propia con base en datos de Federal Reserve Economic Data") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        legend.position = "bottom") +
  scale_y_continuous(limits = c(-0.8, 4.5),
                      breaks = (-0.8:4.5))

# Visualizamos la grafica final. 
x11()
graph_2