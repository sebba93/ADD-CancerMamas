if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

if (!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE )
  require (tidyverse)
}

########################
#                      #
#    Carga de Datos    #
#                      #
########################

data <- read.csv(file.choose(), head = FALSE)
names(data) = c("Class","age","menopause","tumor_size",
                "inv_nodes","node_caps","deg_malig","breast",
                "breast_quad","irradiat")

##########################
#                        #
#  ANALISIS DESCRIPTIVO  #
#                        #
##########################

#EDAD DE LOS PACIENTES.

#Tabla de Frecuencias
contEdad <- xtabs(~age, data = data)
contEdadTot <- marginSums(contEdad)
contEdadFinal <- addmargins(contEdad,1)
print(contEdadFinal)

#Tabla de Proporciones
contEdadProp <- prop.table(contEdad)
contEdadProp <- addmargins(contEdadProp,1)
print(contEdadProp)

#Tabla de Porcentajes
contEdadPor <- round(prop.table(contEdad), 6)*100
contEdadPor <- addmargins(contEdadPor)
print(contEdadPor)

#Grafico de Barra
contEdadDF<-as.data.frame(contEdad)
barraEdad <- ggbarplot(contEdadDF,
                       x = "age",
                       y = "Freq",
                       fill = c("pink","purple","pink",
                                "purple","pink","purple"),
                       title = "Cantidad de Pacientes en rango etáreo",
                       xlab = "Edad",
                       ylab = "Frecuencia")

print(barraEdad)

#------------------------------------------------------------------------#
#------------------------------------------------------------------------#
#------------------------------------------------------------------------#

#FRECUENCIA DEL CUADRANTE.

#Tabla de Frecuencias
contCuad <- xtabs(~ breast + breast_quad, data = data)
contCuadTot <- marginSums(contCuad)
contCuadFinal <- addmargins(contCuadTot,1)
print(contCuadFinal)

#Tabla de Porcentajes
contCuadPor <- round(prop.table(contCuad), 6)*100
contCuadPor <- addmargins(contEdadPor)
print(contEdadPor)

#Grafico de Barra
contEdadDF<-as.data.frame(contEdad)
barraEdad <- ggbarplot(contEdadDF,
                       x = "age",
                       y = "Freq",
                       fill = c("pink","purple","pink",
                                "purple","pink","purple"),
                       title = "Cantidad de Pacientes en rango etáreo",
                       xlab = "Edad",
                       ylab = "Frecuencia")

print(barraEdad)

