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

#FRECUENCIA DE LA MAMA Y CUADRANTE DE ESTA.

#Tabla de Frecuencias
contCuad <- xtabs(~ breast + breast_quad, data = data)
contCuadTot <- addmargins(contCuad)
contCuadTot


#Tabla de Porcentajes
contCuadPor <- round(prop.table(contCuad),7)*100
contCuadPorTot <- addmargins(contCuadPor)
contCuadPorTot


#Gráficos de Barras
#Barra Segmentada
contCuadDF <- as.data.frame(contCuad)
bSegCuad <- ggplot(contCuadDF, aes(fill = breast, y = Freq, x = breast_quad))
bSegCuad <- bSegCuad + geom_bar(position = "stack", stat = "identity")
bSegCuad <- bSegCuad + labs(y = "Frecuencias") + ggtitle("Frecuencias por Cuadrante y Mama") + labs(x = "Cuadrantes")
bSegCuad <- bSegCuad + theme_pubr()
bSegCuad

#Barra Agrupada
contCuadDF <- as.data.frame(contCuad)
bAgrCuad <- ggplot(contCuadDF, aes(fill = breast, y = Freq, x = breast_quad))
bAgrCuad <- bAgrCuad + geom_bar(position = "dodge", stat = "identity")
bAgrCuad <- bAgrCuad + labs(y = "Frecuencias") + ggtitle("Frecuencias por Cuadrante y Mama") + labs(x = "Cuadrantes")
bAgrCuad <- bAgrCuad + theme_pubr()
bAgrCuad

#Barra Segmentada Estandarizada
contCuadDF <- as.data.frame(contCuad)
bEstCuad <- ggplot(contCuadDF, aes(fill = breast, y = Freq, x = breast_quad))
bEstCuad <- bEstCuad + geom_bar(position = "fill", stat = "identity")
bEstCuad <- bEstCuad + labs(y = "Frecuencias") + ggtitle("Frecuencias por Cuadrante y Mama") + labs(x = "Cuadrantes")
bEstCuad <- bEstCuad + theme_pubr()
bEstCuad

#------------------------------------------------------------------------#
#------------------------------------------------------------------------#
#------------------------------------------------------------------------#

#























