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




#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------


#Tabla de Frecuencias
contDeg <- xtabs(~deg_malig, data = data)
contDegTot <- marginSums(contDeg)
contDegFinal <- addmargins(contDeg,1)
print(contDegFinal)

#Tabla de Proporciones
contDegProp <- prop.table(contDeg)
contDegProp <- addmargins(contDegProp,1)
print(contDegProp)

#Tabla de Porcentajes
contDegPor <- round(prop.table(contDeg), 3)*100
contDegPor <- addmargins(contDegPor)
print(contDegPor)

#Grafico de Barra
contDegDF<-as.data.frame(contDeg)
barraDeg <- ggbarplot(contDegDF,
                       x = "deg_malig",
                       y = "Freq",
                       fill = c("pink","purple","pink"),
                       title = "Cantidad de Pacientes con grado de tumor ",
                       xlab = "Grado",
                       ylab = "Frecuencia")

print(barraDeg)

#------------------------------------------------------------------------#
#------------------------------------------------------------------------#
#------------------------------------------------------------------------#

#FRECUENCIA DEL GRADO DEL TUMOR Y LA EXISTENCIA DE RADIOTERAPIA.

#Tabla de Frecuencias
contGrado <- xtabs(~ irradiat + deg_malig, data = data)
contGradoTot <- addmargins(contGrado)
contGradoTot


#Tabla de Porcentajes
contGradoPor <- round(prop.table(contGrado),7)*100
contGradoPorTot <- addmargins(contGradoPor)
contGradoPorTot


#Gráficos de Barras
#Barra Segmentada
contGradoDF <- as.data.frame(contGrado)
bSegGrado <- ggplot(contGradoDF, aes(fill = irradiat, y = Freq, x = deg_malig))
bSegGrado <- bSegGrado + geom_bar(position = "stack", stat = "identity")
bSegGrado <- bSegGrado + labs(y = "Frecuencias") + ggtitle("Frecuencias por Grado y radioterapia") + labs(x = "Grado")
bSegGrado <- bSegGrado + theme_pubr()
bSegGrado

#Barra Agrupada
contGradoDF <- as.data.frame(contGrado)
bAgrGrado <- ggplot(contGradoDF, aes(fill = irradiat, y = Freq, x = deg_malig))
bAgrGrado <- bAgrGrado + geom_bar(position = "dodge", stat = "identity")
bAgrGrado <- bAgrGrado + labs(y = "Frecuencias") + ggtitle("Frecuencias por Grado y Radioterapia") + labs(x = "Grado")
bAgrGrado <- bAgrGrado + theme_pubr()
bAgrGrado

#Barra Segmentada Estandarizada
contGradoDF <- as.data.frame(contGrado)
bEstGrado <- ggplot(contGradoDF, aes(fill = irradiat, y = Freq, x = deg_malig))
bEstGrado <- bEstGrado + geom_bar(position = "fill", stat = "identity")
bEstGrado <- bEstGrado + labs(y = "Frecuencias") + ggtitle("Frecuencias por Grado y Radioterapia") + labs(x = "Grado")
bEstGrado <- bEstGrado + theme_pubr()
bEstGrado


















