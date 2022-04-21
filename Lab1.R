if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

if (!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE )
  require (tidyverse)
}

if (!require(stringr)){
  install.packages("stringr", dependencies = TRUE )
  require (stringr)
}

if (!require(dplyr)){
  install.packages("stringr", dependencies = TRUE )
  require (dplyr)
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

#Tabla de Proporciones
contCuadProp <- prop.table(contCuadPor)
contCuadProp <- addmargins(contCuadProp,1)
print(contCuadProp)

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


#Tabla de Proporciones
contGradoProp <- prop.table(contGradoPor)
contGradoProp <- addmargins(contGradoProp,1)
print(contGradoProp)

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

#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------

#FRECUENCIA DEL GRADO DEL TUMOR Y LA EXISTENCIA DE celular cancerigenas que atravesaron.

#Tabla de Frecuencias
contNode <- xtabs(~ node_caps + deg_malig, data = data)
contNodeTot <- addmargins(contNode)
contNodeTot


#Tabla de Porcentajes
contNodePor <- round(prop.table(contNode),7)*100
contNodePorTot <- addmargins(contNodePor)
contNodePorTot


#Tabla de Proporciones
contNodeProp <- prop.table(contNodePor)
contNodeProp <- addmargins(contNodeProp,1)
print(contNodeProp)

#Gráficos de Barras
#Barra Segmentada
contNodeDF <- as.data.frame(contNode)
bSegNode <- ggplot(contNodeDF, aes(fill = node_caps, y = Freq, x = deg_malig))
bSegNode <- bSegNode + geom_bar(position = "stack", stat = "identity")
bSegNode <- bSegNode + labs(y = "Frecuencias") + ggtitle("Frecuencias por Grado y nodulos cancer") + labs(x = "Grado")
bSegNode <- bSegNode + theme_pubr()
bSegNode

#Barra Agrupada
contNodeDF <- as.data.frame(contNode)
bAgrNode <- ggplot(contNodeDF, aes(fill = node_caps, y = Freq, x = deg_malig))
bAgrNode <- bAgrNode + geom_bar(position = "dodge", stat = "identity")
bAgrNode <- bAgrNode + labs(y = "Frecuencias") + ggtitle("Frecuencias por Grado y nodulos cancer") + labs(x = "Grado")
bAgrNode <- bAgrNode + theme_pubr()
bAgrNode

#Barra Segmentada Estandarizada
contNodeDF <- as.data.frame(contNode)
bEstNode <- ggplot(contNodeDF, aes(fill = node_caps, y = Freq, x = deg_malig))
bEstNode <- bEstNode + geom_bar(position = "fill", stat = "identity")
bEstNode <- bEstNode + labs(y = "Frecuencias") + ggtitle("Frecuencias por Grado y nodulos cancer") + labs(x = "Grado")
bEstNode <- bEstNode + theme_pubr()
bEstNode










#------------------------------------------------------------------------#
#------------------------------------------------------------------------#
#------------------------------------------------------------------------#

#Los datos están en string asi que los pasaremos a numérico y de paso sacaremos
#a marca de clase de cada elemento de manera inversa, ya que NO tenemos los
#datos originales asi que si asinamos el valor de la marca de clase a cada
#elemento original podremos hacer el análisis descriptivo pero NUMERICO

#Transformación de valores para "age" y obtener marca de clase
ager1<-as.numeric(str_extract(data[["age"]],'[0-9]'))*10
ager2<-as.numeric(str_extract(data[["age"]],'[0-9]'))*10+9
ageMC<-((ager2)+(ager1))/2


#Transformación de valores para "tumor_size" y obtener marca de clase
tumorMC<-1
i<-1
while(i<287){
  if(data[i,4]=="0-4"){
    tumorMC[i]<-2
  }
  if(data[i,4]=="5-9"){
    tumorMC[i]<-7
  }
  if(data[i,4]=="10-14"){
    tumorMC[i]<-12
  }
  if(data[i,4]=="15-19"){
    tumorMC[i]<-17
  }
  if(data[i,4]=="20-24"){
    tumorMC[i]<-22
  }
  if(data[i,4]=="25-29"){
    tumorMC[i]<-27
  }
  if(data[i,4]=="30-34"){
    tumorMC[i]<-32
  }
  if(data[i,4]=="35-39"){
    tumorMC[i]<-37
  }
  if(data[i,4]=="40-44"){
    tumorMC[i]<-42
  }
  if(data[i,4]=="45-49"){
    tumorMC[i]<-47
  }
  if(data[i,4]=="50-54"){
    tumorMC[i]<-52
  }
  if(data[i,4]=="55-59"){
    tumorMC[i]<-57
  }
  i <- i+1
}

#Transformación de valores para "inv_nodes" y obtener marca de clase
nodesMC<-1
i<-1
while(i<287){
  if(data[i,5]=="0-2"){
    nodesMC[i]<-1
  }
  if(data[i,5]=="3-5"){
    nodesMC[i]<-4
  }
  if(data[i,5]=="6-8"){
    nodesMC[i]<-7
  }
  if(data[i,5]=="9-11"){
    nodesMC[i]<-10
  }
  if(data[i,5]=="12-14"){
    nodesMC[i]<-13
  }
  if(data[i,5]=="15-17"){
    nodesMC[i]<-16
  }
  if(data[i,5]=="24-26"){
    nodesMC[i]<-25
  }
  
  i <- i+1
}

#Se proceden a crear los resúmenes para los datos numericos
resumenAge <-data%>%summarize(meanAge = mean(ageMC),
                              medianAge = median(ageMC),
                              varAge = var(ageMC),
                              sdAge = sd(ageMC),
                              IQRAge = IQR(ageMC))

resumenTumor <-data%>%summarize(meanAge = mean(tumorMC),
                                medianAge = median(tumorMC),
                                varAge = var(tumorMC),
                                sdAge = sd(tumorMC),
                                IQRAge = IQR(tumorMC))

resumenNodes <-data%>%summarize(meanAge = mean(nodesMC),
                                medianAge = median(nodesMC),
                                varAge = var(nodesMC),
                                sdAge = sd(nodesMC),
                                IQRAge = IQR(nodesMC))


data = cbind(data,ageMC)
data = cbind(data,tumorMC)
data = cbind(data,nodesMC)





#------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------

# Hacer prueba chi - cuadrado de independencia del grado del tumor y con si recibio radiacion .

pruebaChiGrado <- chisq.test ( contGradoProp )

cat ("\ nResultado de la prueba :\n")
print ( pruebaChiGrado )

#no sirven contDegProp, contEdadProp

# Hacer prueba chi - cuadrado de independencia cuadrante y mama .
pruebaChiCuad <- chisq.test ( contCuadProp )

cat ("\ nResultado de la prueba :\n")
print ( pruebaChiCuad )


# Hacer prueba chi - cuadrado de independencia del grado del tumor y si presenta celulas cancerigenas en nodos linfaticos.
pruebaChiNode <- chisq.test ( contNodeProp )

cat ("\ nResultado de la prueba :\n")
print ( pruebaChiNode )



