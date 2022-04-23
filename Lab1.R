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
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(Hmisc)){
  install.packages("Hmisc", dependencies = TRUE )
  require (Hmisc)
}

if (!require(corrplot)){
  install.packages("corrplot", dependencies = TRUE )
  require (corrplot)
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
#Tamaño de los tumores 

#Tabla de Frecuencias
contSize <- xtabs(~tumor_size, data = data)
contSizeTot <- marginSums(contSize)
contSizeFinal <- addmargins(contSize,1)
print(contSizeFinal)

#Grafico de Barra
contSizeDF<-as.data.frame(contSize)
contSizeID   <- c(1:11)
contSizeVal  <- c(8,4,28,30,50,54,60,19,22,3,8)
contSizeName <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-55")

#Se plotea
barplot(contSizeVal,names.arg=contSizeName,xlab="Tamaño [mm]",
        ylab="Frecuencia",col="purple",
        main="Frecuencias de Tamaños",border="pink")

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
#Tabla de Proporciones
contNodeProp <- prop.table(contNode)
contNodeProp <- addmargins(contNodeProp,1)
print(contNodeProp)

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


#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------

#FRECUENCIA de radioterapia Y LA EXISTENCIA DE celular cancerigenas que atravesaron.

#Tabla de Frecuencias
contirra <- xtabs(~ irradiat + node_caps, data = data)
contirraTot <- addmargins(contirra)
contirraTot
#Tabla de Proporciones
contirraProp <- prop.table(contirra)
contirraProp <- addmargins(contirraProp,1)
print(contirraProp)

#Tabla de Porcentajes
contirraPor <- round(prop.table(contirra),7)*100
contirraPorTot <- addmargins(contirraPor)
contirraPorTot


#Tabla de Proporciones
contirraProp <- prop.table(contirraPor)
contirraProp <- addmargins(contirraProp,1)
print(contirraProp)

#Gráficos de Barras
#Barra Segmentada
contirraDF <- as.data.frame(contirra)
bSegirra <- ggplot(contirraDF, aes(fill = irradiat, y = Freq, x = node_caps))
bSegirra <- bSegirra + geom_bar(position = "stack", stat = "identity")
bSegirra <- bSegirra + labs(y = "Frecuencias") + ggtitle("Frecuencias por radioterapias y nodulos cancer") + labs(x = "Radioterapia")
bSegirra <- bSegirra + theme_pubr()
bSegirra

#Barra Agrupada
contirraDF <- as.data.frame(contirra)
bAgrirra <- ggplot(contirraDF, aes(fill = irradiat, y = Freq, x = node_caps))
bAgrirra <- bAgrirra + geom_bar(position = "dodge", stat = "identity")
bAgrirra <- bAgrirra + labs(y = "Frecuencias") + ggtitle("Frecuencias por radioterapias y nodulos cancer") + labs(x = "Radioterapia")
bAgrirra <- bAgrirra + theme_pubr()
bAgrirra

#Barra Segmentada Estandarizada
contirraDF <- as.data.frame(contirra)
bEstirra <- ggplot(contirraDF, aes(fill = irradiat, y = Freq, x = node_caps))
bEstirra <- bEstirra + geom_bar(position = "fill", stat = "identity")
bEstirra <- bEstirra + labs(y = "Frecuencias") + ggtitle("Frecuencias por radioterapias y nodulos cancer") + labs(x = "Radioterapia")
bEstirra <- bEstirra + theme_pubr()
bEstirra





#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------

#FRECUENCIA quadrante Y LA EXISTENCIA DE celular cancerigenas que atravesaron.

#Tabla de Frecuencias
contNoqu <- xtabs(~ node_caps + breast_quad, data = data)
contNoquTot <- addmargins(contNoqu)
contNoquTot
#Tabla de Proporciones
contNoquProp <- prop.table(contNoqu)
contNoquProp <- addmargins(contNoquProp,1)
print(contNoquProp)

#Tabla de Porcentajes
contNoquPor <- round(prop.table(contNoqu),5)*100
contNoquPorTot <- addmargins(contNoquPor)
contNoquPorTot


#Tabla de Proporciones
contNoquProp <- prop.table(contNoquPor)
contNoquProp <- addmargins(contNoquProp,1)
print(contNoquProp)

#Gráficos de Barras
#Barra Segmentada
contNoquDF <- as.data.frame(contNoqu)
bSegNoqu <- ggplot(contNoquDF, aes(fill = node_caps, y = Freq, x = breast_quad))
bSegNoqu <- bSegNoqu + geom_bar(position = "stack", stat = "identity")
bSegNoqu <- bSegNoqu + labs(y = "Frecuencias") + ggtitle("Frecuencias por cuadrante y nodulos cancer") + labs(x = "Cuadrante")
bSegNoqu <- bSegNoqu + theme_pubr()
bSegNoqu

#Barra Agrupada
contNoquDF <- as.data.frame(contNoqu)
bAgrNoqu <- ggplot(contNoquDF, aes(fill = node_caps, y = Freq, x = breast_quad))
bAgrNoqu <- bAgrNoqu + geom_bar(position = "dodge", stat = "identity")
bAgrNoqu <- bAgrNoqu + labs(y = "Frecuencias") + ggtitle("Frecuencias por cuadrante y nodulos cancer") + labs(x = "Cuadrante")
bAgrNoqu <- bAgrNoqu + theme_pubr()
bAgrNoqu

#Barra Segmentada Estandarizada
contNoquDF <- as.data.frame(contNoqu)
bEstNoqu <- ggplot(contNoquDF, aes(fill = node_caps, y = Freq, x = breast_quad))
bEstNoqu <- bEstNoqu + geom_bar(position = "fill", stat = "identity")
bEstNoqu <- bEstNoqu + labs(y = "Frecuencias") + ggtitle("Frecuencias por cuadrante y nodulos cancer") + labs(x = "Cuadrante")
bEstNoqu <- bEstNoqu + theme_pubr()
bEstNoqu



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


# Hacer prueba chi - cuadrado de independencia cuadrante y mama .
pruebaChiCuad <- chisq.test ( contCuadProp )

cat ("\ nResultado de la prueba :\n")
print ( pruebaChiCuad )



#------------------------------------------------------------------------#
#------------------------------------------------------------------------#
#------------------------------------------------------------------------#

#Realizaremos una tabla de corelacción,Usamos chi cuadrado Pearson para variables 
#continuas, y Fisher para variables dicotómicas y la cual tomará una matriz en donde
#los datos dicotómigos tomarán valores 0 1 en la variable Class, irriadat,
#node-caps

classMC<-1
i<-1
while(i<287){
  if(data[i,1]=="no-recurrence-events"){
    classMC[i]<-0
  }
  if(data[i,1]=="recurrence-events"){
    classMC[i]<-1
  }
  i <- i+1
}

nodecapsMC<-1
i<-1
while(i<287){
  if(data[i,6]=="yes"){
    print(data[i,6])
    nodecapsMC[i]<-1
  }
  if(data[i,6]=="no"){
    nodecapsMC[i]<-0
  }
  if(data[i,6]=="?"){
    nodecapsMC[i]<-0
  }
  i <- i+1
}
nodecapsMC

irriadiatMC<-1
i<-1
while(i<287){
  if(data[i,10]=="no"){
    irriadiatMC[i]<-0
  }
  if(data[i,10]=="yes"){
    irriadiatMC[i]<-1
  }
  i <- i+1
}

#Creamos la matriz y aplicamos correlacion
matrizCorr = cbind(ageMC,tumorMC)
matrixCor = round(cor(matrizCorr),2)
matrixCor

#calculamos ahora el valor P
pMatrixCor = rcorr(as.matrix(matrizCorr))
pMatrixCor



#------------------------------------------------------------------------#
#------------------------------------------------------------------------#
#------------------------------------------------------------------------#
#Prueba de fisher
#nodesMC,nodecapsMC,irriadiatMC,classMC

fisher1 <-cbind(nodesMC,nodecapsMC)
fisher1 <-xtabs(~.,fisher1)

testFisher1 <- fisher.test(fisher1,0.95,simulate.p.value=TRUE) 
testFisher1

#--------------------------------#
fisher2 <-cbind(nodesMC,irriadiatMC)
fisher2 <-xtabs(~.,fisher2)

testFisher2 <- fisher.test(fisher2,0.95,simulate.p.value=TRUE) 
testFisher2

#--------------------------------#
fisher3 <-cbind(nodesMC,classMC)
fisher3 <-xtabs(~.,fisher3)


testFisher3 <- fisher.test(fisher3,0.95,simulate.p.value=TRUE) 
testFisher3

#--------------------------------#
fisher4 <-cbind(nodecapsMC,irriadiatMC)
fisher4 <-xtabs(~.,fisher4)


testFisher4 <- fisher.test(fisher4,0.95,simulate.p.value=TRUE) 
testFisher4

#--------------------------------#
fisher5 <-cbind(nodecapsMC,classMC)
fisher5 <-xtabs(~.,fisher5)


testFisher5 <- fisher.test(fisher5,0.95,simulate.p.value=TRUE) 
testFisher5

#--------------------------------#
fisher6 <-cbind(irriadiatMC,classMC)
fisher6 <-xtabs(~.,fisher6)


testFisher6 <- fisher.test(fisher6,0.95,simulate.p.value=TRUE) 
testFisher6

##TEST ANOVA

