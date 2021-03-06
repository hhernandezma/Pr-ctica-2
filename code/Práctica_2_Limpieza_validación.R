# 1. Descripci�n del dataset. �Por qu� es importante y c�al pregunta/problema 
# pretende # responder?

#Carpeta de trabajo
setwd("H:/Master_Ciencia_datos")

# Lectura de datos

data <- read.csv("H:/Master_Ciencia_Datos/Pr�ctica_2/iris.csv")
attach(data)
str(data)

# Las primeras 5 filas
head(data,5)

#Las �ltimas 5 filas
tail(data ,5)


# 2. Integraci�n y selecci�n de los datos de inter�s a analizar.

# eliminar columna Id

data$Id <- NULL

# Transformar Species en Factor

data <- data.frame(SepalLengthCm, SepalWidthCm, PetalLengthCm, PetalWidthCm, 
                   Species, stringsAsFactors=TRUE)
# Prueba
str(data)

# 3. Limpieza de los datos.

# 3.1

# �Los datos contienen ceros o elementos vac�os?

# valores ausentes

anyNA(data)

# 3.2. Identificaci�n y tratamiento de valores extremos.

# Estadisticos descriptivos

summary(data)

# Estadisticos descriptivos por especie

tapply(data$SepalLengthCm, data$Species, summary)
tapply(data$SepalWidthCm, data$Species, summary)
tapply(data$PetalLengthCm, data$Species, summary)
tapply(data$PetalWidthCm, data$Species, summary)


# Por cada varaible en un solo gr�fico

boxplot(SepalLengthCm, SepalWidthCm, PetalLengthCm, PetalWidthCm,   
        names =c("Longitud del s�palo","Ancho del s�palo","Longitud del p�talo",
                "Ancho del p�talo"), horizontal=FALSE,main="Diagramas de caja Iris",
        col = c("orange3", "yellow3", "green3", "grey"),
        xlab = "Atributos", ylab = "Centimetros")

# Por cada variable seg�n su especie

library(ggplot2)
library(gridExtra)
library(gridBase)
library(plyr)


BpSl <- ggplot(iris, aes(Species, SepalLengthCm, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Longitud del s�palo (cm)", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")

BpSw <-  ggplot(iris, aes(Species, SepalWidthCm, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Ancho del s�palo (cm)", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")

BpPl <- ggplot(iris, aes(Species, PetalLengthCm, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Longitud del p�talo (cm)", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")

BpPw <-  ggplot(iris, aes(Species, PetalWidthCm, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Ancho del p�talo (cm)", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Box Plot", x = "Species")

grid.arrange(BpSl  + ggtitle(""),
             BpSw  + ggtitle(""),
             BpPl + ggtitle(""),
             BpPw + ggtitle(""),
             nrow = 2)

# Exportaci�n de los datos preprocesados

# Una vez realizado sobre el conjunto de datos inicial los 
# procedimientos de integraci�n, validaci�n y limpieza anteriores, procedemos 
# a guardar estos en un nuevo fichero denominado Automobile_data_clean.csv:


write.csv(data, "data_clean.csv")


# 4. An�isis de los datos.


# 4.1. Selecci�n de los grupos de datos que se quieren analizar/comparar 
# (planificaci�nn de los an�lisis a aplicar).

# Separar en grupos seg�n un factor

setosa <- data[1:50, 1:4]
versicolor <- data[51:100, 1:4]
virginica <- data[101:150, 1:4]

# verificaci�n

str(setosa)
str(versicolor)
str(virginica)


# 4.2. Comprobaci�nn de la normalidad y homogeneidad de la varianza.

# Histograma y curva de densidad

# Longitud del s�palo

ggplot(data = data, aes(x = SepalLengthCm)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "gray60") +
  geom_density(aes(color = "Longitud de s�palo"), lwd = 0.95) +
  stat_function(aes(color = "Normal"), fun = dnorm, lwd = 0.95,
                args = list(mean = mean(data$SepalLengthCm),
                            sd = sd(data$SepalLengthCm))) +
  scale_colour_manual("Densidad", values = c("red", "blue")) +
  labs(x = "Longitud de s�palo (cm)", y = "Densidad",
       title = "Distribuci�n de la longitud de s�palo vs curva normal") +
  theme_bw()

# gr�fico de cuantiles te�ricos (Q-Q plot)

qqnorm(y = data$SepalLengthCm)
qqline(y = data$SepalLengthCm)

# Pruebas de normalidad

#library(normtest)

library(nortest)

# Anderson-Darling

ad.test(data$SepalLengthCm)


# Histograma y curva de densidad

# Ancho del s�palo

ggplot(data = data, aes(x = SepalWidthCm)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "gray60") +
  geom_density(aes(color = "Ancho del s�palo"), lwd = 0.95) +
  stat_function(aes(color = "Normal"), fun = dnorm, lwd = 0.95,
                args = list(mean = mean(data$SepalWidthCm),
                            sd = sd(data$SepalWidthCm))) +
  scale_colour_manual("Densidad", values = c("red", "blue")) +
  labs(x = "Ancho del s�palo (cm)", y = "Densidad",
       title = "Distribuci�n del ancho del s�palo vs curva normal") +
  theme_bw()

# gr�fico de cuantiles te�ricos (Q-Q plot)

qqnorm(y = data$SepalWidthCm)
qqline(y = data$SepalWidthCm)

# Prueba de normalidad

# Anderson-Darling

ad.test(data$SepalWidthCm)


# Longitud del p�talo 

# Histograma y curva de densidad


ggplot(data = data, aes(x = PetalLengthCm)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "gray60") +
  geom_density(aes(color = "Longitud del p�talo"), lwd = 0.95) +
  stat_function(aes(color = "Normal"), fun = dnorm, lwd = 0.95,
                args = list(mean = mean(data$PetalLengthCm),
                            sd = sd(data$PetalLengthCm))) +
  scale_colour_manual("Densidad", values = c("red", "blue")) +
  labs(x = "Longitud del p�talo (cm)", y = "Densidad",
       title = "Distribuci�n de la longitud del p�talo vs curva normal") +
  theme_bw()

# gr�fico de cuantiles te�ricos (Q-Q plot)

qqnorm(y = data$PetalLengthCm)
qqline(y = data$PetalWidthCm)

# Prueba de normalidad

# Anderson-Darling

ad.test(data$PetalLengthCm)


# Ancho del p�talo 

# Histograma y curva de densidad


ggplot(data = data, aes(x = PetalWidthCm)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "gray60") +
  geom_density(aes(color = "Ancho del p�talo"), lwd = 0.95) +
  stat_function(aes(color = "Normal"), fun = dnorm, lwd = 0.95,
                args = list(mean = mean(data$PetalWidthCm),
                            sd = sd(data$PetalWidthCm))) +
  scale_colour_manual("Densidad", values = c("red", "blue")) +
  labs(x = "Ancho del p�talo (cm)", y = "Densidad",
       title = "Distribuci�n del ancho del p�talo vs curva normal") +
  theme_bw()

# gr�fico de cuantiles te�ricos (Q-Q plot)

qqnorm(y = data$PetalWidthCm)
qqline(y = data$PetalWidthCm)

# Prueba de normalidad

# Anderson-Darling

ad.test(data$PetalWidthCm)


# Seg�n especie 


library(MVN)

# tipo de flor setosa

# Histograma y curva de densidad

result<- mvn(data=setosa, mvnTest="royston", univariatePlot="histogram")

# Gr�fico Q-Q

result<-mvn(data=setosa, mvnTest = "royston", univariatePlot = "qqplot")

# Test de Anderson Darling

result <- mvn(data = setosa, mvnTest = "royston", univariateTest = "AD", desc = TRUE)
result

# tipo de flor versicolor

# Histograma y curva de densidad

result<- mvn(data=versicolor, mvnTest="royston", univariatePlot="histogram")

# Gr�fico Q-Q

result<-mvn(data=versicolor, mvnTest = "royston", univariatePlot = "qqplot")

# Test de Anderson Darling

result <- mvn(data = versicolor, mvnTest = "royston", univariateTest = "AD", desc = TRUE)
result


# tipo de flor virginica

# Histograma y curva de densidad

result<- mvn(data=virginica, mvnTest="royston", univariatePlot="histogram")

# Gr�fico Q-Q

result<-mvn(data=virginica, mvnTest = "royston", univariatePlot = "qqplot")

# Test de Anderson Darling

result <- mvn(data = virginica, mvnTest = "royston", univariateTest = "AD", desc = TRUE)
result


# An�lisis de frecuencia con el histograma


# Longitud del s�palo 

HisSl <- ggplot(data=data, aes(x=SepalLengthCm))+
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Longitud del s�palo (cm)") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de la longitud del s�palo")+
  geom_vline(data=data, aes(xintercept = mean(SepalLengthCm)),linetype="dashed",color="grey")

# Ancho del s�palo

HistSw <- ggplot(data=data, aes(x=SepalWidthCm)) +
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Ancho del s�palo (cm)") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma del ancho del s�palo")+
  geom_vline(data=data, aes(xintercept = mean(SepalWidthCm)),linetype="dashed",color="grey")

# Longitud del p�talo

HistPl <- ggplot(data=data, aes(x=PetalLengthCm))+
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Longitud del p�talo (cm)") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de la longitud del p�talo")+
  geom_vline(data=data, aes(xintercept = mean(PetalLengthCm)),
             linetype="dashed",color="grey")

# Ancho del p�talo

HistPw <- ggplot(data=data, aes(x=PetalWidthCm))+
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Ancho del p�talo (cm)") +  
  ylab("Frequency") + 
  theme(legend.position="right" )+
  ggtitle("Histograma del ancho del p�talo")+
  geom_vline(data=data, aes(xintercept = mean(PetalWidthCm)),linetype="dashed",color="grey")

# Visualizaci�n en conjunto

grid.arrange(HisSl + ggtitle(""),
             HistSw + ggtitle(""),
             HistPl + ggtitle(""),
             HistPw  + ggtitle(""),
             nrow = 2)

# An�lisis de densidad

# Longitud del p�talo

DhistPl <- ggplot(data, aes(x=PetalLengthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(PetalLengthCm),  colour=Species),linetype="dashed",color="grey", size=1)+
  xlab("Longitud del p�talo (cm)") +  
  ylab("Densidad")+
  theme(legend.position="none")

# Ancho del p�talo

DhistPw <- ggplot(data, aes(x=PetalWidthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(PetalWidthCm),  colour=Species),linetype="dashed",color="grey", size=1)+
  xlab("Ancho del p�talo (cm)") +  
  ylab("Densidad")

# Ancho del s�palo

DhistSw <- ggplot(data, aes(x=SepalWidthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(SepalWidthCm),  colour=Species), linetype="dashed",color="grey", size=1)+
  xlab("Ancho del s�palo (cm)") +  
  ylab("Densidad")+
  theme(legend.position="none")

# Longitud del s�palo

DhistSl <- ggplot(data, aes(x=SepalLengthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(SepalLengthCm),  colour=Species),linetype="dashed", color="grey", size=1)+
  xlab("Longitud del s�palo (cm)") +  
  ylab("Densidad")+
  theme(legend.position="none")

# visualizaci�n conjunta

grid.arrange(DhistSl + ggtitle(""),
             DhistSw  + ggtitle(""),
             DhistPl + ggtitle(""),
             DhistPw  + ggtitle(""),
             nrow = 2)



# Homogeneidad

# Test de Levene

library(car)

leveneTest(y = data$SepalLengthCm, group = data$Species, center = "median")

leveneTest(y = data$SepalWidthCm, group = data$Species, center = "median")

leveneTest(y = data$PetalLengthCm, group = data$Species, center = "median")

leveneTest(y = data$PetalWidthCm, group = data$Species, center = "median")


# 4.3. Aplicaci�n de pruebas estad�sticas para comparar los grupos de datos. 
#En funci�n de los datos y el objetivo del estudio, aplicar pruebas de 
# contraste de hip�tesis, correlaciones, regresiones, etc. 
# Aplicar al menos tres m�todos de an�lisis diferentes.  

# Anova

# Longitud del s�palo
AnovaSL <- aov(SepalLengthCm ~ Species, data = data)
summary(AnovaSL)

# Ancho del s�palo
AnovaSW <- aov(SepalWidthCm ~ Species, data = data)
summary(AnovaSW)

# Longitud del p�talo
AnovaPL <- aov(PetalLengthCm ~ Species, data = data)
summary(AnovaPL)

# Ancho de p�talo
AnovaPW <- aov(PetalWidthCm ~ Species, data = data)
summary(AnovaPW)

# Correlaci�n

library(corrplot)

# entre todas sin especie

M <- cor(data[,1:4])
corrplot(M, method = "ellipse")
M

# desglozada por especie

# longitud del s�palo y ancho del p�talo

lapply(split(data, Species), function(x){cor(x[,1], x[,2])})

ggplot(data, aes(x=SepalLengthCm, y=SepalWidthCm, shape=Species, color=Species))+
geom_point() +
geom_smooth(method=lm, se=F, fullrange=F)+
scale_color_brewer(palette="Dark2")+
theme_minimal()+
stat_ellipse(type = "norm")

# longitud del s�palo y longitud del p�talo

lapply(split(data, Species), function(x){cor(x[,1], x[,3])})

ggplot(data, aes(x=SepalLengthCm, y=PetalLengthCm, shape=Species, color=Species))+
  geom_point() +
  geom_smooth(method=lm, se=F, fullrange=F)+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()+
  stat_ellipse(type = "norm")


# longitud del s�palo y ancho del p�talo 

lapply(split(data, Species), function(x){cor(x[,1], x[,4])})

ggplot(data, aes(x=SepalLengthCm, y=PetalWidthCm, shape=Species, color=Species))+
  geom_point() +
  geom_smooth(method=lm, se=F, fullrange=F)+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()+
  stat_ellipse(type = "norm")

# ancho del s�palo y longitud del p�talo

lapply(split(data, Species), function(x){cor(x[,2], x[,3])})

ggplot(data, aes(x=SepalWidthCm, y=PetalLengthCm, shape=Species, color=Species))+
  geom_point() +
  geom_smooth(method=lm, se=F, fullrange=F)+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()+
  stat_ellipse(type = "norm")

# ancho del s�palo y ancho del p�talo

lapply(split(data, Species), function(x){cor(x[,2], x[,4])})

ggplot(data, aes(x=SepalWidthCm, y=PetalWidthCm, shape=Species, color=Species))+
  geom_point() +
  geom_smooth(method=lm, se=F, fullrange=F)+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()+
  stat_ellipse(type = "norm")

# longitud del p�talo y ancho de p�talo

lapply(split(data, Species), function(x){cor(x[,3], x[,4])})

ggplot(data, aes(x=PetalLengthCm, y=PetalWidthCm, shape=Species, color=Species))+
  geom_point() +
  geom_smooth(method=lm, se=F, fullrange=F)+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()+
  stat_ellipse(type = "norm")


# Diagrama de dispersi�n

pairs(x = data[, -5], col = c("firebrick", "green", "blue")[data$Species],
      pch = 20)

par(mfrow=c(1,1))

# LDA

# verificar normalidad multivariante

# test de royston

royston_test <- mvn(data = data[,-5], mvnTest = "royston", multivariatePlot = "qq")
royston_test$multivariateNormality


# la matriz de covarianza es constante en todos los grupos

library(biotools)

boxM(data = data[, -5], grouping = data[, 5])


# C�lculo de la funci�n discriminante

library(MASS)

modelo_lda <- lda(Species ~ SepalWidthCm + SepalLengthCm + PetalLengthCm +
                    PetalWidthCm, data = data)
modelo_lda

# 5. Representaci�n de los resultados a partir de tablas y gr�ficas.

# Realiza la predicci�nn con el modelo LDA

prediccion<-predict(modelo_lda,data[-5])

prediccion$class

# Matriz de confusi�n

m_confusion<-table(data$Species,prediccion$class,
                   dnn=c("Real","Predicho"))
m_confusion

mosaicplot(m_confusion,col=2:4)

# Presici�n (training error)

precision=mean(data$Species==prediccion$class)
precision
error=1-precision
error

# Visualizaci�n de las clasificaciones


library(klaR)

partimat(Species ~ SepalWidthCm + SepalLengthCm + PetalLengthCm + PetalWidthCm,
         data = data, method = "lda", prec = 200,
         image.colors = c("firebrick", "green", "blue"),
         col.mean = "firebrick")


# Creaci�n de un sets de entrenamiento y prueba

library(dplyr)

set.seed(1234)
data_entrenamiento <- sample_frac(data, .7)
data_prueba <- setdiff(data, data_entrenamiento)

str(data_entrenamiento)
str(data_prueba)


# LDA con el set de entrenamiento

modelo_lda <- lda(Species ~ SepalWidthCm + SepalLengthCm + PetalLengthCm +
                    PetalWidthCm, data = data_entrenamiento)
modelo_lda


# Realiza la predicci�nn con el modelo LDA con el set de prueba

prediccion<-predict(modelo_lda,data_prueba[-5])

prediccion$class

# Matriz de confusi�n

m_confusion<-table(data_prueba$Species,prediccion$class,
                   dnn=c("Real","Predicho"))
m_confusion

mosaicplot(m_confusion,col=2:4)

# Presici�n (test error)

precision=mean(data_prueba$Species==prediccion$class)
precision
error=1-precision
error

# Visualizaci�n de las clasificaciones

partimat(Species ~ SepalWidthCm + SepalLengthCm + PetalLengthCm + PetalWidthCm,
         data = data_prueba, method = "lda", prec = 200,
         image.colors = c("firebrick", "green", "blue"),
         col.mean = "firebrick")
