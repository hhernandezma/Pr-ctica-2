# 1. Descripción del dataset. ¿Por qué es importante y qué pregunta/problema pretende 
# responder?

#Carpeta de trabajo
setwd("H:/Master_Ciencia_datos")

# Lectura de datos

data <- read.csv("H:/Master_Ciencia_Datos/Pr�ctica_2/iris.csv")
attach(data)
str(data)

# Las primeras 5 filas
head(data,5)

#Las últimas 5 filas
tail(data ,5)


# 2. Integración y selección de los datos de interés a analizar.

# eliminar columna Id

data$Id <- NULL

# Transformar Species en Factor

data <- data.frame(SepalLengthCm, SepalWidthCm, PetalLengthCm, PetalWidthCm, 
                   Species, stringsAsFactors=TRUE)
# Prueba
str(data)

# 3. Limpieza de los datos.

# 3.1

# ¿Los datos contienen ceros o elementos vacíos?

# valores ausentes

anyNA(data)

# 3.2. Identificación y tratamiento de valores extremos.

# Estadisticos descriptivos

summary(data)

# Estadisticos descriptivos por especie

tapply(data$SepalLengthCm, data$Species, summary)
tapply(data$SepalWidthCm, data$Species, summary)
tapply(data$PetalLengthCm, data$Species, summary)
tapply(data$PetalWidthCm, data$Species, summary)


# Por cada varaible en un solo gráfico

boxplot(SepalLengthCm, SepalWidthCm, PetalLengthCm, PetalWidthCm,   
        names =c("Longitud del sépalo","Ancho del sépalo","Longitud del pétalo",
                "Ancho del pétalo"), horizontal=FALSE,main="Diagramas de caja Iris",
        col = c("orange3", "yellow3", "green3", "grey"),
        xlab = "Atributos", ylab = "Centímetros")

# Por cada variable según su especie

library(ggplot2)
library(gridExtra)
library(grid)
library(plyr)


BpSl <- ggplot(iris, aes(Species, SepalLengthCm, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Longitud del sépalo (cm)", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")

BpSw <-  ggplot(iris, aes(Species, SepalWidthCm, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Ancho del sépalo (cm)", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")

BpPl <- ggplot(iris, aes(Species, PetalLengthCm, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Longitud del pétalo (cm)", breaks= seq(0,30, by=.5))+
  theme(legend.position="none")

BpPw <-  ggplot(iris, aes(Species, PetalWidthCm, fill=Species)) + 
  geom_boxplot()+
  scale_y_continuous("Ancho del pétalo (cm)", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Box Plot", x = "Species")

grid.arrange(BpSl  + ggtitle(""),
             BpSw  + ggtitle(""),
             BpPl + ggtitle(""),
             BpPw + ggtitle(""),
             nrow = 2,
             top = textGrob("Diagramas de caja de los atributos según Especie", 
                            gp=gpar(fontsize=15))
)


# 4. Análisis de los datos.


# 4.1. Selección de los grupos de datos que se quieren analizar/comparar 
# (planificación de los análisis a aplicar).

# Separar en grupos según un factor

data2 <- split(data, data$Species)
str(data2)
View(data)

setosa <- data[1:50, 1:4]
versicolor <- data[51:100, 1:4]
virginica <- data[101:150, 1:4]

str(setosa)
str(versicolor)
str(virginica)


# 4.2. Comprobación de la normalidad y homogeneidad de la varianza.

#################################################

# Histograma y curva de densidad

# Longitud del sépalo

ggplot(data = data, aes(x = SepalLengthCm)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "gray60") +
  geom_density(aes(color = "Longitud de sépalo"), lwd = 0.95) +
  stat_function(aes(color = "Normal"), fun = dnorm, lwd = 0.95,
                args = list(mean = mean(data$SepalLengthCm),
                            sd = sd(data$SepalLengthCm))) +
  scale_colour_manual("Densidad", values = c("red", "blue")) +
  labs(x = "Longitud de sépalo (cm)", y = "Densidad",
       title = "Distribución de la longitud de sépalo vs curva normal") +
  theme_bw()

# gráfico de cuantiles teóricos (Q-Q plot)

qqnorm(y = data$SepalLengthCm)
qqline(y = data$SepalLengthCm)

# Pruebas de normalidad

#library(normtest)
library(nortest)

# Anderson-Darling
ad.test(data$SepalLengthCm)


# Histograma y curva de densidad

# Ancho del sépalo

ggplot(data = data, aes(x = SepalWidthCm)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "gray60") +
  geom_density(aes(color = "Ancho del sépalo"), lwd = 0.95) +
  stat_function(aes(color = "Normal"), fun = dnorm, lwd = 0.95,
                args = list(mean = mean(data$SepalWidthCm),
                            sd = sd(data$SepalWidthCm))) +
  scale_colour_manual("Densidad", values = c("red", "blue")) +
  labs(x = "Ancho del sépalo (cm)", y = "Densidad",
       title = "Distribución del ancho del sépalo vs curva normal") +
  theme_bw()

# gráfico de cuantiles teóricos (Q-Q plot)

qqnorm(y = data$SepalWidthCm)
qqline(y = data$SepalWidthCm)

# Prueba de normalidad

# Anderson-Darling
ad.test(data$SepalWidthCm)


# Longitud del pétalo 

# Histograma y curva de densidad


ggplot(data = data, aes(x = PetalLengthCm)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "gray60") +
  geom_density(aes(color = "Longitud del pétalo"), lwd = 0.95) +
  stat_function(aes(color = "Normal"), fun = dnorm, lwd = 0.95,
                args = list(mean = mean(data$PetalLengthCm),
                            sd = sd(data$PetalLengthCm))) +
  scale_colour_manual("Densidad", values = c("red", "blue")) +
  labs(x = "Longitud del pétalo (cm)", y = "Densidad",
       title = "Distribución de la longitud del pétalo vs curva normal") +
  theme_bw()

# gráfico de cuantiles teóricos (Q-Q plot)

qqnorm(y = data$PetalLengthCm)
qqline(y = data$PetalWidthCm)

# Prueba de normalidad

# Anderson-Darling
ad.test(data$PetalLengthCm)


# Ancho del pétalo 

# Histograma y curva de densidad


ggplot(data = data, aes(x = PetalWidthCm)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "gray60") +
  geom_density(aes(color = "Ancho del pétalo"), lwd = 0.95) +
  stat_function(aes(color = "Normal"), fun = dnorm, lwd = 0.95,
                args = list(mean = mean(data$PetalWidthCm),
                            sd = sd(data$PetalWidthCm))) +
  scale_colour_manual("Densidad", values = c("red", "blue")) +
  labs(x = "Ancho del pétalo (cm)", y = "Densidad",
       title = "Distribución del ancho del pétalo vs curva normal") +
  theme_bw()

# gráfico de cuantiles teóricos (Q-Q plot)

qqnorm(y = data$PetalWidthCm)
qqline(y = data$PetalWidthCm)

# Prueba de normalidad

# Anderson-Darling
ad.test(data$PetalWidthCm)


# Según especie 


library(MVN)

# tipo de flor setosa

# Histograma y curva de densidad

result<- mvn(data=setosa, mvnTest="royston", univariatePlot="histogram")

# Gráfico Q-Q

result<-mvn(data=setosa, mvnTest = "royston", univariatePlot = "qqplot")

# Test de Anderson Darling

result <- mvn(data = setosa, mvnTest = "royston", univariateTest = "AD", desc = TRUE)
result

# tipo de flor versicolor

# Histograma y curva de densidad

result<- mvn(data=versicolor, mvnTest="royston", univariatePlot="histogram")

# Gráfico Q-Q

result<-mvn(data=versicolor, mvnTest = "royston", univariatePlot = "qqplot")

# Test de Anderson Darling

result <- mvn(data = versicolor, mvnTest = "royston", univariateTest = "AD", desc = TRUE)
result


# tipo de flor virginica

# Histograma y curva de densidad

result<- mvn(data=virginica, mvnTest="royston", univariatePlot="histogram")

# Gráfico Q-Q

result<-mvn(data=virginica, mvnTest = "royston", univariatePlot = "qqplot")

# Test de Anderson Darling

result <- mvn(data = virginica, mvnTest = "royston", univariateTest = "AD", desc = TRUE)
result


# Análisis de frecuencia con el histograma


# Longitud del sépalo 

HisSl <- ggplot(data=data, aes(x=SepalLengthCm))+
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Longitud del sépalo (cm)") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de la longitud del sépalo")+
  geom_vline(data=data, aes(xintercept = mean(SepalLengthCm)),linetype="dashed",color="grey")

# Ancho del sépalo

HistSw <- ggplot(data=data, aes(x=SepalWidthCm)) +
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Ancho del sépalo (cm)") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma del ancho del sépalo")+
  geom_vline(data=data, aes(xintercept = mean(SepalWidthCm)),linetype="dashed",color="grey")

# Longitud del pétalo

HistPl <- ggplot(data=data, aes(x=PetalLengthCm))+
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Longitud del pétalo (cm)") +  
  ylab("Frequencia") + 
  theme(legend.position="none")+
  ggtitle("Histograma de la longitud del pétalo")+
  geom_vline(data=data, aes(xintercept = mean(PetalLengthCm)),
             linetype="dashed",color="grey")

# Ancho del pétalo

HistPw <- ggplot(data=data, aes(x=PetalWidthCm))+
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Ancho del pétalo (cm)") +  
  ylab("Frequency") + 
  theme(legend.position="right" )+
  ggtitle("Histograma del ancho del pétalo")+
  geom_vline(data=data, aes(xintercept = mean(PetalWidthCm)),linetype="dashed",color="grey")

# Visualización en conjunto

grid.arrange(HisSl + ggtitle(""),
             HistSw + ggtitle(""),
             HistPl + ggtitle(""),
             HistPw  + ggtitle(""),
             nrow = 2,
             top = textGrob("Histograma de frecuencias según especies de iris", 
                            gp=gpar(fontsize=15))
)

# Análisis de densidad

# Longitud del pétalo

DhistPl <- ggplot(data, aes(x=PetalLengthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(PetalLengthCm),  colour=Species),linetype="dashed",color="grey", size=1)+
  xlab("Longitud del pétalo (cm)") +  
  ylab("Densidad")+
  theme(legend.position="none")

# Ancho del pétalo

DhistPw <- ggplot(data, aes(x=PetalWidthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(PetalWidthCm),  colour=Species),linetype="dashed",color="grey", size=1)+
  xlab("Ancho del pétalo (cm)") +  
  ylab("Densidad")

# Ancho del sépalo

DhistSw <- ggplot(data, aes(x=SepalWidthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(SepalWidthCm),  colour=Species), linetype="dashed",color="grey", size=1)+
  xlab("Ancho del sépalo (cm)") +  
  ylab("Densidad")+
  theme(legend.position="none")

# Longitud del sépalo

DhistSl <- ggplot(data, aes(x=SepalLengthCm, colour=Species, fill=Species)) +
  geom_density(alpha=.3) +
  geom_vline(aes(xintercept=mean(SepalLengthCm),  colour=Species),linetype="dashed", color="grey", size=1)+
  xlab("Longitud del sépalo (cm)") +  
  ylab("Densidad")+
  theme(legend.position="none")

# visualización conjunta

grid.arrange(DhistSl + ggtitle(""),
             DhistSw  + ggtitle(""),
             DhistPl + ggtitle(""),
             DhistPw  + ggtitle(""),
             nrow = 2,
             top = textGrob("Curva de densidad según especies de iris", 
                            gp=gpar(fontsize=15))
)

# Diagrama de dispersión

pairs(x = data[, -5], col = c("firebrick", "green", "blue")[data$Species],
      pch = 20)


# Homogeneidad

# Test de Levene

library(car)

#####
leveneTest(y = data$SepalLengthCm, group = data$Species, center = "median")

leveneTest(y = data$SepalWidthCm, group = data$Species, center = "median")

leveneTest(y = data$PetalLengthCm, group = data$Species, center = "median")

leveneTest(y = data$PetalWidthCm, group = data$Species, center = "median")



# 4.3. Aplicación de pruebas estadísticas para comparar los grupos de datos. 
#En función de los datos y el objetivo del estudio, aplicar pruebas de 
# contraste de hipótesis, correlaciones, regresiones, etc. 
# Aplicar al menos tres métodos de análisis diferentes.  

# Anova

# Longitud del sépalo
AnovaSL <- aov(SepalLengthCm ~ Species, data = data)
summary(AnovaSL)

# Ancho del sépalo
AnovaSW <- aov(SepalWidthCm ~ Species, data = data)
summary(AnovaSW)

# Longitud del pétalo
AnovaPL <- aov(PetalLengthCm ~ Species, data = data)
summary(AnovaPL)

# Ancho de pétalo
AnovaPW <- aov(PetalWidthCm ~ Species, data = data)
summary(AnovaPW)

# Correlación

library(corrplot)

# entre todas sin especie

M <- cor(data[,1:4])
corrplot(M, method = "ellipse")
M

# desglozada por especie

# longitud del sépalo y ancho del pétalo

lapply(split(data, Species), function(x){cor(x[,1], x[,2])})

ggplot(data, aes(x=SepalLengthCm, y=SepalWidthCm, shape=Species, color=Species))+
geom_point() +
geom_smooth(method=lm, se=F, fullrange=F)+
scale_color_brewer(palette="Dark2")+
theme_minimal()+
stat_ellipse(type = "norm")

# longitud del sépalo y longitud del pétalo

lapply(split(data, Species), function(x){cor(x[,1], x[,3])})

ggplot(data, aes(x=SepalLengthCm, y=PetalLengthCm, shape=Species, color=Species))+
  geom_point() +
  geom_smooth(method=lm, se=F, fullrange=F)+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()+
  stat_ellipse(type = "norm")


# longitud del sépalo y ancho del pétalo 

lapply(split(data, Species), function(x){cor(x[,1], x[,4])})

ggplot(data, aes(x=SepalLengthCm, y=PetalWidthCm, shape=Species, color=Species))+
  geom_point() +
  geom_smooth(method=lm, se=F, fullrange=F)+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()+
  stat_ellipse(type = "norm")

# ancho del sépalo y longitud del pétalo

lapply(split(data, Species), function(x){cor(x[,2], x[,3])})

ggplot(data, aes(x=SepalWidthCm, y=PetalLengthCm, shape=Species, color=Species))+
  geom_point() +
  geom_smooth(method=lm, se=F, fullrange=F)+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()+
  stat_ellipse(type = "norm")

# ancho del sépalo y ancho del pétalo

lapply(split(data, Species), function(x){cor(x[,2], x[,4])})

ggplot(data, aes(x=SepalWidthCm, y=PetalWidthCm, shape=Species, color=Species))+
  geom_point() +
  geom_smooth(method=lm, se=F, fullrange=F)+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()+
  stat_ellipse(type = "norm")

# longitud del pétalo y ancho de pétalo

lapply(split(data, Species), function(x){cor(x[,3], x[,4])})

ggplot(data, aes(x=PetalLengthCm, y=PetalWidthCm, shape=Species, color=Species))+
  geom_point() +
  geom_smooth(method=lm, se=F, fullrange=F)+
  scale_color_brewer(palette="Dark2")+
  theme_minimal()+
  stat_ellipse(type = "norm")


# Diagrama de dispersión

pairs(x = data[, -5], col = c("firebrick", "green", "blue")[data$Species],
      pch = 20)

par(mfrow=c(1,1))


####################
# según especie 

# setosa
Msetosa <- cor(setosa)
corrplot(Msetosa, method = "number")

# versicolor
Mversicolor <- cor(versicolor)
corrplot(Mversicolor, method = "number")

# virginica

Mvirginica <- cor(virginica)
corrplot(Mvirginica, method = "number")
#######################################

# LDA

# verificar normalidad multivariante

# test de normalidad
# test de Mardia en MVN

# test de royston

library(MVN)

royston_test <- mvn(data = data[,-5], mvnTest = "royston", multivariatePlot = "qq")
royston_test$multivariateNormality

###############################################
# setosa

result <- mvn(data = setosa, mvnTest = "mardia")
result$multivariateNormality

#versicolor

result <- mvn(data = versicolor, mvnTest = "mardia")
result$multivariateNormality

# virginica

result <- mvn(data = virginica, mvnTest = "mardia")
result$multivariateNormality
######################################################

# la matriz de covarianza es constante en todos los grupos

library(biotools)
boxM(data = data[, -5], grouping = data[, 5])


# Cálculo de la función discriminante

library(MASS)

modelo_lda <- lda(Species ~ SepalWidthCm + SepalLengthCm + PetalLengthCm +
                    PetalWidthCm, data = data)
modelo_lda

# 5. Representación de los resultados a partir de tablas y gráficas.

# Realiza la predicción con el modelo LDA

prediccion<-predict(modelo_lda,data[-5])

prediccion$class

# Matriz de confusión

m_confusion<-table(data$Species,prediccion$class,
                   dnn=c("Real","Predicho"))
m_confusion

mosaicplot(m_confusion,col=2:4)

# Presición (training error)

precision=mean(data$Species==prediccion$class)
precision
error=1-precision
error

str(data)

flores <- data
str(flores)

# Visualización de las clasificaciones


library(klaR)
library(MASS)
partimat(Species ~ SepalWidthCm + SepalLengthCm + PetalLengthCm + PetalWidthCm,
         data = data, method = "lda", prec = 200,
         image.colors = c("darkgoldenrod1", "snow2", "skyblue2"),
         col.mean = "firebrick")


remove.packages("klaR")


library(klaR)
partimat(Species~.,
         data=data,
         method="lda",
         image.colors=c("orange","yellow","salmon"),
         col.mean="blue")

library(MASS)
data(iris)
partimat(Species ~ ., data = flores, method = "lda")




library(MASS)
data(iris)
partimat(Species ~ ., data = iris, method = "lda")



