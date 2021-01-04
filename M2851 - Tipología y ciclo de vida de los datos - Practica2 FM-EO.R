## ----setup, include=FALSE------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- message=FALSE------------------------------------------------------------------------------------------------------------
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(arc)){
  install.packages("arc")
  library(arc)
}

if(!require(ggcorrplot)){
  install.packages("ggcorrplot")
  library(ggcorrplot)
}

if(!require(ggpubr)){
  install.packages("ggpubr")
  library(ggpubr)
}

if(!require(BSDA)){
  install.packages("BSDA")
  library(BSDA)
}
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(lattice)){
  install.packages("lattice")
  library(lattice)
}

if(!require(caret)){
  install.packages("caret")
  library(caret)
}

if(!require(plyr)){
  install.packages("plyr")
  library(plyr)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(lattice)){
  install.packages("lattice")
  library(lattice)
}

if(!require(rsample)){
  install.packages("rsample")
  library(rsample)
}

if(!require(yardstick)){
  install.packages("yardstick")
  library(yardstick)
}

if(!require(randomForest)){
  install.packages("randomForest")
  library(randomForest)
}

if(!require(kernlab)){
  install.packages("kernlab")
  library(kernlab)
}



## ------------------------------------------------------------------------------------------------------------------------------
ttc <- read.csv("./Data/train.csv",na.strings=c(""," ","NA"))
head(ttc)
str(ttc)
# Contamos con 891 observaciones de las 12 variables decritas al inicio de esta seccion. 


## ------------------------------------------------------------------------------------------------------------------------------
ttc$Sex <- as.factor(ttc$Sex)
ttc$Embarked <- as.factor(ttc$Embarked)
ttc$Survived <- as.factor(ttc$Survived)
ttc$Pclass <- as.factor(ttc$Pclass)

str(ttc)


## ------------------------------------------------------------------------------------------------------------------------------
decimalAges<-c()

for (i in 1:(nrow(ttc))){
  if(!is.na(ttc$Age[i])){
    if(as.integer(ttc$Age[i]) != ttc$Age[i])
      decimalAges<-c(decimalAges,ttc$Age[i])
  }
}
decimalAges
length(decimalAges)


## ------------------------------------------------------------------------------------------------------------------------------
roundValues <- function(x){
  if (!is.na(x['Age'])){
    if(x['Age'] < 1)
      x['Age'] = 1
    else
      x['Age'] = round(as.numeric(x['Age']))
  }
  return(x['Age'])
}


ttc$Age <- apply(ttc,1,roundValues)
ttc$Age <- as.integer(ttc$Age)
str(ttc)


## ------------------------------------------------------------------------------------------------------------------------------
colSums(is.na(ttc))



## ------------------------------------------------------------------------------------------------------------------------------
nrow = nrow(ttc)


## ------------------------------------------------------------------------------------------------------------------------------
ttc$Cabin <- NULL
head(ttc)


## ------------------------------------------------------------------------------------------------------------------------------
ttc <- ttc[!is.na(ttc$Embarked),]
colSums(is.na(ttc))



## ------------------------------------------------------------------------------------------------------------------------------
imputationFunct <- function(x){
    if (is.na(x["Age"])){
      x["Age"]<- median(ttc$Age[ttc$Pclass==x["Pclass"] & !is.na (ttc$Age)])
    } else{
      x<-x
    }
  return (x["Age"])
}

ttc$Age <- apply(ttc,1,imputationFunct)
ttc$Age <- as.numeric(ttc$Age)
sapply(ttc,function(x) sum(is.na(x)))


## ------------------------------------------------------------------------------------------------------------------------------
# Boxplot para la variable Age
summary(ttc$Age)
ggplot(data = ttc, aes(y = Age)) +
  geom_boxplot(outlier.colour = "red") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggtitle("Passenger Age Boxplot") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

# Boxplot para la variable SibSp
summary(ttc$SibSp)
ggplot(data = ttc, aes(y = SibSp)) +
  geom_boxplot(outlier.colour = "red") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggtitle("Passenger sibings/spouses Boxplot") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

# Boxplot para la variable Parch
summary(ttc$Parch)
ggplot(data = ttc, aes(y = Parch)) +
  geom_boxplot(outlier.colour = "red") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggtitle("Passenger parents/children Boxplot") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

# Boxplot para la variable Fare
summary(ttc$Fare)
ggplot(data = ttc, aes(y = Fare)) +
  geom_boxplot(outlier.colour = "red") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggtitle("Passenger paid Fare Boxplot") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))


## ------------------------------------------------------------------------------------------------------------------------------
outlier_cases = nrow(ttc[ttc$Fare > 500,])
mean_Fare = mean(ttc$Fare)
median_Fare = median(ttc$Fare)

outlier_cases
mean_Fare
median_Fare


## ------------------------------------------------------------------------------------------------------------------------------
factors = unlist(lapply(ttc, is.factor))
which(factors, arr.ind = TRUE)



## ------------------------------------------------------------------------------------------------------------------------------

mytableSex <- table(ttc$Sex)
pctSex <- round(mytableSex/sum(mytableSex)*100)
lblsSex <- paste(names(mytableSex), "\n", pctSex, sep="")
lblsSex <- paste (lblsSex, '%', sep="")
pie(mytableSex, labels = lblsSex,
    main="Distribución de la variable Sex\n", col=c("brown4","darkblue"))

mytableSurvived <- table(ttc$Survived)
pctSurvived <- round(mytableSurvived/sum(mytableSurvived)*100)
lblsSurvived<- paste(names(mytableSurvived), "\n", pctSurvived, sep="")
lblsSurvived <- paste (lblsSurvived, '%', sep="")
pie(mytableSurvived, labels = lblsSurvived,
    main="Distribución de la variable Survived\n", col = c("coral","cyan3"))


tablePclass<-table(ttc$Pclass)
dfPclass<-data.frame(tablePclass)

p<-ggplot(data=dfPclass, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="#00b159")+
  theme_minimal()+
  xlab("Clase")+
  ylab("Número de pasajeros")
p

tableEmb<-table(ttc$Embarked)
dfEmb<-data.frame(tableEmb)

p<-ggplot(data=dfEmb, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="#00aedb")+
  theme_minimal()+
  xlab("Puerta de embarque")+
  ylab("Número de pasajeros")
p





## ------------------------------------------------------------------------------------------------------------------------------
numerics = unlist(lapply(ttc, is.numeric))
which(numerics, arr.ind = TRUE)


## ------------------------------------------------------------------------------------------------------------------------------
# Histograma para la variable Age
ggplot(ttc, aes(x = Age)) + 
  geom_histogram(aes(y = ..density..)) +
  geom_density(alpha = 0.1, fill = "blue") + 
  ggtitle("Passengers Age Density Histogram") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

# Histograma para la variable SibSp
ggplot(ttc, aes(x = SibSp)) + 
  geom_histogram(aes(y = ..density..)) +
  geom_density(alpha = 0.1, fill = "green") + 
  ggtitle("Passengers sibings/spouses Density Histogram") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

# Histograma para la variable Parch
ggplot(ttc, aes(x = Parch)) + 
  geom_histogram(aes(y = ..density..)) +
  geom_density(alpha = 0.1, fill = "darkred") + 
  ggtitle("Passengers parents/children Density Histogram") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

# Histograma para la variable Fare
ggplot(ttc, aes(x = Fare)) + 
  geom_histogram(aes(y = ..density..)) +
  geom_density(alpha = 0.1, fill = "yellow") + 
  ggtitle("Passengers paid Fare Density Histogram") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))


## ------------------------------------------------------------------------------------------------------------------------------
# Por ejemplo, la frecuencia de supervivientes por Sexo
ggplot(as.data.frame(table(ttc$Survived, ttc$Sex)), aes(Var2, Freq, fill=Var1)) + 
  geom_bar(stat="identity") +
  scale_fill_discrete(name = "Passenger Survived?", labels = c("No", "Yes")) +
  ggtitle("Passenger Survival Frequency by Sex") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  xlab("Passenger Sex") + ylab("Frequency")

# O la frecuencia de supervivientes por Clase
ggplot(as.data.frame(table(ttc$Survived, ttc$Pclass)), aes(Var2, Freq, fill=Var1)) + 
  geom_bar(stat="identity") +
  scale_fill_discrete(name = "Passenger Survived?", labels = c("No", "Yes")) +
  ggtitle("Passenger Survival Frequency by Class") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  xlab("Passenger Class") + ylab("Frequency")

# O incluso la frecuencia de supervivientes por puerto de embarque
ggplot(as.data.frame(table(ttc$Survived, ttc$Embarked)), aes(Var2, Freq, fill=Var1)) + 
  geom_bar(stat="identity") +
  scale_fill_discrete(name = "Passenger Survived?", labels = c("No", "Yes")) +
  ggtitle("Passenger Survival by Port of Embarcation") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  xlab("Passenger Port of Embarcation") + ylab("Frequency")

# Tambien por el grupo de Edad, aunque previamente debemos discretizar la variable Age 
# ya que es numerica continua.

ttc$AgeD <- discretize(ttc$Age, 
                       method = "cluster", breaks = 3, labels=c("Young", "MidAge", "Old"))

ggplot(as.data.frame(table(ttc$Survived, ttc$AgeD)), aes(Var2, Freq, fill=Var1)) + 
  geom_bar(stat="identity") +
  scale_fill_discrete(name = "Passenger Survived?", labels = c("No", "Yes")) +
  ggtitle("Passenger Survival by Age Group") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  xlab("Passenger Age Group") + ylab("Frequency")

# Otra grafica interesante puede ser aquella que muestre la frecuencia de supervivencia 
# dependiendo de si el pasajero tenia familiares con el en el barco o viajaban solos

ggplot(as.data.frame(table(ttc$Survived, ttc$SibSp)), aes(Var2, Freq, fill=Var1)) + 
  geom_bar(stat="identity") +
  scale_fill_discrete(name = "Passenger Survived?", labels = c("No", "Yes")) +
  ggtitle("Passenger Survival by number of Siblings/Spouses") +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  xlab("Passenger number of siblings/Spouses") + ylab("Frequency")

ggplot(as.data.frame(table(ttc$Survived, ttc$Parch)), aes(Var2, Freq, fill=Var1)) + 
  geom_bar(stat="identity") +
  scale_fill_discrete(name = "Passenger Survived?", labels = c("No", "Yes")) +
  ggtitle("Passenger Survival by number of Parents/Children") +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  xlab("Passenger number of Parents/Children") + ylab("Frequency")

# O en general, si el pasajero tenia familia a bordo
ttc$PassengerFamily <- ifelse(ttc$SibSp != 0 | ttc$Parch != 0, 'FamilyOnBorad', "AlonePassenger") 
table(ttc$Survived, ttc$PassengerFamily)
ggplot(as.data.frame(table(ttc$Survived, ttc$PassengerFamily)), aes(Var2, Freq, fill=Var1)) + 
  geom_bar(stat="identity") +
  scale_fill_discrete(name = "Passenger Survived?", labels = c("No", "Yes")) +
  ggtitle("Passenger Survival by Family On Board") +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  xlab("Passenger Family On Board") + ylab("Frequency")

# Por ultimo una relacion interesante, es la frecuencia de supervicencia asociada
# a la longitud del nombre del pasajero, bajo una premisa inicial de que, cuanto
# mas largo fuera el nombre, el pasajero podria tener una clase social mas elevada
ttc$NameLength <- vector("numeric", nrow(ttc))
for (i in 1:nrow(ttc)) {
  ttc$NameLength[i] <- nchar(as.character(ttc$Name)[i])
}
ttc$NameLengthD <- discretize(ttc$NameLength, 
           method = "cluster", breaks = 4, labels=c("ShortName (<21)", 
                                                    "MediumName (<28)", 
                                                    "LongName (<40)", 
                                                    "VeryLongName (<82)"))
ggplot(as.data.frame(table(ttc$Survived, ttc$NameLengthD)), aes(Var2, Freq, fill=Var1)) + 
  geom_bar(stat="identity") +
  scale_fill_discrete(name = "Passenger Survived?", labels = c("No", "Yes")) +
  ggtitle("Passenger Survival by Name Length") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  xlab("Passenger Name Length, number of characters") + ylab("Frequency")


## ------------------------------------------------------------------------------------------------------------------------------
ttc_num <- subset(ttc, select=c(Age, SibSp, Parch, Fare))
ttccorr <- cor(ttc_num)
ggcorrplot(ttccorr, method = "circle")


## ------------------------------------------------------------------------------------------------------------------------------
mean(as.numeric(as.character(ttc[sample(which(ttc$Pclass == 1),100),]$Survived)))


## ------------------------------------------------------------------------------------------------------------------------------
# Generamos un array de medias sobre 100 submuestras aleatorias de 100
# pasajeros de primera clase cada submuestra.
iter <- 100
vars <- 1
First_class_SampleMeans <- matrix(ncol=vars, nrow=iter)
for(i in 1:iter){
  set.seed(i*16)
  First_class_SampleMeans[i,] <- mean(as.numeric(as.character
                                                 (ttc[sample(which
                                                             (ttc$Pclass == 1)
                                                             ,100),]$Survived)))
}
mean(First_class_SampleMeans)
First_class_SampleMeans <- data.frame(First_class_SampleMeans)

# Hacemos lo mismo pero para 100 submuestras aleatorias de 100 pasajeros de tercera clase 
# cada submuestra.
Third_class_SampleMeans <- matrix(ncol=vars, nrow=iter)
for(i in 1:iter){
  set.seed(i*16)
  Third_class_SampleMeans[i,] <- mean(as.numeric(as.character
                                                 (ttc[sample(which
                                                             (ttc$Pclass == 3)
                                                             ,100),]$Survived)))
}
mean(Third_class_SampleMeans)
Third_class_SampleMeans <- data.frame(Third_class_SampleMeans)

# Revisamos la distribucion de estas variables a nivel grafico de densidad:
SF_FirstClass_Density <- ggplot(First_class_SampleMeans, aes(x = First_class_SampleMeans)) + 
  geom_histogram(aes(y = ..density..)) +
  geom_density(alpha = 0.5, fill = "green") + 
  ggtitle("Survival rate Frequency in First Class") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))

SF_ThirdClass_Density <- ggplot(Third_class_SampleMeans, aes(x = Third_class_SampleMeans)) + 
  geom_histogram(aes(y = ..density..)) +
  geom_density(alpha = 0.5, fill = "darkred") + 
  ggtitle("Survival rate Frequency in Third Class") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))

ggarrange(SF_FirstClass_Density , SF_ThirdClass_Density)


## ------------------------------------------------------------------------------------------------------------------------------
shapiro.test(First_class_SampleMeans$First_class_SampleMeans)
shapiro.test(Third_class_SampleMeans$Third_class_SampleMeans)



## ------------------------------------------------------------------------------------------------------------------------------
qqnorm(First_class_SampleMeans$First_class_SampleMeans)
qqline(First_class_SampleMeans$First_class_SampleMeans)
qqnorm(Third_class_SampleMeans$Third_class_SampleMeans)
qqline(Third_class_SampleMeans$Third_class_SampleMeans)


## ------------------------------------------------------------------------------------------------------------------------------
bartlett.test(list(First_class_SampleMeans$First_class_SampleMeans
                   ,Third_class_SampleMeans$Third_class_SampleMeans))



## ------------------------------------------------------------------------------------------------------------------------------
# Generamos un array de medias sobre 100 submuestras aleatorias de 100
# pasajeros de sexo masculino.
iter <- 100
vars <- 1
Male_sex_SampleMeans <- matrix(ncol=vars, nrow=iter)
for(i in 1:iter){
  set.seed(i*16)
  Male_sex_SampleMeans[i,] <- mean(as.numeric(as.character
                                                 (ttc[sample(which
                                                             (ttc$Sex == 'male')
                                                             ,100),]$Survived)))
}
mean(Male_sex_SampleMeans)
Male_sex_SampleMeans <- data.frame(Male_sex_SampleMeans)

# Hacemos lo mismo pero para 100 submuestras aleatorias de 100 pasajeros de sexo
# femenino.
Female_sex_SampleMeans <- matrix(ncol=vars, nrow=iter)
for(i in 1:iter){
  set.seed(i*16)
  Female_sex_SampleMeans[i,] <- mean(as.numeric(as.character
                                                 (ttc[sample(which
                                                             (ttc$Sex == 'female')
                                                             ,100),]$Survived)))
}
mean(Female_sex_SampleMeans)
Female_sex_SampleMeans <- data.frame(Female_sex_SampleMeans)

# Revisamos la distribucion de estas variables a nivel grafico de densidad:
SF_MaleSex_Density <- ggplot(Male_sex_SampleMeans, aes(x = Male_sex_SampleMeans)) + 
  geom_histogram(aes(y = ..density..)) +
  geom_density(alpha = 0.5, fill = "green") + 
  ggtitle("Survival rate Frequency for Males") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))

SF_FemaleSex_Density <- ggplot(Female_sex_SampleMeans, aes(x = Female_sex_SampleMeans)) + 
  geom_histogram(aes(y = ..density..)) +
  geom_density(alpha = 0.5, fill = "darkred") + 
  ggtitle("Survival rate Frequency for Females") +
  theme(plot.title = element_text(size = 10, hjust = 0.5))

ggarrange(SF_MaleSex_Density , SF_FemaleSex_Density)


## ------------------------------------------------------------------------------------------------------------------------------
shapiro.test(Male_sex_SampleMeans$Male_sex_SampleMeans)
shapiro.test(Female_sex_SampleMeans$Female_sex_SampleMeans)



## ------------------------------------------------------------------------------------------------------------------------------
qqnorm(Male_sex_SampleMeans$Male_sex_SampleMeans)
qqline(Male_sex_SampleMeans$Male_sex_SampleMeans)
qqnorm(Female_sex_SampleMeans$Female_sex_SampleMeans)
qqline(Female_sex_SampleMeans$Female_sex_SampleMeans)


## ------------------------------------------------------------------------------------------------------------------------------
# Test de Homogeneidad de Varianzas
bartlett.test(list(Male_sex_SampleMeans$Male_sex_SampleMeans
                   ,Female_sex_SampleMeans$Female_sex_SampleMeans))



## ------------------------------------------------------------------------------------------------------------------------------
z.test(First_class_SampleMeans$First_class_SampleMeans, 
       Third_class_SampleMeans$Third_class_SampleMeans, 
       sigma.x=sqrt(var(First_class_SampleMeans$First_class_SampleMeans)),
       sigma.y=sqrt(var(Third_class_SampleMeans$Third_class_SampleMeans)),
       alternative="greater",
       conf.level = 0.95)



## ------------------------------------------------------------------------------------------------------------------------------
z.test(Male_sex_SampleMeans$Male_sex_SampleMeans, 
       Female_sex_SampleMeans$Female_sex_SampleMeans, 
       sigma.x=sqrt(var(Male_sex_SampleMeans$Male_sex_SampleMeans)),
       sigma.y=sqrt(var(Female_sex_SampleMeans$Female_sex_SampleMeans)),
       alternative="less",
       conf.level = 0.95)
# El resultado es que el p-value es inferior a nuestro nivel de confianza. Esto es, con un 
# nivel de confianza del 95%, podemos descartar la hipotesis nula y aceptar la 
# alternativa:
# La media del ratio de supervivencia de los hombres es inferior a la media del ratio de 
# supervivencia para las mujeres


## ------------------------------------------------------------------------------------------------------------------------------
ttc$Age <- cut(ttc$Age,breaks = 5*(0:16))
head(ttc$Age)


## ------------------------------------------------------------------------------------------------------------------------------
ttc$SibSp <- cut(ttc$SibSp,breaks = 2*(0:4), include.lowest = TRUE)
ttc$Parch <- cut(ttc$Parch,breaks = 2*(0:3), include.lowest = TRUE)

head(ttc$SibSp)
head(ttc$Parch)


## ------------------------------------------------------------------------------------------------------------------------------
tab_Pclass <- table(ttc$Pclass,ttc$Survived)
chisq_Pclass <- chisq.test(tab_Pclass)
chisq_Pclass


## ------------------------------------------------------------------------------------------------------------------------------
related_features <- c("Pclass")



## ------------------------------------------------------------------------------------------------------------------------------
tab_Sex <- table(ttc$Sex, ttc$Survived)
chisq_sex <- chisq.test(tab_Sex)
chisq_sex


## ------------------------------------------------------------------------------------------------------------------------------
related_features <- c(related_features,"Sex")



## ------------------------------------------------------------------------------------------------------------------------------
tab_Age <- table (ttc$Age, ttc$Survived)
chisq_Age <- chisq.test(tab_Age)
chisq_Age


## ------------------------------------------------------------------------------------------------------------------------------
related_features <- c(related_features,"Age")



## ------------------------------------------------------------------------------------------------------------------------------
tab_SibSp <- table (ttc$SibSp, ttc$Survived)
chisq_SibSp <- chisq.test(tab_SibSp)
chisq_SibSp



## ------------------------------------------------------------------------------------------------------------------------------
related_features <- c(related_features,"SibSp")



## ------------------------------------------------------------------------------------------------------------------------------
tab_Parch <- table(ttc$Parch, ttc$Survived)
chisq_Parch <- chisq.test(tab_Parch)
chisq_Parch


## ------------------------------------------------------------------------------------------------------------------------------
tab_Embarked <- table(ttc$Embarked, ttc$Survived)
chisq_Embarked <- chisq.test(tab_Embarked)
chisq_Embarked


## ------------------------------------------------------------------------------------------------------------------------------
related_features <- c(related_features,"Embarked")



## ------------------------------------------------------------------------------------------------------------------------------
related_features


## ------------------------------------------------------------------------------------------------------------------------------
related_features <- c(related_features,"Survived")
ttc_model <- ttc %>% select(related_features)
head(ttc_model)


## ------------------------------------------------------------------------------------------------------------------------------
ttc_vectors <- ttc_model %>%
  mutate_if(is.factor, as.numeric)
head(ttc_vectors)

ttc_vectors$Survived <- as.factor(ttc_vectors$Survived)


ttc_vectors$Survived <- revalue(ttc_vectors$Survived, c("1"="NO", "2"="YES"))
head(ttc_vectors)

train_test_split <- initial_split(ttc_vectors, prop=3/4)


TRAIN <- training(train_test_split)
TEST <- testing(train_test_split)

head(TRAIN)
head(TEST)

write.csv(TRAIN, "ttc_train_clean.csv")
write.csv(TEST, "ttc_test_clean.csv")


## ------------------------------------------------------------------------------------------------------------------------------
control <- trainControl(method="repeatedcv", number=10, repeats=3, 
                        summaryFunction = twoClassSummary, classProbs = TRUE)
glm <- train(Survived~., data = TRAIN, method="glm", trControl=control)


## ------------------------------------------------------------------------------------------------------------------------------
glm


## ------------------------------------------------------------------------------------------------------------------------------
glm.TEST <- predict(glm, TEST)

class_glm <- predict(glm, TEST)

confusionMatrix(data=class_glm, TEST$Survived)


## ------------------------------------------------------------------------------------------------------------------------------
rf <- train(Survived~., data = TRAIN, method="rf", trControl=control, 
            tuneLength=4)
rf



## ------------------------------------------------------------------------------------------------------------------------------
rf.TEST <- predict(rf, TEST)
class_rf <- predict(rf, TEST)

confusionMatrix(data=class_rf, TEST$Survived)


## ------------------------------------------------------------------------------------------------------------------------------

svm_poly <- train(Survived~., data = TRAIN, method="svmPoly", 
                    trControl=control,preProcess = c("center","scale"),
                    tuneLength = 4)
svm_poly



## ------------------------------------------------------------------------------------------------------------------------------
class_svm_poly <- predict(svm_poly, TEST)
confusionMatrix(data=class_svm_poly, TEST$Survived)


## ------------------------------------------------------------------------------------------------------------------------------

ttc_test_origin <- read.csv("./Data/test.csv",na.strings=c(""," ","NA"), 
                     stringsAsFactors = TRUE)
head(ttc_test_origin)

features<-head(related_features,-1)
ttc_test <- ttc_test_origin %>% select(features)
head(ttc_test)


ttc_test$Age <- apply(ttc_test,1,roundValues)
ttc_test$Age <- as.numeric(ttc_test$Age)

imputationFunct_test <- function(x){
  if (is.na(x["Age"])){
    x["Age"]<- median(ttc_test$Age[ttc_test$Pclass==x["Pclass"] & 
                                     !is.na (ttc_test$Age)])
  } else{
    x<-x
  }
  return (x["Age"])
}
ttc_test$Age <- apply(ttc_test,1,imputationFunct_test)
ttc_test$Age <- as.numeric(ttc_test$Age)

ttc_test_vectors <- ttc_test %>%
  mutate_if(is.factor, as.numeric)
head(ttc_test_vectors)


ttc_test_vectors.class <- predict(rf, ttc_test_vectors)
ttc_test_origin$Survived <- ttc_test_vectors.class
ttc_test_df = ttc_test_origin %>% select("PassengerId", "Survived")
ttc_test_df$Survived = revalue(ttc_test_df$Survived, c("YES"=1, "NO"=0))
ttc_test_df$Survived = as.numeric(ttc_test_df$Survived)

write.csv(ttc_test_df,"./Data/test_results.csv", row.names = FALSE)


