#Define o path para ler os arquivos .csv
setwd ("G:/Area de Trabalho/titanic") #SetWorkDirectory

#Lê o csv de treinamento.
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)

#Lê o csv de teste.
titanic.test <-read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

#Calcula a mediana da variável Age desconsiderando os registro NA (na.rm = TRUE).
median(titanic.test$Age, na.rm = TRUE)
median(titanic.train$Age, na.rm = TRUE)

#Define IsTrainSet como TRUE se o registro pertencer ao Train, assim, na junção dos dataset será possível identificar a origem do registro. 
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

#Mostra os nomes dos campos
names(titanic.test)
names(titanic.train)

#Verifica o número de colunas 
ncol(titanic.test)
ncol(titanic.train)

#Cria o campo Survived em titanic.test que é a informação a ser prevista
titanic.test$Survived <- NA # Cria o Campo e atribui valor NA = NOt Avaliable
ncol(titanic.test) #verifica o número de colunas
names(titanic.test) #verifica os nomes das colunas

#Junta as bases de teste e treino
titanic.full <- rbind(titanic.test , titanic.train)

#Filtra a base de dados pelo campo Embarked, selecionando os valores em branco e faz um replace com "s"
titanic.full[titanic.full$Embarked == '', "Embarked"] <- 'S'
table(titanic.full$Embarked)


#Preparação de dados da coluna Age, Fare
table(is.na(titanic.full$Age)) #consulta quantos registro tem o valor NA
age.median <- median(titanic.full$Age , na.rm = TRUE) #Calcula a mediana de Age desconsiderando os registros NA
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median #Atribui a mediana aos campos onde NA estava registrado
table(is.na(titanic.full$Age))

table(is.na(titanic.full$Fare))
fare.median <-median(titanic.full$Fare, na.rm=TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median
table(is.na(titanic.full$Fare))

#Transforma variáveis em variáveis categóricas.
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

#Reatribui as variáveis train e test as informações tratadas
titanic.train <- titanic.full[titanic.full$IsTrainSet == TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet == FALSE,]
titanic.train$Survived <- as.factor(titanic.train$Survived)


#Criando o MODELO

#Cria a variavel equation que guarda as colunas consideradas
survived.equation <- "Survived ~ Pclass +  Sex + Age + SibSp + Parch + Fare + Embarked"

#as.formula extrair fórmulas que foram includias em outros objetos
survived.formula <- as.formula(survived.equation)


install.packages("randomForest")
library(randomForest)

titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01*nrow(titanic.test))

Survived <- predict(titanic.model, newdata = titanic.test)

#Cria um dataframe que será a saída para o csv

PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file = "kaggle_submission.csv", row.names = FALSE)


