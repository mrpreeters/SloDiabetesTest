Description: R package SloDiabetes


library(caret)
install.packages('httr')

data <- readRDS("sloDiabetes_NA_6,1.rds")
data$Class <- as.factor(data$Class) 

# priprava baze s 5. spremenljivkami
data1 <- data[,c(which(names(data) == "Sistolicni"),
                 which(names(data) == "Starost"),
                 which(names(data) == "TelesnaTeza"),
                 which(names(data) == "V4"),
                 which(names(data) == "Diastolicni"),
                 # which(names(data) == "Starost.NA"),
                 which(names(data) == "TelesnaTeza.NA"),
                 which(names(data) == "Sistolicni.NA"),
                 which(names(data) == "V4.NA"),
                 which(names(data) == "Diastolicni.NA"),
                 which(names(data) == "Class")
)]

# 10 fold cross-validation ponovljena 5 krat
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, summaryFunction = twoClassSummary,classProbs = TRUE)
set.seed(324)

# gradnja modela
gridGbm <- expand.grid( .n.trees = 50, .interaction.depth =1, .shrinkage = 0.1, .n.minobsinnode = 10 )
gbmFit <- train(Class~., 
                data = data1, 
                method = "gbm",
                tuneGrid=gridGbm, 
                trControl = fitControl, 
                metric="ROC")



# metoda napovednega modela
    diabetesTest <- function(podatki){
        cont <- predict(gbmFit, newdata=podatki, type="prob")
        return (cont=cont)
        #return (cont=cont[,2]) # vrne % POS
    }

# klicanje metode Test
diabetesTest(podatki)
