library(tidyverse)
library(caret)

## Load data

churn <- read.csv('churn.csv')

str(churn)

                    
## Split data

train_tets_split <- function(data, size){
    set.seed(11)
    n <- nrow(data)
    id <- sample(n, size*n)
    train_data <- data[id, ]
    test_data <- data[-id, ]
    return(list(train_data, test_data))
}

split_data <- train_tets_split(churn, size = 0.8)

##train data

ctrl <- trainControl(method = 'repeatedcv',
                    number = 5,
                    repeats = 5
                    )

glm_model <- train(churn ~ accountlength + internationalplan + voicemailplan + numbervmailmessages + numbercustomerservicecalls,
                   data = split_data[[1]],
                   method = 'glm',
                   trControl = ctrl)

##test data and evaluation

p <- predict(glm_model, newdata = split_data[[2]])

confusionMatrix(p, factor(split_data[[2]]$churn))
