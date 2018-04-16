rm(list=ls(all=TRUE))
library(keras)

tic.tac.toe <- read.csv("C:/Users/mob/Desktop/tic-tac-toe.txt")

Moves <- tic.tac.toe[,1:9]
result <-tic.tac.toe[,10]
Moves<- sapply(Moves,as.numeric)-1
result<- as.numeric(result)-1



X <- to_categorical(Moves[,1]) 
for (i in 2:ncol(Moves)) {
  X <- cbind(X,to_categorical(Moves[,i])) 
}
X<-X[,-c(seq(1,27,3))]

Moves<- X

set.seed(80)
# Determine sample size
ind <- sample(seq_len (nrow(Moves)), size = round(nrow(Moves)*0.8,0))

# Train/test split
Moves.training <- Moves[ind,]
Moves.test <- Moves[-ind,]

# Split the class attribute
result.trainingtarget <- result[ind]
result.testtarget <- result[-ind]



result.trainLabels<- to_categorical(result.trainingtarget)
result.testLabels<- to_categorical(result.testtarget)

# Initialize a sequential model
model <- keras_model_sequential()

model %>% 
  layer_dense(units = 9, activation = 'relu', input_shape = c(18)) %>% 
  layer_dense(units = 9, activation = 'relu')%>% 
  layer_dense(units = 2, activation = 'sigmoid')


# Compile the model
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)


# Fit the model to the data
history <- model %>% fit(
  Moves.training, result.trainLabels, 
  epochs = 100, batch_size = 10, 
  validation_split = 0.2
)

#plot
plot(history)

#---------------------------------------------------------------------------------------------------------+
# If your training data accuracy keeps improving while your validation data accuracy gets worse,          |
# you are probably overfitting: your model starts to just memorize the data instead of learning from it.  | 
#                                                                                                         |
# If the trend for accuracy on both datasets is still rising for the last few epochs,                     |
# you can clearly see that the model has not yet over-learned the training dataset.                       |
#---------------------------------------------------------------------------------------------------------+ 

# Predict the classes for the test data
classes <- model %>% predict_classes(Moves.test, batch_size = 10)

# Confusion matrix
table(result.testtarget, classes)

# Evaluate the model
score <- model %>% evaluate(Moves.test, result.testLabels, batch_size = 128)

# Print the score
print(score)
