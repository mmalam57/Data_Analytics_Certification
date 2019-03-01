#### Assignment for Trees and Forest with a focus on class imbalance

## You all have to explore rpart parameters list to handle class imbalance
## FYI, there is a parameter called loss_matrix which can do this.
## A link for that: https://stats.stackexchange.com/questions/96081/how-do-i-specify-a-loss-matrix-in-rpart

install.packages("ROSE") # It has the required data set
library(ROSE)

data(hacide) # This will load two files in workspace:
             # - hacide.train: training data
             # - hacide.test: testing data

# checking columns information
str(hacide) # you should see that there are three columns
            # "cls" is the output variable

table(hacide.train$cls)  # you can see that there is a 98-2 ratio between classes. We will consider this later


# Question -1: a)  Train a decision tree model.

## your code here

set.seed(123)
dec_tree = rpart(cls~., method = "class", data = hacide.train)

# Visualize Tree
library(rpart.plot)
prp(dec_tree)
summary(dec_tree)
dec_tree$variable.importance
print(dec_tree)


# Question -1: b)   Test your model by first using predict function and then check accuracy,precision and recall

> predict_hacide <- predict(dec_tree, newdata = hacide.test, type = "class") 
> table(predict_hacide,hacide.test$cls) 
               
predict_hacide   0   1 
             0 245   4 
             1   0   1 
>  
> #Calculate Accuracy 
>  
> (245+1) / (245 + 4 + 0+1) 
[1] 0.984 
>  # 245 is true positive while 759 is true negative 
>   
> # type 'Prob' 
> predict_hacide_prb <- predict(dec_tree, newdata = hacide.test, type = "prob") 
> #for precision and recall  
> > pre_rec = prediction(predict_hacide_prb[,2] , hacide.test$cls) 
> pref = performance(pre_rec, "prec", "rec") 
> plot(pref) 
>  
0.0 
0.2 
0.4 
0.6 
0.8 
1.0 

> Accuracy <- (245+1) / (245+4+0+1)
> Error_rate <- 1-Accuracy 
> Precision <- 245/(245+0)
> # Recall It is a measure of actual observations which are labeled (predicted) correctly i.e. how many observations of positive class are labeled correctly. It is also known as ‘Sensitivity’.
> # formula - True Positive / (True Positive + False Negative)
> Recall <- 245/(245+4)
> Recall
[1] 0.9839357
> Accuracy
[1] 0.984
> Precision
[1] 1
> Error_rate
[1] 0.016
> #Precision - TP / (TP+FP) - It is a measure of actual observations which are labeled (predicted) correctly i.e. how many observations of positive class are labeled correctly. It is also known as ‘Sensitivity’.
> Specificity <- 1 /(1+0)
> Specificity
[1] 1
> accuracy.meas(hacide.test$cls, predict_hacide_prb[,2] )

Call: 
accuracy.meas(response = hacide.test$cls, predicted = predict_hacide_prb[, 
    2])

Examples are labelled as positive when predicted is greater than 0.5 

precision: 1.000
recall: 0.200
F: 0.167

# Question -2: a)  Train a Random Forest model.

> bag.hacide = ranger(cls~., data = hacide.train, mtry=2, splitrule = "gini" ,importance = "permutation")
> bag.hacide
Ranger result

Call:
 ranger(cls ~ ., data = hacide.train, mtry = 2, splitrule = "gini",      importance = "permutation") 

Type:                             Classification 
Number of trees:                  500 
Sample size:                      1000 
Number of independent variables:  2 
Mtry:                             2 
Target node size:                 1 
Variable importance mode:         permutation 
Splitrule:                        gini 
OOB prediction error:             1.50 % 
> # prediction on test set
> yhat.bag = predict(bag.hacide,data=hacide.test )

> hacide.test1 = hacide.test$cls
> # plotting results against actual
> plot(yhat.bag$predictions, hacide.test1)


# Question -2: b)   Test your model by first using predict function and then check accuracy,precision and recall

> yhat.bag=predict(bag.hacide, hacide.test)
> yhat.bag
Ranger prediction

Type:                             Classification 
Sample size:                      250 
Number of independent variables:  2 
> # Calculating variable importances
> sort(importance(bag.hacide), decreasing = TRUE)
         x1          x2 
0.019244094 0.003807329 



### As you can see that results are not very good, especially recall, try to incorporte loss matrix or anything which can counter class imabalance
# Question -3: a)  Train a decision tree model with an additional loss matrix.

## your code here


# Question -3: b)   Test your model by first using predict function and then check accuracy,precision and recall
> fit<- rpart(cls~., data = hacide.train, method = "class", parms= list(split= "information", loss = matrix(c(245,4,0,1) , byrow = TRUE)), control =rpart.control(usesurrogate = 0, maxsurrogate = 0))     


