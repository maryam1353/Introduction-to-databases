Assessment 2:
  Maryam Mahmoodi

Question 1)

p_c1 = 0.0295
p_x1_c1 = 0.96
p_x1_c0 = 0.08
p_c0 = 1 - p_c1
p_x1 = p_x1_c1 * p_c1 + p_x1_c0 * p_c0
p_x1

Question 2)

set.seed(836217)
n = 1000
X = cbind(1, rnorm(n), rnorm(n))
Y = rep(-1, n)
beta = c(0.43, -1.7, 2.8)
for (i in 1:n) {
  z = (beta %*% X[i, ])[1]
  p = pnorm(z)
  if (runif(1) < p) Y[i] = 1
}

XTest = matrix(c(1, 1.0162462, -0.4012721, 1, -2.0693534, -0.2018362, 
                 1, -0.4812785, 0.2505587, 1, 1.1538629, 0.3152341, 
                 1, 0.4399999, 0.8282703), 5, 3, byrow = T)
YTest = c(-1, 1, 1, -1, 1)
tanh <- function(z){
  return (exp(z)-exp(-z))/(exp(z)+exp(-z))
}
x <- rbeta(10000, 0.2, 0.2)
hist(x, 
     main='Histrogram of Beta(0.2, 0.2)')


Question 3)

a)

x <- rbeta(10000, 0.2, 0.2)
hist(x, 
     main='Histrogram of Beta(0.2, 0.2)')

b)

par(mfrow=c(2,4))
times <- c(10, 100, 1000, 10000)
samples <- c(1, 5, 10, 30, 50, 100)
for (tm in times) {
  for (smple in samples) {
    means <- rep(0, tm)
    for (index in tm) {
      x <- rbeta(smple, 0.2, 0.2)
    }
    hist(x,
         main=paste('Sample size: ',
                    smple,
                    ', ',
                    tm,
                    ' times',
                    sep = ''))
  }
}

Question 4)

a)

data <- read.csv('iData.csv')
head(data)

t.test(weight ~ habit, data = data)

b)

weeks <- data$weeks
t.test(weeks, conf.level = 0.99)$conf.int

c)

mother.weight.gained <- data$gained
t.test(mother.weight.gained, mu=30, conf.level = 0.95)

Question 5)

german_credit <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
head(german_credit)

colnames(german_credit) <- c("chk_acct", 
                             "duration", 
                             "credit_his", 
                             "purpose",
                             "amount",
                             "saving_acct", 
                             "present_emp", 
                             "installment_rate",
                             "sex",
                             "other_debtor",
                             "present_resid", 
                             "property",
                             "age",
                             "other_install",
                             "housing",
                             "n_credits", 
                             "job", 
                             "n_people", 
                             "telephone", 
                             "foreign", 
                             "response")

german_credit$response <- german_credit$response - 1
german_credit$response <- as.factor(german_credit$response)

library(lattice)
library(ggplot2)

library(caret)
set.seed(2018)
in.train <- createDataPartition(as.factor(german_credit$response), 
                                p=0.8, 
                                list=FALSE)
german_credit.train <- german_credit[in.train,]
german_credit.test <- german_credit[-in.train,]

credit.glm0 <- glm(response ~ ., family = binomial, german_credit.train)
credit.glm.step <- step(credit.glm0, direction = "backward")

prob.insample <- predict(credit.glm.step, type = "response")
predicted.insample <- prob.insample > 0.1667
predicted.insample <- as.numeric(predicted.insample)
mean(ifelse(german_credit.train$response != predicted.insample, 1, 0))

table(german_credit.train$response, 
      predicted.insample,
      dnn = c("Truth", "Predicted"))

Naive Bates----------------

library(e1071)
naive.bayes <- naiveBayes(response ~ ., 
                          family = binomial, 
                          german_credit.train)

pred.incall <- predict(naive.bayes, 
                       newdata = german_credit.train)
table(german_credit.train$response, 
      pred.incall,
      dnn = c("Truth", "Predicted"))


pred <- predict(naive.bayes, 
                newdata = german_credit.test)
table(german_credit.test$response, 
      pred,
      dnn = c("Truth", "Predicted"))

Logistic Model ROC plo--------
  
library(stats)
library(gplots)
library(ggplot2)
library(stats)

library(ROCR)
library(stats)
library(pROC)

library(grid)
library(base)

library(stats)
library(base)
library(proxy)
library(MASS)
library(lattice)
library(boot)
library(maps)

library(dotCall64)
library(spam)

library(fields)

library(CircStats)
library(dtw)

library(pROC)
  
  
library(verification)
roc.plot(german_credit.train$response == "1", prob.insample)
roc.plot(german_credit.train$response == "1", prob.insample)$roc.vol$Area


credit.glm.final <- glm(response ~ chk_acct + duration +
                          credit_his + amount +
                          saving_acct  +
                          other_install + installment_rate,
                        family = binomial, german_credit.train)

prob.glm1.insample <- predict(credit.glm.final, type = "response")
predicted.glm1.insample <- prob.glm1.insample > 0.1667
predicted.glm1.insample <- as.numeric(predicted.glm1.insample)
mean(ifelse(german_credit.train$response != predicted.glm1.insample, 1, 0))

table(german_credit.train$response, predicted.glm1.insample, dnn = c("Truth", "Predicted"))
roc.plot(german_credit.train$response == "1", prob.glm1.insample)
roc.plot(german_credit.train$response == "1", prob.glm1.insample)$roc.vol$Area


prob.glm1.outsample <- predict(credit.glm.final, 
                               german_credit.test, 
                               type = "response")
predicted.glm1.outsample <- prob.glm1.outsample > 0.1667
predicted.glm1.outsample <- as.numeric(predicted.glm1.outsample)
table(german_credit.test$response, 
      predicted.glm1.outsample, 
      dnn = c("Truth", "Predicted"))
mean(ifelse(german_credit.test$response != predicted.glm1.outsample, 
            1,
            0))
roc.plot(german_credit.test$response == "1",
         prob.glm1.outsample)
roc.plot(german_credit.test$response == "1", 
         prob.glm1.outsample)$roc.vol$Area

Question 6)

p.x <- function(n=1)
{
  return(rnorm(n, mean=8, sd=2) +
           rnorm(n, mean=10, sd=5))
}

RejectionSampling <- function(n)
{
  RN <- NULL
  for(i in 1:n)
  {
    OK <- 0
    while(OK<1)
    {
      U <- p.x()
      if(U <= p.x())
      {
        OK <- 1
        RN <- c(RN,U)
      }
    }
  }
  return(RN)
}

sampleSize <- 3000 
rawDensity <- p.x(sampleSize)
simulatedDensity <- RejectionSampling(sampleSize)
# Calculating the two histograms
histoRaw <- hist(rawDensity)
histoSimulated <- hist(simulatedDensity)
# Q-Q plot raw vs simulated densities
plot( rawDensity )
plot( simulatedDensity, rawDensity )
qqplot(simulatedDensity, rawDensity )
qqline(simulatedDensity, col = 2)
# qqplot( rawDensity, simulatedDensity);
# abline(0,1)
# for comparison Q-Q plot of simulated distribution is quite diff from original
qqplot(simulatedDensity, rnorm(1:sampleSize, 0, 1)) 




