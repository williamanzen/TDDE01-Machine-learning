crabs=read.csv("australian-crabs.csv")
n=dim(crabs)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))

## Logistic regression to classify the sex of crabs

plot(crabs$RW, crabs$CL, col=factor(crabs$sex))
legend(7,40,unique(crabs$sex), col=1:length(crabs$sex),pch=1)

library(MASS)
## Linear discriminant analysis
lda_sex<-lda(sex~CL+RW , data=crabs)
predict_vals<-predict(lda_sex , crabs)
predict_vals
plot(crabs$RW, crabs$CL, col=predict_vals$class)

MisCalcRate <- function(confusion_table, inputdata){
  n=length(inputdata[,1])
  return(1-sum(diag(confusion_table))/n)
}

conf_table <- table(crabs$sex,predict_vals$class)
conf_table

MisCalcRate(conf_table, crabs)

## Adding a prior probability to the prediction
predict_vals_prior <- predict(lda_sex, prior=c(0.1,0.9), crabs)
plot(crabs$RW, crabs$CL, col=predict_vals_prior$class)
conf_table_prior <- table(crabs$sex, predict_vals_prior$class)
conf_table_prior
MisCalcRate(conf_table_prior, crabs)

## Comparing with a linear model
glm_sex<-glm(factor(sex)~CL+RW, family=binomial,data=crabs)
predict_glm <- predict(glm_sex, crabs, type='response')
#male>0.5 = TRUE
plot(crabs$RW, crabs$CL, col=ifelse(predict_glm>0.5,"red","blue"))
conf_table_glm <- table(crabs$sex,predict_glm>0.5)
conf_table_glm

MisCalcRate(conf_table_glm, crabs) #0.035

# install.packages("ggplot2")
 library(ggplot2)

glmPlot <- ggplot(crabs, aes(x=RW,y=CL))
glmPlot <- glmPlot + geom_point(aes(shape = factor(predict_glm>0.5), colour=factor(predict_glm>0.5)))
glmPlot <- glmPlot + geom_abline(intercept = (-glm_sex$coefficients[1]/glm_sex$coefficients[2]), slope= (-glm_sex$coefficients[3]/glm_sex$coefficients[2]))
glmPlot


      