
##Principal Component Analysis
```{r}
library(tidyverse)
library(randomForest)
library(nnet)
library(h2o)

Train<-read.csv("./Train.csv")
Test<-read.csv("./Test.csv")


Train$label<-as.factor(Train$label)

y<-Train[,1]
X<-Train[,-1]


prin_comp<-prcomp(X)
names(prin_comp)


prin_comp$x
biplot(prin_comp)

pr_var<-(prin_comp$sdev)^2
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]
plot(prop_varex[1:50],xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

Train_Final<-data.frame(y,prin_comp$x[,1:60])



```


##Multinomial Regression
```{r}

model_multi<-multinom(y~.,data=Train_Final)

a<-predict(model_multi)

t<-cbind(a,y) 
length(t[a!=y,])/nrow(t)




```


##Random Forest
```{r}



localH2O <- h2o.init(nthreads = -1)
train.h2o <- as.h2o(Train)
test.h20<-as.h2o(Test)


rf1<-h2o.randomForest(y=1,x=2:785,train.h2o,ntrees = 1000, seed = 1122)

Rf_results<-h2o.predict(rf1,train.h2o)


a<-as.data.frame(train.h2o[,1])
b<-as.data.frame(Rf_results[,1])
c<-cbind(a,b)

c[c$label!=c$predict,]


Final_Test<-h2o.predict(rf1,test.h20)

a<-as.data.frame(Final_Test[,1])

write.csv(a,"./Digit.csv")






