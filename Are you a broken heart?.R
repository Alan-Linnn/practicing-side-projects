library(ggplot2)
library(dplyr)
library(car)
getwd()
setwd("/Users/alan0114/Downloads")
data= read.csv('heart.csv',header=T)

glimpse(data) # 檢視資料
# 轉換變數
data2 <- data %>% 
  mutate(sex = if_else(sex == 1, "MALE", "FEMALE"), 
         fbs = if_else(fbs == 1, ">120", "<=120"), #血糖
         exang = if_else(exang == 1, "YES" ,"NO"), #運動心絞痛
         cp = if_else(cp == 1, "ATYPICAL ANGINA", #胸痛種類
                      if_else(cp == 2, "NON-ANGINAL PAIN", "ASYMPTOMATIC")),
         restecg = if_else(restecg == 0, "NORMAL", #休息心電圖
                           if_else(restecg == 1, "ABNORMALITY", "PROBABLE OR DEFINITE")),
         slope = as.factor(slope), # 
         ca = as.factor(ca), #
         thal = as.factor(thal), #
         target =as.factor(target) #
         ) %>%
  mutate_if(is.character, as.factor) %>% 
  dplyr::select(target, sex, fbs, exang, cp, restecg, slope, ca, thal, everything())

set.seed (123)
n <- nrow(data2)
index <- sample(n, 0.7*n)
Traindata<- data2[index,]
Testdata <- data2[-index,]

#Logits Model 
library(GGally)
logit_mod <- glm(target~., data = Traindata, family = binomial(link = 'logit'))
summary(logit_mod) # AIC = 180.55
vif(logit_mod) # 共線性小 沒有超過10

## stepwise regression select variables
stlogit_mod <- step(logit_mod, data=Traindata, direction = 'both')
summary(stlogit_mod) # 選擇保留所有變數 （沒意義）

## Criterions- Based procedure
library(leaps)
subx=regsubsets(target~., nbest=2, data=Traindata)
subsets(subx,statistic="bic") 
subsets(subx,statistic="bic",min.size=3, max.size=7)# exang, restecg,ca, thal, thalach, oldpeak

# 我們選擇Criterions- Based procedure 所挑選的變數(exang, restecg,ca, thal, thalach, oldpeak)
### 模型優化 (bootstrap)


CB.logit_mod <- glm(target~thalach+exang+ca+thal+oldpeak + restecg, data = Traindata, family = binomial(link = 'logit'))
summary(CB.logit_mod)

## marginal effect of thalach, exang, oldpeak
library(margins)

CB.logit <- margins(CB.logit_mod)
summary(CB.logit) # exang, oldpeak, thalach
cplot(CB.logit_mod, "oldpeak", what = "effect")
cplot(CB.logit_mod, "thalach", what = "effect")





#######selected variable visualization

#得心臟病的人數分布
par(mfrow=c(1,2))
ggplot(data2, aes(x=data2$target, fill=data2$target)) + 
  geom_bar() +
  xlab("Heart Disease") +
  ylab("Count") +
  ggtitle("Analysis of Presence and Absence of Heart Disease") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absence", "Presence"))
## thalach vs target 最高心跳頻率
p1 <- ggplot(data=data2, aes(x=thalach,
                       fill=target))+
  geom_bar()+
  geom_text(stat = 'count', aes(label=..count..),
            size=3.5,
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette = 'Paired')+
  labs(x='Thalach', y='Count',title = 'Target vs. Thalach')
## age vs target
p2 <- ggplot(data=data2, aes(x=age,
                       fill=target))+
  geom_bar()+
  geom_text(stat = 'count', aes(label=..count..),
            size=3.5,
            position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette = 'Paired')+
  labs(x='Age', y='Count',title = 'Taget vs. Age')
library(gridExtra)
grid.arrange(p1, p2, nrow = 1)


# exang vs target
p3 <- ggplot(data = data2, aes(x = exang,
                             fill = target)) +
  geom_bar() +
  geom_text(stat = "count", aes(label=..count..),
            size=3.5,
            position = position_stack(vjust = 0.5)) + scale_fill_brewer(palette="Paired") +
  labs(x = 'exang', y = 'Counts',
       title = 'Target vs. Exang') + theme(legend.position = 'bottom')
# ca vs target
p4 <- ggplot(data = data2, aes(x = ca,
                         fill = target)) +
  geom_bar() +
  geom_text(stat = "count", aes(label=..count..),
            size=3.5,
            position = position_stack(vjust = 0.5)) + scale_fill_brewer(palette="Paired") +
  labs(x = 'ca', y = 'Counts',
       title = 'Target vs. Ca') + theme(legend.position = 'bottom')  

# oldpeak vs target
p5 <-ggplot(data = data2, aes(x = oldpeak,
                         fill = target)) +
  geom_bar() +
  geom_text(stat = "count", aes(label=..count..),
            size=3.5,
            position = position_stack(vjust = 0.5)) + scale_fill_brewer(palette="Paired") +
  labs(x = 'olpeak', y = 'Counts',
       title = 'Target vs. oldpeak') + theme(legend.position = 'bottom')
grid.arrange(p3, p4,p5 ,nrow = 2)
##
###########Prediction
library(InformationValue)
predict1 <- predict(CB.logit_mod, newdata = Testdata, type = 'response')
pred1<-ifelse(predict1 > 0.5,1,0)
table(Testdata$target, pred1)
accuracy.origin<- (30+40)/(7+14+28+43) # accuracy(original)=0.76086

#####CB.logit model(optimal cutoff)
set.seed(123)
predict2 <- predict(CB.logit_mod,newdata=Testdata, type = 'response')
optcutoff <-optimalCutoff(actuals = Testdata$target, predictedScores = predict2)
optcutoff # 0.7299~0.73
pred.opt <- ifelse(predict2>0.73 , 1, 0)
table(Testdata$target, pred.opt) # CB.logit model optimal accuracy = (36+39)/(36+39+8+8)= 0.8241
#generate confusion matrix
confusionMatrix(actuals = Testdata$target, predictedScores = pred.opt, threshold = optcutoff)
#missclassification 
misClassError(actuals = Testdata$target, predictedScores = predict2, threshold = optcutoff)
# ROC
plotROC(actuals = Testdata$target, predictedScores = predict2)#面積站84%
plotROC(actuals = Testdata$target, predictedScores = predict0)

#####logit model
predict0 <-predict(logit_mod, newdata= Testdata, type='response')
pred0 <- ifelse(predict0 >0.73,1,0)
table(Testdata$target , pred0)
misClassError(actuals = Testdata$target, predictedScores = predict0, threshold = optcutoff)
plotROC(actuals = Testdata$target, predictedScores = predict0)



############CART##################
# arguments:
# method: 
# - "class" for a classification tree (y is a factor) 分類的純度（gini index, intrope）
# - "anova" for a regression tree 找cost低的，找mse 越低
library(rpart)
library(rpart.plot)
tree1<- rpart(target~., data=Traindata,method='class')

rpart.plot(tree1)
pred_tree_prop <- predict(tree1,newdata = Testdata,type = 'prob')
pred_tree <- predict(tree1, newdata = Testdata, type = 'class')
#confusion matrix
tb1<- table(pred_tree, Testdata$target)
tb1
#missclassification rate
(7+10)/(34+7+10+40) # 0.1868 ,accuracy= 0.8132

# post-pruning(事前修剪法)
plotcp(tree1)
tree1$cptable # cp= 0.0372  xerror =0.5851
tree1.prune <- prune(tree1, cp = tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"])
rpart.plot(tree1.prune)



#看修剪樹的正確率
predicted.prune <- predict(tree1.prune, newdata = Testdata, type = 'class')
tb2 <- table(predicted.prune,Testdata$target)
tb2 
#missclassification rate (pruned)
(10+7)/(34+10+7+40) #0.1868 與修剪前一致 # accuracy= 0.8132

# 為了避免過度配適，使用交叉驗證檢驗
library(caret)
train_control <- trainControl(method='cv',number = 303) # leave one out
train_control.model <- train(target~., data = data2, method='rpart', trControl= train_control)
train_control.model 
#accuracy = 0.71947, cp=0.03985 

###########Random Forest##########
library(randomForest)
rf <- randomForest(target~., data = Traindata, n_tree = 100)
rf.pred.prob <- predict(rf, newdata = Testdata, type = "prob")
rf.pred <- predict(rf, newdata = Testdata, type = "class")
tb3 <- table(rf.pred, Testdata$target)
tb3
# missclassification rate
(4+12)/(32+4+12+43) # 0.1758, accuracy= 0.8242
