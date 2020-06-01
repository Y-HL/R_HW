# (1)
#setwd('D:/riii/hw')
house <- read.csv('./lvr_prices_big5.csv',header = T)

# (2)
str(house)

# (3)
library('dplyr')
house <- house %>% filter(house$city_land_type=='住' & house$total_price>0 & house$building_sqmeter > 0 & house$finish_ymd!='')

# (4)
house$price_per_sqmeter <- round(house$total_price/house$building_sqmeter)

# (5)
house[scale(house$price_per_sqmeter) > 3,]

# (6)
house$building_age <- as.integer(round((Sys.Date() - as.Date(house$finish_ymd))/365))

# (7)
house_danger <- read.csv('./house_danger.csv',header = T)
house <- merge(x = house_danger, y = house, by.x = "ID", by.y  = "X", all.x = T)

# (8)
library(caTools)
set.seed(1)
msk <- sample.split(house[,1], SplitRatio = 0.8)
trainset <- subset(house, msk == TRUE)
testset  <- subset(house, msk == FALSE)
length(trainset[,1])/(length(trainset[,1])+length(testset[,1]))
# 精準分成trainset與testset

# (9)
variable.list <- names(house) %in% c('area','building_age','building_sqmeter', 'building_type','price_per_sqmeter', 'danger')
trainset <- trainset[,variable.list]
testset <- testset[,variable.list]

library('rpart')
con <- rpart.control(minsplit=20,cp=0.01)
house.rp <- rpart(danger ~., data=trainset,control = con)

# (10)
par(mfrow=c(1,1))
plot(house.rp, uniform=TRUE,compress=TRUE,margin=0.02)
text(house.rp, cex=0.8)

# (11)
printcp(house.rp)
plotcp(house.rp)
#不需要剪枝

# (12)
library(caret)
pred_class <- predict(house.rp, testset, type="class")
cm<-confusionMatrix(table(pred_class,testset$danger))
cm$overall[1]
cm$byClass[c(5,6)]

# (13)
library(ROCR)
pred_prob <- predict(house.rp, testset, type="prob")
p_yes <- pred_prob[, "YES"]
predictions <- prediction(p_yes, testset$danger)
per_auc <- performance(predictions, measure ="auc")
per_fpr_tpr <- performance(predictions, measure="tpr",x.measure = "fpr")
plot(per_fpr_tpr,main=paste("AUC:",(per_auc@y.values)))