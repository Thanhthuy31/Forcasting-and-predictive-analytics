rm(list=ls())
#import data
wine_data=read.csv("wine_fraud.csv")
head(wine_data,5)
str(wine_data)

##eda
library(ggplot2)

#countplot of quality
ggplot(wine_data, aes(x = quality)) +
  geom_bar()

#countplot of type
ggplot(wine_data, aes(x = type)) +
  geom_bar()

# boxplot
ggplot(data=wine_data, mapping=aes(x=quality, y=alcohol))+
  geom_boxplot()

ggplot(data=wine_data, mapping=aes(x=quality, y=density))+
  geom_boxplot()

#corr
wine_data$red <- 1*(wine_data$type =="red")
wine_data$fraud <- 1*(wine_data$quality =="Fraud")

wine_data <- wine_data[,-c(12,13)]
wine_data

library(corrplot)
corrplot(cor(wine_data))


#free.sulfur.dioxide and volatile.acidity have the strongest correlation with fraud
#scatterplot

#wine_data$fraud <- as.factor(wine_data$fraud)
        
#plot1
plot1 <- ggplot(wine_data, aes(volatile.acidity,fraud)) + 
  geom_point() +
  geom_smooth( method = "glm", se = FALSE, method.args = list(family = "binomial"))

#plot2
plot2 <-ggplot(wine_data, aes(free.sulfur.dioxide,fraud)) + 
  geom_point() +
  geom_smooth( method = "glm", se = FALSE, method.args = list(family = "binomial"))

library(ggpubr)
figure <- ggarrange(plot1, plot2,
                     ncol = 2, nrow = 1)

annotate_figure(figure)
#plot3
plot(wine_data$free.sulfur.dioxide[wine_data$red==1],wine_data$fraud[wine_data$red==1], xlab="free sulfur dioxide", ylab="fraud")
abline(v=median(wine_data$free.sulfur.dioxide[wine_data$red==1]), col=2, lwd=2)
legend("topright", legend = "median", lty=1, col=2, lwd=2)


# boxplot
wine_data$fraud <- as.factor(wine_data$fraud)
par(mfrow=c(1,1))
boxplot1 <- ggplot(data=wine_data, aes(x=fraud, y=volatile.acidity, col = fraud))+
  geom_boxplot()

boxplot2 <- ggplot(data=wine_data, aes(x=fraud, y=free.sulfur.dioxide, fill = fraud))+
  geom_boxplot()


#install.packages("ggpubr")
library("ggpubr")
dev.off()
figure1 <- ggarrange(boxplot1, boxplot2,
                     ncol = 2, nrow = 1)
annotate_figure(figure1)

#train split test
set.seed(2812)
train <- sample(1:nrow(wine_data), nrow(wine_data)*0.7)
test <- (1:nrow(wine_data))[-train]
train_data <- wine_data[train,]
test_data <- wine_data[test,]

#Fit logistic regression model on the original data set.
log_model <- glm(fraud~., data = train_data, family="binomial")
summary(log_model)

log.final <- glm(fraud~+volatile.acidity + residual.sugar 
                 + free.sulfur.dioxide + density + red,
                 data = train_data, family = "binomial")
summary(log.final)

# Check the independent between categorical variable and target variable
chisq.test(wine_data$red, wine_data$fraud)
# p_value = 0.7679 do not reject the null hypothesis of independence
#prediction for test set
pred_log <- predict(log.final, newdata=test_data, type="response")
pred_log <- 1*(pred_log>0.5)

library(caret)
#accuracy score : 0.9626 , Specificity : 0.03947 
confusionMatrix(as.factor(pred_log),test_data$fraud)

# ROC curve on the original data set
par(pty ="s")
library(pROC)
roc(test_data$fraud, pred_log, plot = TRUE, legacy.axes =TRUE,
    percent = TRUE, xlab = "False Positive Percentage", ylab="True Positive Percentage",
    col = "#377eb8", lwd = 2, print.auc=TRUE)

#check fraud variable levels frequency:
prop.table(table(wine_data$fraud))

## improve the model
#adjusting sampling weight
model_weights <- ifelse(train_data$fraud == "0", (1/table(train_data$fraud)[1])* 0.5,
                        (1/table(train_data$fraud)[2]) * 0.5)

#Upsampling
set.seed(2812)
trainup <- upSample(x=train_data[,-ncol(train_data)],
                    y=train_data$fraud) 
table(trainup$Class)

#downsampling
set.seed(2812)
traindown <- downSample(x=train_data[,-ncol(train_data)],
                        y=train_data$fraud) 
table(traindown$Class)

#ROSE
library("ROSE")
set.seed(2812)
trainrose <- ROSE(fraud~.,data=train_data)$data
table(trainrose$fraud)

#training logistic regression model on balanced data set.
# Weight method
weight.model = glm(fraud~+volatile.acidity + residual.sugar 
                   + free.sulfur.dioxide + density + red,
                   data= train_data, family = 'binomial', weights = model_weights)
pred_weight =  predict(weight.model, newdata = test_data)
pred_weight = 1*(pred_weight>0.5)

confusionMatrix(as.factor(pred_weight),test_data$fraud) 
#0.8805, Specificity : 0.6053
# On balanced data set (upsampling method)
set.seed(2812)
up.model <- glm(Class~+volatile.acidity + residual.sugar 
                + free.sulfur.dioxide + density + red,
                data = trainup, family = "binomial")
summary(up.model)
#prediction for test set
pred_up <- predict(up.model, newdata=test_data, type="response")
pred_up <- 1*(pred_up>0.5)

#accuracy score: 0.7405, Specificity : 0.67105 
confusionMatrix(as.factor(pred_up),test_data$fraud)

# On balanced data set (downsampling method)
set.seed(2812)
down.model <- glm(Class~volatile.acidity + residual.sugar 
                  + free.sulfur.dioxide + density + red,
                data = traindown, family = "binomial")
summary(down.model)
#prediction for test set
pred_down <- predict(down.model, newdata=test_data, type="response")
pred_down <- 1*(pred_down>0.5)

#accuracy score 0.7415, Specificity : 0.69737
confusionMatrix(as.factor(pred_down),test_data$fraud)

# On balanced data set (ROSE method)
set.seed(2812)
rose.model <- glm(fraud~volatile.acidity + residual.sugar 
                  + free.sulfur.dioxide + density + red,
                  data = trainrose, family = "binomial")
summary(rose.model)
#prediction for test set
pred_rose <- predict(rose.model, newdata=test_data, type="response")
pred_rose <- 1*(pred_rose>0.5)

#accuracy score: 0,7226, Specificity : 0.72368
confusionMatrix(as.factor(pred_rose),test_data$fraud)

# check accuracy by measuring auc
library(pROC)
dev.off()
#pdf("C:/Users/Thuy/Documents/unibo/96799 - FORECASTING AND PREDICTIVE ANALYTICS/dataset/plot/AUC plot.pdf", height=4)
par(pty="s")
par(mfrow=c(1,1))
par(mar=c(5, 4, 4, 8))
roc(test_data$fraud, pred_log, plot = TRUE, legacy.axes =TRUE,
    percent = TRUE, xlab = "False Positive Percentage", ylab="True Positive Percentage",
    col = "#2b8cbe", lwd = 2, print.auc=TRUE)
plot.roc(test_data$fraud, pred_weight, percent = TRUE, col="#1c9099", lwd=1,
         print.auc=TRUE, add=TRUE, print.auc.y=40)
plot.roc(test_data$fraud, pred_up, percent = TRUE, col="#fdbb84", lwd=1,
         print.auc=TRUE, add=TRUE, print.auc.y=35)
plot.roc(test_data$fraud, pred_down, percent = TRUE, col="#756bb1", lwd=1,
         print.auc=TRUE, add=TRUE, print.auc.y=30)
plot.roc(test_data$fraud, pred_rose, percent = TRUE, col="#e34a33", lwd=1,
         print.auc=TRUE, add=TRUE, print.auc.y=25)

legend("topleft", legend = c("original", "weight","upsamp","downsamp", "ROSE"), inset=c(0,0),
       lty = 1, xpd=TRUE, mar(c(2,2,2,2)), cex = 0.55, col = c("#2b8cbe", "#1c9099","#fdbb84","#756bb1","#e34a33"))

