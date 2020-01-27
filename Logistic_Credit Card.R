a <- rbind(ds1,ds2,ds3,ds4)
View(a)
a$card_offer <- ifelse(a$card_offer=="FALSE",0,1)

a$card_offer[is.na(a$card_offer)]= 0
summary(a)


boxplot(a$est_income)
boxplot(a$hold_bal)
summary(a$hold_bal)
bench= 30.012 + 1.5*IQR(a$hold_bal)
a$hold_bal[a$hold_bal > bench]= bench


boxplot(a$imp_cscore)
boxplot(a$RiskScore)

summary(a$RiskScore)
ben=730.9 +1.5 * IQR(a$RiskScore)
ben1=609.8 -1.5 * IQR(a$RiskScore)
a$RiskScore[a$RiskScore > ben] = ben
a$RiskScore[a$RiskScore < ben1] = ben1
boxplot(a$imp_crediteval)

View(a)
table(a$demographic_slice)
table(a$country_reg)

library(fastDummies)

a<-dummy_cols(a,select_columns = c("demographic_slice","country_reg","ad_exp"))

colnames(a)

a <- a[-c(2,3,4)]

str(a)

library(car)

##Linear Regression
mod = lm(card_offer~.-customer_id, data=a)
summary(mod)
vif(mod)

##Removing aliased coefficients
a <- a[-c(15,13,17)]

mod = lm(card_offer~.-customer_id, data=a)

summary(mod)
vif(mod)

##Logistic Regression
a$card_offer = as.factor(a$card_offer)

model= glm(card_offer~.-customer_id,family = binomial,data=a)
summary(model)
vif(mod)

a <- a[-c(7,8,11)]

model= glm(card_offer~.-customer_id,family = binomial,data=a)
summary(model)
vif(mod)

model= glm(card_offer~.-(customer_id+RiskScore),family = binomial,data=a)
summary(model)
vif(mod)

model= glm(card_offer~.-(customer_id+RiskScore+ad_exp_N),family = binomial,data=a)
summary(model)
vif(mod)

prob<- predict(model,data=a,type="response")
head(prob)

##confusion matrix
predicted<- rep(0,nrow(a))
predicted[prob>.5]=1

table(predicted,a$card_offer)

(34541+2572) / (34541+2572+978+1909) ##92.78

a<- cbind(predicted,a)
View(a)

pred = ifelse(a$predicted==1,"Yes","No") 
a<- cbind(pred,a)

colnames(a)
a=a[-c(14)]

setwd("C:/Users/admin/Downloads/Desktop/Assign_Logistic_Regression")

write.csv(a,"Predicted Output_Logistic.csv",row.names = FALSE)

install.packages("ROCR")
library(ROCR)

#creating ROCR data
logit_scores <- prediction(predictions=a$predicted,labels=a$card_offer)
logit_pref <- performance(logit_scores,"tpr","fpr")
logit_pref

##plotting the ROC curve

plot(logit_pref, col="darkblue",lwd=2,xaxs="i",yaxs="i",tck=NA,main="ROC Curve")
box()
abline(0,1, lty=300,col="green")
grid(col="aquamarine")

