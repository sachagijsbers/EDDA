library(readr)
library(car)
library(xtable)
birthweight_df <- read_csv("Birthweight_data.csv")
# View(birthweight_df)

df <- read_csv("Birthweight_data.csv", col_types = cols(ID = col_skip(), smoker = col_skip(), lowbwt = col_skip(), mage35 = col_skip()))
View(df)

# Response variable: Birthweight
# Potential predictors: Length, Headcirk, Gestation, mage, mnosig, 
# mheight, mppwt, fage, fedyrs, fnosig, fheight.
# Birthweight, Length, Headcirk, Gestation, mage, mnosig, 
# mheight, mppwt, fage, fedyrs, fnosig, fheight.

# a) problem of potential and influence points, problem of collinearity.

pairs(df)

# Cooks distance



plot(1:42, cooks.distance(lm(df$Birthweight ~ df$Length)), type="b", xlab="Index", ylab="Cook's distance")
plot(1:42, cooks.distance(lm(df$Birthweight ~ df$Headcirc)), type="b", xlab="Index", ylab="Cook's distance")
plot(1:42, cooks.distance(lm(df$Birthweight ~ df$Gestation)), type="b", xlab="Index", ylab="Cook's distance")
plot(1:42, cooks.distance(lm(df$Birthweight ~ df$mage)), type="b", xlab="Index", ylab="Cook's distance")
plot(1:42, cooks.distance(lm(df$Birthweight ~ df$mnocig)), type="b", xlab="Index", ylab="Cook's distance")
plot(1:42, cooks.distance(lm(df$Birthweight ~ df$mheight)), type="b", xlab="Index", ylab="Cook's distance")
plot(1:42, cooks.distance(lm(df$Birthweight ~ df$mppwt)), type="b", xlab="Index", ylab="Cook's distance")
plot(1:42, cooks.distance(lm(df$Birthweight ~ df$fage)), type="b", xlab="Index", ylab="Cook's distance")
plot(1:42, cooks.distance(lm(df$Birthweight ~ df$fedyrs)), type="b", xlab="Index", ylab="Cook's distance")
plot(1:42, cooks.distance(lm(df$Birthweight ~ df$fnocig)), type="b", xlab="Index", ylab="Cook's distance")
plot(1:42, cooks.distance(lm(df$Birthweight ~ df$fheight)), type="b", xlab="Index", ylab="Cook's distance")

# Influence points

birthweight_model <- lm(Birthweight ~ Length + Headcirc + Gestation + mage + mnocig + mheight + mppwt + fage + fedyrs + fnocig + fheight, data = df)
par(mfrow=c(2,2))
plot(birthweight_model)

# Collinearity
birthweight_lm <- lm(Birthweight ~ Length + Headcirc + Gestation + mage + mnocig + mheight + mppwt + fage + fedyrs + fnocig + fheight, data = df)
vif(birthweight_lm)
# All VIF values are below 5, so no collinearity.




# b) step-down method, remove highest p-value
summary(lm(Birthweight ~ Length + Headcirc + Gestation + mage + mnocig + mheight + mppwt + fage + fedyrs + fnocig + fheight, data = df))
# remove fage, p = 0.81802, Multiple R-squared:  0.7678,	Adjusted R-squared:  0.6826 
summary(lm(Birthweight ~ Length + Headcirc + Gestation + mage + mnocig + mheight + mppwt + fedyrs + fnocig + fheight, data = df))
# remove mheight, p = 0.84337 , Multiple R-squared:  0.7674,	Adjusted R-squared:  0.6923 
summary(lm(Birthweight ~ Length + Headcirc + Gestation + mage + mnocig + mppwt + fedyrs + fnocig + fheight, data = df))
# remove fedyrs, p = 0.80967, Multiple R-squared:  0.7671,	Adjusted R-squared:  0.7016
summary(lm(Birthweight ~ Length + Headcirc + Gestation + mage + mnocig + mppwt + fnocig + fheight, data = df))
# remove fnocig, p=0.55712, Multiple R-squared:  0.7666,	Adjusted R-squared:  0.7101 
summary(lm(Birthweight ~ Length + Headcirc + Gestation + mage + mnocig + mppwt + fheight, data = df))
# remove mnocig, p=0.30388, Multiple R-squared:  0.7641,	Adjusted R-squared:  0.7156 
summary(lm(Birthweight ~ Length + Headcirc + Gestation + mage + mppwt + fheight, data = df))
# remove Length, p=0.127536, Multiple R-squared:  0.7566,	Adjusted R-squared:  0.7149 
summary(lm(Birthweight ~ Headcirc + Gestation + mage + mppwt + fheight, data = df))
# remove fheight, p=0.1234, Multiple R-squared:  0.7396,	Adjusted R-squared:  0.7035 
summary(lm(Birthweight ~ Headcirc + Gestation + mage + mppwt, data = df))
# remove mage, p = 0.1991, Multiple R-squared:  0.7216,	Adjusted R-squared:  0.6916 
summary(lm(Birthweight ~ Headcirc + Gestation + mppwt, data = df))
# remove mppwt, p = 0.137, Multiple R-squared:  0.7088,	Adjusted R-squared:  0.6858 
summary(lm(Birthweight ~ Headcirc + Gestation, data = df))
# p-values are below 0.05, so we stop here.
xtable(summary(lm(Birthweight ~ Headcirc + Gestation, data = df)))

vif(lm(Birthweight ~ Headcirc + Gestation, data = df))

# c) 95% CI for lm(Birthweight ~ Headcirc + Gestation, data = df) for the average values of all the predictors in that model.
xtable(confint(lm(Birthweight ~ Headcirc + Gestation, data = df)))
# prediction intervals
xtable(predict(lm(Birthweight ~ Headcirc + Gestation, data = df), interval="prediction"))

# d) LASSO
library(glmnet)
x <- as.matrix(df[,-2])
y <- as.double(as.matrix(df[,2]))
train = sample(1:nrow(x), 0.67*nrow(x))
x.train=x[train,]; y.train=y[train] # data to train
x.test=x[-train,]; y.test=y[-train] # data to test the prediction quality

lasso.mod=glmnet(x.train,y.train,alpha=1)
cv.lasso=cv.glmnet(x.train,y.train,alpha=1,type.measure='mse', grouped=FALSE)
plot(lasso.mod,label=T,xvar="lambda") #have a look at the lasso path
plot(cv.lasso) # the best lambda by cross-validation
plot(cv.lasso$glmnet.fit,xvar="lambda",label=T)

lambda.min=cv.lasso$lambda.min; lambda.1se=cv.lasso$lambda.1se
coef(lasso.mod,s=cv.lasso$lambda.min) #beta’s for the best lambda
y.pred=predict(lasso.mod,s=lambda.min,newx=x.test) #predict for test
mse.lasso=mean((y.test-y.pred)^2) # mse for the predicted test rows
print(mse.lasso)



# e) smoking or older mothers lighter babies?
# read csv with Birthweight, lowbwt, Gestation, smoker, mage35
df2 <- read_csv("Birthweight_data.csv", col_types = cols(ID = col_skip(), Length = col_skip(), Birthweight = col_skip(), Headcirc = col_skip(), mage = col_skip(), mnocig = col_skip(), mheight = col_skip(), mppwt = col_skip(), fage = col_skip(), fedyrs = col_skip(), fnocig = col_skip(), fheight = col_skip()))
View(df2)

boxplot(df2$Birthweight ~ df2$smoker, xlab="smoker", ylab="Birthweight")
boxplot(df2$Birthweight ~ df2$mage35, xlab="mage35", ylab="Birthweight")

# count ones
sum(df2$lowbwt[df2$smoker==0])
sum(df2$lowbwt[df2$smoker==1])
sum(df2$lowbwt[df2$mage35==0])
sum(df2$lowbwt[df2$mage35==1])

length(df2$lowbwt[df2$smoker==0])
length(df2$lowbwt[df2$smoker==1])
length(df2$lowbwt[df2$mage35==0])
length(df2$lowbwt[df2$mage35==1])

sum(df2$lowbwt[df2$smoker==0])/length(df2$lowbwt[df2$smoker==0])
sum(df2$lowbwt[df2$smoker==1])/length(df2$lowbwt[df2$smoker==1])
sum(df2$lowbwt[df2$mage35==0])/length(df2$lowbwt[df2$mage35==0])
sum(df2$lowbwt[df2$mage35==1])/length(df2$lowbwt[df2$mage35==1])




# f) logistic regression model
birthweight_logistic <- glm(lowbwt ~ smoker + mage35, data = df2, family = binomial)
xtable(summary(birthweight_logistic))



tot=xtabs(~smoker+mage35, data=df2)
print(tot)
tot.c=xtabs(lowbwt~smoker+mage35, data=df2)
print(tot.c)
xtable(round(tot.c/tot,2))

# g) interaction between Gestation and smoker, and Gestation and mage35
totage=xtabs(~Gestation, data=df2)
barplot(xtabs(lowbwt~Gestation, data=df2)/totage, col=rainbow(10), ylab="low birth weight (%)", xlab="Gestation (weeks)")

bw_log_smoker=glm(lowbwt~Gestation*smoker, data=df2, family=binomial)
summary(bw_log_smoker)
bw_log_mage35=glm(lowbwt~Gestation*mage35, data=df2, family=binomial)
xtable(summary(bw_log_mage35))

birthweight_logistic <- glm(lowbwt ~ Gestation + smoker + Gestation:smoker, data = df2, family = binomial)
summary(birthweight_logistic)


# h) prob of low baby weight for each combi
df2$smoker=as.factor(df2$smoker)
df2$mage35=as.factor(df2$mage35)
# make new dataframe where gestation is 40
df3 <- df2[df2$Gestation==40,]
View(df3)

glm_h=glm(lowbwt~smoker+mage35, data=df3, family=binomial)
summary(glm_h)

# i) contingency table smoker and mage35 with lowbwt

# Assuming df2 is your dataframe and lowbwt, smoker, and mage35 are the binary columns
table(df2$lowbwt, df2$smoker) # Contingency table for low birth weight and smoker status
table(df2$lowbwt, df2$mage35) # Contingency table for low birth weight and mother age > 35
table(df2$smoker, df2$mage35) # Contingency table for smoker status and mother age > 35

# count ones where lowbwt is 1 and smoker is 1 and mage35 is 1
sum(df2$lowbwt[df2$lowbwt==1 & df2$smoker==0 & df2$mage35==0])

sum(df2$lowbwt[df2$lowbwt==1 & df2$smoker==1 & df2$mage35==1])

sum(df2$lowbwt[df2$lowbwt==1 & df2$smoker==1 & df2$mage35==0])

# count rows where lowbwt is 0 and smoker is 0 and mage35 is 0
length(df2$lowbwt[df2$lowbwt==0 & df2$smoker==1 & df2$mage35==0])

# where mage35 = 0
length(df2$lowbwt[df2$mage35==1])


# Creating the contingency table
contingency_table <- table(df2$lowbwt, df2$smoker, df2$mage35)
print(contingency_table)

counts = matrix(c(18,1,15,2,1,0,4,1), byrow=TRUE, ncol=2, nrow=4)
counts
z = chisq.test(counts)
z

chisq.test(counts,simulate.p.value=TRUE)

xtable(residuals(z))
