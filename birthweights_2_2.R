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
# All VIF values are below 5, so no co-linearity.

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
confint(lm(Birthweight ~ Headcirc + Gestation, data = df))
# prediction intervals
predict(lm(Birthweight ~ Headcirc + Gestation, data = df), interval="prediction")

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
df2 <- read_csv("Birthweight_data.csv", col_types = cols(ID = col_skip(), Length = col_skip(), Headcirc = col_skip(), mage = col_skip(), mnocig = col_skip(), mheight = col_skip(), mppwt = col_skip(), fage = col_skip(), fedyrs = col_skip(), fnocig = col_skip(), fheight = col_skip()))
View(df2)

boxplot(df2$Birthweight ~ df2$smoker, xlab="smoker", ylab="Birthweight")
boxplot(df2$Birthweight ~ df2$mage35, xlab="mage35", ylab="Birthweight")

# t-test for Birthweight ~ smoker
t.test(df2$Birthweight ~ df2$smoker)

# t-test for Birthweight ~ mage35
t.test(df2$Birthweight ~ df2$mage35)

# f) logistic regression model


# g) interaction between Gestation and smoker, and Gestation and mage35


# h) prob of low baby weight for each combi


# i) contingency table