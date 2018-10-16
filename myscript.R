library(UsingR)
library(quantmod)
data(tastesgreat)
tastesgreat
t=tastesgreat
head(t)
dim(t)
str(t)
attach(t)
age=na.omit(age)
length(age)
t1=na.omit(t)
dim(t1)
t.test(age, conf.level=0.97)
mean(age)
#44.75  
median(age) 
#46
t.test(age, mu=46, conf.level=0.97)
n<-length(age)
conf.level<-.95
z<-qt((1+conf.level)/2, df=n-1)
se<-sd(age)/sqrt(n)
ci<-z*se
n-ci 
n+ci
sd(age)
g=table(gender)
sumofall=(g[names(g)=="Male"]+g[names(g)=="Female"])
sumofMen=g[names(g)=="Male"]
prop.test(x=sumofMen, n=sumofall, conf.level=0.99, alt="two.sided")
gender=UsingR::tastesgreat$gender
male<-subset(gender, gender=="Male")
binom.test(length(male), length(gender), p=1/2, conf.level=0.99)
enj=table(enjoyed)
sumofenj=enj[names(enj)==1]
enj=UsingR::tastesgreat$enjoyed
enj<-subset(enj, enj==1)
prop.test(x=c(length(male), length(enj)), n=c(sumofall, sumofall), conf.level=0.99)
prop.test(x=c(length(enj),23), n=c(sumofall, 55), conf.level=0.95)
copper=getSymbols('PCOPPUSDQ', src='FRED', auto.assign=FALSE)
nickel=getSymbols('PNICKUSDQ', src='FRED', auto.assign=FALSE)
uranium=getSymbols('PURANUSDQ', src='FRED', auto.assign=FALSE)
par(mfrow=c(2,2))
plot(copper)
plot(nickel)
plot(uranium)
install.packages('normtest')
library(normtest)
jb.norm.test(copper$PCOPPUSDQ)
jb.norm.test(nickel$PNICKUSDQ)
jb.norm.test(uranium$PURANUSDQ)
install.packages('car')
library(car)
summary(copper$PCOPPUSDQ)
par(mfrow=c(2,2))
qqPlot(as.numeric(copper$PCOPPUSDQ), ylab="copper", main="QQ-plot for copper")
qqPlot(as.numeric(nickel$PNICKUSDQ), ylab="nickel", main="QQ-plot for nickel")
qqPlot(as.numeric(uranium$PURANUSDQ), ylab="uranium", main="QQ-plot for uranium")
chisq.test(age)
library(ISLR)
data(Credit)
attach(Credit)
table(Gender, Education)
GenEdu<-rbind(c(Gender, Education))
chisq.test(GenEdu)
data(diamond)
attach(diamond)
lm(price~carat)
abline(-259.6, 3721.0)
plot(price~carat, main="Regression price~carat")
abline(-259.6, 3721.0)
linreg<-lm(price~carat)
summary(linreg)
plot(fitted(linreg), residuals(linreg), main="residuals and fitted values for linear model")
abline(0,0)
car::qqPlot(residuals(linreg), main="QQ-Plot for residuals of linear model")
linreg1<-data.frame(carat=c(0.14,0.24,0.34,0.40))
head(Credit)
predict(linreg,newdata=linreg1, interval="confidence")
mlinreg=lm(Rating~Income+Balance)
mlinreg
summary(mlinreg)
plot(fitted(mlinreg), residuals(mlinreg), main="residuals and fitted values for multivariative linear model")
abline(0,0)
anova(
lm(Rating~Income),
lm(Rating~Balance),
lm(Rating~Ethnicity),
lm(Rating~Income+Balance),
lm(Rating~Income+Ethnicity),
lm(Rating~Balance+Ethnicity),
lm(Rating~Income+Balance+Ethnicity)
)
library(MASS)
stepAIC(lm(Rating~Income+Balance))
summary(lm(Rating~Income))$adj.r.squared
summary(lm(Rating~Balance))$adj.r.squared
summary(lm(Rating~Ethnicity))$adj.r.squared
summary(lm(Rating~Income+Balance))$adj.r.squared
summary(lm(Rating~Income+Ethnicity))$adj.r.squared
summary(lm(Rating~Balance+Ethnicity))$adj.r.squared
summary(lm(Rating~Income+Balance+Ethnicity))$adj.r.squared
defData <- data.frame(Default)
head(defData)
attach(defData)
# Logistic Model
logFit=glm(default~student+balance+income,family="binomial")
summary(logFit)
stepAIC(logFit)
# Predict using the model
probData <- predict(logFit, type = "response", newdata = defData)
# Convert probabilities to predictions
predData <- rep("No", nrow(defData))
predData[probData > 0.5] = "Yes"      # set threshold 0.5
predData <- as.factor(predData)
# Confusion matrix for predictions
table(default, predData)
summary(default)
# Performance measures for Prediction
cm <- table(default, predData)
TP <- cm[2,2]  # True Positive (Yes predicted as Yes)
TN <- cm[1,1]  # True Negative (No predicted as No)
FP <- cm[1,2]  # False Positive (No predicted as Yes) -- Type I error
FN <- cm[2,1]  # False Negative (Yes predicted as No) -- Type II error
TP
TN
FP
FN
# Classification Accuracy
(TN + TP) / (TN + TP + FN + FP)    # Correct Classification / Total
mean(predData == default)  # Classification accuracy (alt.)

# Precision/Positive predictive value (PPV)
TP /(TP + FP)    

# False Positive Rate (fpr) / Type I error
FP / (TN + FP)      # False Positive / Total Negative
1 - FP / (TN + FP)  # Specificity = 1 - (Type I error)

# True Positive Rate / Sensitivity / Recall / Power
TP / (TP + FN)      # True Positive / Total Positive
1 - TP / (TP + FN)  # Type II error = 1 - Sensitivity

# True negative rate / Specificity

TN / (TN + FP)

train <- sample(10000,7000)
Defaultx <- Default[-train,]
glm.fit <- glm(default~ balance + student, data = Default, 
family = binomial, subset = train)
glm.probs <- predict(glm.fit, Defaultx, type = "response")
glm.pred <- rep("No", 3000)
glm.pred[glm.probs > .5] = "Yes"
defaultVector <- Defaultx$default 
mean(glm.pred == defaultVector)
# Fit LDA
ldafit1 <- lda(default~student+balance+income, data = Default, subset=train)
ldafit1
plot(ldafit1)
ldafit2 <- lda(default~student+balance+income, data = Default, subset=test)
ldafit2
plot(ldafit2)
# QDA
qdafit<- qda(default~student+balance+income, data = Default, subset=train)
qdafit
qdapred<-predict(qdafit)
str(qdapred)
install.packages("class")
library(class)
Default.subset=Default[c("income", "balance", "student")]
Default.subset=Default.subset[complete.cases(Default.subset),]
idxs<-1:7000
trainDefault<-Default.subset[idxs,]
testDefault <- Default.subset[-idxs,]
trainDefault$student <- as.numeric(trainDefault$student)
testDefault$student <- as.numeric(testDefault$student)
trainDefault$default <- as.numeric(trainDefault$default)
testDefault$default <- as.numeric(testDefault$default)
train.def <- Default$default[idxs]
test.def <- Default$default[-idxs]
nn3 <- knn(trainDefault,testDefault,train.def,k=3)
100 * sum(test.def == nn3)/100
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
data(decathlon2)
tail(decathlon2)
dec<-decathlon2
attach(dec)
dec.active <- dec[1:23, 1:10]
head(dec.active[, 1:6], 4)
res.pca <- FactoMineR::PCA(dec.active,scale.unit=TRUE)
barplot(res.pca$eig[,1],main='Eigenvalues',names.arg=paste('dim',1:nrow(res.pca$eig)))
colSums(res.pca$eig[1:2,])
head(round(cbind(res.pca$ind$coord[,1:4],res.pca$ind$cos2[,1:4],res.pca$ind$contrib[,1:4]),2),40)
round(res.pca$eig,2)
install.packages("corrplot")
library("corrplot")
var <- get_pca_var(res.pca)
head(var$cos2, 4)
corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(res.pca, choice = "var", axes = 1:2)
tail(decathlon2)
dec2<-decathlon2
res.pca2=FactoMineR::PCA(dec2,scale.unit=TRUE,quali.sup = 13)
concat.data <- cbind.data.frame(dec2$Competition,res.pca2$ind$coord)
ellipse.coord <- coord.ellipse(concat.data,bary = TRUE)
plot.PCA(res.pca2,habillage=13,ellipse=ellipse.coord,cex=0.8)
PCA(dec.active,graph=FALSE)
dimdesc(res.pca)
round(res.pca$var$cos2,4)
head(res.pca$var$contrib)
plot(res.pca, choix = "var")
fviz_pca_var(res.pca)
dimdesc(res.pca)
pairs(dec,main='Correlation between the variables')

