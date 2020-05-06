#https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.names

#https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/

#not good exampleof code... really just playing

xs = runif(100)
#> xs
#[1] 0.60529819 0.34055921 0.04117032 0.40175251 0.07905965
#[6] 0.31255265 0.32507594 0.07836646 0.15021977 0.15242507
#[11] 0.91342360 0.70225290 0.82054832 0.66556309 0.21618511
#[16] 0.46306240 0.71685305 0.82218805 0.82074193 0.19994455
#[21] 0.60113549 0.42983600 0.63756833 0.42002401 0.87987004
#[26] 0.32072803 0.31531284 0.49357208 0.33767030 0.22278599
#[31] 0.83023967 0.53849678 0.09869789 0.10959014 0.57385503




xs[1]
set.seed(0.6052982)

getwd()
list.files("C:/Users/saund/Desktop/")
dat=read.csv("C:/Users/saund/Desktop/letter-recognition.txt", header = F)
head(dat)

dim(dat)
summary(dat$V1)
dat.uv=dat[dat$V1%in%c("V", "U"), ]

head(dat.uv)
summary(dat.uv)

dat.uv$V1<-paste(dat.uv$V1)

str(dat.uv)

(2 / 3) * sum(table(dat.uv$V1))
set.seed(100029879)
rows.trn=sort(sample(1:1577, size = 1051, replace = F))

trn.dat=dat.uv[rows.trn,]
tst.dat=dat.uv[-rows.trn,]

summary(trn.dat)
table(trn.dat$V1)
boxplot(V2~V1, data = trn.dat)
boxplot(V3~V1, data = trn.dat)
boxplot(V4~V1, data = trn.dat)
boxplot(V5~V1, data = trn.dat)
boxplot(V6~V1, data = trn.dat)
boxplot(V7~V1, data = trn.dat)
boxplot(V8~V1, data = trn.dat)
boxplot(V9~V1, data = trn.dat)
boxplot(V10~V1, data = trn.dat)
boxplot(V11~V1, data = trn.dat)
boxplot(V12~V1, data = trn.dat)
boxplot(V13~V1, data = trn.dat)
boxplot(V14~V1, data = trn.dat)
boxplot(V15~V1, data = trn.dat)
boxplot(V16~V1, data = trn.dat)
boxplot(V17~V1, data = trn.dat)

install.packages("mclust")
library(mclust)
?MclustDA
X.dat=trn.dat[,-1]
Class.dat=trn.dat[,1]

#install.packages("ISLR")
#library(ISLR)
#head(Auto)
#mod.auto = cbind(cl = 1*(Auto$mpg > quantile(Auto$mpg, .5)), 
#                   Auto)
#names(mod.auto)

mod.DA.G1= MclustDA(X.dat, Class.dat, G = 1)
mod.DA.G2= MclustDA(X.dat, Class.dat, G = 2)
mod.DA.G3= MclustDA(X.dat, Class.dat, G = 3)
mod.DA.G4= MclustDA(X.dat, Class.dat, G = 4)
summary(mod.DA.G1)
summary(mod.DA.G2)
summary(mod.DA.G3)
summary(mod.DA.G4)

results.3=cbind(paste(predict.MclustDA(mod.DA.G3, newdata = tst.dat[, -1])$classification), paste(tst.dat[, 1]))

results.2=cbind(paste(predict.MclustDA(mod.DA.G2, newdata = tst.dat[, -1])$classification), paste(tst.dat[, 1]))


mean(results.2[,1]==results.2[,2])
mean(results.3[,1]==results.3[,2])

##Looks like G3 model gets one or two more correct classifications!


##Lets play with a cross validation
dat.qo = dat[dat$V1 %in% c("Q", "O"),1:5]
head(dat.qo)
summary(dat.qo)
dat.qo$V1 <- paste(dat.qo$V1)
str(dat.qo)
dim(dat.qo)

permut.n = sample(1:1536, 1536)

folds=cbind(sort(rep(seq(1, 10, 1), 154))[1:1536], permut.n)

storer = NULL

for (i in 1:10)
{
    index.i = folds[folds[,1]==i, 2]
    dat.qo.i.tst = dat.qo[index.i,]
    dat.qo.i.trn = dat.qo[ - index.i,]
    #dat.qo.i.trn
    X.dat = dat.qo.i.trn[, -1]
    Class.dat = dat.qo.i.trn[, 1]
    mod.i = MclustDA(X.dat, Class.dat, G = 4)
    results.i = cbind(paste(predict.MclustDA(mod.i, newdata = dat.qo.i.tst[, -1])$classification), paste(dat.qo.i.tst[, 1]))
    storer=c(storer,mean(results.i[, 1] == results.i[, 2]))
}
mean(storer)
library(mclust)
mod = Mclust(dat.qo[-1])
summary(mod)
plot.Mclust(mod, what="BIC")












1