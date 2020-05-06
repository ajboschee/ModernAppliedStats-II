



#https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.names

#https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/

#not good example 

getwd()
list.files("C:/Users/andre/OneDrive/Documents/STAT 602")
dat=read.csv("C:/Users/andre/OneDrive/Documents/STAT 602/letter-recognition.txt", header = F)
head(dat)

dim(dat)
summary(dat$V1)
dat.uv=dat[dat$V1%in%c("V", "U"), ]

head(dat.uv)
summary(dat.uv)

dat.uv$V1<-paste(dat.uv$V1)

str(dat.uv)

(2/3)*sum(table(dat.uv$V1))

rows.trn=sort(sample(1:1577, size = 1050, replace = F))

trn.dat=dat.uv[rows.trn,]
tst.dat=dat.uv[-rows.trn,]

summary(trn.dat)

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
plot(mod.DA.G1)
summary(mod.DA.G1)
#######
mod.DA.G1.v15.v17= MclustDA(X.dat[, c(14, 16)], Class.dat, G = 1)

plot(mod.DA.G1.v15.v17)
summary(mod.DA.G1.v15.v17)

names(mod.DA.G1.v15.v17$models$U)

mod.DA.G1.v15.v17$models$U$parameters
mod.DA.G1.v15.v17$models$V$parameters
######








