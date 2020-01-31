library(glmnet)
library(MASS)
library(readxl)
library(plotmo)
library(MLmetrics)
library(lavaan)

#Input data
data <- read_excel("Math Department Diet Mood total .xlsx")
data = na.omit(data)

#Check the data
for (i in 1:length(data)) {
  data[,i] = as.factor(as.matrix(data[,i]))
  print(unique(data[,i]))
}

#Cleaning
data = data [-which(data$`1. What is your gender?`=='Other'),]
which(data$`3. In which region you are living now?`=='Option 8')
which(data$`5. What is your dietary pattern?`=='Sophomore')
which(data$`6. On average week, how many times a day do you exercise at least 20 minutes?`==unique(data$`6. On average week, how many times a day do you exercise at least 20 minutes?`)[7])
which(data$`9. On average week, how many times a day do you have dairy products?`==unique(data$`9. On average week, how many times a day do you have dairy products?`)[8])
which(data$`15. On average week, how many times do you eat dark green leafy vegetables?`==unique(data$`15. On average week, how many times do you eat dark green leafy vegetables?`)[7])
a1 = which(data$`3. In which region you are living now?`==unique(data$`3. In which region you are living now?`)[6])
a2 = which(data$`3. In which region you are living now?`==unique(data$`3. In which region you are living now?`)[9])
a3 = which(data$`3. In which region you are living now?`==unique(data$`3. In which region you are living now?`)[10])
b1 = which(data$`3. In which region you are living now?`==unique(data$`3. In which region you are living now?`)[3])
b2 = which(data$`3. In which region you are living now?`==unique(data$`3. In which region you are living now?`)[5])
data$`3. In which region you are living now?`[a1]=data$`3. In which region you are living now?`[a3]=unique(data$`3. In which region you are living now?`)[9]
data$`3. In which region you are living now?`[b1]=unique(data$`3. In which region you are living now?`)[5]
data = data[-which(data$`3. In which region you are living now?`=='Option 8'),]
#delete 2
m1 = which(data$`5. What is your dietary pattern?`==unique(data$`5. What is your dietary pattern?`)[1])
m2 = which(data$`5. What is your dietary pattern?`==unique(data$`5. What is your dietary pattern?`)[4])
m3 = which(data$`5. What is your dietary pattern?`==unique(data$`5. What is your dietary pattern?`)[5])
m4 = which(data$`5. What is your dietary pattern?`==unique(data$`5. What is your dietary pattern?`)[10])
w1 = which(data$`5. What is your dietary pattern?`==unique(data$`5. What is your dietary pattern?`)[3])
w2 = which(data$`5. What is your dietary pattern?`==unique(data$`5. What is your dietary pattern?`)[6])
w3 = which(data$`5. What is your dietary pattern?`==unique(data$`5. What is your dietary pattern?`)[11])
v1 = which(data$`5. What is your dietary pattern?`==unique(data$`5. What is your dietary pattern?`)[7])
v2 = which(data$`5. What is your dietary pattern?`==unique(data$`5. What is your dietary pattern?`)[8])
v3 = which(data$`5. What is your dietary pattern?`==unique(data$`5. What is your dietary pattern?`)[12])
data$`5. What is your dietary pattern?`[m2]=data$`5. What is your dietary pattern?`[m3]=data$`5. What is your dietary pattern?`[m4]=unique(data$`5. What is your dietary pattern?`)[1]
data$`5. What is your dietary pattern?`[w2]=data$`5. What is your dietary pattern?`[w3]=unique(data$`5. What is your dietary pattern?`)[3]
data$`5. What is your dietary pattern?`[v1]=data$`5. What is your dietary pattern?`[v2]=data$`5. What is your dietary pattern?`[w3]=unique(data$`5. What is your dietary pattern?`)[12]
data = data[-which(data$`6. On average week, how many times a day do you exercise at least 20 minutes?`==unique(data$`6. On average week, how many times a day do you exercise at least 20 minutes?`)[7]),]
#delete 2
n1 = which(data$`7. In the past 7 days, how many times did you eat breakfast?`==unique(data$`7. In the past 7 days, how many times did you eat breakfast?`)[7])
n2 = which(data$`7. In the past 7 days, how many times did you eat breakfast?`==unique(data$`7. In the past 7 days, how many times did you eat breakfast?`)[8])
data$`7. In the past 7 days, how many times did you eat breakfast?`[n1]=data$`7. In the past 7 days, how many times did you eat breakfast?`[n2]=unique(data$`7. In the past 7 days, how many times did you eat breakfast?`)[7]
a1 = which(data$`8. On average week, how many times a day do you eat whole grain products?`==unique(data$`8. On average week, how many times a day do you eat whole grain products?`)[3])
a2 = which(data$`8. On average week, how many times a day do you eat whole grain products?`==unique(data$`8. On average week, how many times a day do you eat whole grain products?`)[7])
data$`8. On average week, how many times a day do you eat whole grain products?`[a1]=data$`8. On average week, how many times a day do you eat whole grain products?`[a2]=unique(data$`8. On average week, how many times a day do you eat whole grain products?`)[3]
data = data[-which(data$`9. On average week, how many times a day do you have dairy products?`==unique(data$`9. On average week, how many times a day do you have dairy products?`)[8]),]
#delete 8
a1 = which(data$`9. On average week, how many times a day do you have dairy products?`==unique(data$`9. On average week, how many times a day do you have dairy products?`)[1])
a2 = which(data$`9. On average week, how many times a day do you have dairy products?`==unique(data$`9. On average week, how many times a day do you have dairy products?`)[7])
data$`9. On average week, how many times a day do you have dairy products?`[a2]=unique(data$`9. On average week, how many times a day do you have dairy products?`)[1]
n1 = which(data$`10. On average week, how many times a day do you consume coffee or other sources of caffeine?`==unique(data$`10. On average week, how many times a day do you consume coffee or other sources of caffeine?`)[4])
n2 = which(data$`10. On average week, how many times a day do you consume coffee or other sources of caffeine?`==unique(data$`10. On average week, how many times a day do you consume coffee or other sources of caffeine?`)[7])
data$`10. On average week, how many times a day do you consume coffee or other sources of caffeine?`[n2]=unique(data$`10. On average week, how many times a day do you consume coffee or other sources of caffeine?`)[4]
data = data[-which(data$`15. On average week, how many times do you eat dark green leafy vegetables?`==unique(data$`15. On average week, how many times do you eat dark green leafy vegetables?`)[7]),]
#delete 2
a1 = which(data$`16. On average week, how many times do you eat beans?`==unique(data$`16. On average week, how many times do you eat beans?`)[4])
a2 = which(data$`16. On average week, how many times do you eat beans?`==unique(data$`16. On average week, how many times do you eat beans?`)[7])
data$`16. On average week, how many times do you eat beans?`[a1]=unique(data$`16. On average week, how many times do you eat beans?`)[7]
a1 = which(data$`20. On average week, how many times do you take fish oil supplements?`==unique(data$`20. On average week, how many times do you take fish oil supplements?`)[2])
a2 = which(data$`20. On average week, how many times do you take fish oil supplements?`==unique(data$`20. On average week, how many times do you take fish oil supplements?`)[7])
data$`20. On average week, how many times do you take fish oil supplements?`[a2]=unique(data$`20. On average week, how many times do you take fish oil supplements?`)[2]
a1 = which(data$`21. During the past month, about how often did you feel NERVOUS?`=='None')
data$`21. During the past month, about how often did you feel NERVOUS?`[a1]=unique(data$`21. During the past month, about how often did you feel NERVOUS?`)[4]

data=na.omit(data)
for (i in 1:length(data)) {
  data[,i] = as.factor(as.matrix(data[,i]))
  print(unique(data[,i]))
}

names(data) = c("timestamp","gender","age","region","education","diet","exercise","breakfast","whole grain","dairy product","coffee","fruits","flaxseed/nuts","rice/pasta","meat","vegetables","beans","fish","fast food","multivitamin","fish oil","nervous","hopeless","restless","depressed","effort","worthless")
names(data)

ud=unique(data$diet)
oned=c(which(data$diet==ud[4]),which(data$diet==ud[5]),which(data$diet==ud[6]),which(data$diet==ud[7]))
data=data[-oned,]

#convert status into scores
x=as.data.frame(data[,2:21])
y=as.matrix(data[,22:27])
s=matrix(0,dim(y)[1],dim(y)[2])

for (i in 1:dim(y)[1]) {
  for (j in 1:dim(y)[2]) {
    if (y[i,j]=='None of the time'){
      s[i,j]=5
    }
    if (y[i,j]=='A little of the time'){
      s[i,j]=4
    }
    if (y[i,j]=='Some of the time'){
      s[i,j]=3
    }
    if (y[i,j]=='Most of the time'){
      s[i,j]=2
    }
    if (y[i,j]=='All the time'){
      s[i,j]=1
    }
  }
}
y=rowMeans(s)

#some plot
barplot(prop.table(table(y)),main = 'Mental health score histogram',xlab = 'score', ylab = 'frequency')

#order the data
u2=unique(x[,2])
x[,2]=factor(x[,2],levels = c(as.character(u2[1]),as.character(u2[2]),as.character(u2[3]),as.character(u2[4])),ordered = TRUE)
u4=unique(x[,4])
x[,4]=factor(x[,4],levels=c(as.character(u4[6]),as.character(u4[4]),as.character(u4[1]),as.character(u4[8]),as.character(u4[2]),as.character(u4[3]),as.character(u4[5]),as.character(u4[7])),ordered = TRUE)
u6=unique(x[,6])
x[,6]=factor(x[,6],levels = c(as.character(u6[1]),as.character(u6[2]),as.character(u6[3]),as.character(u6[6]),as.character(u6[4]),as.character(u6[5])),ordered = TRUE)
u7=unique(x[,7])
x[,7]=factor(x[,7],levels = c(as.character(u7[7]),as.character(u7[3]),as.character(u7[6]),as.character(u7[5]),as.character(u7[1]),as.character(u7[4]),as.character(u7[2])),ordered = TRUE)
u8=unique(x[,8])
x[,8]=factor(x[,8],levels = c(as.character(u8[4]),as.character(u8[2]),as.character(u8[3]),as.character(u8[6]),as.character(u8[5]),as.character(u8[1])),ordered = TRUE)
u9=unique(x[,9])
x[,9]=factor(x[,9],levels = c(as.character(u9[6]),as.character(u9[2]),as.character(u9[3]),as.character(u9[4]),as.character(u9[5]),as.character(u9[1])),ordered = TRUE)
u10=unique(x[,10])
x[,10]=factor(x[,10],levels = c(as.character(u10[4]),as.character(u10[6]),as.character(u10[3]),as.character(u10[2]),as.character(u10[5]),as.character(u10[1])),ordered = TRUE)
u11=unique(x[,11])
x[,11]=factor(x[,11],levels = c(as.character(u11[6]),as.character(u11[3]),as.character(u11[4]),as.character(u11[1]),as.character(u11[2]),as.character(u11[5])),ordered = TRUE)
u12=unique(x[,12])
x[,12]=factor(x[,12],levels = c(as.character(u12[3]),as.character(u12[1]),as.character(u12[5]),as.character(u12[2]),as.character(u12[6]),as.character(u12[4])),ordered = TRUE)
u13=unique(x[,13])
x[,13]=factor(x[,13],levels = c(as.character(u13[6]),as.character(u13[5]),as.character(u13[2]),as.character(u13[4]),as.character(u13[3]),as.character(u13[1])),ordered = TRUE)
u14=unique(x[,14])
x[,14]=factor(x[,14],levels = c(as.character(u14[6]),as.character(u14[5]),as.character(u14[2]),as.character(u14[4]),as.character(u14[3]),as.character(u14[1])),ordered = TRUE)
u15=unique(x[,15])
x[,15]=factor(x[,15],levels = c(as.character(u15[6]),as.character(u15[1]),as.character(u15[2]),as.character(u15[5]),as.character(u15[3]),as.character(u15[4])),ordered = TRUE)
u16=unique(x[,16])
x[,16]=factor(x[,16],levels = c(as.character(u16[1]),as.character(u16[2]),as.character(u16[4]),as.character(u16[5]),as.character(u16[3]),as.character(u16[6])),ordered = TRUE)
u17=unique(x[,17])
x[,17]=factor(x[,17],levels = c(as.character(u17[2]),as.character(u17[1]),as.character(u17[4]),as.character(u17[3]),as.character(u17[6]),as.character(u17[5])),ordered = TRUE)
u18=unique(x[,18])
x[,18]=factor(x[,18],levels = c(as.character(u18[1]),as.character(u18[2]),as.character(u18[5]),as.character(u18[6]),as.character(u18[3]),as.character(u18[4])),ordered = TRUE)
u19=unique(x[,19])
x[,19]=factor(x[,19],levels = c(as.character(u19[1]),as.character(u19[4]),as.character(u19[3]),as.character(u19[5]),as.character(u19[6]),as.character(u19[2])),ordered = TRUE)
u20=unique(x[,20])
x[,20]=factor(x[,20],levels = c(as.character(u20[1]),as.character(u20[2]),as.character(u20[4]),as.character(u20[5]),as.character(u20[6]),as.character(u20[3])),ordered = TRUE)

for (i in 1:dim(x)[2]) {
  barplot(prop.table(table(x[,i])),main = as.character(colnames(x)[i]))
}

xm=matrix(0,dim(x)[1],dim(x)[2])
for (i in 6:20) {
  for (j in 1:nrow(x)) {
    if (x[j,i]=='None'){
      xm[j,i]=0
    }
    if (x[j,i]=='1 time'){
      xm[j,i]=1
    }
    if ((x[j,i]=='2 times')|(x[j,i]==unique(x[,8])[3])){
      xm[j,i]=2
    }
    if ((x[j,i]=='3 times')|(x[j,i]==unique(x[,12])[2])){
      xm[j,i]=3
    }
    if (x[j,i]=='4 times'){
      xm[j,i]=4
    }
    if ((x[j,i]=='5 times')|(x[j,i]=='More than 4 times')){
      xm[j,i]=5
    }
    if (x[j,i]=='6 or 7 times'){
      xm[j,i]=6
    }
  }
}

for (i in 6:20) {
  print(unique(xm[,i]))
}

x[,6:20]=xm[,6:20]

savex = x
x = savex

xmm = matrix(0,dim(x)[1],1)
for (i in 1:dim(x)[1]) {
  if (x[i,4]=="Less than High School"){
    xmm[i,1]=1
  }
  if (x[i,4]=="High School"){
    xmm[i,1]=2
  }
  if (x[i,4]=="2 or 4 Years of College Degree (AA, BA, BS)"){
    xmm[i,1]=3
  }
  if (x[i,4]=="Graduate"){
    xmm[i,1]=4
  }
  if (x[i,4]=="Master's Degree"){
    xmm[i,1]=5
  }
  if (x[i,4]=="Doctoral Degree"){
    xmm[i,1]=6
  }
  if (x[i,4]=="Professional Degree (MD, JD, PharmD, ...)"){
    xmm[i,1]=7
  }
  if (x[i,4]=="Professional"){
    xmm[i,1]=8
  }
}
x[,4]=xmm



par(mfrow=c(2,2))
for (i in 1:dim(x)[2]) {
  what=matrix(0,1,length(unique(x[,i])))
  colnames(what)=sort(unique(x[,i]))
  for (j in 1:length(unique(x[,i]))) {
    what[1,j]=mean(y[x[,i]==sort(unique(x[,i]))[j]])
  }
  barplot(what,main=as.character(colnames(x)[i]),xlab='type/frequency',ylab = 'average score')
}
par(mfrow=c(1,1))

for (i in 1:dim(x)[2]) {
  what=matrix(0,1,length(unique(x[,i])))
  colnames(what)=sort(unique(x[,i]))
  for (j in 1:length(unique(x[,i]))) {
    what[1,j]=mean(y[x[,i]==sort(unique(x[,i]))[j]])
  }
  gp = ggplot(data = data, aes(x = x[,i], y = y))+geom_jitter(width = .3)+ggtitle(as.character(colnames(x)[i]))+xlab('type/frequency')
  gp = gp + 
  print(gp)
}
i = 8
what=matrix(0,1,length(unique(x[,i])))
colnames(what)=sort(unique(x[,i]))
for (j in 1:length(unique(x[,i]))) {
  what[1,j]=mean(y[x[,i]==sort(unique(x[,i]))[j]])
}
whatcol = c()
for (j in 1:length(what)-1){
  whatcol = c(whatcol,what[j+1] >= what[j])
}
whatcol = as.numeric(whatcol)+1
for (j in 1:length(whatcol)) {
  if (whatcol[j] == 1){
    whatcol[j] = 4
  }
}
whatcol = c(whatcol,99)
p =ggplot(data = data, aes(x = x[,i], y = y))+geom_jitter(width = .3)+ggtitle(as.character(colnames(x)[i]))+xlab('type/frequency')+ stat_summary(aes(y = y,group=1), fun.y=mean, colour=whatcol, geom="line",group=1,size = 2)
print(p)
for (i in 1:dim(x)[2]) {
  
}


cl_data=cbind(y,x)

#split the data
set.seed(123)
train = cl_data[sample(nrow(cl_data),0.75*nrow(cl_data)),]
test = cl_data[sample(nrow(cl_data),0.25*nrow(cl_data)),]
train_y=train[,1]
train_x=train[,-1]
test_y=test[,1]
test_x=test[,-1]

#Training

#lmod
lmod=lm(y~.,data = train)
#glm
mod_glm = glm(y~.,data = train)
#backward AIC
lmod_aic = stepAIC(lmod, direction = 'backward')
#lasso
mod_lasso = glmnet(data.matrix(train_x), train_y, alpha = 1)
mod_lasso2 = glmnet(data.matrix(train_x), train_y, alpha = 1, lambda = min(mod_lasso$lambda))
#ridge
mod_ridge = glmnet(data.matrix(train_x), train_y, alpha = 0)
mod_ridge2 = glmnet(data.matrix(train_x), train_y, alpha = 0, lambda = min(mod_ridge$lambda))
#elastic net
mod_el = glmnet(data.matrix(train_x), train_y, alpha = .5)
mod_el2 = glmnet(data.matrix(train_x), train_y, alpha = .5, lambda = min(mod_el$lambda))

#Testing

#lmod
MSE(predict(lmod,test_x),test_y)
#glm
MSE(predict(mod_glm,test),test_y)
#backward AIC
MSE(predict(lmod_aic, test_x),test_y)
#lasso
MSE(predict.glmnet(mod_lasso2,data.matrix(test_x)),test_y)
#ridge
MSE(predict.glmnet(mod_ridge2,data.matrix(test_x)),test_y)
#elastic net
MSE(predict.glmnet(mod_el2,data.matrix(test_x)),test_y)

#Visualize the residuals
par(mfrow=c(1,1))
plot(test_y[1:50], pch=16, cex=1, ylim = c(0.7,5.3),
     xlab = 'Observation', ylab = 'Mental health score', main = 'Residual Plot of first 50 observations')

lines(predict(lmod,test_x)[1:50], col = 'red')
lines(predict(mod_glm,test)[1:50], col = 'blue')
lines(predict(lmod_aic, test_x)[1:50], col = 'green')
lines(predict.glmnet(mod_lasso2,data.matrix(test_x))[1:50], col = 'yellow')
lines(predict.glmnet(mod_ridge2,data.matrix(test_x))[1:50], col = 'orange')
lines(predict.glmnet(mod_el2,data.matrix(test_x))[1:50], col = 'purple')

legend(0,2.5,legend = c('Linear regression','Generalized linear model'
                       ,'Backward AIC','Lasso regression','Ridge regression','Elastic net')
       ,col = c('red','blue','green','yellow','orange','purple'),lty = 1:6, cex = 0.6)

#range
c(min(predict(lmod,test_x)),max(predict(lmod,test_x)))

#coef

unique(train$gender)
unique(train$diet)
unique(train$region)

#Try to split data according to gender/diet/region
train_f = train[(train[,2]=='Female'),c(-2,-4,-6)]
test_f = test[(test[,2]=='Female'),c(-2,-4,-6)]
train_f_x = train_f[,-1]
train_f_y = train_f[,1]
test_f_x = test_f[,-1]
tesf_f_y = test_f[,1]

train_m = train[(train[,2]=='Male'),c(-2,-4,-6)]
test_m = test[(test[,2]=='Male'),c(-2,-4,-6)]
train_m_x = train_m[,-1]
train_m_y = train_m[,1]
test_m_x = test_m[,-1]
tesf_m_y = test_m[,1]

train_wes = train[(train[,6]==unique(train[,6])[2]),c(-2,-4,-6)]
test_wes = test[(test[,6]==unique(train[,6])[2]),c(-2,-4,-6)]
train_wes_x = train_wes[,-1]
train_wes_y = train_wes[,1]
test_wes_x = test_wes[,-1]
test_wes_y = test_wes[,1]

train_est = train[(train[,6]==unique(train[,6])[3]),c(-2,-4,-6)]
test_est = test[(test[,6]==unique(train[,6])[3]),c(-2,-4,-6)]
train_est_x = train_est[,-1]
train_est_y = train_est[,1]
test_est_x = test_est[,-1]
test_est_y = test_est[,1]

train_med = train[(train[,6]==unique(train[,6])[1]),c(-2,-4,-6)]
test_med = train[(test[,6]==unique(train[,6])[1]),c(-2,-4,-6)]
train_med_x = train_med[,-1]
train_med_y = train_med[,1]
test_med_x = test_med[,-1]
test_med_y = test_med[,1]

