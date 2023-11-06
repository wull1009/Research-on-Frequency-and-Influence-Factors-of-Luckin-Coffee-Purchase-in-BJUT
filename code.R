write.csv(yy,file="抽样草稿.csv",row.names=T)
library(openxlsx)
dat=read.xlsx("C:/Users/wulinlin/Desktop/抽样调查期末报告/报告数据.xlsx")
head(dat)
dat=dat[,-c(1,23)]

#改名
colnames(dat)=c("用时","性别","宿舍楼","购买瑞幸","不购买原因","必要","次数",
                "金额","健康次数","口味","价格","包装","服务","便捷度","品种","宣传",
                "瑞幸其他产品","校内其他产品","身高","体重","生活费")
a1=dat[which(dat$购买瑞幸=="否"),]
a2=dat[which(dat$购买瑞幸=="是"),]
head(a2)
#数据处理
a2$次数=as.numeric(a2$次数)
for (i in 1:nrow(a2)){
  if(a2[i,8]=="0-20"&a2[i,7]>3){a2=a2[-i,]}
  if(a2[i,8]=="150及以上"&a2[i,7]<5){a2=a2[-i,]}
}
nrow(a2)
a2=a2[,-5]
a2=a2[-c(72,73),]
head(a2)
#描述性分析、可视化
a3=a2[,-c(4,16,17,20)]
head(a3)
a3$用时=gsub("秒$","",a3$用时)
a3$用时=as.numeric(a3$用时)
for (i in c(2,4,6)){
  a3[,i]=as.factor(a3[,i])
  a3[,i]=as.numeric(a3[,i])
}
for(i in c(1,3,5,7,8:16)){
  a3[,i]=as.numeric(a3[,i])
}
a3=a3[,-6]
head(a3)
Mean=sapply(a3,mean)
Min=sapply(a3,min)
Median=sapply(a3,median)
Max=sapply(a3,max)
SD=sapply(a3, sd)
x=cbind(Mean,Min,Median,Max,SD)
x=round(x,4)
x
a4=a3[,c(1,5,6,7,8,9,0,11,12,13,14,15)]
round(cor(a4),3)
attach(a4)
pairs(a4)
#可视化
library(ggplot2)
dt=data.frame(dat)
head(dat)
xx=c(14,95)
yy=c("否","是")
df=data.frame(type=yy,nums=xx)
label_value=paste('(', round(df$nums/sum(df$nums) * 100, 1), '%)', sep = '')
label=paste(df$type,label_value)
p1=ggplot(df,aes(x="Content",y=nums,fill=type))+
  geom_bar(stat = "identity",position="stack",width=1)+
  coord_polar(theta="y")+
  labs(title="购买和不购买瑞幸的比例")+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(y = df$nums/2 + c(0, cumsum(df$nums)[-length(df$nums)]), x = sum(df$nums)/120, label = label));p1
p2=ggplot(dat,aes(x=身高,y=体重))+
  geom_point(aes(colour=factor(购买瑞幸),shape=factor(性别)))+
  theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1))+
  labs(title="身高概率密度图对比")+
  theme(plot.title = element_text(hjust = 0.5));p2
library(dplyr)
library(jiebaR)
mixseg=worker()
w=a1$不购买原因
dic=c("不爱喝","喝了睡不着","贵","没时间买","不知道有这店","需求不高")
new_user_word(mixseg, dic)
seg <- segment(w, mixseg)
seg
jiebaR::freq(seg)

lm1=lm(a2$次数~as.factor(a2$生活费))
library(car)
Anova(lm1,type ="III")
summary(lm1)

a2$次数=as.numeric(a2$次数)
boxplot(a2$次数~a2$生活费)
p3=ggplot(a2,aes(x=生活费,y=次数,color=生活费))+
  geom_boxplot();p3
dat$生活费<- factor(dat$生活费,levels=c("1000以下","1000-1500","1500-2000","2000-2500","2500-3000","3000-3500","3500及以上"))
p4=ggplot(dat,aes(x=生活费,fill=购买瑞幸))+
  geom_bar(stat = "count",alpha=1);p4
#购买瑞幸同学的可视化
da=a2
head(da)
nrow(da)
library(ggplot2)
p5=ggplot(data=dat,mapping=aes(x=性别, fill=购买瑞幸))+
  geom_bar(stat="count", position = 'stack') +
  guides(fill = guide_legend(reverse = TRUE));p5
xx=c(39,49)
yy=c("是","否")
df=data.frame(type=yy,nums=xx)
label_value=paste('(', round(df$nums/sum(df$nums) * 100, 1), '%)', sep = '')
label=paste(df$type,label_value)
p6=ggplot(df,aes(x="Content",y=nums,fill=type))+
  geom_bar(stat = "identity",position="stack")+
  coord_polar(theta="y")+
  labs(title="认为购买瑞幸必要或不必要的比例")+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(y = df$nums/2 + c(0, cumsum(df$nums)[-length(df$nums)]), x = sum(df$nums)/90, label = label));p6
p7=ggplot(da,aes(x=必要,y=次数,color=必要))+
  geom_violin();p7
da$金额<- factor(da$金额,levels=c("0-20","20-50","50-100","100-150","150及以上"))
p8=ggplot(da,aes(x=金额,y=次数,fill=金额))+
  geom_boxplot();p8
p9=ggplot(da)+
  geom_smooth(aes(x=健康次数,y=次数),method="lm",formula=y~I(x*x))+
  geom_point(data=da,aes(x=健康次数,y=次数,colour=健康次数))+
  scale_colour_gradient2(low = "green",mid="black",high = "red");p9
for(i in 9:15){
  da[,i]=as.numeric(da[,i])
}
p10=ggplot(da)+
  geom_smooth(aes(x=宣传,y=次数))+
  geom_point(data=da,aes(x=宣传,y=次数,colour=宣传))+
  scale_colour_gradient2(low = "yellow",mid="black",high = "purple");p10
library(dplyr)
library(jiebaR)
mixseg=worker()
w=da$瑞幸其他产品
dic=c("不会购买","饼干类","蛋糕类","半熟芝士")
new_user_word(mixseg, dic)
seg <- segment(w, mixseg)
seg
seg=table(seg)
seg=sort(seg,decreasing=T)
jiebaR::freq(seg)
library(wordcloud2)
wordcloud2(seg, size = 0.5, color = "random-light", shape="circle")
dic=c("罐装咖啡","京客隆咖啡","超市麦咖啡")
new_user_word(mixseg, dic)
seg2 <- segment(da$校内其他产品, mixseg)
seg2=table(seg2)
seg2=sort(seg2,decreasing = T)
seg2
ncol(da)
shi="是"
for (i in 1:nrow(da)){
  if(grepl(shi,da$校内其他产品[i])==TRUE){da[i,21]="是"}
  else{da[i,21]="否"}
}
p11=ggplot(da,aes(x=V21,y=次数,fill=V21))+
  geom_boxplot()+
  scale_fill_manual(values = c('#999999', '#E69F00'));p11


#模型建立
colnames(da[21])="校内其他产品a"
d=da
ncol(d)
d=d[,-c(1,3,4,7,17,18,19)]
head(d)
colnames(d)[14]="校内其他产品"
library(MASS)
d=d[,-12]
shapiro.test(d$次数)
hist(d$次数,breaks=30,freq=F)
result=boxcox((d$次数)~1)
result=boxcox((d$次数+1)~1)
I=which(result$y==max(result$y))
result$x[I]
hist((d$次数+1)^result$x[I],breaks=10)
shapiro.test((d$次数+1)^result$x[I])

for(i in c(3:11)){
  d[,i]=as.numeric(d[,i])
}
#线性模型
y=(d$次数+1)^result$x[I]
lm1=lm(y~as.factor(性别)+as.factor(必要)+健康次数+口味+
         价格+包装+服务+便捷度+品种+宣传,data=d)
summary(lm1)
#二次项模型
lm1=lm((次数+1)^result$x[I]~as.factor(性别)+as.factor(必要)+健康次数+口味+价格+
         包装+服务+便捷度+品种+宣传+I(健康次数^2),data=d)
summary(lm1)
lm.aic=step(lm1,trace=F)
summary(lm.aic)
lm.bic=step(lm1,k=log(length(d[,1])),trace=F) 
summary(lm.bic)
par(mfrow=c(2,2)) 
plot(lm.aic,which=c(1:4))
library(car)
round(vif(lm.aic),2)



lm11=lm(log((次数+1)^result$x[I])~价格,data=d)
summary(lm11)
lm22=lm((次数+1)^result$x[I]~便捷度,data=d)
summary(lm11)
par(mfrow=c(2,2)) 
plot(lm1,which=c(1:4))
summary(lm1)
library(car)
round(vif(lm1),2)
lm.aic=step(lm1,trace=F)
summary(lm.aic)
lm.bic=step(lm1,k=log(length(d[,1])),trace=F) 
summary(lm.bic)
d2=d[-c(1,49,68),]
lm2=lm((次数+1)^result$x[I]~as.factor(性别)+as.factor(必要)+健康次数+口味+价格+
         包装+服务+便捷度+品种+宣传+I(健康次数^2),data=d3)
summary(lm2)
par(mfrow=c(2,2)) 
plot(lm2,which=c(1:4))
d3=d[-c(2,57,83),]
#分段回归
library(segmented)
x=d$价格
segmented.mod <- segmented(lm1, seg.Z = ~x, psi=9)
plot(segmented.mod, col='pink', lwd= 2.5 ,add=T)
summary(segmented.mod)

#预测

head(d)
dy=read.xlsx("C:/Users/wulinlin/Desktop/抽样调查期末报告/预测数据.xlsx")
head(dy)
ncol(dy)
y=predict(lm.aic,dy)
predict(lm.aic, newdata = dy, interval = "prediction")
pred.int <- predict(lm.aic, interval = "prediction")
mydata <- cbind(d, pred.int)
library("ggplot2")
p <- ggplot(mydata, aes(y=次数)) +
  geom_point() +
  stat_smooth(method = lm)
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")
