#Importing dataset


pca=prcomp(Project_data,scale = T)
pca$sdev
biplot(pca, scale=0)
pr.var=pca$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve


plot(pca$sdev, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,2),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')

library(qcc)
c=cusum(new1[,-1],decision.interval = 10)
summary(c)
C21=cusum(Project_data,decision.interval = 10,se.shift = 1)
summary(C21)
e=ewma(new1[,-1])

e=ewma(new5[,-1],lambda = .24,nsigmas = 3,plot=T)



summary(e)



library(MSQC)
cus=mult.chart(type = "mcusum", x=new1[,-1], alpha = 0.0027, h = 19.97, phase = 1, method = "sw")




summary(cus)


cus$t2

itr_points1=c(785:815,821:894)

new2=new1[-itr_points1,]

cus2=mult.chart(type = "mcusum", x=new2[,-1], alpha = 0.0027, k = 2, h = 19.97, phase = 1, method = "sw")
cus2$t2

itr_points2=c(761:805)
new3=new2[-itr_points2,]

cus3=mult.chart(type = "mcusum", x=new3[,-1], alpha = 0.0027, k = 2, h = 19.97, phase = 1, method = "sw")
cus3$t2


itr_points3=c(755:764)
new4=new3[-itr_points3,]

cus4=mult.chart(type = "mcusum", x=new4[,-1], alpha = 0.0027, k = 2, h = 19.97, phase = 1, method = "sw")
cus4$t2








t2=mult.chart(type = 't2',new1[,-1],phase = 1,alpha = 0.0027)
cus=mult.chart(type = "mewma", x=new1[,-1], alpha = 0.002, phase = 1, method = "sw",lambda = .25)







data("carbon1")
View(carbon1)
mult.chart(type='mewma',carbon1)


qcc





indices=c(1:1000)

data=cbind(indices,Project_data)

lim=limits.T2(ngroups=1000,size=1,nvars=25,conf=0.9973)



CHART=mqcc(data[,-1], type = c("T2"),
           pred.limits = FALSE,confidence.level = .9973,
          limits=TRUE, rules = shewhart.rules,
           plot = TRUE)

violation=CHART$violations$beyond.limits
new=data[-violation,]
CHART1=mqcc(new[,-1], type = c("T2"), center=NULL, cov=NULL,
           limits = TRUE, pred.limits = FALSE,
           confidence.level = .9973, rules = shewhart.rules,
           plot = TRUE)
CHART$center
CHART1$center
CHART$cov

violation1=CHART1$violations$beyond.limits
new1=new[-violation1,]
CHART2=mqcc(new1[,-1], type = c("T2"), center=NULL, cov=NULL,
            limits = TRUE, pred.limits = FALSE,
            confidence.level = .9973, rules = shewhart.rules,
            plot = TRUE)

CHART2$center









violation2=CHART2$violations$beyond.limits
new2=new1[-violation2,]

CHART3=mqcc(new2, type = c("T2"), center=NULL, cov=NULL,
            limits = TRUE, pred.limits = FALSE,
            confidence.level = .95, rules = shewhart.rules,
            plot = TRUE)


violation3=CHART3$violations$beyond.limits
new3=new2[-violation3,]
CHART4=mqcc(new3, type = c("T2"), center=NULL, cov=NULL,
            limits = TRUE, pred.limits = FALSE,
            confidence.level = .95, rules = shewhart.rules,
            plot = TRUE)
violation4=CHART4$violations$beyond.limits
new4=new3[-violation4,]
CHART5=mqcc(new4, type = c("T2"), center=NULL, cov=NULL,
            limits = TRUE, pred.limits = FALSE,
            confidence.level = .95, rules = shewhart.rules,
            plot = TRUE)
violation5=CHART5$violations$beyond.limits
new5=new4[-violation5,]
CHART6=mqcc(new5, type = c("T2"), center=NULL, cov=NULL,
            limits = TRUE, pred.limits = FALSE,
            confidence.level = .95, rules = shewhart.rules,
            plot = TRUE)
summary(CHART6)
cova =CHART6$cov




inc=new1[,1]

doc=c(1:1000)
ooc=doc[-c(inc)]

1/0.0027
ooc




z=cov(new5[,-1])


z1=as.data.frame(z)
mean(z1)
write.csv(z1,'covariance.csv')



#ooc root cause detection


#install.packages("qicharts")
library(qicharts)
y1=Project[,1]

df=data.frame(Project[,1],doc)
x1=qcc(data=Project,type='xbar',sizes = 1)
