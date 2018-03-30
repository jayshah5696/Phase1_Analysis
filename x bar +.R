#Importing dataset


pca=prcomp(Project,scale = t)
pca$sdev
biplot(pca, scale=0)
pr.var=pca$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve


plot(pca$sdev, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')

library(qcc)
c=cusum(Project_data)
e=ewma(Project_data)
library(MSQC)

indices=c(1:1000)
data=cbind(indices,Project_data)
lim=c(0,49.1616)
lim
CHART=mqcc(data[,2:26], type = c("T2"), center=NULL, cov=NULL,
           limits = TRUE, pred.limits = FALSE,
           limits=lim, rules = shewhart.rules,
           plot = TRUE)

violation=CHART$violations$beyond.limits
new=data[-violation,]
CHART1=mqcc(new[,2:26], type = c("T2"), center=NULL, cov=NULL,
            limits = TRUE, pred.limits = FALSE,
            confidence.level = .9973, rules = shewhart.rules,
            plot = TRUE)

violation1=CHART1$violations$beyond.limits
new1=new[-violation1,]
CHART2=mqcc(new1[,2:26], type = c("T2"), center=NULL, cov=NULL,
            limits = TRUE, pred.limits = FALSE,
            confidence.level = .9973, rules = shewhart.rules,
            plot = TRUE)
violation2=CHART2$violations$beyond.limits
new2=new1[-violation2,]

CHART3=mqcc(new2[,2:26], type = c("T2"), center=NULL, cov=NULL,
            limits = TRUE, pred.limits = FALSE,
            confidence.level = .9973, rules = shewhart.rules,
            plot = TRUE)


violation3=CHART3$violations$beyond.limits
new3=new2[-violation3,]
CHART4=mqcc(new3[,2:26], type = c("T2"), center=NULL, cov=NULL,
            limits = TRUE, pred.limits = FALSE,
            confidence.level = .973, rules = shewhart.rules,
            plot = TRUE)
violation4=CHART4$violations$beyond.limits



#new4=new3[-violation4,]
#CHART5=mqcc(new4[,2:26], type = c("T2"), center=NULL, cov=NULL,
            limits = TRUE, pred.limits = FALSE,
            confidence.level = .973, rules = shewhart.rules,
            plot = TRUE)
#violation5=CHART5$violations$beyond.limits
#new5=new4[-violation5,]
#CHART6=mqcc(new5[,2:26], type = c("T2"), center=NULL, cov=NULL,
            limits = TRUE, pred.limits = FALSE,
            confidence.level = .973, rules = shewhart.rules,
            plot = TRUE)
#summary(CHART6)
#cova =CHART6$cov




inc=new1[,1]

doc=c(1:1000)
ooc=doc[-c(inc)]


ooc
inc_data=Project_data[-ooc,]
summary(inc_data)





mean(CHART2$means[c(inc,516),1])
CHART2$center


c=cusum(new5[,2:26])



#ooc root cause detection


#install.packages("qicharts")
library(qicharts)
y1=Project[,1]



for (i in 1:25){
  x_i=qcc(data=new5[ooc,i],type='xbar.one',sizes = 1,,confidence.level = 1-0.0027,newdata = Project_data[ooc,])
}


df=data.frame(Project[,1],doc)
for i in 1:25


x_1=qcc(data=new5[-ooc,2],type='xbar.one',sizes = 1,,confidence.level = 1-0.0027,newdata = Project_data[ooc,])

x21=qcc(data=Project_data[,21],type='xbar.one',sizes = 1,label.limits=c(-3,3))
