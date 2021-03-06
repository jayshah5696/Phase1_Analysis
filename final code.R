
library(readxl)
Project_data <- read_excel("Project_data.xlsx", 
                           col_names = FALSE)
#View(Project_data)
#PCA
pca=prcomp(Project_data,scale = T)
z=pca$x

write.csv(z,'pcadata.csv')

#Pca before xbar
pca1=prcomp(new5[,-1],scale = T)
z1=pca1$x
z1=cbind(new5[,1],z1)
write.csv(z1,'pcadata1.csv')




biplot(pca, scale=0)
pr.var=pca$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
pareto.chart(pve)
pve2=ordered(pve*100)
plot(pca$sdev, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1.3),type='b')
hist(as.numeric(pve2[2]), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained")
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')





#T2 Chart
library(qcc)
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


violation1=CHART1$violations$beyond.limits
new1=new[-violation1,]
CHART2=mqcc(new1[,-1], type = c("T2"), center=NULL, cov=NULL,
            limits = TRUE, pred.limits = FALSE,
            confidence.level = .9973, rules = shewhart.rules,
            plot = TRUE)

CHART2$center


#Mcusum CHart
library(MSQC)
cus=mult.chart(type = "mcusum", x=new1[,-1], alpha = 0.0027, k = 2, h = 19.97, phase = 1, method = "sw")
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




#checking in t2 chart

Chart3=mqcc(new4[,-1], type = c("T2"), center=NULL, cov=NULL,
                   limits = TRUE, pred.limits = FALSE,
                   confidence.level = .9973, rules = shewhart.rules,
                   plot = TRUE)
violation2=Chart3$violations$beyond.limits
new5=new4[-violation2,]

Chart4=mqcc(new5[,-1], type = c("T2"), center=NULL, cov=NULL,
            limits = TRUE, pred.limits = FALSE,
            confidence.level = .9973, rules = shewhart.rules,
            plot = TRUE)

#checking in cusum chart
cus5=mult.chart(type = "mcusum", x=new5[,-1], alpha = 0.0027, k = 2, h = 19.97, phase = 1, method = "sw")
cus5$t2



write.csv(new5[,-1],"new5.csv")







#checking individual xbar chart

x1=qcc(data=new5[,2],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation1=x1$violations$beyond.limits


x2=qcc(data=new5[,3],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation2=x2$violations$beyond.limits

x3=qcc(data=new5[,4],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation3=x3$violations$beyond.limits


x4=qcc(data=new5[,5],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation4=x4$violations$beyond.limits


x5=qcc(data=new5[,6],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation5=x5$violations$beyond.limits


x6=qcc(data=new5[,7],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation6=x6$violations$beyond.limits


x7=qcc(data=new5[,8],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation7=x7$violations$beyond.limits

x8=qcc(data=new5[,9],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation8=x8$violations$beyond.limits

x9=qcc(data=new5[,10],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation9=x9$violations$beyond.limits

x10=qcc(data=new5[,11],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation10=x10$violations$beyond.limits

x11=qcc(data=new5[,12],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation11=x11$violations$beyond.limits

x12=qcc(data=new5[,13],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation12=x12$violations$beyond.limits

x13=qcc(data=new5[,14],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation13=x13$violations$beyond.limits

x14=qcc(data=new5[,15],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation14=x14$violations$beyond.limits

x15=qcc(data=new5[,16],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation15=x15$violations$beyond.limits

x16=qcc(data=new5[,17],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation16=x16$violations$beyond.limits

x17=qcc(data=new5[,18],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation17=x17$violations$beyond.limits

x18=qcc(data=new5[,19],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation18=x18$violations$beyond.limits

x19=qcc(data=new5[,20],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation19=x19$violations$beyond.limits

x20=qcc(data=new5[,21],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation20=x20$violations$beyond.limits

x21=qcc(data=new5[,22],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation21=x21$violations$beyond.limits

x22=qcc(data=new5[,23],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation22=x22$violations$beyond.limits

x23=qcc(data=new5[,24],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation23=x23$violations$beyond.limits

x24=qcc(data=new5[,25],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation24=x24$violations$beyond.limits

x25=qcc(data=new5[,26],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation25=x25$violations$beyond.limits



#ooc
xbarv=c(68, 739, 753,
        542, 206,
        397, 287,
        43, 242, 642,
        170, 196, 311, 328, 451, 768, 779, 	
        160, 167, 170, 174, 175, 179, 183, 188, 191, 193 ,200, 203, 208, 210, 214, 216, 219, 231, 246, 251, 256, 260, 281, 282 ,285,
        287, 295, 297, 307, 311, 324, 329, 330, 332, 334, 335, 339, 341, 347, 112, 122, 367, 440 ,502, 707, 724,
        497, 746, 294, 333, 567,
        202, 315, 328, 	
        134, 434,
        660,
        102 ,545,
        646,
        331, 439, 530, 706,
        186,  28, 633,
        623,
        652,
        703,
        306, 622, 748, 751,
        433,  44,
        426, 510, 278,
        123, 711)




new6=new5[-xbarv,]








x1=qcc(data=new6[,2],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation1=x1$violations$beyond.limits


x2=qcc(data=new6[,3],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation2=x2$violations$beyond.limits

x3=qcc(data=new6[,4],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation3=x3$violations$beyond.limits


x4=qcc(data=new6[,5],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation4=x4$violations$beyond.limits


x5=qcc(data=new6[,6],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation5=x5$violations$beyond.limits


x6=qcc(data=new6[,7],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation6=x6$violations$beyond.limits


x7=qcc(data=new6[,8],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation7=x7$violations$beyond.limits

x8=qcc(data=new6[,9],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation8=x8$violations$beyond.limits

x9=qcc(data=new6[,10],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation9=x9$violations$beyond.limits

x10=qcc(data=new6[,11],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation10=x10$violations$beyond.limits

x11=qcc(data=new6[,12],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation11=x11$violations$beyond.limits

x12=qcc(data=new6[,13],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation12=x12$violations$beyond.limits

x13=qcc(data=new6[,14],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation13=x13$violations$beyond.limits

x14=qcc(data=new6[,15],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation14=x14$violations$beyond.limits

x15=qcc(data=new6[,16],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation15=x15$violations$beyond.limits

x16=qcc(data=new6[,17],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation16=x16$violations$beyond.limits

x17=qcc(data=new6[,18],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation17=x17$violations$beyond.limits

x18=qcc(data=new6[,19],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation18=x18$violations$beyond.limits

x19=qcc(data=new6[,20],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation19=x19$violations$beyond.limits

x20=qcc(data=new6[,21],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation20=x20$violations$beyond.limits

x21=qcc(data=new6[,22],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation21=x21$violations$beyond.limits

x22=qcc(data=new6[,23],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation22=x22$violations$beyond.limits

x23=qcc(data=new6[,24],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation23=x23$violations$beyond.limits

x24=qcc(data=new6[,25],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation24=x24$violations$beyond.limits

x25=qcc(data=new6[,26],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation25=x25$violations$beyond.limits




xbarv2=c( 223,
          93, 164, 188, 211, 215, 239, 345, 703,
          149, 164, 169, 171, 173, 174, 179, 181, 184, 196, 202, 208, 211, 217, 223, 233, 239, 265, 271, 279, 281, 284, 287, 289, 538,
          7)

new7=new6[-xbarv2,]


x1=qcc(data=new7[,2],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation1=x1$violations$beyond.limits


x2=qcc(data=new7[,3],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation2=x2$violations$beyond.limits

x3=qcc(data=new7[,4],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation3=x3$violations$beyond.limits


x4=qcc(data=new7[,5],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation4=x4$violations$beyond.limits


x5=qcc(data=new7[,6],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation5=x5$violations$beyond.limits


x6=qcc(data=new7[,7],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation6=x6$violations$beyond.limits


x7=qcc(data=new7[,8],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation7=x7$violations$beyond.limits

x8=qcc(data=new7[,9],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation8=x8$violations$beyond.limits

x9=qcc(data=new7[,10],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation9=x9$violations$beyond.limits

x10=qcc(data=new7[,11],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation10=x10$violations$beyond.limits

x11=qcc(data=new7[,12],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation11=x11$violations$beyond.limits

x12=qcc(data=new7[,13],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation12=x12$violations$beyond.limits

x13=qcc(data=new7[,14],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation13=x13$violations$beyond.limits

x14=qcc(data=new7[,15],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation14=x14$violations$beyond.limits

x15=qcc(data=new7[,16],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation15=x15$violations$beyond.limits

x16=qcc(data=new7[,17],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation16=x16$violations$beyond.limits

x17=qcc(data=new7[,18],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation17=x17$violations$beyond.limits

x18=qcc(data=new7[,19],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation18=x18$violations$beyond.limits

x19=qcc(data=new7[,20],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation19=x19$violations$beyond.limits

x20=qcc(data=new7[,21],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation20=x20$violations$beyond.limits

x21=qcc(data=new7[,22],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation21=x21$violations$beyond.limits

x22=qcc(data=new7[,23],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation22=x22$violations$beyond.limits

x23=qcc(data=new7[,24],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation23=x23$violations$beyond.limits

x24=qcc(data=new7[,25],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation24=x24$violations$beyond.limits

x25=qcc(data=new7[,26],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation25=x25$violations$beyond.limits


xbarv3=c(  142, 157, 168, 175, 177, 188, 195, 196, 202, 205, 242, 254,
           7, 
           507,
           160)

new8=new7[-xbarv3,]



x1=qcc(data=new8[,2],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation1=x1$violations$beyond.limits


x2=qcc(data=new8[,3],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation2=x2$violations$beyond.limits

x3=qcc(data=new8[,4],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation3=x3$violations$beyond.limits


x4=qcc(data=new8[,5],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation4=x4$violations$beyond.limits


x5=qcc(data=new8[,6],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation5=x5$violations$beyond.limits


x6=qcc(data=new8[,7],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation6=x6$violations$beyond.limits


x7=qcc(data=new8[,8],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation7=x7$violations$beyond.limits

x8=qcc(data=new8[,9],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation8=x8$violations$beyond.limits

x9=qcc(data=new8[,10],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation9=x9$violations$beyond.limits

x10=qcc(data=new8[,11],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation10=x10$violations$beyond.limits

x11=qcc(data=new8[,12],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation11=x11$violations$beyond.limits

x12=qcc(data=new8[,13],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation12=x12$violations$beyond.limits

x13=qcc(data=new8[,14],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation13=x13$violations$beyond.limits

x14=qcc(data=new8[,15],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation14=x14$violations$beyond.limits

x15=qcc(data=new8[,16],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation15=x15$violations$beyond.limits

x16=qcc(data=new8[,17],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation16=x16$violations$beyond.limits

x17=qcc(data=new8[,18],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation17=x17$violations$beyond.limits

x18=qcc(data=new8[,19],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation18=x18$violations$beyond.limits

x19=qcc(data=new8[,20],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation19=x19$violations$beyond.limits

x20=qcc(data=new8[,21],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation20=x20$violations$beyond.limits

x21=qcc(data=new8[,22],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation21=x21$violations$beyond.limits

x22=qcc(data=new8[,23],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation22=x22$violations$beyond.limits

x23=qcc(data=new8[,24],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation23=x23$violations$beyond.limits

x24=qcc(data=new8[,25],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation24=x24$violations$beyond.limits

x25=qcc(data=new8[,26],type='xbar.one',sizes = 1,std.dev = "MR")
xbar_violation25=x25$violations$beyond.limits





z=cov(new8[,-1])

