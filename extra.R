pca=prcomp(Project_data,scale = T)
pca$sdev
biplot(pca, scale=0)
pr.var=pca$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pca$sdev, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,2),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')



xbar_violation1
xbar_violation2
xbar_violation3
xbar_violation4
xbar_violation5
xbar_violation6
xbar_violation7
xbar_violation8
xbar_violation9
xbar_violation10
xbar_violation11
xbar_violation12
xbar_violation13
xbar_violation14
xbar_violation15
xbar_violation16
xbar_violation17
xbar_violation18
xbar_violation19
xbar_violation20
xbar_violation21
xbar_violation22
xbar_violation23
xbar_violation24
xbar_violation25



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


xbarv2=c( 223,
  93, 164, 188, 211, 215, 239, 345, 703,
  149, 164, 169, 171, 173, 174, 179, 181, 184, 196, 202, 208, 211, 217, 223, 233, 239, 265, 271, 279, 281, 284, 287, 289, 538,
  7)


xbarv3=c(  142, 157, 168, 175, 177, 188, 195, 196, 202, 205, 242, 254,
  7, 
  507,
  160)



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



cus8=mult.chart(type = "mcusum", x=new8[,-1], alpha = 0.0027, k = 2, h = 19.97, phase = 1, method = "sw")

CHART2=mqcc(new8[,-1], type = c("T2"), center=NULL, cov=NULL,
            limits = TRUE, pred.limits = FALSE,
            confidence.level = .9973, rules = shewhart.rules,
            plot = TRUE)
