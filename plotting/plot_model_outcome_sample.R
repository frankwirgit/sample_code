# An example for plotting model outcome using matplot

m1_los <- m_los
n1_los <- n_los
h1_los <- h_los
s1_los <- s_los
ch1_los <- ch_los
cs1_los <- cs_los


m2_los <- aggregate(ewmp[,c(2:7)], by=list(as.numeric(as.character(ewmp$x))), mean)
colnames(m2_los)[1]="x"

n2_los <- aggregate(ewmp[,c(2:7)], by=list(ewmp$u), mean)
colnames(n2_los)[1]="active_feature"

h2_los <- aggregate(ewmp[,c(6)], by=list(as.numeric(as.character(ewmp$x)), ewmp$u), mean)
colnames(h2_los) = c("x","active_feature","v")
#tv3 <- subset(h2_los, h2_los$x==3)

s2_los <- aggregate(ewmp[,c(6)], by=list(ewmp$u, as.numeric(as.character(ewmp$x))), mean)
colnames(s2_los) = c("active_feature","x","v")



ch2_los <- cast(h2_los, active_feature ~ x, mean, value="v")
colnames(ch2_los)[2:10]=c("s2","s3","s4","s5","s6","s7","s8","s9","all")

cs2_los <- cast(s2_los, x ~ active_feature, mean, value="v")
colnames(cs2_los)[2:11]=c("good","b0","b1","b2","b3","b4","b5","b6","b7","b8")



par(mfrow=c(1,2))
matplot(ch2_los$active_feature, cbind(ch2_los[,c(2:10)]), type="l", 
xlab="Active features", ylab="Predicted P",
axes=F, col=c(1:9), pch=19, 
main="Predicted probability - \nStep2->Step10 (with colour: gray->black))")
axis(2)
axis(1,at=seq(-1,8,by=1))
abline(h=0)

matplot(cs2_los$x, cbind(cs2_los[,c(2:11)]), type="l", 
xlab="Questioning Step", ylab="Predicted P",
axes=F, col=c(1:9), pch=19, 
main="Predicted probability - \nFeature good->worst (with colour: gray->black))")
axis(2)
axis(1,at=seq(2,10,by=1))
abline(h=0)



par(mfrow=c(1,2))
matplot(m1_los$x, cbind(m1_los$v, m2_los$v), type="l", 
xlab="Questioning Step", ylab="Predicted P",
axes=F, col=c(1:2), pch=19, 
main="Predicted probability across steps \nYounger+same BMI (gray) vs. Older+same BMI (red)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(m1_los$x, cbind(m1_los$q, m2_los$q), type="l", 
xlab="Questioning Step", ylab="Upper Quartiles %",
axes=F, col=c(1:2), pch=19, 
main="Upper Quartiles upon predictions \nYounger+same BMI (gray) vs. Older+same BMI (red)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

par(mfrow=c(1,2))
matplot(m1_los$x, cbind(m1_los$v, m2_los$v), type="l", 
xlab="Questioning Step", ylab="Predicted P",
axes=F, col=c(1:2), pch=19, 
main="Predicted probability across steps \nOlder+same BMI (gray) vs. Older+same BMI (red)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(m1_los$x, cbind(m1_los$q, m2_los$q), type="l", 
xlab="Questioning Step", ylab="Upper Quartiles %",
axes=F, col=c(1:2), pch=19, 
main="Upper Quartiles upon predictions \nOlder+same BMI (gray) vs. Older+same BMI (red)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)


par(mfrow=c(1,2))
matplot(m1_los$x, cbind(m1_los$v, m2_los$v), type="l", 
xlab="Questioning Step", ylab="Predicted P",
axes=F, col=c(1:2), pch=19, 
main="Predicted probability across steps \nMale (gray) vs. Female BMI (red)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(m1_los$x, cbind(m1_los$q, m2_los$q), type="l", 
xlab="Questioning Step", ylab="Upper Quartiles %",
axes=F, col=c(1:2), pch=19, 
main="Upper Quartiles upon predictions \nMale (gray) vs. Female BMI (red)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)


par(mfrow=c(1,2))
matplot(m1_los$x, cbind(m1_los$v, m2_los$v), type="l", 
xlab="Questioning Step", ylab="Predicted P",
axes=F, col=c(1:2), pch=19, 
main="Predicted probability across steps \nYounger+Lower BMI (gray) vs. Younger+Higher BMI (red)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(m1_los$x, cbind(m1_los$q, m2_los$q), type="l", 
xlab="Questioning Step", ylab="Upper Quartiles %",
axes=F, col=c(1:2), pch=19, 
main="Upper Quartiles upon predictions \nYounger+Lower BMI (gray) vs. Younger+Higher BMI (red)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

par(mfrow=c(1,2))
matplot(m1_los$x, cbind(m1_los$v, m2_los$v), type="l", 
xlab="Questioning Step", ylab="Predicted P",
axes=F, col=c(1:2), pch=19, 
main="Predicted probability across steps \nOlder+Lower BMI (gray) vs. Younger+Higher BMI (red)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(m1_los$x, cbind(m1_los$q, m2_los$q), type="l", 
xlab="Questioning Step", ylab="Upper Quartiles %",
axes=F, col=c(1:2), pch=19, 
main="Upper Quartiles upon predictions \nOlder+Lower BMI (gray) vs. Younger+Higher BMI (red)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)


par(mfrow=c(1,2))
matplot(m1_los$x, cbind(m1_los$v, m2_los$v), type="l", 
xlab="Questioning Step", ylab="Predicted P",
axes=F, col=c(1:2), pch=19, 
main="Predicted probability across steps \nOlder+Lower BMI (gray) vs. Older+Higher BMI (red)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(m1_los$x, cbind(m1_los$q, m2_los$q), type="l", 
xlab="Questioning Step", ylab="Upper Quartiles %",
axes=F, col=c(1:2), pch=19, 
main="Upper Quartiles upon predictions \nOlder+Lower BMI (gray) vs. Older+Higher BMI (red)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

par(mfrow=c(1,2))
matplot(m1_los$x, cbind(m1_los$v, m2_los$v), type="l", 
xlab="Questioning Step", ylab="Predicted P",
axes=F, col=c(1:2), pch=19, 
main="Predicted probability across steps \nYounger+Higher BMI (gray) vs. Older+Higher BMI (red)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)

matplot(m1_los$x, cbind(m1_los$q, m2_los$q), type="l", 
xlab="Questioning Step", ylab="Upper Quartiles %",
axes=F, col=c(1:2), pch=19, 
main="Upper Quartiles upon predictions \nYounger+Higher BMI (gray) vs. Older+Higher BMI (red)")
axis(2)
axis(1,at=seq(1,10,by=1))
abline(h=0)



matplot(ch1_los$active_feature, cbind(ch1_los[,c(6)],ch2_los[,c(6)]), type="l", 
xlab="Active features", ylab="Predicted P",
axes=F, col=c(1:2), pch=19, 
main="Predicted probability step6 \nOlder+Lower BMI (gray) vs. Younger+Higher BMI (red)")
axis(2)
axis(1,at=seq(-1,8,by=1))
abline(h=0)

matplot(ch1_los$active_feature, cbind(ch1_los[,c(10)],ch2_los[,c(10)]), type="l", 
xlab="Active features", ylab="Predicted P",
axes=F, col=c(1:2), pch=19, 
main="Predicted probability step10 \nOlder+Lower BMI (gray) vs. Younger+Higher BMI (red)")
axis(2)
axis(1,at=seq(-1,8,by=1))
abline(h=0)


matplot(cs1_los$x, cbind(cs1_los[,c(5)],cs2_los[,c(5)]), type="l", 
xlab="Questioning Step", ylab="Predicted P",
axes=F, col=c(1:2), pch=19, 
main="Predicted probability with 3 active feature \nOlder+Lower BMI (gray) vs. Younger+Higher BMI (red)")
axis(2)
axis(1,at=seq(2,10,by=1))
abline(h=0)

matplot(cs1_los$x, cbind(cs1_los[,c(2)],cs2_los[,c(2)]), type="l", 
xlab="Questioning Step", ylab="Predicted P",
axes=F, col=c(1:2), pch=19, 
main="Predicted probability with antibiotics taken & no active comorbidities \nOlder+Lower BMI (gray) vs. Younger+Higher BMI (red)")
axis(2)
axis(1,at=seq(2,10,by=1))
abline(h=0)
