
install.packages("HSAUR2")
install.packages("MVA")

library(tools)
library(HSAUR2)
library(MVA)

path="/path/"
fname="data.csv"
mydata=read.csv(paste(path, fname, sep=""))
rownames(mydata)=mydata[,1]
mydata1=mydata
mydata1[, c(1, 8)]=NULL
summary(mydata1)
sapply(mydata1, sd)

plotfile=paste(path, "boxplot.pdf", sep="")
pdf(file=plotfile)
boxplot(mydata1, range=0)

boxplot(scale(mydata1), range=0, ylimit=c(-5, 5))
graphics.off()

cor(mydata1)



plotfile=paste(path, "scatterplot.pdf", sep="")
pdf(file=plotfile)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

pairs(mydata1, upper.panel = panel.cor)
graphics.off()



plotfile=paste(path, "investigate normality.pdf", sep="")
pdf(file=plotfile)

layout(matrix(1:8, nc = 2))
sapply(colnames(mydata1), function(x) {
   qqnorm(mydata1[[x]], main = x)
   qqline(mydata1[[x]])})

mydata2 <- na.omit(mydata1)
dim(mydata2)
cm <- colMeans(mydata2)
S <- cov(mydata2, use="complete.obs")

d <- apply(mydata2, 1, function(mydata2) t(mydata2 - cm) %*% solve(S) %*% (mydata2 - cm))

plot(qc <- qchisq((1:nrow(mydata2) - 1/2) / nrow(mydata2), df = 6),sd <- sort(d),  
     xlab = expression(paste(chi[6]^2, " Quantile")), ylab = "Ordered distances")
oups <- which(rank(abs(qc - sd), ties = "random") > nrow(mydata2) - 3)
text(qc[oups], sd[oups] - 1.5, names(oups))
abline(a = 0, b = 1)
graphics.off()




plotfile=paste(path, "bivariate boxplot.pdf", sep="")
pdf(file=plotfile)
MeanTlab="Mean Ta (C)"
MeanDlab="Mean Td (C)"
outboundary=match(lab <- c("1/27/2007", "11/17/2007", "2/12/2012", "12/29/2014"), rownames(mydata2))
x <- mydata2[, c(1, 2)]
bvbox(x, mtitle = "", xlab = MeanTlab, ylab = MeanDlab)
text(x[outboundary, 2], x[outboundary, 2], labels = lab,cex = 0.7, pos = c(2, 2, 4, 2, 2))
graphics.off()




plotfile=paste(path, "scatterplot with seperation.pdf", sep="")
pdf(file=plotfile)
with(x, plot(mydata2[,1], mydata2[,2],pch=19,cex=0.5))
index <- which(mydata2$MeanHumidity > 50)
with(mydata2, points(mydata2[,1][index],mydata2[,2][index],col="red"))
sub=aggregate( . ~ mydata2$MeanHumidity > 50, mydata2, mean)
graphics.off()




