setwd("/Users/jones/GitHubRepos/bits-and-bobs/")
wt<-read.csv("Weight.csv")
head(wt)
wt$Start <- as.character(wt$Start)
wt$Finish <- as.character(wt$Finish)
wt$Start2 <- as.Date(wt$Start, "%d/%m/%y")

wt$kg <- wt$Weight..lb.* 0.45359237

plot(wt$Start2,wt$kg,type="b",ylim = c(72,80),
     xlim=c(min(wt$Start2),(7*14)+max(wt$Start2)),
     ylab="Weight (kg)",xlab = "Date")
points(wt$Start2,wt$kg,pch=16,col="darkgrey")
model <- lm(kg~as.numeric(wt$Start2),data=wt)

abline(model)
abline(v=as.Date("01/06/15","%d/%m/%y"))
startv<-min(as.numeric(wt$Start2))
abline(v=seq(startv,startv+(7*30),10),col="grey",lty=2)
summary(model)
abline(h=72:80,col="grey",lty=3)

4/as.numeric(7*model$coefficients[2])

