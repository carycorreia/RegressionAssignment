# mtcars Regression Analysis
#
# Load and explore database mtcars
data(mtcars)
mtcars
#
# Draw pairwise graphs
pairs(mtcars)

# Convert all discrete numerics into factors
mtcars$am<-factor(mtcars$am)
mtcars$gear<-factor(mtcars$gear)
mtcars$carb<-factor(mtcars$carb)
mtcars$vs<-factor(mtcars$vs)
mtcars$cyl<-factor(mtcars$cyl)

#2 Initial analysis for am variable
plot(mpg~am, mtcars)
# add in t-test

#3 explore am with other variables
step.fit<-step(lm(mpg ~ ., mtcars), trace=0, steps=20000)
summary(step.fit)
par(mfrow=c(2,2));plot(step.fit)
# Rsq 0.8659 RsqAdj 0.8401
# model = cyl, hp, wt and am: note am1 not significant nor cyl 18

step.fit.wI<-lm(mpg ~ wt:am + cyl:hp+ +am:hp + cyl:wt + am + wt + cyl +hp, mtcars)
summary(step.fit.wI)
par(mfrow=c(2,2));plot(step.fit.wI)
# Rsq 0.9074, RsqAdj 0.8564
# significant factors: wt:am1, cyl:hp --> plus most other variables not significant

best.fit.wI<-lm(mpg ~ wt:am + cyl:hp +  am + wt + cyl +hp, mtcars)
summary(best.fit.wI)
par(mfrow=c(2,2));plot(best.fit.wI)
plot(predict(best.fit.wI), resid(best.fit.wI), pch=19)
# Rsq 0.9036, RsqAdj 0.8701
# significant factors: am1, wt, wt:am

# basic plots

# mpg ~ wt + am--> running two separate regressions:
regr1<- lm(mpg[am==0] ~ wt[am==0], mtcars)
regr2<- lm(mpg[am==1] ~ wt[am==1], mtcars)
plot(mpg ~ wt, pch=19, main="MPG vs Weight and Transmission", mtcars)
  points(mpg ~ wt, pch=19, data= mtcars, col=((mtcars$am=="0")*1+1))
  abline(regr1[1], regr1[2], col="red", lwd=3, lty=1)
  abline(regr2[1], regr2[2], col="black", lwd=3,lty=1)
  temp<-legend("topright", legend=c("Automatic","Manual "), 
                text.width=strwidth("Automatic"),
                lty =c(1,1), col=c("red", "black"), xjust=1, yjust=1,
                title=("Transmission Types"))

# mpg ~ wt + am--> running just one regression and two slopes
lmAM<- lm(mpg ~ wt + am, mtcars)
plot(mpg ~ wt, pch=19, main="MPG vs Weight and Transmission", mtcars)
points(mpg ~ wt, pch=19, data= mtcars, col=((mtcars$am=="0")*1+1))
abline(c(lmAM$coeff[1], lmAM$coeff[2]), col="red", lwd=3, lty=1)
abline(c(lmAM$coeff[1]+lmAM$coeff[3], lmAM$coeff[2]), col="black", lwd=3, lty=1)
temp<-legend("topright", legend=c("Automatic","Manual "), 
             text.width=strwidth("Automatic"),
             lty =c(1,1), col=c("red", "black"), xjust=1, yjust=1,
             title=("Transmission Types"))

# mpg ~ wt + am + am:wt--> running just one regression and two slopes
lmAM<- lm(mpg ~ wt + am + am:wt, mtcars)
plot(mpg ~ wt, pch=19, main="MPG vs Weight and Transmission with Interaction", mtcars)
points(mpg ~ wt, pch=19, data= mtcars, col=((mtcars$am=="0")*1+1))
abline(c(lmAM$coeff[1], lmAM$coeff[2]), col="red", lwd=3, lty=1)
abline(c(lmAM$coeff[1]+lmAM$coeff[3], lmAM$coeff[4]), col="black", lwd=3, lty=1)
temp<-legend("topright", legend=c("Automatic","Manual "), 
             text.width=strwidth("Automatic"),
             lty =c(1,1), col=c("red", "black"), xjust=1, yjust=1,
             title=("Transmission Types"))

# Interaction plots
with(mtcars, {
              cyl <- ordered(cyl)
              interaction.plot(cyl, am, mpg, fixed = TRUE, col = 2:3, leg.bty = "o")
              interaction.plot(carb, am, mpg, fixed = TRUE, col = 2:3, leg.bty = "o")
              interaction.plot(vs, am, mpg, fixed = TRUE, col = 2:3, leg.bty = "o")
              interaction.plot(gear, cyl, mpg, fixed = TRUE, col = 2:3, leg.bty = "o")
              interaction.plot(vs, cyl, mpg, fixed = TRUE, col = 2:3, leg.bty = "o")
})



