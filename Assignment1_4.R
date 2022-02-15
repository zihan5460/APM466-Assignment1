


#dirty price = accrued interest + clean price, where accrued interest = (#of days since the last coupon payment/365)*annual coupon rate
library(readxl)
rm(list=ls())
#read dirty data
dirty_data=read_excel("C:/Users/zhao/Desktop/APM466_2022/Assignment1_dirtyprice_zzh.xlsx")
dirty_data



###4(a)

#I would like to make a new matrix, and the items in this matrix indicate #of years to maturity.
years_to_maturity<- data.frame(matrix(, nrow = 10, ncol = 10))
for(i in 1:5){
  years_to_maturity[,i] <- as.numeric( as.Date( dirty_data$`MATURITY DATE`) - as.Date( as.character(paste0("2022/01/",i+9)), format = "%Y/%m/%d"))/365
}
for(i in 6:10){
  years_to_maturity[,i] <- as.numeric( as.Date( dirty_data$`MATURITY DATE`) - as.Date( as.character(paste0("2022/01/",i+11)), format = "%Y/%m/%d"))/365
}
years_to_maturity

#create a new matrix, and the items in this matrix indicate raw yield to maturity.
yield_to_maturity <- data.frame(matrix(, nrow = 10, ncol = 10))
par<-100
#Dirty price=p_1*exponential(-r(t_1)*t_1)+p_2*exponential(-r(t_2)*t_2)+...
#t_1 is the maturity less than 6 months, ytm=-log(P/N)/time_to_maturity
#the first bond CAN 0.5 Feb 22is less than 6 months to maturity, coupon rate is 0.005
coupon_bond1<-par*0.005/2
for (i in 1:10){
  yield_to_maturity[1,i] <- -log(dirty_data[1,i+7]/(par+coupon_bond1))/(years_to_maturity[1,i])
}
#I would like to use Newton's Method to calculate rest yield to maturity.
#P=sigma(p_i*exp(-r*t_i))
for(i in 8:17){
  for(j in 2:10){
    coupon <- dirty_data$`COUPON RATE`[j]*par/2 
    f <- function(r){
      sum_1 <- 0 
      m  <- 1
      while (years_to_maturity[j,i-7]-0.5*m > 0) {
        sum_1 <- sum_1 - exp(-r*years_to_maturity[j, i-7]) 
        m <- m+1
      }
      z = dirty_data[j,i] + coupon*sum_1 - (par+coupon)*exp(-r*years_to_maturity[j, i-7])
    }
    df <- function(r){
      sum_2 <- 0
      t <- 1
      while (years_to_maturity[j,i-7]-0.5*t > 0) {
        sum_2 <- sum_2 + years_to_maturity[j, i-7]*exp(-r*years_to_maturity[j, i-7])
        t <- t+1
      }
      z = coupon*sum_2 + (par+coupon)*years_to_maturity[j, i-7]*exp(-r*years_to_maturity[j, i-7])
    }
    x <- 0.05 #take some small value of x
    n <- 1
    while(n<10){
      x <- x - f(x)/df(x)
      n <- n+1
    }
    yield_to_maturity[j, i-7] <- x
  }
}

#interpolate the yield to maturity
ytm_final <- data.frame(matrix(, nrow = 10, ncol = 10))
for (i in c(1:10))
{
  for (j in c(1:10))
  {
    ytm_final[j,i]=approx(dirty_data$`month until maturity`,yield_to_maturity[[i]],xout=6*j)$y
  }
}
ytm_final

#plot the yield to maturity curve
plot(seq(0, 4.5, by = 0.5), ytm_final$X1*100, type = "l", ylim = c(0,2.5), xlab="Maturity (in years)", ylab="Yield To Maturity(%)", col="black", main="4(a) 5-Year Yield to Maturity Curve")
lines(seq(0, 4.5, by = 0.5), ytm_final$X2*100, col="blue")
lines(seq(0, 4.5, by = 0.5), ytm_final$X3*100, col="purple")
lines(seq(0, 4.5, by = 0.5), ytm_final$X4*100, col="green")
lines(seq(0, 4.5, by = 0.5), ytm_final$X5*100, col="yellow")
lines(seq(0, 4.5, by = 0.5), ytm_final$X6*100, col="grey")
lines(seq(0, 4.5, by = 0.5), ytm_final$X7*100, col="brown")
lines(seq(0, 4.5, by = 0.5), ytm_final$X8*100, col="orange")
lines(seq(0, 4.5, by = 0.5), ytm_final$X9*100, col="red")
lines(seq(0, 4.5, by = 0.5), ytm_final$X10*100, col="light pink")
legend("topleft", cex=0.5, legend=c("Jan 10th", "Jan 11th", "Jan 12th", "Jan 13th", "Jan 14th", "Jan 17th", "Jan 18th", "Jan 19th", "Jan 20th", "Jan 21th"), col=c("black", "blue",  "purple", "green", "yellow", "grey", "brown", "orange", "red", "light pink"), lty=c(1,1), lwd=c(2,2))





#4b
#create a new matrix, and the items in this matrix indicate raw spot rate.
spot_rate <- data.frame(matrix(, nrow = 10, ncol = 10))
##the first bond CAN 0.5 Feb 22is less than 6 months to maturity
#2*[(dirty price/(par value + 0.5*coupon)^(-1/(2*years to maturity))-1]
#coupon rate of this bond is dirty_data[1,3]
coupon_bond1<-par*dirty_data[1,3]/2
for (i in 1:10){
  spot_rate[1,i]=2*((dirty_data[1,7+i]/(coupon_bond1+par))^(-1/(2*years_to_maturity[1,i]))-1)
}
for (i in c(2:10)) {
  for (j in c(1:10))  {
    dirtyprice <- dirty_data[i,7+j]
    coupon <- dirty_data$`COUPON RATE`[i]*par/2 
    sum3=0 #initialize the present value of coupons
    coupon_times = seq((6-4)/12,(dirty_data$`month until maturity`[i]-1)/12,1/2)
    for (k in c(1:length(coupon_times)))
    {
      sum3=sum3+coupon*(1+ytm_final[k,j]/2)^(-2*coupon_times[k])
    }
    spot_rate[i,j]=2*(((dirtyprice-sum3)/(coupon+par))^(-1/(2*years_to_maturity[i,j]))-1)
    sum3=0 
  }
}
spot_rate
#interpolate the spot curve
spot_final <- data.frame(matrix(, nrow = 10, ncol = 10))
for (i in c(1:10))
{
  for (j in c(1:10))
  {
    spot_final[j,i]=approx(dirty_data$`month until maturity`,spot_rate[[i]],xout=6*j)$y
  }
}
spot_final

#plot the spot curve
plot(seq(0, 4.5, by = 0.5), spot_final$X1*100, type = "l", ylim = c(0,3), xlab="Maturity (in years)", ylab="spot rate(%)", col="black", main="4(b) Spot Rate Curve")
lines(seq(0, 4.5, by = 0.5), spot_final$X2*100, col="blue")
lines(seq(0, 4.5, by = 0.5), spot_final$X3*100, col="purple")
lines(seq(0, 4.5, by = 0.5), spot_final$X4*100, col="green")
lines(seq(0, 4.5, by = 0.5), spot_final$X5*100, col="yellow")
lines(seq(0, 4.5, by = 0.5), spot_final$X6*100, col="grey")
lines(seq(0, 4.5, by = 0.5), spot_final$X7*100, col="brown")
lines(seq(0, 4.5, by = 0.5), spot_final$X8*100, col="orange")
lines(seq(0, 4.5, by = 0.5), spot_final$X9*100, col="red")
lines(seq(0, 4.5, by = 0.5), spot_final$X10*100, col="light pink")
legend("topright", cex=0.5, legend=c("Jan 10th", "Jan 11th", "Jan 12th", "Jan 13th", "Jan 14th", "Jan 17th", "Jan 18th", "Jan 19th", "Jan 20th", "Jan 21th"), col=c("black", "blue",  "purple", "green", "yellow", "grey", "brown", "orange", "red", "light pink"), lty=c(1,1), lwd=c(2,2))


##4c
forward_rate <- data.frame(matrix(, nrow = 4, ncol = 10))
for (j in c(1:4)) {
  for (i in c(1:10))  {
    x1=(1 + spot_final[2*j, i] / 2 )^ ( 2*j )
    y1=(1+spot_final[2+2*j,i]/2)^(2 + 2*j )
    forward_rate[j,i]=2*((y1/x1)^(1/2)-1)
  }
}
forward_rate
#plot the forward curve
plot(seq(1,4, by=1), forward_rate$X1*100, type = "l", ylim = c(1,2.5), xlab="Time to Maturity (in years)", ylab="spot rate(%)", col="black", main="4(c) Forward Curve")
lines(seq(1,4, by = 1), forward_rate$X2*100, col="blue")
lines(seq(1,4, by = 1), forward_rate$X3*100, col="purple")
lines(seq(1,4, by = 1), forward_rate$X4*100, col="green")
lines(seq(1,4, by = 1), forward_rate$X5*100, col="yellow")
lines(seq(1,4, by = 1), forward_rate$X6*100, col="grey")
lines(seq(1,4, by = 1), forward_rate$X7*100, col="brown")
lines(seq(1,4, by = 1), forward_rate$X8*100, col="orange")
lines(seq(1,4, by = 1), forward_rate$X9*100, col="red")
lines(seq(1,4, by = 1), forward_rate$X10*100, col="light pink")
legend("bottomright", cex=0.5, legend=c("Jan 10th", "Jan 11th", "Jan 12th", "Jan 13th", "Jan 14th", "Jan 17th", "Jan 18th", "Jan 19th", "Jan 20th", "Jan 21th"), col=c("black", "blue",  "purple", "green", "yellow", "grey", "brown", "orange", "red", "light pink"), lty=c(1,1), lwd=c(2,2))




#### Covariance matrix for log-return of yields
x1 = t(log(ytm_final[1:5, 1:9] / ytm_final[1:5, 2:10]))
cov_log_returns = cov(x1, x1)
cov_log_returns


# Covariance matrix for forward rates
x2 = t(log(forward_rate[, 1:9] / forward_rate[, 2:10]))
cov_fwdrates = cov(x2, x2)
cov_fwdrates

# Eigenitems of the above covariance matrices
eigenitems_yield = eigen(cov_log_returns, symmetric = TRUE)
eigenitems_fwd = eigen(cov_fwdrates, symmetric = TRUE)
list(yield = eigenitems_yield, forward = eigenitems_fwd)







