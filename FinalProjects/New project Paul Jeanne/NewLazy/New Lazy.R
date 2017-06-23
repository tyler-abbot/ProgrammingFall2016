#' A Lazy Cobb Douglas function
#' 
#' This function outputs the Marshallian 
#' resulting from a consumer utility 
#' maximization problem. It also outputs
#' the corresponding graph. 
#' 
#' 
#' lazy_Cobb_Douglas <- function(
#'  utility, #Utility function, entered as a character 
#'  (only example: "x1^alpha*x2^(1-alpha)")
#'  income, #income of the budget constraint, single value (real)
#'  price,  #vector of two prices (since we consider only two goods)
#'  alpha   #share of x1  
#' ){
#' 
#' 
#' Example
#' lazy_Cobb_Douglas(
#' utility = "x1^alpha*x2^(1-alpha)", 
#' income = 80, 
#' price = c(4, 3),  
#' alpha = 0.64



# Project Programming Course 

#Jeanne and Paul 

lazy_Cobb_Douglas <- function(
  utility, #Utility function, entered as a character 
            #(only example: "x1^alpha*x2^(1-alpha)")
  income, #income of the budget constraint, single value (real)
  price,  #vector of two prices (since we consider only two goods)
  alpha   #share of x1  
  ){


#Derivative of the utility 
char1<- substr(utility, 1, 3)
char2<- substr(utility, 9, 12)

f<-paste(char1, alpha, char2, (1-alpha))

objectD<-deriv(parse(text=f), c("x1","x2"))


str<-as.character(objectD)

if("stringr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("stringr")}
 
library(stringr)

#Split of the "objectD" object into different vectors
#(chacun correspond à une ligne)
test<-strsplit(str, "\n")

#Basic functions
fun1<-test[[1]][2]
fun2<-test[[1]][3]

f1<-substr(fun1, (str_locate(fun1, "<-")[2]+1), nchar(fun1))
f2<-substr(fun2, (str_locate(fun2, "<-")[2]+1), nchar(fun2))


#Localisation of the derivative
g1<-test[[1]][6]
g2<-test[[1]][7]

#Obtaining derivative
dv1 <- substr(g1, (str_locate(g1, "<-")[2]+1), nchar(g1))
dv2 <- substr(g2, (str_locate(g2, "<-")[2]+1), nchar(g2))


#Adding of the "expressions" 

d1<- sub(".expr2", f2, dv1)
d2<- sub(".expr1", f1, dv2)

#Dropping the blanks
u1<-unlist(strsplit(d1," "))
u1<-paste(u1, collapse = "")
u2<-unlist(strsplit(d2," "))
u2<-paste(u2, collapse = "")
u2<-gsub("(","", u2, fixed = TRUE)
u2<-gsub(")","", u2, fixed = TRUE)


#Simplification of expressions 


char_alpha<-as.character(alpha)
l<-nchar(char_alpha)

  choose_puiss<- function(u, x){
    
    if(substr(u, (str_locate(u, x)[2]+2),
              (str_locate(u, x)[2]+2)) == "-"){
      puiss <- substr(u, (str_locate(u, x)[2]+2), (str_locate(u, x)[2]+2+l))

      } else {
      puiss <- substr(u, (str_locate(u, x)[2]+2), (str_locate(u, x)[2]+1+l))
        
      }
    return( puiss )
  }


p_x1_1<-choose_puiss(u1, "x1")

p_x1_2<-choose_puiss(u2, "x1")

p_x2_1<-choose_puiss(u1, "x2")

p_x2_2<-choose_puiss(u2, "x2")



dif_puis_x1<-as.numeric(p_x1_1)-as.numeric(p_x1_2)
dif_puis_x2<-as.numeric(p_x2_1)-as.numeric(p_x2_2)


# Do not forget the constant from the successive derivatives

choose_coeff<- function(u, x){
  #alpha is always in ]0;1[, therefore when we derive, we are looking for 
  #the coefficient of size l before the variable the expression is derived with 
  #respect to.   
  coeff <- substr(u, (str_locate(u, x)[1]-l-1), (str_locate(u, x)[1]-2))
  
  return(coeff)  
  
}

c1<-as.numeric(choose_coeff(u1, "x1"))
c2<-as.numeric(choose_coeff(u2, "x2"))



ratio_coeff <- c1/c2

# Equating utility to price ratios

#We know here that -dif_puis_x1/dif_puis_x2 = 1; therefore we solve the linear
#equation y = A*x1 (where A contains parameters of prices, powers of initial 
#utility function, etc.)
#Once we found the optimal x1, noted x1_star, we find the second 
#optimal consumption thanks to the budget constraint

x1_star<-solve((price[1]+price[2]*(price[1]/price[2])*(1/ratio_coeff)), income)

x2_star<-(price[1]/price[2])*(1/ratio_coeff)*x1_star
 

#We imagine a case where x1 and x2 could be affected other values than 1 
#Then we have to check the value of the ratio of the powers; 
#we divide the following cases: if the ratio equals one, then we solve the 
#equation as above; if the ratio is an integer (we use the function 
#is.wholenumber() to check this), then we solve it thanks to the function 
#polyroot(). We should also find a solution if the ratio is different from an 
#integer, thanks to optimization functions. 


#is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
#
#if(-dif_puis_x1/dif_puis_x2 == 1){
#  x1_star<-solve((price[1]+price[2]*(price[1]/price[2])*(1/ratio_coeff)), income)
#  x2_star<-(price[1]/price[2])*(1/ratio_coeff)*x1_star
#} else if((-dif_puis_x1/dif_puis_x2) != 1 && 
#            is.wholenumber(-dif_puis_x1/dif_puis_x2)){
#  pow<- -dif_puis_x1/dif_puis_x2
#  eq<-rep(0, (pow+1))
#  eq[2]<-price[1]
#  eq[pow+1]<-price[2]*(price[1]/price[2])^(1/dif_puis_x1)
#  z <- matrix(eq, ncol=1)
#  polyroot(z)
#}



#Tracé de la fonction d'utilité

U0<-x1_star^alpha*x2_star^(1-alpha)
x1<-sort(runif(2000, 0, 100))
x2<-(U0*x1^(-alpha))^(1/(1-alpha))
plot(x1, x2, type = "l", lwd = 2, pch=1, xlim = c(0, round(income/price[1]))
     , ylim = c(0, 1.5*round(income/price[2])), xlab = "x1", ylab = "x2", 
     main = "Cobb-Douglas utility maximization problem - Representation")
abline(a = income/price[2], b =-price[1]/price[2], col = "blue", lwd = 2.5)
points(x2_star~x1_star, pch = 20, col="red", lwd = 3) 

legend("right", legend=c("BC", "Utility"), lty = c(1,1), 
       col = c("blue", "black"), lwd = c(2.5, 2),  cex = 0.8)

#Printing the values of optimal bundle of goods
x1val<-as.character(round(x1_star, 3))
x2val<-as.character(round(x2_star, 3))


text((round(income/price[1])-4),(1.5*round(income/price[2])-1),
     paste("x1* = ",x1val),col='red',cex=1)
text((round(income/price[1])-4),(1.5*round(income/price[2])-4),
     paste("x2* = ",x2val),col='red',cex=1)

}


