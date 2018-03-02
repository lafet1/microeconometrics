# packages
library(maxLik)
library(readstata13)
library(dummies)
library(MASS)
library(dplyr)


###### data load###### 
data <- read.dta13('final_dataset.dta')
x <- data[, c('adsmok42', 'age12x', 'faminc12', 'sex', 'white', 'edulvl',
              'inscov12', 'marry12x', 'famsze12', 'region12', 'badhth',
              'adover42', 'phyexe53', 'employed')]
s <- x %>% select(- c(age12x, faminc12, famsze12)) %>% mutate_all(funs(as.factor))
whatever <- cumsum(sapply(s %>% select_if(is.factor), function(x) length(unique(x))))
dummies <- s %>% select_if(is.factor) %>% dummy.data.frame
dummies <- dummies[, - c(1, 1 + whatever[-length(whatever)])]
X <- bind_cols(Filter(is.integer, x), dummies)
X <- as.matrix(X)
y <- data[, 'obdrv12']
k <- ncol(X)
n <- nrow(X)

# theta is both lambda and beta coefficients
beta <- rep(0, k)
mu <- 1:6 / 5
           
recoder <- function(q, number) {
  # this function is used to create dummies for the different observation groups
  # so that we can obtain only the probability for the given class
  # i.e. sum where the first element is a 0/1 variable that is equal to one only once per
  # the summation space
  z <- rep(0, length(q))
  
  for (i in 1:length(q)){
    if (number == q[i]){
      z[i] <- 1
    }
    else if (q[i] > 5 & number == 6){
      z[i] <- 1
    }
  }
  return(z)
  
}

###### ML functions ###### 
mlf <- function(param){
  # we start by splitting the parameter vector
  bet <- param[1:k]
  mu <- param[(k + 1):(k + 7)]
  # we get the Nx1 matrix of fitted values and prepare an empty vector for probabilities
  xb <- X %*% bet
  p0 <- rep(0, length(y))
  
  # this for loop calculates the probability per observation
  for (j in 0:6){
  
    # firstly we obtain the probability for 0 visits
    if (j == 0){
      p0 <- p0 + plogis(mu[1] - xb) * recoder(y, j)
    }
    # now for for 1 through 5 visits
    else if (j > 0 & j < 6){
      p0 <- p0 + plogis(mu[j + 1] - xb) * recoder(y, j) - 
        plogis(mu[j] - xb) * recoder(y, j)
    }
    # now for more than 5 visits
    else{
      p0 <- p0 + (1 - plogis(mu[j] - xb)) * recoder(y, j)
    }
  }
  
  # the result sis calculated and returned now
  logL <- sum(log(p0))
  
  return(logL)
}



# the ML is calculated using the numerical gradient and hessian, starting points
# can be redefined at the beginnig of code
mles <- maxLik(mlf, grad = NULL, hess = NULL, start = c(beta, mu), method = "bfgs",
              iterlim = 100)


# errors
prop_sigma <- sqrt(diag(solve(-mles$hessian)))
se <- 1.96 * prop_sigma
upper <- mles$estimate + 1.96 * prop_sigma
lower <- mles$estimate - 1.96 * prop_sigma
interval <- data.frame(value = mles$estimate, lower = lower, upper = upper)
interval 


###### Estimation using built-in function ###### 

mydata <- as.data.frame(cbind(as.factor(ifelse(y >= 6, 6, y)) , x))
aux <- mydata %>% select(- c(age12x, faminc12, famsze12)) %>% mutate_all(funs(as.factor))
mydata <- bind_cols(aux, mydata %>% select(age12x, faminc12, famsze12))
colnames(mydata)[1] <- 'y'
ologit <- polr(y ~ age12x + faminc12 + famsze12 + adsmok42 + sex + white + edulvl +
                inscov12 + marry12x + region12 + badhth +
                adover42 + phyexe53 + employed, method = 'logistic', data = mydata)
ologit


##### control #####
cbind(mles$estimate, c(ologit$coefficients, ologit$zeta))

