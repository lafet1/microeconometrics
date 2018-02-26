# packages
library(maxLik)
library(dplyr)
library(readstata13)
library(zeligverse)
library(dummies)


# data load
data <- read.dta13('clean_data_micro.dta')
x <- data[, c('adsmok42', 'age12x', 'ttlp12x', 'sex', 'racewx', 'eduyrdeg',
              'inscov12', 'marry12x', 'famsze12', 'region12', 'rthlth42', 'adinsb42',
              'adover42', 'phyexe53')]
x <- x %>% mutate_if(is.integer, as.numeric) 
s <- x %>% mutate_if(is.character, as.factor)
whatever <- cumsum(sapply(s %>% select_if(is.factor), function(x) length(unique(x))))
dummies <- s %>% select_if(is.factor) %>% dummy.data.frame
dummies <- dummies[, -whatever]
X <- bind_cols(x %>% select_if(is.numeric), dummies)
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

###### ML functions
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


###### Estimation using zelig()

mydata <- as.data.frame(cbind(as.factor(ifelse(y >= 6, 6, y)) , x))
mydata <- mydata %>% mutate_if(is.character, as.factor) 
colnames(mydata) <- c('y', colnames(x))
ologit <- polr(y ~ adsmok42 + age12x + ttlp12x + sex + racewx + eduyrdeg +
                inscov12 + marry12x + famsze12 + region12 + rthlth42 + adinsb42 +
                adover42 + phyexe53, method = 'logistic', data = mydata)
ologit



