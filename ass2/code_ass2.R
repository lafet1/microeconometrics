# packages
library(np)
library(dplyr)
library(stargazer)


##### data load #####
data <- read.csv('workdata.csv')

# data preparation
Z <- select(data, female, age, age2, highschool, bachelor, master, 
            doctorate, married, midwest, south, west, white, black, native, 
            asian, disability, business, hand_labour, retail, educ_health_social, 
            income, health2, health3, health4, health5,
            mental_health2, mental_health3, mental_health4, mental_health5,
            nr_diagnoses, underweight, overweight, obese) %>% 
  mutate_if(is.logical, as.factor)

X <- select(data, female, age, age2, highschool, education, fam_size, 
            midwest, south, west, white, black, native, 
            asian, disability,
            income, health2, health3, health4, health5,
            mental_health2, mental_health3, mental_health4, mental_health5,
            diag1, diag2, underweight, overweight, obese) %>% 
  mutate_if(is.logical, as.factor)

Y1 <- as.integer(data$insured)
Y2 <- data$expenditure


###### Estimation ######

# we start by estimating the model without bootstrap to get and idea
start <- Sys.time()
bw <- npindexbw(xdat = Z, ydat = Y1,
                nmulti = 5, optim.reltol = 0.1)
end <- start - Sys.time()

# the nonparametric approach is very slow so we impose tolerance in the optimization
# routine and get less precise results, still the estimation takes more than 10 minutes

# now we simply estimate the nonparametric probit and get the fitted values
np_probit <- npindex(bws = bw)
fit <- 2 * fitted(np_probit) - 1

# data frame we need for second equation
model_data <- data.frame(Y2, X, as.factor(Y1), fit, fit^2, fit^3, fit^4, fit^5)

# second equation using the Newey estimator
main_model <- lm(Y2 ~ ., data = model_data)


####### Bootstrap ######

boot_se <- function(data, index = nrow(data)){
  # function for bootstrapping the SE's
  
  # bootstrap sample
  data <- data[sample(nrow(data), size = index, replace = T), ]
  
  # we get the variables from the bootstrap sample
  Z <- select(data, female, age, age2, highschool, bachelor, master, 
              doctorate, married, midwest, south, west, white, black, native, 
              asian, disability, business, hand_labour, retail, educ_health_social, 
              income, health2, health3, health4, health5,
              mental_health2, mental_health3, mental_health4, mental_health5,
              nr_diagnoses, underweight, overweight, obese) %>% 
    mutate_if(is.logical, as.factor)
  
  X <- select(data, female, age, age2, highschool, education, fam_size, 
              midwest, south, west, white, black, native, 
              asian, disability,
              income, health2, health3, health4, health5,
              mental_health2, mental_health3, mental_health4, mental_health5,
              diag1, diag2, underweight, overweight, obese) %>% 
    mutate_if(is.logical, as.factor)
  
  Y1 <- as.integer(data$insured)
  Y2 <- data$expenditure

  # we run the nonparametric probit
  bw <- npindexbw(xdat = Z, ydat = Y1, 
                  nmulti = 5, optim.reltol = 0.1)
  np_probit <- npindex(bws = bw)
  fit <- fitted(np_probit)
  
  # prepare the data for second equation
  model_data <- data.frame(Y2, X, Y1 = as.factor(Y1), fit, fit^2, fit^3, fit^4, fit^5)
  
  # estimate second equation
  main_model <- lm(Y2 ~ ., data = model_data)
  
  # return the model summary so we can get the bootstrapped SE's
  return(coef(summary(main_model))[, 1])
  
}

# for loop for estimating the actual bootstrap replication and preparation of the list
results <- list()
for (i in 1:50) {
  results[[i]] <- boot_se(data)
}

# saveRDS(results, file = 'bootstrap_results.rds')
# to load results use readRDS('bootstrap_results.rds') - RDS allows us to save and read
# list objects, this loop took the night, so it is good practice to save it

np_se <- c()
for (i in 1:50){
  np_se <- cbind(np_se, results[[i]])
}


np_boot_1 <- sqrt(rowSums((apply(np_se, 2, function(x) (x - coef(summary(main_model))[, 1])))^2) / 
                 (dim(np_se)[2] - 1))

np_t_1 <- coef(summary(main_model))[, 1] / np_boot_1

