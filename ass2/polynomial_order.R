# packages
library(car)
library(dplyr)

# probit (parametric to determine order of polynomial)
probit_data <- cbind(Z, Y1)
probit <- glm(Y1 ~ ., data = probit_data, family = binomial('probit'))
fit_aux <- 2 * fitted(probit) - 1

# try the possible orders of polynomial, preparing datasets
aux_data <- data.frame(Y2, X, as.factor(Y1), fit_aux, fit_aux^2, fit_aux^3)
aux_data2 <- data.frame(Y2, X, as.factor(Y1), fit_aux, fit_aux^2)
aux_data3 <- data.frame(Y2, X, as.factor(Y1), fit_aux, fit_aux^2, fit_aux^3, fit_aux^4)
aux_data4 <- data.frame(Y2, X, as.factor(Y1), fit_aux, fit_aux^2, fit_aux^3, fit_aux^4, 
                        fit_aux^5)
aux_data5 <- data.frame(Y2, X, as.factor(Y1), fit_aux, fit_aux^2, fit_aux^3, fit_aux^4, 
                        fit_aux^5, fit_aux^6)

# train the actual models ysing those polynomials
aux_model <- lm(Y2 ~ ., data = aux_data)
aux_model2 <- lm(Y2 ~ ., data = aux_data2)
aux_model3 <- lm(Y2 ~ ., data = aux_data3)
aux_model4 <- lm(Y2 ~ ., data = aux_data4)
aux_model5 <- lm(Y2 ~ ., data = aux_data5)

####### Bootstrapping ######

boot_func <- function(data, index = nrow(data)){
  
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
  
  # estimate first stage
  probit_data <- cbind(Z, Y1)
  probit <- glm(Y1 ~ ., data = probit_data, family = binomial('probit'))
  fit_aux <- 2 * fitted(probit) - 1
  
  # create data frame for second equation
  aux_data <- data.frame(Y2, X, as.factor(Y1), fit_aux, fit_aux^2, fit_aux^3)
  aux_data2 <- data.frame(Y2, X, as.factor(Y1), fit_aux, fit_aux^2)
  aux_data3 <- data.frame(Y2, X, as.factor(Y1), fit_aux, fit_aux^2, fit_aux^3, fit_aux^4)
  aux_data4 <- data.frame(Y2, X, as.factor(Y1), fit_aux, fit_aux^2, fit_aux^3, fit_aux^4, 
                          fit_aux^5)
  aux_data5 <- data.frame(Y2, X, as.factor(Y1), fit_aux, fit_aux^2, fit_aux^3, fit_aux^4, 
                          fit_aux^5, fit_aux^6)
  
  # estimate second equation
  aux_model <- lm(Y2 ~ ., data = aux_data)
  aux_model2 <- lm(Y2 ~ ., data = aux_data2)
  aux_model3 <- lm(Y2 ~ ., data = aux_data3)
  aux_model4 <- lm(Y2 ~ ., data = aux_data4)
  aux_model5 <- lm(Y2 ~ ., data = aux_data5)
  
  # get coefficients
  se <- list(coef(summary(aux_model))[, 1], coef(summary(aux_model2))[, 1],
                   coef(summary(aux_model3))[, 1], coef(summary(aux_model4))[, 1],
                   coef(summary(aux_model5))[, 1])
  names(se) <- c('2 and 3', '20', '2, 3 and 4', '2, 3, 4 and 5', '2, 3, 4, 5 and 6')
  return(se)
  
}

# calculate the bootstrap
results_aux <- list()
for (i in 1:50) {
  results_aux[[i]] <- boot_func(data)
}

# prepare the ground for comparison
se_1 <- c()
se_2 <- c()
se_3 <- c()
se_4 <- c()
se_5 <- c()
for (i in 1:50){
  # combine the coefficients in one data frame
  se_1 <- cbind(se_1, results_aux[[i]]$`2 and 3`)
  se_2 <- cbind(se_2, results_aux[[i]]$`20`)
  se_3 <- cbind(se_3, results_aux[[i]]$`2, 3 and 4`)
  se_4 <- cbind(se_4, results_aux[[i]]$`2, 3, 4 and 5`)
  se_5 <- cbind(se_5, results_aux[[i]]$`2, 3, 4, 5 and 6`)
}

# calculate the standard errors for all polynomial orders
boot_1 <- sqrt(rowSums((apply(se_1, 2, function(x) (x - coef(summary(aux_model))[, 1])))^2) / 
  (dim(se_1)[2] - 1))
boot_2 <- sqrt(rowSums((apply(se_2, 2, function(x) (x - coef(summary(aux_model2))[, 1])))^2) / 
  (dim(se_2)[2] - 1))
boot_3 <- sqrt(rowSums((apply(se_3, 2, function(x) (x - coef(summary(aux_model3))[, 1])))^2) / 
  (dim(se_3)[2] - 1))
boot_4 <- sqrt(rowSums((apply(se_4, 2, function(x) (x - coef(summary(aux_model4))[, 1])))^2) / 
  (dim(se_4)[2] - 1))
boot_5 <- sqrt(rowSums((apply(se_5, 2, function(x) (x - coef(summary(aux_model5))[, 1])))^2) / 
  (dim(se_5)[2] - 1))

# get t-statistics
t_1 <- coef(summary(aux_model))[, 1] / boot_1
t_2 <- coef(summary(aux_model2))[, 1] / boot_2
t_3 <- coef(summary(aux_model3))[, 1] / boot_3
t_4 <- coef(summary(aux_model4))[, 1] / boot_4
t_5 <- coef(summary(aux_model5))[, 1] / boot_5

# test hypothesis of joint significance of polynomials
linearHypothesis(aux_model, c('fit_aux = 0', 'fit_aux.2 = 0', 'fit_aux.3 = 0'), 
                 vcov. = diag(boot_1))
linearHypothesis(aux_model2, c('fit_aux = 0', 'fit_aux.2 = 0'),
                 vcov. = diag(boot_2))
linearHypothesis(aux_model3, c('fit_aux = 0', 'fit_aux.2 = 0', 'fit_aux.3 = 0', 'fit_aux.4 = 0'),
                 vcov. = diag(boot_3))
linearHypothesis(aux_model4, c('fit_aux = 0', 'fit_aux.2 = 0', 'fit_aux.3 = 0', 'fit_aux.4 = 0',
                               'fit_aux.5 = 0'),
                 vcov. = diag(boot_4))
linearHypothesis(aux_model5, c('fit_aux = 0', 'fit_aux.2 = 0', 'fit_aux.3 = 0', 'fit_aux.4 = 0',
                               'fit_aux.5 = 0', 'fit_aux.6 = 0'),
                 vcov. = diag(boot_5))




