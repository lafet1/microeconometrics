# packages
library(np)
library(dplyr)


##### data load #####
data <- read.csv('workdata.csv')

# intermediate data
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

# 'estimation'
start <- Sys.time()
bw <- npindexbw(xdat = Z, ydat = Y1,
                nmulti = 5, optim.reltol = 0.1)
end <- start - Sys.time()
# 5 variable - 9.5 min, 6 variables - 58 mins, 7 variables - 54 mins, 
# 8 variables - 1.05 hours
# 

np_probit <- npindex(bws = bw)
fit <- fitted(np_probit)
model_data <- data.frame(Y2, X, as.factor(Y1), fit, fit^2, fit^3)

main_model <- lm(Y2 ~ ., data = model_data)


boot_se <- function(data, index = nrow(data)){
  
  data <- data[sample(nrow(data), size = index, replace = T), ]
  
  # first we get variables to resample 
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

  # second we run the nonparametric probit
  bw <- npindexbw(xdat = Z, ydat = Y1, 
                  nmulti = 5, optim.reltol = 0.1)
  np_probit <- npindex(bws = bw)
  fit <- fitted(np_probit)
  model_data <- data.frame(Y2, X, Y1 = as.factor(Y1), fit, fit^2, fit^3)
  
  main_model <- lm(Y2 ~ ., data = model_data)
  
  return(coef(summary(main_model)))
  
}


results <- list()

for (i in 1:50) {
  results[[i]] <- boot_se(data)
}


