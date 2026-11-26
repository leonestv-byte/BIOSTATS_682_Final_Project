library(R2jags)

##################
###### Data ######
##################

library(readr)

data <- read_csv("ext_2025.csv")

names(data)
# "No"             "Hospital"       "Age"            "Sex"            "Disease2"      
# "Admission_date" "Discharge_date" "Outcome"

data$Hospital <- factor(data$Hospital)
data$Sex <- factor(data$Sex)
data$Disease2 <- factor(data$Disease2)


# Remove Ongoing (33 examples)
data <- data[data$Outcome != 'Ongoing',]

# Finish Factoring
data$Outcome <- factor(data$Outcome)

################################
###### Hierarchical Model ######
################################

n <- nrow(data)

ages <- data$Age
genders <- ifelse(data$Sex == "Male", 1, 0)
survived <- ifelse(data$Outcome == "Recovered", 1, 0)
diseases <- as.numeric(data$Disease2)




model <- function() {
  
  for (i in 1:n) {
    survived[i] ~ dbern(p[i])
    logit(p[i]) <- beta + (beta_age *  ages[i]) + (beta_gender * genders[i]) + (beta_disease * diseases[i])
  }
  
  beta ~ dnorm(0, 0.01)
  beta_age ~ dnorm(0, 0.01)
  beta_gender ~ dnorm(0, 0.01)
  beta_disease ~ dnorm(0, 0.01)
}

jags_data <- list("n", "survived", "ages", "genders", "diseases")

jags_inits <- function() {
  list(
    beta = rnorm(1, 0, 0.01),
    beta_age = rnorm(1, 0, 0.01),
    beta_gender = rnorm(1, 0, 0.01),
    beta_disease = rnorm(1, 0, 0.01)
  )
}

params <- c("beta", "beta_age", "beta_gender", "beta_disease")

jags_fit <- jags(
  data = jags_data,
  inits = jags_inits,
  parameters.to.save = params,
  model.file = model,
  n.chains = 3,
  n.iter = 20000,
  n.burnin = 5000,
  n.thin = 1
)

posterior_draws <- jags_fit$BUGSoutput$sims.list

beta <- mean(posterior_draws$beta)

beta_age_mean <- mean(posterior_draws$beta_age)

beta_gender_mean <- mean(posterior_draws$beta_gender)

beta_disease_mean <- mean(posterior_draws$beta_disease)

logits <- beta + (beta_age_mean *  ages) + (beta_gender_mean * genders) + (beta_disease_mean * diseases)

p <- plogis(logits) 

plot(density(p))

# Done by Probability

survived_estimates <- rbinom(n = length(p), size = 1, prob = p)

accuracy <- mean(survived == survived_estimates)

# Done by Threshold

survived_estimates <- ifelse(p >= 0.8, 1, 0)

accuracy <- mean(survived == survived_estimates)

##########################################
###### Hierarchical Model - Ongoing ######
##########################################
library(readr)
library(R2jags)

data <- read_csv("ext_2025.csv")

data$Hospital <- factor(data$Hospital)
data$Sex <- factor(data$Sex)
data$Disease2 <- factor(data$Disease2)
admission_date <- as.numeric(scale(as.Date(data$Admission_date, format = "%Y-%m-%d")))

ages <- data$Age
genders <- ifelse(data$Sex == "Male", 1, 0)

released <- ifelse(data$Outcome == "Recovered", 1, 0)
died <- ifelse(data$Outcome == "Deceased", 1, 0)
ongoing <- ifelse(data$Outcome == "Ongoing", 1, 0)
diseases <- as.numeric(data$Disease2)


map_alive <- which(died == 0)
n_alive <- length(map_alive)
n <- nrow(data)

model <- function() {
  
  for (i in 1:n) {
    
    # Stage 1 - Death
    died[i] ~ dbern(p[i])
    
    logit(p[i]) <- beta
    + (beta_age * ages[i])
    + (beta_gender * genders[i])
    + (beta_disease * diseases[i])
    
  }
  
  # Stage 2 - Ongoing or Recovered
  for (j in 1:n_alive) {
    released[map_alive[j]] ~ dbern(q[j])
    logit(q[j]) <- alpha + alpha_date * admission_date[map_alive[j]]
  }
  
  for (z in 1:n) {
    logit(q_all[z]) <- alpha + alpha_date * admission_date[z]
    y[z] <- (1 - p[z]) * q_all[z]
  }


    
  
  
  beta ~ dnorm(0, 0.01)
  beta_age ~ dnorm(0, 0.01)
  beta_gender ~ dnorm(0, 0.01)
  beta_disease ~ dnorm(0, 0.01)
  
  alpha ~ dnorm(0, 0.01)
  alpha_date ~ dnorm(0, 0.01)
  
}

jags_data <- list(
  n = n,
  n_alive = n_alive,
  map_alive = map_alive,
  died = died,
  released = released,
  ages = ages,
  genders = genders,
  diseases = diseases,
  admission_date = admission_date
)

jags_inits <- function() {
  list(
    beta = rnorm(1, 0, 0.01),
    beta_age = rnorm(1, 0, 0.01),
    beta_gender = rnorm(1, 0, 0.01),
    beta_disease = rnorm(1, 0, 0.01),
    alpha = rnorm(1, 0, 0.01),
    alpha_date = rnorm(1, 0, 0.01)
  )
}

params <- c("beta", "beta_age", "beta_gender", "beta_disease", "alpha", "alpha_date")

jags_fit <- jags(
  data = jags_data,
  inits = jags_inits,
  parameters.to.save = params,
  model.file = model,
  n.chains = 3,
  n.iter = 20000,
  n.burnin = 5000,
  n.thin = 1
)


# Posterior Draws

posterior_draws <- jags_fit$BUGSoutput$sims.list

# Probability of death

beta <- mean(posterior_draws$beta)

beta_age_mean <- mean(posterior_draws$beta_age)

beta_gender_mean <- mean(posterior_draws$beta_gender)

beta_disease_mean <- mean(posterior_draws$beta_disease)

logits <- beta + (beta_age_mean *  ages) + (beta_gender_mean * genders) + (beta_disease_mean * diseases)

p_died <- plogis(logits)

# Probability of released

alpha <- mean(posterior_draws$alpha)

alpha_date_mean <- mean(posterior_draws$alpha_date)

logits <- alpha + (alpha_date_mean * admission_date)

p_released <- plogis(logits)


# Effect Release Probability

p_released_effective <- (1 - p_died) * p_released

#####
#####
#####

p_deceased <- p_died

p_recovered <- (1 - p_died) * p_released

p_ongoing <- (1 - p_died) * (1 - p_released)


pred_died <- ifelse(p_deceased >= 0.5, 1, 0)
accuracy_died <- mean(pred_died == died)


pred_released <- ifelse(p_released[map_alive] >= 0.5, 1, 0)
accuracy_released <- mean(pred_released == released[map_alive])





