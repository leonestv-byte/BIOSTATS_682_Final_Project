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


model <- function() {
  
  for (i in 1:n) {
    survived[i] ~ dbern(p[i])
    logit(p[i]) <- beta + (beta_age *  ages[i]) + (beta_gender * genders[i])
  }
  
  beta ~ dnorm(0, 0.01)
  beta_age ~ dnorm(0, 0.01)
  beta_gender ~ dnorm(0, 0.01)
}

jags_data <- list("n", "survived", "ages", "genders")

jags_inits <- function() {
  list(
    beta = rnorm(1, 0, 0.01),
    beta_age = rnorm(1, 0, 0.01),
    beta_gender = rnorm(1, 0, 0.01)
  )
}

params <- c("beta", "beta_age", "beta_gender")

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

logits <- beta + (beta_age_mean *  ages) + (beta_gender_mean * genders)

p <- plogis(logits) 

plot(density(p))

survived_estimates <- rbinom(n = length(p), size = 1, prob = p)

accuracy <- mean(survived == survived_estimates)

