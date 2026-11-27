library(readr)
library(R2jags)

data <- read_csv("ext_2025.csv")

data$Hospital <- factor(data$Hospital)
data$Sex <- factor(data$Sex)
data$Disease2 <- factor(data$Disease2)
admission_date <- as.numeric(scale(as.Date(data$Admission_date, format = "%Y-%m-%d")))

ages <- data$Age
genders <- ifelse(data$Sex == "Male", 1, 0)
diseases <- as.numeric(data$Disease2)


released <- ifelse(data$Outcome == "Recovered", 1, 0)
died <- ifelse(data$Outcome == "Deceased", 1, 0)
ongoing <- ifelse(data$Outcome == "Ongoing", 1, 0)

n <- nrow(data)
map_alive <- which(died == 0)
n_alive <- length(map_alive)


###########################
##### MCMC Estimation #####
###########################

# 1) Data To Input Processing
x1 <- ages
x2 <- genders
x3 <- diseases
mortality_X <- cbind(1, x1, x2, x3)

z1 <- admission_date
released_X <- cbind(1, z1)



# 2) Define Log Likelihood, Log Prior, and Log Posterior

log_likelihood <- function(beta, mortality_X, released_X, released, died, ongoing) {
  mortality_intercept <- beta[1]
  K <- ncol(mortality_X)

  
  released_intercept <- beta[K + 1]
  
  mortality_logits <- mortality_intercept
  for (i in 2:K) {
    mortality_logits <- mortality_logits + (beta[i] * mortality_X[, i])
  }
  
  released_logits <- released_intercept
  L <- ncol(released_X)
  for (i in 2:L) {
    released_logits <- released_logits + (beta[K + i] * released_X[, i])
  }
  
  log_died <- plogis(mortality_logits, log.p = TRUE)
  log_not_died <- plogis(mortality_logits, lower.tail = FALSE, log.p = TRUE)
  
  log_released <- plogis(released_logits, log.p = TRUE)
  log_not_released <- plogis(released_logits, lower.tail = FALSE, log.p = TRUE)
  
  likelihood <- sum((died * log_died) + (released * log_not_died * log_released) + (ongoing * log_not_died * log_not_released))
  
  return(likelihood)
}

log_prior <- function(beta, prior_sd = 10) {
  return(sum(dnorm(beta, mean = 0, sd = prior_sd, log = TRUE)))
}

log_posterior <- function(beta, mortality_X, released_X, released, died, ongoing) {
  return(log_likelihood(beta, mortality_X, released_X, released, died, ongoing) + log_prior(beta))
}

# 3) Accept Reject Helpers

sample <- function(beta, proposal_sd) {
  m <- length(beta)
  proposal <- beta + rnorm(m, mean = 0, sd = proposal_sd)
  return(proposal)
}


acceptance_probability <- function(current_value, proposed_value, mortality_X, released_X, released, died, ongoing) {
  proposal_log_post <- log_posterior(proposed_value, mortality_X, released_X, released, died, ongoing)
  current_log_post <- log_posterior(current_value, mortality_X, released_X, released, died, ongoing)
  log_alpha <- proposal_log_post - current_log_post
  return(log_alpha)
}

# 4) MCMC Estimation

run_mcmc <- function(mortality_X, released_X, released, died, ongoing, n_iter, proposal_sd) {
  m <- ncol(mortality_X) + ncol(released_X)
  current_beta <- rep(0, m)
  samples <- list(current_beta)
  
  for (i in 2:n_iter) {
    proposed_beta <- sample(current_beta, proposal_sd)
    alpha <- acceptance_probability(current_beta, proposed_beta, mortality_X, released_X, released, died, ongoing)
    current_beta <- if (runif(1) < alpha) proposed_beta else current_beta
    samples[[i]] <- current_beta
  }
  return(samples)
}


posterior_samples <- run_mcmc(mortality_X, released_X, released, died, ongoing, 15000, 0.15)




