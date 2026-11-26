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

###########################
##### MCMC Estimation #####
###########################

# 1) Get Data

n <- nrow(data)
x1 <- as.numeric(data$Hospital)
x2 <- ifelse(data$Sex == "Male", 1, 0)
x3 <- as.numeric(data$Disease2)
X <- cbind(1, x1, x2, x3)

true_beta <- c(1, rnorm(1,0,1), rnorm(1,0,1), rnorm(1,0,1))

y <- ifelse(data$Outcome == "Recovered", 1, 0)

calculate_probability <- function(X, beta) {
  prob <- 1 / (1 + exp(-(X %*% beta)))
  return(prob)
}

# 2) Define Log Likelihood, Log Prior, and Log Posterior

log_likelihood <- function(beta, X, y) {
  eta <- as.vector(X %*% beta)
  return(sum(y * eta - log1p(exp(eta))))
}

log_prior <- function(beta, prior_sd = 10) {
  return(sum(dnorm(beta, mean = 0, sd = prior_sd, log = TRUE)))
}

log_posterior <- function(beta, X, y) {
  return(log_likelihood(beta, X, y) + log_prior(beta))
}

# 3) Accept-Reject Helpers

sample <- function(beta, proposal_sd) {
  m <- length(beta)
  proposal <- beta + rnorm(m, mean = 0, sd = proposal_sd)
  return(proposal)
}

acceptance_probability <- function(current_value, proposed_value, X, y) {
  proposal_log_post <- log_posterior(proposed_value, X, y)
  current_log_post <- log_posterior(current_value, X, y)
  log_alpha <- proposal_log_post - current_log_post
  return(log_alpha)
}

# 4) MCMC

mcmc <- function(X, y, n_iter, proposal_sd) {
  current_beta <- rep(0, ncol(X))
  samples <- list(current_beta)
  
  for (i in 2:n_iter) {
    proposed_beta <- sample(current_beta, proposal_sd)
    alpha <- acceptance_probability(current_beta, proposed_beta, X, y)
    current_beta <- if (runif(1) < alpha) proposed_beta else current_beta
    samples[[i]] <- current_beta
  }
  
  return(samples)
}


# 4) Run Sampler and Summarize Posterior

posterior_samples <- mcmc(X, y, 15000, 0.15)

burnin <- 10000

posterior_samples_burned_in <- posterior_samples[-(1:burnin)]

posterior_matrix <- do.call(rbind, posterior_samples_burned_in)

mean_posterior <- colMeans(posterior_matrix)

# Check Accuracy


predicted_probabilities <- plogis(
  as.vector(X %*% mean_posterior)
)

predicted_classes <- ifelse(predicted_probabilities >= 0.5, 1, 0)

accuracy <- mean(predicted_classes == y)

cat("Accuracy:", accuracy * 100, "%\n")

# Confusion matrix
table(
  Actual = y,
  Predicted = predicted_classes
)
