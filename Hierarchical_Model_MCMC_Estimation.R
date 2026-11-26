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

###############################
##### Log Likelihood ##########
###############################

log_likelihood <- function(theta,
                           ages,
                           genders,
                           diseases,
                           admission_date,
                           died,
                           released,
                           ongoing) {
  
  # Extract parameters
  beta <- theta[1]
  beta_age <- theta[2]
  beta_gender <- theta[3]
  beta_disease <- theta[4]
  
  alpha <- theta[5]
  alpha_date <- theta[6]
  
  # Stage 1: Mortality
  eta_death <- beta +
    beta_age * ages +
    beta_gender * genders +
    beta_disease * diseases
  
  # Stage 2: Recovery conditional on survival
  eta_recovery <- alpha +
    alpha_date * admission_date
  
  # Numerically stable log probabilities
  log_p <- plogis(eta_death, log.p = TRUE)
  log_not_p <- plogis(eta_death, lower.tail = FALSE, log.p = TRUE)
  
  log_q <- plogis(eta_recovery, log.p = TRUE)
  log_not_q <- plogis(eta_recovery, lower.tail = FALSE, log.p = TRUE)
  
  # Joint log likelihood
  ll <- sum(
    died * log_p +
      released * (log_not_p + log_q) +
      ongoing * (log_not_p + log_not_q)
  )
  
  return(ll)
}


###############################
##### Log Prior ###############
###############################

log_prior <- function(theta, prior_sd = 10) {
  
  return(
    sum(dnorm(theta,
              mean = 0,
              sd = prior_sd,
              log = TRUE))
  )
  
}


###############################
##### Log Posterior ###########
###############################

log_posterior <- function(theta,
                          ages,
                          genders,
                          diseases,
                          admission_date,
                          died,
                          released,
                          ongoing) {
  
  return(
    log_likelihood(
      theta,
      ages,
      genders,
      diseases,
      admission_date,
      died,
      released,
      ongoing
    ) +
      log_prior(theta)
  )
  
}


###############################
##### Accept-Reject Helpers ###
###############################

propose <- function(theta, proposal_sd) {
  
  m <- length(theta)
  
  proposal <- theta +
    rnorm(m, mean = 0, sd = proposal_sd)
  
  return(proposal)
  
}


acceptance_probability <- function(current_value,
                                   proposed_value,
                                   ages,
                                   genders,
                                   diseases,
                                   admission_date,
                                   died,
                                   released,
                                   ongoing) {
  
  proposal_log_post <- log_posterior(
    proposed_value,
    ages,
    genders,
    diseases,
    admission_date,
    died,
    released,
    ongoing
  )
  
  current_log_post <- log_posterior(
    current_value,
    ages,
    genders,
    diseases,
    admission_date,
    died,
    released,
    ongoing
  )
  
  log_alpha <- proposal_log_post - current_log_post
  
  return(log_alpha)
  
}


###############################
##### MCMC Estimation #########
###############################

mcmc <- function(ages,
                 genders,
                 diseases,
                 admission_date,
                 died,
                 released,
                 ongoing,
                 n_iter,
                 proposal_sd) {
  
  # Initial parameter values
  current_theta <- rep(0, 6)
  
  # Store posterior samples
  samples <- matrix(NA, nrow = n_iter, ncol = 6)
  
  colnames(samples) <- c(
    "beta",
    "beta_age",
    "beta_gender",
    "beta_disease",
    "alpha",
    "alpha_date"
  )
  
  samples[1, ] <- current_theta
  
  # Acceptance counter
  accepted <- 0
  
  # MCMC iterations
  for (i in 2:n_iter) {
    
    # Propose new parameters
    proposed_theta <- propose(current_theta, proposal_sd)
    
    # Calculate acceptance probability
    log_alpha <- acceptance_probability(
      current_theta,
      proposed_theta,
      ages,
      genders,
      diseases,
      admission_date,
      died,
      released,
      ongoing
    )
    
    # Accept or reject
    if (log(runif(1)) < min(0, log_alpha)) {
      
      current_theta <- proposed_theta
      
      accepted <- accepted + 1
    }
    
    # Store current parameter values
    samples[i, ] <- current_theta
    
  }
  
  # Return results
  return(list(
    samples = samples,
    acceptance_rate = accepted / (n_iter - 1)
  ))
  
}

###############################
##### Run MCMC ################
###############################

set.seed(123)

posterior_results <- mcmc(
  ages = ages,
  genders = genders,
  diseases = diseases,
  admission_date = admission_date,
  died = died,
  released = released,
  ongoing = ongoing,
  n_iter = 20000,
  proposal_sd = c(0.15, 0.10, 0.10, 0.10, 0.15, 0.10)
)

# Posterior samples
posterior_samples <- posterior_results$samples

# Acceptance rate
posterior_results$acceptance_rate

###############################
##### Posterior Summaries #####
###############################

burnin <- 5000

posterior_samples_burned_in <-
  posterior_samples[-(1:burnin), ]

# Posterior means
mean_posterior <- colMeans(
  posterior_samples_burned_in
)

# Posterior standard deviations
sd_posterior <- apply(
  posterior_samples_burned_in,
  2,
  sd
)

# 95% credible intervals
credible_intervals <- apply(
  posterior_samples_burned_in,
  2,
  quantile,
  probs = c(0.025, 0.975)
)

# Combine results
posterior_summary <- data.frame(
  Parameter = colnames(posterior_samples),
  Mean = mean_posterior,
  SD = sd_posterior,
  Lower_95 = credible_intervals[1, ],
  Upper_95 = credible_intervals[2, ]
)

print(posterior_summary)


