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


##################
##### Priors #####
##################

# A) Noninformative Prior.

alpha <- 50
beta <- 50

alpha_posterior <- alpha + nrow(data[data$Outcome == 'Recovered',])
beta_posterior <- beta + nrow(data[data$Outcome == 'Deceased',])

x <- seq(0, 1, length.out = 500)

samples_posterior <- dbeta(x, alpha_posterior, beta_posterior)
samples_prior <- dbeta(x, alpha, beta)
# Likelihood multiplied to put it on the same scale as dbeta * (nrow(data) + 1)
samples_likelihood <- dbinom(nrow(data[data$Outcome == 'Recovered',]), size = nrow(data), prob = x) * (nrow(data) + 1)

plot(x, samples_posterior, xlim = c(0, 1), col = 'purple', type = 'l')
lines(x, samples_prior, col = 'blue')
lines(x, samples_likelihood, col = 'red')

# B) Prior from Research Literature 

alpha <- 85
beta <- 15

alpha_posterior <- alpha + nrow(data[data$Outcome == 'Recovered',])
beta_posterior <- beta + nrow(data[data$Outcome == 'Deceased',])

x <- seq(0, 1, length.out = 500)

samples_posterior <- dbeta(x, alpha_posterior, beta_posterior)
samples_prior <- dbeta(x, alpha, beta)
samples_likelihood <- dbinom(nrow(data[data$Outcome == 'Recovered',]), size = nrow(data), prob = x) * (nrow(data) + 1)

plot(x, samples_posterior, xlim = c(0, 1), col = 'purple', type = 'l')
lines(x, samples_prior, col = 'blue')
lines(x, samples_likelihood, col = 'red')

#####################################
##### Conditional Distributions #####
#####################################

#####################################
# Sex A)

conditional_bayes_probability_distributions <- function(data, field, field_value) {
  sub_data <- data[data[[field]] == field_value,]
  
  # Posterior
  alpha_posterior <- nrow(sub_data[sub_data$Outcome == 'Recovered',])
  beta_posterior <- nrow(sub_data[sub_data$Outcome == 'Deceased',])
  samples_posterior <- rbeta(1000, alpha_posterior, beta_posterior)
  
  expected <- alpha_posterior / (alpha_posterior + beta_posterior)
  print(paste("Expected value: ", expected))
  
  # Bayes Stats
  # P(R | S) = P(S | R) P(R) / P(S)
  p_s <- nrow(sub_data) / nrow(data)
  p_r <- nrow(data[data$Outcome == 'Recovered',]) / nrow(data)
  p_s_given_r <- nrow(data[data$Outcome == 'Recovered' & data[[field]] == field_value,]) / nrow(data[data$Outcome == 'Recovered',])
  bayes_expected <- (p_s_given_r * p_r) / p_s
  print(paste("Value from Bayes: ", bayes_expected))
  
  # Plot
  plot(density(samples_posterior))
}


## A 1) Male
conditional_bayes_probability_distributions(data, "Sex", "Male")
# "Expected value:  0.935483870967742"
# "Value from Bayes:  0.935483870967742"

## A 2) Female
conditional_bayes_probability_distributions(data, "Sex", "Female")
# "Expected value:  0.902654867256637"
# "Value from Bayes:  0.902654867256637"

#####################################

# B) Hospital

## B 1) Hospital 1
conditional_bayes_probability_distributions(data, "Hospital", "A")
# "Expected value:  0.931034482758621"
# "Value from Bayes:  0.931034482758621"

## B 2) Hospital 2
conditional_bayes_probability_distributions(data, "Hospital", "B")
# "Expected value:  0.950617283950617"
# "Value from Bayes:  0.950617283950617"

## B 3) Hospital 3
conditional_bayes_probability_distributions(data, "Hospital", "C")
# "Expected value:  0.885416666666667"
# "Value from Bayes:  0.885416666666667"

#####################################

