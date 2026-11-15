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
  
  #N
  print(paste("N: ", nrow(sub_data)))
  
  # Plot
  plot(density(samples_posterior))
}


## A 1) Male
conditional_bayes_probability_distributions(data, "Sex", "Male")
# "Expected value:  0.935483870967742"
# "Value from Bayes:  0.935483870967742"
# "N:  93"

## A 2) Female
conditional_bayes_probability_distributions(data, "Sex", "Female")
# "Expected value:  0.902654867256637"
# "Value from Bayes:  0.902654867256637"
# "N:  113"


#####################################

# B) Hospital

## B 1) Hospital 1
conditional_bayes_probability_distributions(data, "Hospital", "A")
# "Expected value:  0.931034482758621"
# "Value from Bayes:  0.931034482758621"
# "N:  29"

## B 2) Hospital 2
conditional_bayes_probability_distributions(data, "Hospital", "B")
# "Expected value:  0.950617283950617"
# "Value from Bayes:  0.950617283950617"
# "N:  81"

## B 3) Hospital 3
conditional_bayes_probability_distributions(data, "Hospital", "C")
# "Expected value:  0.885416666666667"
# "Value from Bayes:  0.885416666666667"
# "N:  96"

#####################################

# C) Disease

## C 1) Gastrointestinal
conditional_bayes_probability_distributions(data, "Disease2", "Gastrointestinal")
# "Expected value:  0.985714285714286"
# "Value from Bayes:  0.985714285714286"
# "N:  70"

## C 2) Musculoskeletal
conditional_bayes_probability_distributions(data, "Disease2", "Musculoskeletal")
# "Expected value:  0.76"
# "Value from Bayes:  0.76"
# "N:  25"

## C 3) Integument
conditional_bayes_probability_distributions(data, "Disease2", "Integument")
# "Expected value:  0.95"
# "Value from Bayes:  0.95"
# "N:  40"

## C 4) TB
conditional_bayes_probability_distributions(data, "Disease2", "TB")
# "Expected value:  0"
# "Value from Bayes:  0"
# "N:  1"


## C 5) Eye
conditional_bayes_probability_distributions(data, "Disease2", "Eye")
# "Expected value:  1"
# "Value from Bayes:  1"
# "N:  33"

## C 6) EEHV
conditional_bayes_probability_distributions(data, "Disease2", "EEHV")
# "Expected value:  0.875"
# "Value from Bayes:  0.875"
# "N:  16"

## C 7) Dental/Oral
conditional_bayes_probability_distributions(data, "Disease2", "Dental/Oral")
# "Expected value:  0.5"
# "Value from Bayes:  0.5"
# "N:  2"

## C 8) Reproductive
conditional_bayes_probability_distributions(data, "Disease2", "Reproductive")
# "Expected value:  0.666666666666667"
# "Value from Bayes:  0.666666666666667"
# "N:  6"

## C 9) Miscellaneous
conditional_bayes_probability_distributions(data, "Disease2", "Miscellaneous")
# "Expected value:  1"
# "Value from Bayes:  1"
# "N:  7"

## C 10) Other
conditional_bayes_probability_distributions(data, "Disease2", "Other")
# "Expected value:  0"
# "Value from Bayes:  0"
# "N:  1"

## C 11) Urinary/Renal
conditional_bayes_probability_distributions(data, "Disease2", "Urinary/Renal")
# "Expected value:  1"
# "Value from Bayes:  1"

## C 12) Respiratory
conditional_bayes_probability_distributions(data, "Disease2", "Respiratory")
# "Expected value:  0.666666666666667"
# "Value from Bayes:  0.666666666666667"
# "N:  1"

## C 13) Toxicity
conditional_bayes_probability_distributions(data, "Disease2", "Toxicity")
# "Expected value:  1"
# "Value from Bayes:  1"
# "N:  1"

#####################################


