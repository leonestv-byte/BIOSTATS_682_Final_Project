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

# Sex A)

## A 1) Male

male_data <- data[data$Sex == 'Male', ]

alpha_posterior <- nrow(male_data[male_data$Outcome == 'Recovered',])
beta_posterior <- nrow(male_data[male_data$Outcome == 'Deceased',])

samples_posterior <- rbeta(1000, alpha_posterior, beta_posterior)
plot(density(samples_posterior))


alpha_posterior / (alpha_posterior + beta_posterior) # 93%

# Bayes Stats
# P(R | M) = P(M | R) P(R) / P(M)
p_m <- nrow(male_data) / nrow(data)
p_r <- nrow(data[data$Outcome == 'Recovered',]) / nrow(data)
p_m_given_r <- nrow(data[data$Outcome == 'Recovered' & data$Sex == 'Male',]) / nrow(data[data$Outcome == 'Recovered',])
(p_m_given_r * p_r) / p_m # 93% 

## A 2) Female

female_data <- data[data$Sex == 'Female', ]

alpha_posterior <- nrow(female_data[female_data$Outcome == 'Recovered',])
beta_posterior <- nrow(female_data[female_data$Outcome == 'Deceased',])

samples_posterior <- rbeta(1000, alpha_posterior, beta_posterior)
plot(density(samples_posterior))

alpha_posterior / (alpha_posterior + beta_posterior) # 90%

# Bayes Stats
# P(R | F) = P(F | R) P(R) / P(F)
p_f <- nrow(female_data) / nrow(data)
p_r <- nrow(data[data$Outcome == 'Recovered',]) / nrow(data)
p_f_given_r <- nrow(data[data$Outcome == 'Recovered' & data$Sex == 'Female',]) / nrow(data[data$Outcome == 'Recovered',])
(p_f_given_r * p_r) / p_f # 90% 


# B) Hospital


## B 1) Hospital 1

h1_data <- data[data$Hospital == 'A', ]

alpha_posterior <- nrow(h1_data[h1_data$Outcome == 'Recovered',])
beta_posterior <- nrow(h1_data[h1_data$Outcome == 'Deceased',])

samples_posterior <- rbeta(1000, alpha_posterior, beta_posterior)
plot(density(samples_posterior))

alpha_posterior / (alpha_posterior + beta_posterior) # 93%

# Bayes Stats
# P(R | F) = P(F | R) P(R) / P(F)
p_h1 <- nrow(h1_data) / nrow(data)
p_r <- nrow(data[data$Outcome == 'Recovered',]) / nrow(data)
p_h1_given_r <- nrow(data[data$Outcome == 'Recovered' & data$Hospital == 'A',]) / nrow(data[data$Outcome == 'Recovered',])
(p_h1_given_r * p_r) / p_h1 # 93% 


## B 2) Hospital 2
h2_data <- data[data$Hospital == 'B', ]

alpha_posterior <- nrow(h2_data[h2_data$Outcome == 'Recovered',])
beta_posterior <- nrow(h2_data[h2_data$Outcome == 'Deceased',])

samples_posterior <- rbeta(1000, alpha_posterior, beta_posterior)
plot(density(samples_posterior))

alpha_posterior / (alpha_posterior + beta_posterior) # 95%

# Bayes Stats
# P(R | F) = P(F | R) P(R) / P(F)
p_h2 <- nrow(h2_data) / nrow(data)
p_r <- nrow(data[data$Outcome == 'Recovered',]) / nrow(data)
p_h2_given_r <- nrow(data[data$Outcome == 'Recovered' & data$Hospital == 'B',]) / nrow(data[data$Outcome == 'Recovered',])
(p_h2_given_r * p_r) / p_h2 # 95% 

## B 3) Hospital 3
h3_data <- data[data$Hospital == 'C', ]

alpha_posterior <- nrow(h3_data[h3_data$Outcome == 'Recovered',])
beta_posterior <- nrow(h3_data[h3_data$Outcome == 'Deceased',])

samples_posterior <- rbeta(1000, alpha_posterior, beta_posterior)
plot(density(samples_posterior))

alpha_posterior / (alpha_posterior + beta_posterior) # 88%

# Bayes Stats
# P(R | F) = P(F | R) P(R) / P(F)
p_h3 <- nrow(h3_data) / nrow(data)
p_r <- nrow(data[data$Outcome == 'Recovered',]) / nrow(data)
p_h3_given_r <- nrow(data[data$Outcome == 'Recovered' & data$Hospital == 'C',]) / nrow(data[data$Outcome == 'Recovered',])
(p_h3_given_r * p_r) / p_h3 # 88% 

