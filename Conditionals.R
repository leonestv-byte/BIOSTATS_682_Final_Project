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

# Logistic Regression Model 
model <- glm(Outcome ~ Hospital + Sex + Disease2, data = data, family = "binomial")

##################
##### Priors #####
##################

# A) Noninformative Prior.

alpha <- 50
beta <- 50

alpha_posterior <- alpha + nrow(data[data$Outcome == 'Recovered',])
beta_posterior <- beta + nrow(data[data$Outcome == 'Deceased',])

samples_posterior <- rbeta(1000, alpha_posterior, beta_posterior)
samples_prior <- rbeta(1000, alpha, beta)
plot(density(samples_posterior))
lines(density(samples_prior))

# B) Prior from Research Literature 

alpha <- 85
beta <- 15

alpha_posterior <- alpha + nrow(data[data$Outcome == 'Recovered',])
beta_posterior <- beta + nrow(data[data$Outcome == 'Deceased',])

samples_posterior <- rbeta(1000, alpha_posterior, beta_posterior)
samples_prior <- rbeta(1000, alpha, beta)
plot(density(samples_posterior))
lines(density(samples_prior))

#####################################
##### Conditional Distributions #####
#####################################

# A) Male

male_data <- data[data$Sex == 'Male', ]

alpha <- nrow(male_data[male_data$Outcome == 'Recovered',])
beta <- nrow(male_data[male_data$Outcome == 'Deceased',])

samples_posterior <- rbeta(1000, alpha_posterior, beta_posterior)
plot(density(samples_posterior))


alpha / (alpha + beta) # 93%

# Bayes Stats
# P(R | M) = P(M | R) P(R) / P(M)
p_m <- nrow(male_data) / nrow(data)
p_r <- nrow(data[data$Outcome == 'Recovered',]) / nrow(data)
p_m_given_r <- nrow(data[data$Outcome == 'Recovered' & data$Sex == 'Male',]) / nrow(data[data$Outcome == 'Recovered',])
(p_m_given_r * p_r) / p_m # 93% 

# B) Female

female_data <- female_data[female_data$Sex == 'Female', ]

alpha <- nrow(female_data[female_data$Outcome == 'Recovered',])
beta <- nrow(female_data[female_data$Outcome == 'Deceased',])

samples_posterior <- rbeta(1000, alpha_posterior, beta_posterior)
plot(density(samples_posterior))

alpha / (alpha + beta) # 90%

# Bayes Stats
# P(R | F) = P(F | R) P(R) / P(F)
p_f <- nrow(female_data) / nrow(data)
p_r <- nrow(data[data$Outcome == 'Recovered',]) / nrow(data)
p_f_given_r <- nrow(data[data$Outcome == 'Recovered' & data$Sex == 'Female',]) / nrow(data[data$Outcome == 'Recovered',])
(p_f_given_r * p_r) / p_f # 90% 



