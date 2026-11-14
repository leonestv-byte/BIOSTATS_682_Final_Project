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

alpha <- 0.5
beta <- 0.5

alpha_posterior <- alpha + nrow(data[data$Outcome == 'Recovered',])
beta_posterior <- beta + nrow(data[data$Outcome == 'Deceased',])

samples_posterior <- rbeta(1000, alpha_posterior, beta_posterior)
samples_prior <- rbeta(1000, alpha, beta)
plot(density(samples_posterior))
lines(density(samples_prior))

# B) Prior from Research Literature 

alpha <- 0.85
beta <- 0.15

alpha_posterior <- alpha + nrow(data[data$Outcome == 'Recovered',])
beta_posterior <- beta + nrow(data[data$Outcome == 'Deceased',])

samples_posterior <- rbeta(1000, alpha_posterior, beta_posterior)
samples_prior <- rbeta(1000, alpha, beta)
plot(density(samples_posterior))
lines(density(samples_prior))



