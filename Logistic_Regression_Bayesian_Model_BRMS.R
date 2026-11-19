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

#####

library(brms)

fit <- brm(
  Outcome ~ Hospital  + Sex + Disease2 + Age,
  data = data,
  family = bernoulli(),
  chains = 4,
  iter = 2000,
  cores = 4
)

summary(fit)
theta <- posterior_linpred(fit, transform = TRUE)

theta_mean <- colMeans(theta)
y_pred <- ifelse(theta_mean > 0.5, 1, 0)
y_true <- ifelse(data$Outcome == 'Recovered', 1, 0)
accuracy <- mean(y_pred == y_true)
accuracy

plot(density(theta_mean))
