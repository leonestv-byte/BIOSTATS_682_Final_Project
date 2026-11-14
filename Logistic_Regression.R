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

# We use Jeffrey's Prior to get a prior probability of the model coefficients P(B).
# We combine it with the likelihood to get our probability recovered.

# pi(Recovered | B) \propto L(B|Recovered) P(B)
# pi(Recovered | B) \propto Pi(pi^{yi})(1-pi)^{1-y_i} P(B)


