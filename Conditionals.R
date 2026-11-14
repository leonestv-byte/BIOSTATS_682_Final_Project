library(readr)

data <- read_csv("ext_2025.csv")

names(data)
# "No"             "Hospital"       "Age"            "Sex"            "Disease2"      
# "Admission_date" "Discharge_date" "Outcome"

data$Hospital <- factor(data$Hospital)
data$Sex <- factor(data$Sex)
data$Disease2 <- factor(data$Disease2)
data$Outcome <- factor(data$Outcome)

# Simple Age Linear Regression Model
model <- lm(Age ~ Hospital + Sex + Disease2, data = data, na.action = na.exclude)
