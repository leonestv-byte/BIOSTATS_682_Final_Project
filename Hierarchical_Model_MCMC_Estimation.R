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

