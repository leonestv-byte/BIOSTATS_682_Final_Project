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

# pi(B | Recovered) \propto P(Recovered|B) P(B)
# pi(B | Recovered) \propto Pi(pi^{yi})(1-pi)^{1-y_i} P(B)

# Calculate probabilities - this is useful for offline work.
probabilities <- predict(model, type = "response")
probabilities

# Work:
# $$P(\textbf{Recovered}| \textbf{Sex, Hospital}) = \frac{1}{1+e^{0.6371 + 0.6141SexMale -0.6937HospitalB  -1.9604HospitalC}}$$
#  $$ P(\beta |\textbf{Recovered} ) \propto L(\beta | \textbf{Recovered}) P (\beta)$$
#  $$ \pi(\beta) \propto |J(\beta)|^{1/2}$$
#  $$ |J(\beta)| = -E[\frac{\partial^2ln(\pi(y | \beta))}{\partial} | \beta]$$
#  $$ |J(\beta)| = -E[\frac{\partial^2 l(\beta)}{\partial} | \beta] $$ \newline
# Let us define $\eta = 0.6371 + 0.6141SexMale -0.6937HospitalB  -1.9604HospitalC$
#  $$ p(\beta) = \frac{1}{1+e^\eta}$$
#  $$ln(l(\beta)) = yln(p) + (1-y)ln(p)$$
#  $$ln(l(\beta)) = yln(\frac{1}{1+e^\eta}) + (1-y)ln(1 - \frac{1}{1+e^\eta})$$
#  $$ln(l(\beta)) = -yln(1+e^\eta) + (1-y)ln(1+e^\eta)$$
#  $$ln(l(\beta)) \partial \beta = (1-y) -  \frac{e^\eta}{1 + e^\eta} = (1-y) - (1-p) = p - y$$
#  $$ln(l(\beta)) \partial^2 \beta = -p(1-p)$$
# $$ |J(\beta)| = -E[ -p(1-p) XX^T| \beta] $$
# $$ |J(\beta)| = E[ p(1-p) XX^T| \beta] $$
# $$\pi(\beta) \propto \sqrt{det(X^Tdiag(p_i(1-p_i))X)}$$
# Now, we calculate the posterior, the probability of recovered given beta.
# $$ P(\beta | \textbf{Recovered}) \propto \Pi_{i=1}^n p_i^{y_i} (1-p_i)^{1-y_i} \sqrt{det(X^Tdiag(p_i(1-p_i))X)} $$
  

