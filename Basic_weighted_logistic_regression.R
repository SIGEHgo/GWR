set.seed(123)
survey_data <- data.frame(
  health_outcome = rbinom(100, 1, 0.3),
  smoking_status = rbinom(100, 1, 0.4),
  age = rnorm(100, mean = 50, sd = 10),
  weight = runif(100, min = 1, max = 5)
)

# Inspect the first few rows of the dataset
head(survey_data)

# Fit weighted logistic regression model
weighted_logit_model <- glm(health_outcome ~ smoking_status + age, 
                            family = binomial(link = "logit"), 
                            data = survey_data, 
                            weights = survey_data$weight)

# Summarize the model
summary(weighted_logit_model)
weighted_logit_model$coefficients




#### Otro


set.seed(123)
survey_data <- data.frame(
  health_outcome = rbinom(100, 1, 0.3),
  smoking_status = rbinom(100, 1, 0.4),
  age = rnorm(100, mean = 50, sd = 10),
  weight = 10
)

# Inspect the first few rows of the dataset
head(survey_data)

# Fit weighted logistic regression model
weighted_logit_model <- glm(health_outcome ~ smoking_status + age, 
                            family = binomial(link = "logit"), 
                            data = survey_data, 
                            weights = survey_data$weight)

# Summarize the model
summary(weighted_logit_model)


logit_model <- glm(health_outcome ~ smoking_status + age, 
                   family = binomial(link = "logit"), 
                   data = survey_data)

summary(logit_model)


weighted_logit_model$coefficients
logit_model$coefficients