library(tidyverse)
library(lme4)

#################### Question 1 ############################

#Load the data 
rhyming_data <- read.csv('data/rhyming.csv')

#tidying the data
rhyming_data_2 <- rhyming_data %>%
  unite(stimulus, image_1, image_2, sep = "_") #this is to merge columns image_1 and image_2 into a new column named stimulus  


# Calculate the mean reaction time for each combination of `type` and `high_low_verbal`
summary_data <- rhyming_data_2 %>%
  group_by(type, high_low_verbal) %>%
  summarise(mean_rt = mean(rt))

# Plot the data using a bar graph
ggplot(summary_data, aes(x = type, y = mean_rt, fill = high_low_verbal)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Reaction Times by Stimulus Type and Level of Inner Voice",
       x = "Stimulus Type",
       y = "Reaction Time (ms)") +
  theme_minimal() 


#Chose a linear mixed-effects model (LMM) to analyze the data
# Justification:
  #A mixed-effects model can account for both fixed effects (stimulus type, level of inner voice) and random effects (participant variability)
  #The data has multiple trials per participant.
rhyming_model <- lmer(data = rhyming_data, rt ~ type * high_low_verbal + (1 | worker_id))
summary(rhyming_model)

# Justification:
# - The model includes `type` and `high_low_verbal` as fixed effects, and their interaction.
# - The random effect `(1 | worker_id)` accounts for variability between participants.
# - This structure allows us to test whether the effect of stimulus type on reaction times depends on the level of inner speech.
#each person has an individual slope but no intercept 
#each person goes through one trial and one condition 

# To check for significance of fixed effects
anova(rhyming_model)

#Interpret the output




########################## QUESTION 2 ###############################


#####3.1
#beta distribution consists of 2 parameters: alpha and beta 
#The beta distribution is a continuous probability distribution defined on the interval [0, 1]
#To make our dependent variable fit within this interval
#Our DV here is the percentages of marks. we need to convert these percentages to decimals to fit within this range
#The Beta distribution takes two shape parameters: alpha (α) and beta (β).
# Since the Beta distribution is defined on the interval [0, 1], we need to rescale the marks (0 to 100) to [0, 1].
#we divide the percentages by 100 (considering marks are out of 100)
   #For example: 
     # - A mark of 67% would become 0.67
     # - A mark of 85% would become 0.85



####3.2 
# Set seed for reproducibility
set.seed(100)

# Simulate marks out of 100 (as an example)
x <- seq(0, 100, by = 1)  # Marks range from 0 to 100

# Rescaling marks to fit into the [0, 1] interval
marks <- x / 100

# Define parameters for the Beta distribution
alpha <- 10
beta <- 7

# Calculate the probability density function (PDF) for the Beta distribution
y <- dbeta(marks, alpha, beta)

# Create a data frame for plotting
beta_data <- tibble(x = marks, y = y)

# Plot the Beta distribution
ggplot(beta_data, aes(x = marks, y = y)) +
  geom_line(color = "red", linewidth = 1.0) +
  labs(title = "Example Beta Distribution for Marks") +
  theme_minimal()


#####3.3

# Defining parameters for informative prior: based on prior knowledge
alpha_informative <- rnorm(n= 1, mean= 67, sd=5)
beta_informative <- rnorm(n= 1, mean= 45, sd=5)

# Defining parameters for weakly-informative prior: Broad and less specific
alpha_weakly <- rnorm(n= 1, mean= 50, sd=20)
beta_weakly <-  rnorm(n= 1, mean= 45, sd=15)

# Calculate densities for informative and weakly informative priors
y_informative <- dbeta(marks, alpha_informative, beta_informative)
y_weakly <- dbeta(marks, alpha_weakly, beta_weakly)

# Creating tables for plotting
informative_data <- tibble(x = marks, y = y_informative, Prior = "Informative")
weakly_data <- tibble(x = marks, y = y_weakly, Prior = "Weakly-Informative")

# Plot for Informative Prior
ggplot(informative_data, aes(x = x, y = y)) +
  geom_path(color = "blue", size = 1) +
  labs(title = "Informative Prior",
       x = "Marks",
       y = "Density") +
  theme_minimal()

# Plot for Weakly-Informative Prior 
ggplot(weakly_data, aes(x = x, y = y)) +
  geom_path(color = "red", size = 1) +  
  labs(title = "Weakly-Informative Prior",
       x = "Marks ",
       y = "Density") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_minimal()



#### 3.4 

# Function to compute the density for a given alpha and beta
gen_prior_pred <- function(n, alpha_prior, beta_prior) { 
  x <- seq(0, 1, 0.01)  # Normalized marks in [0, 1]
  d <- tibble(n = n, alpha = alpha_prior, beta = beta_prior, 
              x = x, 
              y = dbeta(x, alpha_prior, beta_prior)) 
  return(d) 
}


# Create informative priors (narrow and specific)
priors_informative <- tibble(n = 1:50) %>% 
  group_by(n) %>% 
  mutate(alpha_prior = alpha_informative,  # Informative alpha (centered around 67)
         beta_prior = beta_informative    # Informative beta (centered around 45)
  )

# Create weakly informative priors (broad and less specific)
priors_weakly <- tibble(n = 1:50) %>% 
  group_by(n) %>% 
  mutate(alpha_prior = alpha_weakly,  # Weakly informative alpha (broad distribution)
         beta_prior = beta_weakly   # Weakly informative beta (broad distribution)
  )


# Apply function to each prior sample for informative priors
prior_llh_informative <- pmap_df(priors_informative, gen_prior_pred)

# Apply function to each prior sample for weakly informative priors
prior_llh_weakly <- pmap_df(priors_weakly, gen_prior_pred)

# Plot for Informative Priors
ggplot(prior_llh_informative, aes(x, y, group = interaction(alpha, beta))) +  
  geom_path(alpha = 0.50, color = "blue") +  
  labs(title = "Informative Priors",
       x = "Marks",
       y = "Density") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_minimal()

# Plot for Weakly Informative Priors
ggplot(prior_llh_weakly, aes(x, y, group = interaction(alpha, beta))) +  
  geom_path(alpha = 0.50, color = "red") +  
  labs(title = "Informative Priors",
       x = "Marks",
       y = "Density") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_minimal()





