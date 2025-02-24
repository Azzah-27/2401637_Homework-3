library(tidyverse)
library(lme4)

#################### Question 1 ############################

#Load the data 
rhyming_data <- read.csv('data/rhyming.csv')

#tidying the data
rhyming_data_2 <- rhyming_data %>%
  unite(stimulus, image_1, image_2, sep = "_") %>% #this is to merge columns image_1 and image_2 into a new column named stimulus  
mutate (participant_number = row_number()) %>%
 select(stimulus, rt, correct, type, VerbalScored, high_low_verbal, participant_number)


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

#fitting a model 
rhyming_model <- lmer(data = rhyming_data_2, rt ~ type * high_low_verbal + (1|participant_number))
summary(rhyming_model)   #doesnt work


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


# To check for significance of fixed effects
anova(rhyming_model)

#Interpret the output




########################## QUESTION 2 ###############################


#####3.1
#beta distribution consists of 2 parameters: alpha and beta 
#The beta distribution is a continuous probability distribution defined on the interval [0, 1]
#To make our dependent variable fit within this interval
#Our DV here is the percentages of marks. we need to convert these percentages to decimals to fit within this range
#we divide the percentages by 100 (considering marks are out of 100)
   #For example: 
     # - A mark of 67% would become 0.67
     # - A mark of 85% would become 0.85



####3.2 

# Define the range of values for marks (0 to 1)
marks <- seq(0, 1, by = 0.01)

# Define parameters for the Beta distribution
alpha <- as.numeric(13)  
beta <- as.numeric(7)   

# Calculate the probability density function (PDF) for the Beta distribution
y <- dbeta(marks, alpha, beta)

# Create a data frame for plotting
beta_data <- tibble(x = marks, y = y)

# Plot the Beta distribution
ggplot(beta_data, aes(x = marks, y = y)) +
  geom_line(color = "red", linewidth = 1.5) 


#####3.3

# Defining parameters for informative prior
alpha_informative <- as.numeric(12)
beta_informative <- as.numeric(7)  

#create a rnorm for both 

# Defining parameters for weakly-informative prior
alpha_weakly <- as.numeric(1.50)  
beta_weakly <- as.numeric(1.50)  

# Calculate densities for informative and weakly informative priors
y_informative <- dbeta(marks, alpha_informative, beta_informative)
y_weakly <- dbeta(marks, alpha_weakly, beta_weakly)


# Creating tables for plotting
informative_data <- tibble(x = marks, y = y_informative, Prior = "Informative")
weakly_data <- tibble(x = marks, y = y_weakly, Prior = "Weakly-Informative")

# Combining data 
prior_data <- rbind(informative_data, weakly_data)

# Plotting informative and weakly informative priors 
ggplot(prior_data, aes(x = x, y = y, color = Prior)) +
  geom_line (size= 0.5) +
  labs(title = "Informative vs. Weakly-Informative Priors",
       x = "Proportion",
       y = "Density") +
  theme_minimal()


#### 3.4 

#using the code from the lecture 
# μ ∼ N(67,5) σ ∼ Exp(0.5)
alpha_prior <- rnorm(1, 67, 5)  
beta_prior <- rexp(1, 0.5)  

# Simulate student marks from a Beta distribution
sim_students <- tibble(marks = rbeta(100, alpha_prior, beta_prior))

#create table of alpha and beta values for priors 
priors <- tibble(n = 1:50) %>% group_by(n) %>% 
  mutate(alpha_prior = runif(1, 1, 5), beta_prior = runif(1, 1, 5))

# Function to compute the density for a given alpha and beta
gen_prior_pred <- function(n, alpha_prior, beta_prior) { 
  
  x <- seq(0, 100, 0.1) 
  d <- tibble(n = n, alpha = alpha_prior, beta = beta_prior, 
              x = x, 
              y = dbeta(x, alpha_prior, beta_prior)) 
  
  return(d) 
} 

# apply function to each prior sample 
prior_llh <- pmap_df(priors, gen_prior_pred)

#plotting prior predictions 
ggplot(prior_llh, aes(x, y, group = alpha_prior)) +  
  geom_path(alpha = 0.50) +
 labs(title = "Beta Distribution for Priors",
       x = "Marks",
       y = "Density") 
  







