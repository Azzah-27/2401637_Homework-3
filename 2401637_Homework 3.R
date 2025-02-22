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
summary(rhyming_model)

#Chose a linear mixed-effects model (LMM) to analyze the data
# Justification:
#A mixed-effects model accounts for the variability within participants
#The data has multiple trials per participant
# A mixed-effects model can account for both fixed effects (stimulus type, level of inner voice) and random effects (participant variability)
rhyming_model <- lmer(rt ~ type * high_low_verbal + (1 | worker_id), data = rhyming_data)
summary(rhyming_model)

# To check for significance of fixed effects
anova(rhyming_model)

