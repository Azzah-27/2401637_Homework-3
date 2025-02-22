library(tidyverse)

#################### Question 1 ############################

#Load the data 
rhyming_data <- read.csv('data/rhyming.csv')

#tidying the data
rhyming_data_2 <- rhyming_data %>%
  unite(stimulus, image_1, image_2, sep = "_") %>% #this is to merge columns image_1 and image_2 into a new column named stimulus  
mutate (participant_number = row_number()) %>%
 select(stimulus, rt, correct, type, VerbalScored, high_low_verbal, participant_number)