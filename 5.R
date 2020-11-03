library(dplyr)
head(welfare)

head(welfare$age)

welfare=welfare %>% 
  mutate(ageg = ifelse(age<30, 'young', ifelse(age<=59, 'middle', 'old')))

table(welfare$ageg)
