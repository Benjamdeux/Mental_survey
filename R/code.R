
# 1. Packages et données --------------------------------------------------
Packages <- c('tidyverse','readr','questionr','rlang')
lapply(Packages, library, character.only = TRUE)

df <- read_csv("clean_data/clean_data.csv")

source('R/functions.R')

# Refaire le changement des variables character en factor
non_factor <- c('Age','Gender', 'Country', 'state', 'comments')
df <- df %>%
  mutate(across(!all_of(non_factor), \(x) as.factor(x)))

# Relevel no_employees 
df <- df %>%
  mutate(no_employees = fct_relevel(no_employees,
                                    "1-5", "6-25", "26-100", "100-500", "500-1000", "More than 1000"))




# 2. Individual's Variables' summary ----------------------------------------

list_of_table <- tibble(var = names(df), Gender = rep('Gender', length(names(df)))) %>%
  filter(!(var %in% c('Age','Gender','comments')))


df %>%
  group_by(Gender) %>%
  summarise(Mean = mean(Age),
            Median = median(Age))
# Quite symmetrical distribution of Age between gender (mean age of 30)


df %>%
  select(c(Gender,Country,self_employed)) %>%
  map(table) %>%
  map(freq)
# Sample's individuals are mostly:
# * Male (81%)
# * American (56%) or British (16 %)
# * Employed by a company (88%)


# 3. Companies' Variables' Summary ----------------------------------------




table_gender(c('care_options','wellness_program','mental_health_consequence',
               'phys_health_consequence'))





work_interfere : If you have a mental health condition, do you feel that it interferes with your work?
  
  no_employees: How many employees does your company or organization have?
  
  remote_work: Do you work remotely (outside of an office) at least 50% of the time?
  
  tech_company: Is your employer primarily a tech company/organization?
  
  benefits: Does your employer provide mental health benefits?
  
