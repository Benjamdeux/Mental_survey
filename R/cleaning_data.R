# 1. Packages et données --------------------------------------------------
Packages <- c('tidyverse','readr','questionr')
lapply(Packages, library, character.only = TRUE)


df_raw <- read_csv("raw_data/survey.csv")


# 1. Clean Gender ---------------------------------------------------------
# Only keep male or female individuals
interest <- c('female','Female','male','Male','woman','Woman','Man','man')

df_raw <- df_raw %>%
  filter(Gender %in% interest) %>%
  mutate(Gender = str_to_title(Gender)) %>%
  mutate(Gender = if_else(Gender == 'Man' |
                            Gender == 'Male', 'Male', 'Female'))

# 2. Clean Age ------------------------------------------------------------
# Only keep individuals between 15 and 80 years old

df_raw <- df_raw %>% 
  filter(Age > 15 & Age < 80) 



# 3. Check Na ----------------------------------------

df_raw %>%
  map(\(x) sum(is.na(x)))

# Important number of NA for :
# * State : coherent with the fact that only US citizens will have an element in this column
# * Self employed, work_inference
# * Comments : A large part of the sample didn't want to include comments


# 4. Delete Timestamp and transform to factor -----------------------------

# Delete Timestamp
df_raw <- df_raw %>%
  select(-Timestamp)

# Create a vector of non-factor variables and transform all other varibales to factors
non_factor <- c('Age','Gender', 'Country', 'state', 'comments')
df_raw <- df_raw %>%
  mutate(across(!all_of(non_factor), \(x) as.factor(x)))

# Relevel no_employees 
df_raw <- df_raw %>%
  mutate(no_employees = fct_relevel(no_employees,
    "1-5", "6-25", "26-100", "100-500", "500-1000", "More than 1000"
  ))

write.csv(df_raw, file = 'clean_data/clean_data.csv', row.names = FALSE)



           