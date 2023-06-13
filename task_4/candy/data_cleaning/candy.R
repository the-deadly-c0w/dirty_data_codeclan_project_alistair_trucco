

library(tidyverse)
library(janitor)
library(assertr)
library(readxl)

here::here()


candy_2015 <- read_xlsx("Desktop/CodeClan Origins Part 2/dirty_data_codeclan_project_alistair_trucco/task_4/candy/data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_xlsx("Desktop/CodeClan Origins Part 2/dirty_data_codeclan_project_alistair_trucco/task_4/candy/data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_xlsx("Desktop/CodeClan Origins Part 2/dirty_data_codeclan_project_alistair_trucco/task_4/candy/data/boing-boing-candy-2017.xlsx")



# removing all "q1_"s from 2017

names(candy_2017) <-  str_remove(names(candy_2017), "^...")


#initial clean of column names

candy_2015_names <- candy_2015 %>% 
  clean_names()
candy_2016_names <- candy_2016 %>% 
  clean_names()
candy_2017_names <- candy_2017 %>% 
  clean_names()

# we don't need the 

candy_2015_chopped <- candy_2015_names %>% 
  select(2:96, 115)

candy_2016_chopped <- candy_2016_names %>% 
  select(2:106,-6)

candy_2017_chopped <- candy_2017_names %>% 
  select(2:109,-6)


# standardising col names for age, country, etc.
 
candy_2015_renamed <- candy_2015_chopped %>% 
  rename("age" = "how_old_are_you",
         "going_out" = "are_you_going_actually_going_trick_or_treating_yourself",
  )


candy_2016_renamed <- candy_2016_chopped %>% 
rename("age" = "how_old_are_you",
       "gender" = "your_gender",
       "going_out" = "are_you_going_actually_going_trick_or_treating_yourself",
       "country" = "which_country_do_you_live_in",
       )




#                          AGE


# changing the age column to integer, characters will be turned to NAs

candy_2015_age <- candy_2015_renamed %>% 
  mutate(age = as.integer(age))


candy_2016_age <- candy_2016_renamed %>% 
  mutate(age = as.integer(age))

candy_2017_age <- candy_2017_chopped %>% 
  mutate(age = as.integer(age))


#candy_2015_age %>% 
#  filter(is.na(age)) %>% 
#  nrow()
# NAs at 285 compared with 200

#candy_2016_renamed %>% 
#  filter(is.na(age)) %>% 
#  nrow()
# NAs at 68 compared with 68



#                         GOING OUT


# changing "yes" "no" to a boolean for convenience

candy_2015_go <- candy_2015_age %>% 
  mutate(going_out = str_detect(going_out, "Yes"))

candy_2016_go <- candy_2016_age %>% 
  mutate(going_out = str_detect(going_out, "Yes"))

candy_2017_go <- candy_2017_age %>% 
  mutate(going_out = str_detect(going_out, "Yes"))

candy_2017_go %>% 
  filter(is.na(going_out)) %>% 
  nrow()



#                         YEAR COLUMNS



candy_2015_year <- candy_2015_go %>% 
  mutate(year = 2015)

candy_2016_year <- candy_2016_go %>% 
  mutate(year = 2016)

candy_2017_year <- candy_2017_go %>% 
  mutate(year = 2017)



#                     COUNTRY




# convert all countries to lower case to reduce mismatching cases

candy_2016_year <- candy_2016_year %>% 
  mutate(country = str_to_lower(country))
candy_2017_year <- candy_2017_year %>%
  mutate(country = str_to_lower(country))







# edit all countries to have consistent names across all years

candy_2016_year$country <- case_when(str_detect(candy_2016_year$country, "kingdom|u.k.|kindom|england|endland|scotland") ~ "uk",
                                     str_detect(candy_2016_year$country, "state|alaska|new jersey|pittsburgh|california|usa|u\\.s\\.|america|united s|^us$|murica|merica|ussa|trumpistan|yoo|cascadia|u s a|murrika|amerca|new york|u s|us of a|north carolina|eua") ~ "usa",
                                     str_detect(candy_2016_year$country, "españa") ~ "spain",
                                     str_detect(candy_2016_year$country, "the netherlands") ~ "netherlands",
                                     str_detect(candy_2016_year$country, "can") ~ "canada",
                                     str_detect(candy_2016_year$country, "[0-9]|^a$|^ud$|atlantis|narnia|europe|earth|old men|insanity lately|this|see above|denial|god\\'s|somewhere|best ones|neverland|equator|i don't know anymore|fear and loathing") ~ NA,
                                     .default = candy_2016_year$country)



candy_2017_year$country <- case_when(str_detect(candy_2017_year$country, "kingdom|u.k.|kindom|england|endland|scotland") ~ "uk",
                                     str_detect(candy_2017_year$country, "state|alaska|new jersey|pittsburgh|california|usa|u\\.s\\.|america|united s|^us$|murica|merica|ussa|trumpistan|yoo|cascadia|u s a|murrika|amerca|new york|u s|us of a|north carolina|eua") ~ "usa",
                                     str_detect(candy_2017_year$country, "españa") ~ "spain",
                                     str_detect(candy_2017_year$country, "the netherlands") ~ "netherlands",
                                     str_detect(candy_2017_year$country, "can") ~ "canada",
                                     str_detect(candy_2017_year$country, "[0-9]|^a$|^ud$|atlantis|narnia|europe|earth|old men|insanity lately|this|see above|denial|god\\'s|somewhere|best ones|neverland|equator|i don't know anymore|fear and loathing") ~ NA,
                                     .default = candy_2017_year$country)



# candy_2016_renamed <- candy_2016_renamed %>% 
#   mutate(country_clean = 
#            if(str_detect(country_clean, "state")){
#             "usa"
# })
# 
# 
# for(i in 1:length(candy_2016_renamed$country_clean)){
#   if(str_detect(candy_2016_renamed$country_clean[i], "state")){
#     candy_2016_renamed$country_clean[i] = "usa"
# }
#}
#setdiff(colnames(candy_2015_chopped), colnames(candy_2016_chopped)) 
#setdiff(colnames(candy_2016_chopped), colnames(candy_2015_chopped))
#setdiff(colnames(candy_2016_chopped), colnames(candy_2017_chopped)) 




#                 PIVOTING LONG 

candy_2015_pivot <- pivot_longer(candy_2015_year,
             cols = "butterfinger":"necco_wafers",
             names_to = "candy",
             values_to = "rating",
             values_drop_na = TRUE)

candy_2016_pivot <- pivot_longer(candy_2016_year,
             cols = "x100_grand_bar":"york_peppermint_patties",
             names_to = "candy",
             values_to = "rating",
             values_drop_na = TRUE)

candy_2017_pivot <- pivot_longer(candy_2017_year,
             cols = "x100_grand_bar":"york_peppermint_patties",
             names_to = "candy",
             values_to = "rating",
             values_drop_na = TRUE)


#to make 2015 compatible with later years


candy_2015_pivot <- add_column(candy_2015_pivot, NA, .after = "going_out") %>% 
  rename("gender" = "NA")


candy_2015_pivot <- add_column(candy_2015_pivot, NA, .after = "gender") %>% 
  rename("country" = "NA")


gigachad <- bind_rows(candy_2015_pivot, candy_2016_pivot, candy_2017_pivot)
