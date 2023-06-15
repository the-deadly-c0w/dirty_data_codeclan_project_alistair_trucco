

library(tidyverse)
library(janitor)
library(assertr)
library(readxl)

here::here()


candy_2015 <- read_xlsx("Desktop/CodeClan Origins Part 2/dirty_data_codeclan_project_alistair_trucco/task_4/candy/data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_xlsx("Desktop/CodeClan Origins Part 2/dirty_data_codeclan_project_alistair_trucco/task_4/candy/data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_xlsx("Desktop/CodeClan Origins Part 2/dirty_data_codeclan_project_alistair_trucco/task_4/candy/data/boing-boing-candy-2017.xlsx")



# removing all "q[n]_"s from 2017

names(candy_2017) <-  str_remove(names(candy_2017), "^...")


#initial clean of column names

candy_2015_names <- candy_2015 %>% 
  clean_names()
candy_2016_names <- candy_2016 %>% 
  clean_names()
candy_2017_names <- candy_2017 %>% 
  clean_names()

# we don't need the time-stamp or id columns 
# nor do we need the questions at the end 
# or the non-candy items

candy_2015_chopped <- candy_2015_names %>% 
  select(2:96, 115,
         -18, -23, -26, -27, -28, -33, -38, -45, -88, -93, #non food items
         -11, -34, -35, -41, -56, -82, -91, -94 , -95)     #non candy food items

candy_2016_chopped <- candy_2016_names %>% 
  select(2:106,-6,
         -12, -14, -15, -21, -26, -27, -31, -32, -43, -79, -102, #non food items
         -22, -38, -39, -49, -69, -90, -92, -99, -104, -105)     #non candy food items

candy_2017_chopped <- candy_2017_names %>% 
  select(2:109,-6,
         -12, -14, -15, -21, -26, -27, -31, -32, -43, -69, -81, -105, #non food items
         -22, -38, -39, -49, -70, -86, -92, -94, -102, -107, -108)    #non candy food items


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




#                         YEAR COLUMNS


# adding year columns to each year for analysis after joining the databases

candy_2015_year <- candy_2015_renamed %>% 
  mutate(year = 2015)

candy_2016_year <- candy_2016_renamed %>% 
  mutate(year = 2016)

candy_2017_year <- candy_2017_chopped %>% 
  mutate(year = 2017)







#                 PIVOTING LONG 



# pivoting all candy long to get ratings into one column

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


# adding missing column names to make 2015 compatible with later years


candy_2015_pivot <- add_column(candy_2015_pivot, NA, .after = "going_out") %>% 
  rename("gender" = "NA")


candy_2015_pivot <- add_column(candy_2015_pivot, NA, .after = "gender") %>% 
  rename("country" = "NA")



# merging all three datasets

candy_joined <- bind_rows(candy_2015_pivot, candy_2016_pivot, candy_2017_pivot)








#                          AGE


# changing the age column to integer, characters will be turned to NAs

candy_age <- candy_joined %>% 
  mutate(age = as.integer(age))

# remove ages beyond the oldest age
candy_age$age <- case_when(candy_age$age > 120 ~ NA,
                                .default = candy_age$age)



#                         GOING OUT


# changing "yes" "no" to a boolean for convenience


# no need for extra conditions as there are no NAs in any of the columns
candy_go <- candy_age %>% 
  mutate(going_out = str_detect(going_out, "Yes"))





#                     COUNTRY




# convert all countries to lower case to reduce mismatching cases

candy_go <- candy_go %>% 
  mutate(country = str_to_lower(country))








# edit all countries to have consistent names across all years
### commented sections are for when not using 'other' as the default 
### to include all countries


candy_go$country <- case_when(str_detect(candy_go$country, "uk|kingdom|u.k.|kindom|england|endland|scotland") ~ "uk",
                                     str_detect(candy_go$country, "usa|state|alaska|new jersey|pittsburgh|california|usa|u\\.s\\.|america|united s|^us$|murica|merica|ussa|trumpistan|yoo|cascadia|u s a|murrika|amerca|new york|u s|us of a|north carolina|eua") ~ "usa",
                                  #  str_detect(candy_go$country, "españa") ~ "spain",
                                  #  str_detect(candy_go$country, "the netherlands") ~ "netherlands",
                                     str_detect(candy_go$country, "canada|can") ~ "canada",
                                     str_detect(candy_go$country, "[0-9]|^a$|^ud$|atlantis|narnia|europe|earth|old men|insanity lately|this|see above|denial|god\\'s|somewhere|best ones|neverland|equator|i don't know anymore|fear and loathing") ~ NA,
                                     .default = "other")


# writing into the data folder

write_csv(candy_go, "Desktop/CodeClan Origins Part 2/dirty_data_codeclan_project_alistair_trucco/task_4/candy/data/candy_clean.csv")
