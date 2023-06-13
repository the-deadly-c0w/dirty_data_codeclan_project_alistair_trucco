

library(tidyverse)
library(janitor)
library(assertr)
library(readxl)

here::here()


candy_2015 <- read_xlsx("Desktop/CodeClan Origins Part 2/dirty_data_codeclan_project_alistair_trucco/task_4/candy/data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_xlsx("Desktop/CodeClan Origins Part 2/dirty_data_codeclan_project_alistair_trucco/task_4/candy/data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_xlsx("Desktop/CodeClan Origins Part 2/dirty_data_codeclan_project_alistair_trucco/task_4/candy/data/boing-boing-candy-2017.xlsx")


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
  select(2:106)

candy_2017_chopped <- candy_2017_names %>% 
  select(2:109)


# removing all "q1_"s from 2017

names(candy_2017_chopped) <-  str_remove(names(candy_2017_chopped), "^...")

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



#                     COUNTRY




# finding all spellings of all countries to decide how to standardise them


unique(candy_2016_renamed$country)
#93
# holy shit

unique(candy_2017_chopped$country)
# 118

# h o l y s h i t
# it's a lucky thing for the participants that 
# they didn't include address or emails in this survey 

# start by changing everything to lower case, should rid us of a lot of overlap

candy_2016_renamed <- candy_2016_renamed %>% 
  mutate(country_clean = str_to_lower(country))

candy_2017_chopped <- candy_2017_chopped %>% 
  mutate(country_clean = str_to_lower(country))


unique(candy_2016_renamed$country_clean)
#75


unique(candy_2017_chopped$country_clean)
# 96


# testing if only usa entries have the word state

test <- candy_2016_renamed %>% 
  filter(str_detect(country_clean, "state")) %>% 
  select(country_clean)
unique(test)



candy_2016_renamed <- candy_2016_renamed %>% 
  mutate(country_clean = 
           if(str_detect(country_clean, "state")){
            "usa"
})


for(i in 1:length(candy_2016_renamed$country_clean)){
  if(str_detect(candy_2016_renamed$country_clean[i], "state")){
    candy_2016_renamed$country_clean[i] = "usa"
}
}
#setdiff(colnames(candy_2015_chopped), colnames(candy_2016_chopped)) 
#setdiff(colnames(candy_2016_chopped), colnames(candy_2015_chopped))
#setdiff(colnames(candy_2016_chopped), colnames(candy_2017_chopped)) 


test <- candy_2016_renamed %>% 
  filter(str_detect(country_clean, "state")) %>% 
  select(country_clean)
unique(test)






#                          AGE


# changing the 

candy_2015_age <- candy_2015_renamed %>% 
  mutate(age = as.integer(age))


candy_2016_age <- candy_2016_renamed %>% 
  mutate(age = as.integer(age))


#candy_2015_age %>% 
#  filter(is.na(age)) %>% 
#  nrow()
#285 compared with 200

#candy_2016_renamed %>% 
#  filter(is.na(age)) %>% 
#  nrow()
#68 compared with 68


