

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






#setdiff(colnames(candy_2015_chopped), colnames(candy_2016_chopped)) 
#setdiff(colnames(candy_2016_chopped), colnames(candy_2015_chopped))
#setdiff(colnames(candy_2016_chopped), colnames(candy_2017_chopped)) 
