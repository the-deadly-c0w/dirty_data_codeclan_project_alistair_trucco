

library(tidyverse)
library(janitor)
library(assertr)
library(here)

decathlon <- read_rds("data/decathlon.rds")




## first we are going to change the names from a row name to a named column for
## ease of manipulation


names_vector <- row.names(decathlon)


remove_rownames(decathlon) %>% 
mutate(name = names_vector, .before = "100m") -> decathlon_named


## next we will tidy the names of the columns

decathlon_clean_names <- decathlon_named %>% 
  clean_names() #%>% 
 # rename("100m" = "x100m") %>% 
 # rename("400m" = "x400m") %>% 
 # rename("1500m" = "x1500m") %>% 
 # rename("110m_hurdle" = "x110m_hurdle")
#

# for(i in 1:ncol(decathlon_clean_names)){
#   if (str_detect(colnames(decathlon_clean_names[i]),"^x")){
#           <-  str_remove(colnames(decathlon_clean_names)[i], "x")
#   }
# }



# make sure names are consistent across competitions

decathlon_title <- decathlon_clean_names %>% 
  mutate(name = str_to_title(name))



write_csv(decathlon_title, "data/decathlon_clean.csv")
