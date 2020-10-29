library(tidyverse)
library(nhts2017)

# Sample down the households ======================
my_hh <- nhts_households %>%
  filter(msasize %in% c("04", "05")) %>% # households that live in metro areas more than 1M
  sample_n(5000)

my_persons <- nhts_persons %>%
  filter(houseid %in% my_hh$houseid)

my_trips <- nhts_trips %>%
  filter(houseid %in% my_hh$houseid)

# Build tour data ==========
my_activities <- build_activities(my_trips)
my_dap <- build_tours(my_activities)

my_persons <- my_persons %>% 
  left_join(my_dap, by = c("houseid", "personid"))

# Create choice data ===========
