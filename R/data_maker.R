library(tidyverse)
library(nhts2017)

set.seed(42)

# function to code factor levels as the attribute labels
relabel <- function(x){as_factor(x, levels = "labels")}

# Sample down the households ======================
my_hh <- nhts_households %>%
  filter(msasize %in% c("04", "05")) %>% # households that live in metro areas more than 1M
  select(houseid, hhvehcnt, hhsize, hhfaminc) %>%
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
out_data <- my_persons %>%
  left_join(my_hh, by = "houseid") %>%
  filter(r_age > 18) %>%
  transmute(
    id = str_c(houseid, personid),
    dap = ifelse(is.na(DAP), "H",  DAP),
    dap2 = ifelse(is.na(DAP_sub), "H", DAP_sub),
    r_age, educ, r_hisp, r_sex, r_race, 
    worker, 
    disttowk17 = ifelse(disttowk17 < 0, NA, disttowk17),
    hhvehcnt, hhsize, hhfaminc
  )  %>%
  mutate( across(c(educ, r_hisp, r_sex, r_race, worker, hhfaminc), relabel) )


write_rds(out_data, "data/person_dap.rds")
