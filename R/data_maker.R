#' Label attributed vector
#' 
#' @param x An attributed vector, as is common in NHTS tables.
#' @return A factor vector with labeled attributes
relabel <- function(x){as_factor(x, levels = "labels")}

#' Sample households from 2017 NHTS
#' 
#' @return A tibble with households
get_households <- function(){
  nhts_households %>%
    filter(msasize %in% c("04", "05")) %>% # households that live in metro areas more than 1M
    mutate(
      income = case_when(
        hhfaminc %in% c("01", "02", "03") ~ "< $25,000",
        hhfaminc %in% c("04", "05") ~ "$25,000 - $50,000",
        hhfaminc %in% c("06", "07") ~ "$50,000 - $100,000",
        hhfaminc %in% c("08", "09", "10", "11") ~ "> $100,000"
      ),
      income = fct_relevel(income, "< $25,000", "$25,000 - $50,000", 
                           "$50,000 - $100,000", "> $100,000"),
      hhfaminc = relabel(hhfaminc)
    ) %>%
    select(houseid, hhvehcnt, hhsize, income, hhfaminc) %>%
    sample_n(1000)
}


#' Get persons from 2017 NHTS for sampled households
#' 
#' @param houseid A vector of household id's from the NHTS
#' @return 
#' 
get_persons <- function(ids){
  nhts_persons %>%
    filter(houseid %in% ids) %>%
    
    # filter out people over 18
    mutate(r_age = ifelse(r_age < 0, r_age_imp, r_age)) %>%
    filter(r_age > 18) %>%
    
    # create person type and wheelchair use variables
    mutate(
      # distance to work
      disttowk17 = ifelse(disttowk17 < 0, NA, disttowk17),
      
      
      # determine wheelchair status
      wheelchair =  case_when(
        # wheelchair status variables
        w_chair == "07" | w_mtrchr == "08" | w_scootr == "06" ~ T,
        TRUE ~ F
      ),
      
      # age groupings
      age_bin = case_when(
        r_age < 40 ~ "05-39",
        r_age < 65 ~ "40-64",
        r_age < 80 ~ "65-79",
        TRUE ~ "80+"
      ),
      
      # Person Type
      person_type = case_when(
        wkftpt == "01" ~ "FW",
        wkftpt == "02" ~ "PW",
        prmact == "03" ~ "NW",
        prmact == "06" & worker == "02" ~ "RT",
        r_age >= 65 & worker == "02" & prmact != "03" ~ "RT",
        r_age >= 18 & worker == "02" ~ "NW", 
        worker == "01" ~ "FW"
      ),
      
      person_type = factor(person_type, levels = c("FW", "PW", "NW", "RT")),
      
    )  %>%
    filter(!is.na(person_type)) %>%
    mutate( across(c(educ, r_hisp, r_sex, r_race, worker), relabel) ) %>%
    select(
      houseid, personid, person_type, r_age, age_bin, educ, r_hisp, r_sex, r_race, 
      worker, wheelchair
      
    ) 
  
}


#' Get trips made by sampled persons and households
#'
#' @param persons A tibble of persons from the nhts
#' @return A tibble with the persons including their 
#' 
get_trips <- function(persons) {
  
  # build 
  tours <- nhts_trips %>%
    filter(houseid %in% persons$houseid) %>%
    build_activities() %>%
    build_tours()
  
  persons %>% 
    left_join(tours, by = c("houseid", "personid"))
  
}

#' Join persons and households together
#' 
#' @param persons_dap persons tibble with DAP attached
#' @param hh households tibble
#' 
#' @return A tibble with the persons including household attributes
#' 
#' @details This join is also necessary to append DAP for individuals who
#'   do not leave their home, and therefore do not show up in the trips data.
#'   
#'   
make_data <- function(person_dap, hh){
  
  # Create choice data ===========
  person_dap %>%
    left_join(hh, by = "houseid") %>%
    mutate(
      id = str_c(houseid, personid),
      dap = ifelse(is.na(DAP), "H",  DAP),
      dap2 = ifelse(is.na(DAP_sub), "H", DAP_sub),
    )   %>%
    select(-DAP, -DAP_sub) 
}



