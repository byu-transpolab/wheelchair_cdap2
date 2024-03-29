#' Get persons / households File from activitysim
#' 
#' @param file to data file from activitysim output.
#' @return tibble containing dap information
#' 
asim_persons_base <- function(path){
  if(!file.exists(path)){
    # originally from UTA: June 4 2021
    download.file("https://byu.box.com/shared/static/8r52cwwapl519t4gha9vn2muz2krn1lf.csv", # from box
                  destfile = path)
  } else {
    message(path, " already available")
  }
  return(
    read_csv(path) %>%
      transmute(ID = person_id, hh = household_id, age, sex, ptype, 
                DAP_before = cdap_activity)
  )  
}



asim_persons_wc <- function(path){
  if(!file.exists(path)){
    # originally from UTA: June 4 2021
    download.file("https://byu.box.com/shared/static/d9oaak03j7ktj2mb9y2ha5obtcqcttee.csv", # from box
                  destfile = path)
  } else {
    message(path, " already available")
  }
  
  return(
    read_csv(path) %>%
      transmute(ID = person_id, hh = household_id, age, sex, ptype, 
                DAP_after = cdap_activity, wc_var)
  )  
}

asim_households_wc <- function(path){
  if(!file.exists(path)){
    # originally from UTA: June 4 2021
    download.file("https://byu.box.com/shared/static/2m4scpkwm5ffyr7wooastrdnamuzm3km.csv", # from box
                  destfile = path)
  } else {
    message(path, " already available")
  }
  return(read_csv(path)) # to use file target, need to return path to data. 
}


# ActivitySim join data --------------------------------------------------------

asim_join <- function(persons_base, persons_wc, households_wc){
  
  # set the wc users
  wheelchairs <- persons_wc %>%
    filter(wc_var == 1)
  
  left_join(persons_wc, persons_base, by = c("ID", "hh", "age", "sex", "ptype")) %>%
    # join households with relevant variables
    left_join(
      households_wc %>% 
        transmute(hh=household_id, income, hhsize, auto_ownership, num_workers, income_segment), 
      by=c("hh")
    ) %>%
    mutate(different = ifelse(DAP_after == DAP_before, 0, 1),
           ptype = as_factor(ptype),
           Group = case_when(
             hh %in% wheelchairs$hh & wc_var == FALSE ~ "Household Members",
             hh %in% wheelchairs$hh & wc_var == TRUE ~ "Wheelchair Users",
             TRUE ~ "Not Affected"),
           Group = factor(Group, levels = c("Wheelchair Users", "Household Members", "Not Affected"))
           )
  
}



# ActivitySim dap model estimation results -------------------------------------

#' Build table from dap data to compare before and after dap choices
#' 
#' @param asim_dap data set
#' @return Comparison table
#' 
#' 
#' 
build_table <- function(asim_dap) {
  
  asim_dap %>%
    group_by(Group, DAP_after, DAP_before) %>%
    tally() %>%
    pivot_wider(values_from = n, names_from = DAP_after)
  
}


#' Make a map of affected households
#' 
#' @param asim_dap data set
#' @param asim_persons_wc
#' 
map_affected <- function(asim_dap, households_wc, taz){
  
  changed_dap <- asim_dap %>%
    filter(DAP_after != DAP_before)
  
  n_hh <- households_wc %>%
    group_by(TAZ) %>%
    summarise(n_households = n())
  
  where_changed <- households_wc %>%
    filter(household_id %in% changed_dap$hh) %>%
    group_by(TAZ) %>%
    summarise(n = n())
  
  taz %>%
    select(TAZ = TAZID) %>%
    left_join(n_hh, by = "TAZ") %>%
    left_join(where_changed, by = "TAZ")  %>%
    mutate(
      n_households = ifelse(is.na(n_households), 0, n_households),
      n = ifelse(is.na(n), 0, n),
      pct = ifelse(n_households == 0, 0, n / n_households)
    )
  
}




