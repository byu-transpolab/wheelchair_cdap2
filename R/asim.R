#' Get persons / households File from activitysim
#' 
#' @param file to data file
#' @return Stores a file in the appropriate location
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

# asim_households_base <- function(path){
#   if(!file.exists(path)){
#     # originally from UTA: June 4 2021
#     download.file("https://byu.box.com/shared/static/em4qhq8i0bg3yb0oor76tu0n0lb770oo.csv", # from box
#                   destfile = path)
#   } else {
#     message(path, " already available")
#   }
#   return(read_csv(path)) # to use file target, need to return path to data. 
# }

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
build_table <- function(asim_dap) {
  
  asim_dap %>%
    group_by(Group, DAP_after, DAP_before) %>%
    tally() %>%
    spread(DAP_after, -Group)
  
}


