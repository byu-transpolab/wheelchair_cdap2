#' Estimate basic model
#' 
#' @param data Cleaned data set
#' @return a nested tibble with a column called model containing mlogit estimation results
#' 
#' 
estimate_models <- function(data){
  
  # build function specific for DAP mlogit data
  dap_data <- function(df){
    mlogit.data(df, choice = "dap", chid.var = "id", shape = "wide")
  }
  
  
  d <- data %>%
    group_by(person_type) %>%
    nest() %>%
    mutate( data = map(data, dap_data) )
    
  
  models <- list()
  
  # FW model ----------------------------------
  fw_index <- which(d$person_type == "FW")
  models[["Full-time worker"]] <- mlogit(
    dap ~ 1 | wheelchair + works_home + male + bach_degree + income + age_bin, 
    data = d$data[[fw_index]],
    weights = wtperfin)
  
  
  # NW model ----------------------------------
  nw_index <- which(d$person_type == "NW")
  models[["Non-worker"]] <- mlogit(
    dap ~ 1 | wheelchair + male + bach_degree + income + age_bin,  
    data = d$data[[nw_index]],
    weights = wtperfin)
  
  
  # PW model ----------------------------------
  pw_index <- which(d$person_type == "PW")
  models[["Part-time worker"]] <- mlogit(
    dap ~ 1 | wheelchair + works_home + male + bach_degree + income + age_bin,  # add auto here
    data = d$data[[pw_index]],
    weights = wtperfin)
  
  
  # RT model ----------------------------------
  rt_index <- which(d$person_type == "RT")
  models[["Retired"]] <- mlogit(
    dap ~ 1 | wheelchair + male + bach_degree + income + age_bin,  
    data = d$data[[rt_index]],
    weights = wtperfin)
  
  models
}



make_ptsummary <- function(pt_models) {
  
  m_list <- modelsummary(pt_models, output = "modelsummary_list")
  
  
  for(m in names(m_list)){
    m_list[[m]]$tidy <- m_list[[m]]$tidy %>%
      separate(term, c("variable", "term"), sep = ":", fill = "right")
    
  }
  
  modelsummary(m_list, group = variable + term ~ model)

}



#' Build table from nhts data to dap choices by person type
#' 
#' @param data data set from nhts
#' @return stats table on dap by person type
#' 
#' 
#' 
build_stats <- function(data){
  
  dap_top <- data %>%
    group_by(dap, person_type) %>%
    tally() %>%
    spread(dap, n, fill = 0) %>%
    filter(person_type != "NA")
  # build the total count
  dap_total <- data %>%
    group_by(dap) %>%
    tally() %>%
    spread(dap, n, fill = 0)
  
  bind_rows(dap_top, dap_total) %>%
    transmute(
      person_type = factor(c("FW", "NW", "PW", "RT", "Total")),
      `Person Type` = case_when(
        person_type == "FW" ~ "Full-time Worker",
        person_type == "PW" ~ "Part-time Worker", 
        person_type == "NW" ~ "Non-Worker",
        person_type == "RT" ~ "Retired",
        TRUE ~ "Total"),
      Home = H,
      Mandatory = M,
      `Non-Mandatory` = NM) %>%
    select(-person_type)
  
}


#' Build table from nhts data to describe person types
#' 
#' @param data data set from nhts
#' @return stats table on person type variables in a latex format
#' 
#' 
#' 
#' 


description_table <- function(data) {
  
  summary1 <- list(
    "Respondents" = list( "N" = ~ length(.data$houseid) ),
    "Age" =
      list("Under 39" = ~ qwraps2::n_perc(.data$age_bin == "05-39"),
           "40-64" = ~ qwraps2::n_perc(.data$age_bin == "40-64"),
           "65-79" = ~ qwraps2::n_perc(.data$age_bin == "65-79"),
           "Over 80" = ~ qwraps2::n_perc(.data$age_bin == "80+")
      ),
    # Add inc_cont as a variable to the persons file
    # "Income" =
    #   list("Median (IQR) ($000)" = ~ qwraps2::median_iqr(.data$inc_cont / 1000, digits = 1),
    #        "< $25,000" = ~ qwraps2::n_perc(.data$income == "< $25,000"),
    #        "$25,000 - $50,000" = ~ qwraps2::n_perc(.data$income == "$25,000 - $50,000"),
    #        "$50,000 - $100,000" = ~ qwraps2::n_perc(.data$income == "$50,000 - $100,000"),
    #        "> $100,000" = ~ qwraps2::n_perc(.data$income == "> $100,000")
    #   ),
    "Gender" =
      list("Male" = ~ qwraps2::n_perc(.data$r_sex == "01"),
           "Female" = ~ qwraps2::n_perc(.data$r_sex == "02")
      ),
    "Employment Status" = 
      list("Employed" = ~ qwraps2::n_perc(.data$worker == "01"),
           "Unemployed" = ~ qwraps2::n_perc(.data$worker == "02")
      ),
    # Add wkftpt as a variable to the persons file
    # "Employment Classification" = 
    #   list("Full-time" = ~ qwraps2::n_perc(.data$wkftpt == "01"),
    #        "Part-time" = ~ qwraps2::n_perc(.data$wkftpt == "02")
    #   ),
    "Works from Home" = 
      list("Yes" = ~ qwraps2::n_perc(.data$wrk_home == "01"),
           "No" = ~ qwraps2::n_perc(.data$wrk_home == "02")
           ),
    # Add prmact as a variable to the persons file
    # "Retired Status" = 
    #   list("Yes" = ~ qwraps2::n_perc(.data$prmact == "06"),
    #        "No" = ~ qwraps2::n_perc(.data$prmact != "06")
    #        ),
    # Add auto_own as a variable to the persons file
    # "Auto Ownership" = 
    #   list("Sufficient" =  ~ qwraps2::n_perc(.data$auto_own == "sufficient"),
    #        "Insufficient" = ~ qwraps2::n_perc(.data$auto_own == "insufficient")
    #        ),
    "Educational Attainment" = 
      list("Bachelors Degree or More" = ~ qwraps2::n_perc(.data$educ %in% c("04", "05")))
  )
  
  data %>% 
    filter(person_type %in% c("RT", "NW", "PW", "FW")) %>%
    mutate(person_type = fct_relevel(person_type, "FW", "NW", "PW", "RT")) %>%
    group_by(`Segment` = person_type) %>%
    summary_table(summary1)
}