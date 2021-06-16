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
  sum_data <- data %>% 
    filter(!is.na(person_type)) %>%
    transmute(
      Type = factor(person_type, levels = c("FW", "NW", "PW", "RT"),
                    labels = c("Full-time worker", "Non-worker", "Part-time worker", 
                               "Retired")),
      Age = age_bin,
      Wheelchair = wheelchair,
      `Bachelors or more` = bach_degree,
      `Income` = income,
      Sex = r_sex,
      `Works from Home` = wrk_home
    ) 
  
  datasummary_balance(
    ~Type,
    data = sum_data,
    dinm = FALSE
  )

}