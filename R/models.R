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
    dap ~ 1 | wheelchair + r_sex + age_bin, 
    data = d$data[[fw_index]])
  
  
  # NW model ----------------------------------
  nw_index <- which(d$person_type == "NW")
  models[["Non-worker"]] <- mlogit(
    dap ~ 1 | wheelchair + r_sex + hhfaminc +  r_age, 
    data = d$data[[nw_index]])
  
  
  # PW model ----------------------------------
  pw_index <- which(d$person_type == "PW")
  models[["Part-time worker"]] <- mlogit(
    dap ~ 1 | wheelchair + r_sex + hhfaminc +  r_age, 
    data = d$data[[pw_index]])
  
  
  # RT model ----------------------------------
  rt_index <- which(d$person_type == "RT")
  models[["Retired"]] <- mlogit(
    dap ~ 1 | wheelchair + r_sex + hhfaminc +  r_age, 
    data = d$data[[rt_index]])
  
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