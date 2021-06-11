library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

#--------Dictionary------------------------
# tar_visnetwork() - 
# tar_read(hh) - view data
# how can I view model results?
#------------------------------------------

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/data_maker.R")
source("R/models.R")
source("R/asim.R")


# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "nhts2017", "mlogit", "modelsummary"))
set.seed(42)

# Set path to activitysim zip files
asim_path <- "R/data"

# End this file with a list of target objects.
list(
  
  # Build dataset
  tar_target(hh,  get_households()),
  tar_target(persons, get_persons(hh$houseid)),
  tar_target(person_dap, get_trips(persons)),
  tar_target(data, make_data(person_dap, hh)),
  
  # Estimate nhts models
  tar_target(pt_models, estimate_models(data)),
  tar_target(pt_modelsummary, make_ptsummary(pt_models)),
  
  # Get data from activitysim
  tar_target(persons_base, asim_persons_base(file.path(asim_path, "persons_base.csv"))),
  #tar_target(households_base, asim_households_base(file.path(asim_path, "households_base.csv"))),
  tar_target(persons_wc, asim_persons_wc(file.path(asim_path, "persons_wc.csv"))),
  tar_target(households_wc, asim_households_wc(file.path(asim_path, "households_wc.csv"))),
  tar_target(asim_dap, asim_join(persons_base, persons_wc, households_wc)),
  
  # Estimate dap models
  tar_target(dap_table, build_table(asim_dap)),
  
  # Dummy
  tar_target(dummy, message("Done"))
)
