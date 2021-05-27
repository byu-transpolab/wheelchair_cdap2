library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.


# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/data_maker.R")


# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "nhts2017"))
set.seed(42)

# End this file with a list of target objects.
list(
  
  # Build dataset
  tar_target(hh,  get_households()),
  tar_target(persons, get_persons(hh$houseid)),
  tar_target(person_dap, get_trips(persons)),
  tar_target(data, make_data(person_dap, hh)),
  
  # Estimate models
  tar_target(dummy, message("Done"))
)
