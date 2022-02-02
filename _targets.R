library(targets)
library(tarchetypes)
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

# timeout to download large files
options(timeout=200)

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "nhts2017", "mlogit", "modelsummary"))
set.seed(42)

# Set path to activitysim zip files
dir.create("data", showWarnings = FALSE)

# targets to conduct analysis
tar_plan(
  
  # Build dataset
  hh =  get_households(),
  persons = get_persons(hh$houseid),
  person_dap = get_trips(persons),
  data = make_data(person_dap, hh),
  
  # Estimate nhts models
  dap_stats = build_stats(data),
  descriptives = description_table(data),
  pt_models = estimate_models(data),
  pt_modelsummary = make_ptsummary(pt_models),
  
  # Get data from activitysim scenarios
  persons_base = asim_persons_base("data/persons_base.csv"),
  persons_wc = asim_persons_wc("data/persons_wc.csv"),
  households_wc = asim_households_wc("data/households_wc.csv"),
  asim_dap = asim_join(persons_base, persons_wc, households_wc),
  
  # Estimate dap models
  dap_table = build_table(asim_dap)
)

