library(simmer)
library(dplyr)
library(tidyverse)

set.seed(42)

env <- simmer("CLINIC_SIM")

INTAKE_TIME <- 10
CONSULT_TIME <- 2
ADMIN_TIME <- 10
INTER_ARRIVAL_TIME <- 3
SIM_RUN_TIME <- 1000

patient <- trajectory("patients' path") %>%
  ## add an intake activity 
  seize("nurse", 1) %>%
  timeout(INTAKE_TIME) %>%
  release("nurse", 1) %>%
  ## add a consultation activity
  seize("doctor", 1) %>%
  timeout(CONSULT_TIME) %>%
  release("doctor", 1) %>%
  ## add a planning activity
  seize("administration", 1) %>%
  timeout(ADMIN_TIME) %>%
  release("administration", 1)

env %>%
  add_resource("nurse", 1) %>%
  add_resource("doctor", 1) %>%
  add_resource("administration", 1) %>%
  add_generator("patient", patient, from_to(0, SIM_RUN_TIME, function() {INTER_ARRIVAL_TIME}))

env %>%
  run(1000) %>%
  now()

mon_arrivals <- get_mon_arrivals(env)
mon_resources <- get_mon_resources(env)

## Calculate doctor wait times

names <- colnames(mon_resources)
df <- as_tibble(mon_resources)

mon_doc <- df %>%
  filter(resource == 'doctor') %>%
  arrange(time) %>%
  mutate(state_time_diff=c(diff(time), 0))

mon_doc_waiting <- df %>% 


