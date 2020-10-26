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
res_df <- as_tibble(mon_resources)

mon_doc <- res_df %>%
  filter(resource == 'doctor') %>%
  arrange(time) %>%
  mutate(state_time_diff=c(diff(time), 0))

mon_doc_waiting <- mon_doc %>% 
  filter(server == 0 & queue == 0)

## Calculate nurse wait times
mon_nurse <- res_df %>%
  filter(resource == 'nurse') %>%
  arrange(time) %>%
  mutate(state_time_diff=c(diff(time), 0))

mon_nurse_waiting <- mon_nurse %>% 
  filter(server == 0 & queue == 0)

# Calculate patient wait times 
mon_pt <- mon_arrivals %>%
  mutate(wait_time = end_time - start_time - activity_time)

# Wait times df
temp_wait_doc <- as_tibble(
  data.frame(mon_doc_waiting$resource, mon_doc_waiting$time, mon_doc_waiting$state_time_diff))
names(temp_wait_doc) <- c("entity", "sys_time", "wait_time")
temp_wait_nurse <- as_tibble(
  data.frame(mon_nurse_waiting$resource, mon_nurse_waiting$time, mon_nurse_waiting$state_time_diff))
names(temp_wait_nurse) <- c("entity", "sys_time", "wait_time")
temp_wait_pt <- as_tibble(
  data.frame(mon_pt$name, mon_pt$start_time, mon_pt$wait_time)
)
names(temp_wait_pt) <- c("entity", "sys_time", "wait_time")
waittimes_df <- rbind(temp_wait_doc, temp_wait_nurse, temp_wait_pt)


