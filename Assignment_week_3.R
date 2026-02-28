############################################################
# Load Libraries
############################################################
library(lubridate)
library(tidyverse)
library(haven)

setwd("D:/R training/UpdatedCDISCPilotData-main/UpdatedCDISCPilotData-main/UpdatedCDISCPilotData")

############################################################
# 1. Multi-Domain Join (ADSL + VS)
############################################################

# Read ADSL (Safety population only)
adsl <- read_xpt("ADAM/ADSL.XPT") %>%
  filter(SAFFL == "Y") %>%
  select(USUBJID, SAFFL, TRT01A)

# Read VS domain
vs <- read_xpt("SDTM/VS.XPT") %>%
  select(USUBJID, VSTESTCD, VSSTRESN, VISITNUM)

# Count records before join
before_count <- vs %>%
  summarise(record_count = n())

# Inner join and filter BP parameters
vs_joined <- inner_join(adsl, vs, by = "USUBJID") %>%
  filter(VSTESTCD %in% c("SYSBP", "DIABP"))

# Count records after join
after_count <- vs_joined %>%
  summarise(record_count = n())

############################################################
# 2. Transpose Method (Wide Format Summary)
############################################################

advs_wide <- read_xpt("SDTM/VS.XPT") %>%
  filter(VSTESTCD == "SYSBP") %>%
  group_by(VSPOS, VISITNUM, VISIT) %>%
  summarise(
    MEAN_AVAL = mean(VSSTRESN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(VSPOS, VISITNUM) %>%
  pivot_wider(
    names_from  = VISIT,
    values_from = MEAN_AVAL
  )

############################################################
# 3. Summary Table for Multiple Parameters
############################################################

advs_summary <- read_xpt("SDTM/VS.XPT") %>%
  filter(VSTESTCD %in% c("SYSBP", "DIABP", "PULSE", "TEMP")) %>%
  group_by(VSTESTCD, VSPOS, VISIT) %>%
  summarise(
    N    = sum(!is.na(VSSTRESN)),
    MEAN = mean(VSSTRESN, na.rm = TRUE),
    SD   = sd(VSSTRESN, na.rm = TRUE),
    .groups = "drop"
  )

############################################################
# 4. Date Derivation (ASTDY Calculation)
############################################################

ae <- tibble(
  TRTSDT  = ymd("2024-01-15"),
  AESTDTC = ymd("2024-01-20")
)

ae <- ae %>%
  mutate(
    TRTSDT = as.Date(TRTSDT),
    AESTDT = as.Date(AESTDTC),
    
    DURATION = as.numeric(AESTDT - TRTSDT),
    
    ASTDY = case_when(
      !is.na(AESTDT) & !is.na(TRTSDT) & AESTDT >= TRTSDT ~ DURATION + 1,
      !is.na(AESTDT) & !is.na(TRTSDT) & AESTDT < TRTSDT  ~ DURATION,
      TRUE ~ NA_real_
    )
  )

############################################################
# End of Script
############################################################