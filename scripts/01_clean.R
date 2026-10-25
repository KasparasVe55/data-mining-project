library(dplyr)

raw <- read.csv("data/ess_baltic_R9-R11.csv", stringsAsFactors = FALSE)

#drop unused variables
raw <- raw %>% select(-wkhtot, -chldhhe, -hlthhmp, -rlgatnd)

#recode missing values to NA
to_na <- function(x, codes) replace(x, x %in% codes, NA)

scale_0_10 <- c("ppltrst", "stfeco", "stfhlth", "stflife", "trstprl", "rlgdgr")
short_7_9 <- c("aesfdrk", "health", "sclact", "gndr", "domicil", "hincfel", "uemp3m")
long_77_99 <- c("inprdsc", "sclmeet", "hhmmb", "hinctnta", "mnactic", "rshpsts")

d <- raw
d[scale_0_10] <- lapply(d[scale_0_10], to_na, codes = c(77, 88, 99))
d[short_7_9] <- lapply(d[short_7_9], to_na, codes = c(7, 8, 9))
d[long_77_99] <- lapply(d[long_77_99], to_na, codes = c(77, 88, 99))

d$agea <- to_na(d$agea, 999)
d$eisced <- to_na(d$eisced, c(0, 55, 77, 88, 99))
d$mnactic <- to_na(d$mnactic, 66)
d$lnghom1 <- ifelse(d$lnghom1 %in% c("777", "888", "999"), NA, d$lnghom1)

#create derived variables
d <- d %>%
  mutate(
    cntry = factor(cntry, levels = c("LT", "LV", "EE")),
    round = factor(
      essround,
      levels = c(9, 10, 11),
      labels = c("R9 2018/19", "R10 2020-22", "R11 2023/24")
    ),
    age5 = cut(
      agea,
      breaks = c(14, 29, 44, 59, 74, Inf),
      labels = c("15-29", "30-44", "45-59", "60-74", "75+")
    ),
    age3 = cut(
      agea,
      breaks = c(14, 34, 59, Inf),
      labels = c("15-34", "35-59", "60+")
    ),
    female = as.integer(gndr == 2),
    russian_home = as.integer(lnghom1 == "RUS"),
    #66 = no partner in household
    partner = case_when(
      rshpsts %in% 1:6 ~ 1L,
      rshpsts == 66 ~ 0L,
      TRUE ~ NA_integer_
    ),
    ever_unemployed = as.integer(uemp3m == 1),
    activity = factor(
      mnactic,
      levels = 1:9,
      labels = c(
        "Paid work", "Education", "Unemployed, looking",
        "Unemployed, not looking", "Sick/disabled",
        "Retired", "Military/community", "Housework", "Other"
      )
    ),
    activity_grp = case_when(
      mnactic == 1 ~ "Working",
      mnactic == 2 ~ "Studying",
      mnactic %in% 3:4 ~ "Unemployed",
      mnactic == 6 ~ "Retired",
      mnactic %in% c(5, 7, 8, 9) ~ "Other"
    ),
    activity_grp = factor(
      activity_grp,
      levels = c("Working", "Studying", "Unemployed", "Retired", "Other")
    ),
    income_missing = as.integer(is.na(hinctnta)),
    #make psu and stratum unique across rounds and countries
    strat_id = paste(essround, cntry, stratum, sep = "_"),
    psu_id = paste(essround, cntry, psu, sep = "_")
  )

#sanity checks
stopifnot(
  all(d$stflife %in% c(0:10, NA)),
  all(d$hincfel %in% c(1:4, NA)),
  all(d$hinctnta %in% c(1:10, NA)),
  nrow(d) == nrow(raw)
)

cat("Rows:", nrow(d), "\n")
print(table(d$round, d$cntry))

saveRDS(d, "data/ess_baltic_clean.rds")