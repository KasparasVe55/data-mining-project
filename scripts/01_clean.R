# Data cleaning for ESS rounds 9-11 (LT, LV, EE)
# input: data/ess_baltic_R9-R11.csv
# output: data/ess_baltic_clean.rds
# working directory should be the repo folder

library(dplyr)

raw <- read.csv("data/ess_baltic_R9-R11.csv", stringsAsFactors = FALSE)

# variables we decided not to use
# wkhtot - too many missing, strange values
# chldhhe - almost no correlation with stflife
# hlthhmp - similar to health
# rlgatnd - similar to rlgdgr
raw <- raw %>% select(-wkhtot, -chldhhe, -hlthhmp, -rlgatnd)


# 1. missing codes to NA
# short scales: 7, 8, 9 = refusal, don't know, no answer
# 0-10 scales and others: 77, 88, 99
to_na <- function(x, codes) replace(x, x %in% codes, NA)

scale_0_10 <- c("ppltrst", "stfeco", "stfhlth", "stflife", "trstprl", "rlgdgr")
short_7_9  <- c("aesfdrk", "health", "sclact", "gndr",
                "domicil", "hincfel", "uemp3m")
long_77_99 <- c("inprdsc", "sclmeet", "hhmmb", "hinctnta",
                "mnactic", "rshpsts")

d <- raw
d[scale_0_10] <- lapply(d[scale_0_10], to_na, codes = c(77, 88, 99))
d[short_7_9]  <- lapply(d[short_7_9],  to_na, codes = c(7, 8, 9))
d[long_77_99] <- lapply(d[long_77_99], to_na, codes = c(77, 88, 99))

d$agea    <- to_na(d$agea,    999)
d$eisced  <- to_na(d$eisced,  c(0, 55, 77, 88, 99))  # 0 and 55 don't fit the 1-7 scale
d$mnactic <- to_na(d$mnactic, 66)
d$lnghom1 <- ifelse(d$lnghom1 %in% c("777", "888", "999"), NA, d$lnghom1)


# 2. new variables
d <- d %>%
  mutate(
    # LT is the reference country
    cntry = factor(cntry, levels = c("LT", "LV", "EE")),
    round = factor(essround, levels = c(9, 10, 11),
                   labels = c("R9 2018/19", "R10 2020-22", "R11 2023/24")),

    # age groups (not final yet)
    age5 = cut(agea, breaks = c(14, 29, 44, 59, 74, Inf),
               labels = c("15-29", "30-44", "45-59", "60-74", "75+")),
    age3 = cut(agea, breaks = c(14, 34, 59, Inf),
               labels = c("15-34", "35-59", "60+")),

    female = as.integer(gndr == 2),
    russian_home = as.integer(lnghom1 == "RUS"),

    # rshpsts 66 = no partner in household, so it is 0 here, not NA
    # use partner, not rshpsts
    partner = case_when(rshpsts %in% 1:6 ~ 1L,
                        rshpsts == 66    ~ 0L,
                        TRUE             ~ NA_integer_),

    ever_unemployed = as.integer(uemp3m == 1),

    activity = factor(mnactic, levels = 1:9,
                      labels = c("Paid work", "Education", "Unemployed, looking",
                                 "Unemployed, not looking", "Sick/disabled",
                                 "Retired", "Military/community", "Housework",
                                 "Other")),

    # 5 groups for models, Working is the reference
    # sick/disabled, military, housework and other go to Other
    activity_grp = case_when(mnactic == 1      ~ "Working",
                             mnactic == 2      ~ "Studying",
                             mnactic %in% 3:4  ~ "Unemployed",
                             mnactic == 6      ~ "Retired",
                             mnactic %in% c(5, 7, 8, 9) ~ "Other"),
    activity_grp = factor(activity_grp,
                          levels = c("Working", "Studying", "Unemployed",
                                     "Retired", "Other")),

    # hinctnta only for the missing income table, use hincfel in models
    income_missing = as.integer(is.na(hinctnta)),

    # stratum and psu numbers repeat between rounds/countries
    strat_id = paste(essround, cntry, stratum, sep = "_"),
    psu_id   = paste(essround, cntry, psu,     sep = "_")
  )


# 3. checks
stopifnot(all(d$stflife %in% c(0:10, NA)),
          all(d$hincfel %in% c(1:4, NA)),
          all(d$hinctnta %in% c(1:10, NA)),
          nrow(d) == nrow(raw))

cat("Rows:", nrow(d), "\n")
print(table(d$round, d$cntry))

saveRDS(d, "data/ess_baltic_clean.rds")
