#Survey design with weights
#run 01_clean.R first
#use in your script: source("scripts/survey_design.R")
#d = clean data, des = data with weights for the survey package

library(survey)

d <- readRDS("data/ess_baltic_clean.rds")

#needed when a stratum has only one psu
options(survey.lonely.psu = "adjust")

des <- svydesign(ids     = ~psu_id,
                 strata  = ~strat_id,
                 weights = ~anweight,
                 data    = d,
                 nest    = TRUE)

#examples
#weighted.mean(d$stflife, d$anweight, na.rm = TRUE)
#svymean(~stflife, des, na.rm = TRUE)
#svyquantile(~stflife, des, c(0.25, 0.5, 0.75), na.rm = TRUE)
#svyby(~stflife, ~cntry + age3, des, svymean, na.rm = TRUE)
#ggplot(d, aes(stflife, weight = anweight)) + geom_bar()
