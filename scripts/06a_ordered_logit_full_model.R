# Ordered logit for life satisfaction (stflife 0-10), the full model M3
# M3 uses all 22 predictors, as the other group models, and its outputs
# go into the model comparison
# how M3 was built (M1, M2) and the final M3 on all data (odds ratios for the
# report) are in 06b_ordered_logit_analysis.R, the checks in 06c_ordered_logit_checks.R
# 06_ordered_logit_documentation.qmd explains how this model is fitted
# run 01_clean.R and 03_train_test_split.R first, working directory = repo folder
# input:  data/ess_train.rds (with fold column), data/ess_test.rds,
#         data/ess_baltic_clean.rds (survey design for the standard errors)
# output: outputs/tables/ologit_metrics.csv (test set results)
#         outputs/tables/ologit_coefficients.csv (coefficients, design-based SEs, odds ratios)
#         outputs/predictions/ologit_test.csv (test predictions, for comparing all models)
#         data/ologit_model.rds (fitted model, not uploaded)
# packages: dplyr, MASS, survey
#   install.packages(c("dplyr", "survey"))  # MASS comes with R

library(dplyr)
library(MASS, exclude = "select")
library(survey)

train <- readRDS("data/ess_train.rds")
test  <- readRDS("data/ess_test.rds")

# same predictors as in 03_train_test_split.R
# agea enters the model as age groups (age5), cntry and round as factors
predictors <- c("agea", "female", "hincfel", "activity_grp", "health", "sclmeet",
                "partner", "trstprl", "rlgdgr", "eisced", "domicil", "russian_home",
                "inprdsc", "sclact", "hhmmb", "ppltrst", "stfeco", "stfhlth",
                "aesfdrk", "ever_unemployed", "cntry", "round")

dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/predictions", showWarnings = FALSE, recursive = TRUE)


# 1. data and coding
# missing predictors are already filled in 03_train_test_split.R
# stflife as an ordered factor, the ordered logit predicts its categories
# age5 rebuilt from the filled agea, same breaks and labels as in 01_clean.R
# numeric (near equal steps): hincfel, health, aesfdrk, inprdsc, 0-10 scales
# factor (not linear or not a scale): domicil, sclact, sclmeet, eisced, hhmmb
# levels fixed, so a level missing in the test set can't shift them
prep <- function(x) {
  x %>%
    select(idno, stflife, essround, all_of(predictors), anweight, imputed) %>%
    mutate(stflife = factor(stflife, levels = 0:10, ordered = TRUE),
           age5    = cut(agea, breaks = c(14, 29, 44, 59, 74, Inf),
                         labels = c("15-29", "30-44", "45-59", "60-74", "75+")),
           domicil = factor(domicil, levels = 1:5),       # ref 1 big city
           sclact  = factor(sclact,  levels = 1:5),       # ref 1 much less than most
           sclmeet = factor(sclmeet, levels = 1:7),       # ref 1 never
           # ref 4 upper secondary, largest group
           eisced  = relevel(factor(eisced, levels = 1:7), ref = "4"),
           # 7+ people only 61 respondents, so capped at 6
           hhmmb   = factor(pmin(hhmmb, 6), levels = 1:6,
                            labels = c(1:5, "6+")),       # ref 1 lives alone
           # Latvia R10 was a self-completion survey, this indicator absorbs
           # the survey-mode effect so it does not distort the Latvia and R10 effects
           lv_r10  = as.integer(cntry == "LV" & round == "R10 2020-22"))
}

d      <- prep(train) %>% mutate(w = anweight / mean(anweight))  # weights, mean 1
test_d <- prep(test)
stopifnot(!anyNA(d[predictors]), !anyNA(test_d[predictors]))


# 2. model
# polr with anweight (rescaled to mean 1, so the weighted n equals the real n)
m3 <- polr(stflife ~ cntry + round + lv_r10 + age5 + female + hincfel + activity_grp +
             health + sclmeet + partner + trstprl + rlgdgr +
             eisced + domicil + russian_home + inprdsc + sclact + hhmmb +
             ppltrst + stfeco + stfhlth + aesfdrk + ever_unemployed,
           data = d, weights = w, Hess = TRUE)


# 3. design-based standard errors
# polr SEs treat the rows as a simple random sample, svyolr uses the ESS
# design (PSUs, strata, anweight) as in survey_design.R
# coefficients are the same as polr, only the SEs change
# the design is built on the full data and then subset to the train rows,
# so the strata keep all their PSUs
options(survey.lonely.psu = "adjust")
full <- readRDS("data/ess_baltic_clean.rds")

key <- function(x) paste(x$cntry, x$round, x$idno)
dat <- full %>%
  select(idno, cntry, round, psu_id, strat_id, anweight) %>%
  mutate(key = key(.)) %>%
  left_join(d %>% mutate(key = key(d), in_model = TRUE) %>%
              select(-idno, -cntry, -round, -anweight, -w),
            by = "key") %>%
  mutate(in_model = !is.na(in_model))
des <- subset(svydesign(ids = ~psu_id, strata = ~strat_id, weights = ~anweight,
                        data = dat, nest = TRUE), in_model)
stopifnot(nrow(des) == nrow(d))

s3 <- svyolr(formula(m3), design = des)
b  <- coef(s3)
se <- sqrt(diag(vcov(s3)))[names(b)]

# OR = exp(coef), OR > 1 = higher odds of a higher stflife score
# cut-points are the thresholds between scores, no odds ratio for them
coefs <- data.frame(term  = names(b),
                    type  = ifelse(names(b) %in% names(m3$zeta), "cutpoint", "coefficient"),
                    coef  = round(b, 4),
                    se    = round(se, 4),
                    p     = signif(2 * pnorm(-abs(b / se)), 3),
                    OR    = round(exp(b), 4),
                    OR_lo = round(exp(b - 1.96 * se), 4),
                    OR_hi = round(exp(b + 1.96 * se), 4),
                    row.names = NULL)
coefs[coefs$type == "cutpoint", c("OR", "OR_lo", "OR_hi")] <- NA
print(coefs)


# 4. test set
# the model predicts a probability for each score 0-10, the prediction is the
# expected score, sum of score x probability, comparable with the other models
probs     <- predict(m3, newdata = test_d, type = "probs")
pred_ologit <- drop(probs %*% 0:10)
y_test    <- as.numeric(as.character(test_d$stflife))

# weighted metrics, same as wmetrics in 04_tree_mars.R
wmetrics <- function(obs, pred, w) {
  c(RMSE     = sqrt(sum(w * (obs - pred)^2) / sum(w)),
    MAE      = sum(w * abs(obs - pred)) / sum(w),
    Rsquared = 1 - sum(w * (obs - pred)^2) /
                   sum(w * (obs - weighted.mean(obs, w))^2))
}

metrics <- rbind(`Ordinal logit` = wmetrics(y_test, pred_ologit, test_d$anweight))
metrics <- data.frame(Model = rownames(metrics), round(metrics, 4), row.names = NULL)
print(metrics)


# 5. save
write.csv(metrics, "outputs/tables/ologit_metrics.csv", row.names = FALSE)
write.csv(coefs, "outputs/tables/ologit_coefficients.csv", row.names = FALSE)

# test predictions with ids, so the group can join all models on the same rows
predictions <- data.frame(idno     = test$idno,
                          cntry    = test$cntry,
                          essround = test$essround,
                          stflife  = test$stflife,
                          anweight = test$anweight,
                          imputed  = test$imputed,
                          pred_ologit = pred_ologit)
write.csv(predictions, "outputs/predictions/ologit_test.csv", row.names = FALSE)

saveRDS(m3, "data/ologit_model.rds")
