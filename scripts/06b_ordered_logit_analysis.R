# Ordered logit for life satisfaction (stflife 0-10), full analysis
# all models (M0-M3, M3b), assumption checks, interactions, robustness,
# design-based SEs, CV, test, importance and predicted probabilities
# the full model (M3) is fitted and saved by 06a_ordered_logit_full_model.R,
# this file is for the report and for M3b, the model to interpret if the
# group continues with the ordinal logit
# optional, nothing else depends on it
# run 01_clean.R and 03_train_test_split.R first, working directory = repo folder
# input:  data/ess_train.rds (with fold and imputed columns), data/ess_test.rds,
#         data/ess_baltic_clean.rds (survey design in step 10)
# output: printed only, no files written
# packages: dplyr, MASS, car, survey, ordinal
#   install.packages(c("dplyr", "car", "survey", "ordinal"))  # MASS comes with R
# runs several minutes (mostly the simulations in step 15)
# results checked with R 4.5.2, dplyr 1.1.4, MASS 7.3-65, car 3.1-3,
# survey 4.5, ordinal 2026.7.26

library(dplyr)

train <- readRDS("data/ess_train.rds")

# explanatory variables from Table A2 of the report, plus country and round
# cleaned versions from 01_clean.R: activity_grp (mnactic),
# russian_home (lnghom1), ever_unemployed (uemp3m)
main_vars  <- c("agea", "age5", "female", "hincfel", "activity_grp", "health",
                "sclmeet", "partner", "trstprl", "rlgdgr")
other_vars <- c("eisced", "domicil", "russian_home", "inprdsc", "sclact",
                "hhmmb", "ppltrst", "stfeco", "stfhlth", "aesfdrk",
                "ever_unemployed")


# 1. data
# missing predictors are already filled in 03_train_test_split.R,
# imputed = TRUE marks rows where something was filled
# age5 is rebuilt from the filled agea (03 fills agea, not age5),
# same breaks and labels as in 01_clean.R
# anweight rescaled to mean 1, so the weighted n equals the real n
# as a function, so the test set (step 11b) is loaded the same way
load_vars <- function(x) {
  x %>%
    select(idno, stflife, cntry, round, all_of(main_vars), all_of(other_vars),
           anweight, imputed, any_of("fold")) %>%
    mutate(stflife = factor(stflife, levels = 0:10, ordered = TRUE),
           age5    = cut(agea, breaks = c(14, 29, 44, 59, 74, Inf),
                         labels = c("15-29", "30-44", "45-59", "60-74", "75+")))
}

d <- load_vars(train) %>% mutate(w = anweight / mean(anweight))

cat("Rows:", nrow(d), " Columns:", ncol(d), "\n\n")

cat("Missing per variable (n):\n")
print(colSums(is.na(d[c(main_vars, other_vars)])))

stopifnot(!anyNA(d[c(main_vars, other_vars)]))
cat("\nRows with imputed values:", sum(d$imputed), "of", nrow(d), "\n")


# 2. variable coding
# age as age5 (agea kept only for a later age + age^2 check)
# numeric (near equal steps): hincfel, health, aesfdrk, inprdsc, 0-10 scales
# factor (not linear or not a scale): domicil, sclact, sclmeet, eisced, hhmmb
# cntry, round, age5, activity_grp are already factors (refs LT, R9, 15-29, Working)
# levels fixed, so a level missing in a smaller data set (test) can't shift them
code_vars <- function(x) {
  x %>%
    mutate(domicil = factor(domicil, levels = 1:5),       # ref 1 big city
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

d <- code_vars(d)

cat("\nFactor levels (first = reference):\n")
fac_vars <- names(d)[sapply(d, is.factor) & names(d) != "stflife"]
for (v in fac_vars) cat(sprintf("  %-13s %s\n", v, paste(levels(d[[v]]), collapse = " | ")))


# 3. sample
# all train rows, missing values filled in 03_train_test_split.R
# d_all kept as the same rows, so later steps that refer to it still work
d_all <- d
imp <- d %>% filter(imputed)

cat("\nSample:", nrow(d), "rows, mean w =", mean(d$w), "\n")

cat("\nRows per country x round:\n")
print(table(d$cntry, d$round))
cat("\nRows per fold:\n")
print(table(d$fold))

# are rows with filled values different? (weighted, original anweight)
wm <- function(x) weighted.mean(as.numeric(as.character(x$stflife)), x$anweight)
cat("\nWeighted mean stflife  all:", round(wm(d), 3),
    " not imputed:", round(wm(filter(d, !imputed)), 3),
    " imputed:", round(wm(imp), 3), "\n")

# where the imputed rows are (unweighted shares, %)
cat("\nImputed rows by country x round (% of that cell):\n")
print(round(100 * table(imp$cntry, imp$round) / table(d$cntry, d$round), 1))
cat("\nImputed rows by age group (% of that group):\n")
print(round(100 * table(imp$age5) / table(d$age5), 1))


# 4. models
library(MASS, exclude = "select")

# odds ratios with 95% CI, OR > 1 = higher odds of a higher stflife score
# CIs from polr, they ignore the survey design (design-based SEs in step 10)
or_table <- function(m) {
  s <- coef(summary(m))[names(coef(m)), , drop = FALSE]
  data.frame(term = rownames(s),
             OR   = exp(s[, "Value"]),
             lo   = exp(s[, "Value"] - 1.96 * s[, "Std. Error"]),
             hi   = exp(s[, "Value"] + 1.96 * s[, "Std. Error"]),
             z    = s[, "t value"],
             row.names = NULL)
}

# M1: country, round and age only, the unadjusted gaps
# lv_r10 in all models (see 9a), Latvia and R10 then compare like-for-like rounds
m1 <- polr(stflife ~ cntry + round + lv_r10 + age5, data = d, weights = w, Hess = TRUE)

cat("\nM1 odds ratios:\n")
print(or_table(m1), digits = 3)
cat("\nM1 cut-points (log-odds scale):\n")
print(round(m1$zeta, 2))

# M2: M1 plus the other main variables
# if country and age effects shrink, the main variables explain them
m2 <- polr(stflife ~ cntry + round + lv_r10 + age5 + female + hincfel + activity_grp +
             health + sclmeet + partner + trstprl + rlgdgr,
           data = d, weights = w, Hess = TRUE)

cat("\nM2 odds ratios:\n")
print(or_table(m2), digits = 3)

cat("\nCountry, round and age, M1 vs M2 (OR):\n")
print(round(data.frame(M1 = exp(coef(m1)), M2 = exp(coef(m2)[names(coef(m1))])), 3))


# 5. is M2 better than M1?
# same rows and weights, so logLik and AIC are comparable
# with weights logLik is a pseudo-likelihood, so the LR p-value is approximate
m0 <- polr(stflife ~ 1, data = d, weights = w, Hess = TRUE)  # null, cut-points only

fit_stats <- function(m) c(df     = length(coef(m)),
                           logLik = as.numeric(logLik(m)),
                           AIC    = AIC(m),
                           # McFadden pseudo R2, share of null logLik explained
                           R2_McF = 1 - as.numeric(logLik(m)) / as.numeric(logLik(m0)))

cat("\nModel fit:\n")
print(round(rbind(M0 = fit_stats(m0), M1 = fit_stats(m1), M2 = fit_stats(m2)), 3))

# likelihood ratio test, LR = 2 x gain in logLik, df = number of added terms
lr_test <- function(small, big) {
  lr <- 2 * (as.numeric(logLik(big)) - as.numeric(logLik(small)))
  df <- length(coef(big)) - length(coef(small))
  c(LR = lr, df = df, p = pchisq(lr, df, lower.tail = FALSE))
}
cat("\nLR test M1 vs M0:\n"); print(signif(lr_test(m0, m1), 3))
cat("LR test M2 vs M1:\n");   print(signif(lr_test(m1, m2), 3))


# 6. assumption checks on M2

# 6a. proportional odds: one effect per variable at every cut-off
# fit binary logits P(stflife >= k) at a few cut-offs, coefficients should
# be close to M2's if the assumption holds
# quasibinomial = same estimates as binomial, no warning for non-integer weights
cutoffs <- c(5, 7, 8, 9)
f_rhs   <- delete.response(terms(m2))

po <- sapply(cutoffs, function(k) {
  dk <- d %>% mutate(y = as.integer(as.numeric(as.character(stflife)) >= k))
  coef(glm(update(f_rhs, y ~ .), data = dk, weights = w,
           family = quasibinomial))[-1]
})
colnames(po) <- paste0(">=", cutoffs)

po_tab <- data.frame(M2 = coef(m2), po, check.names = FALSE)
# spread = largest minus smallest cut-off coefficient, on the log-odds scale
po_tab$spread <- apply(po, 1, function(x) max(x) - min(x))

cat("\n6a. Proportional odds, log-odds coefficients by cut-off:\n")
print(round(po_tab, 2))
cat("\nShare of rows at or above each cut-off:\n")
print(round(sapply(cutoffs, function(k)
  weighted.mean(as.numeric(as.character(d$stflife)) >= k, d$w)), 3))
# decision: holds for hincfel, health, trstprl, rlgdgr, partner, female
# violated for cntry, age5, activity_grp, sclmeet, their M2 ORs are reported
# as average effects and this table goes to the appendix

# 6b. linearity of the numeric variables in M2
# refit M2 with one numeric variable as a factor, if the factor fits much
# better (AIC, LR) the equal-steps assumption fails for that variable
num_vars <- c("hincfel", "health", "trstprl", "rlgdgr")

lin <- lapply(num_vars, function(v) {
  m_f <- update(m2, as.formula(paste(". ~ . -", v, "+ factor(", v, ")")))
  lr  <- lr_test(m2, m_f)
  # effect of each level vs the lowest: from the factor model, and as the
  # numeric slope in M2 implies it (slope x distance from the lowest level)
  lev <- sort(unique(d[[v]]))
  fac <- c(0, coef(m_f)[paste0("factor(", v, ")", lev[-1])])
  list(test = data.frame(variable = v, dAIC = AIC(m2) - AIC(m_f),
                         LR = lr["LR"], df = lr["df"], p = lr["p"],
                         row.names = NULL),
       shape = data.frame(level = lev, factor = fac,
                          linear = coef(m2)[v] * (lev - lev[1]),
                          row.names = NULL))
})
names(lin) <- num_vars

cat("\n6b. Linearity, M2 vs M2 with the variable as a factor",
    "(dAIC > 0 = factor fits better):\n")
print(do.call(rbind, lapply(lin, `[[`, "test")), digits = 3, row.names = FALSE)

for (v in num_vars) {
  cat("\n", v, ", effect vs lowest level (log-odds):\n", sep = "")
  print(round(t(lin[[v]]$shape), 2))
}
# decision: all four kept numeric
# hincfel, health linear, trstprl close to linear with a jump at 10,
# rlgdgr J-shaped (0 above 1-4), its OR is reported as an average trend

# 6c. multicollinearity
# VIF depends only on the predictors, so it is computed from a weighted
# linear model with the same right-hand side as M2
# GVIF^(1/(2 df)) makes factors comparable with 1-df terms (compare with sqrt(5) ~ 2.2)
vif_tab <- car::vif(lm(update(f_rhs, as.numeric(stflife) ~ .), data = d, weights = w))

cat("\n6c. Multicollinearity (GVIF):\n")
print(round(vif_tab, 2))
# decision: no problem, highest GVIF^(1/(2 df)) 1.24 (health),
# age5 and activity_grp overlap (retired / students) but are fine

# 6d. weighted vs unweighted fit
# if the model is correctly specified, weights should not change the
# coefficients much, large differences point to a missing variable or
# interaction (the weights depend on country, age, gender, education, region)
m2_unw <- update(m2, weights = NULL)

wu <- data.frame(weighted   = coef(m2),
                 unweighted = coef(m2_unw)[names(coef(m2))])
wu$diff <- wu$weighted - wu$unweighted
# difference in units of the weighted SE, |z| > 2 = worth a look
wu$diff_se <- wu$diff / sqrt(diag(vcov(m2)))[names(coef(m2))]

cat("\n6d. Weighted vs unweighted M2 (log-odds):\n")
print(round(wu, 2))
# decision: no action, key effects identical
# age 60-74 shifts by 2.2 SE (lead for M4), sclmeet levels all shift
# together = the small reference group "never" (n = 206) moves


# 7. M3: M2 plus the other variables
# with imputed data all models use the same rows, so d3, m0_3 and m2_3 are
# the same as d, m0 and m2 (names kept so later steps don't change)
d3   <- d
m0_3 <- m0
m2_3 <- m2
m3   <- update(m2_3, . ~ . + eisced + domicil + russian_home + inprdsc +
                 sclact + hhmmb + ppltrst + stfeco + stfhlth + aesfdrk +
                 ever_unemployed)

# fit_stats uses m0 for the pseudo R2, so here with the null model on d3
fit3 <- function(m) c(df     = length(coef(m)),
                      logLik = as.numeric(logLik(m)),
                      AIC    = AIC(m),
                      R2_McF = 1 - as.numeric(logLik(m)) / as.numeric(logLik(m0_3)))

cat("\nModel fit on the", nrow(d3), "rows:\n")
print(round(rbind(M2 = fit3(m2_3), M3 = fit3(m3)), 3))
cat("\nLR test M3 vs M2:\n"); print(signif(lr_test(m2_3, m3), 3))

cat("\nM3 odds ratios:\n")
print(or_table(m3), digits = 3)

# how the M2 variables change once the other variables are added
cat("\nM2 variables, M2 vs M3 on the same rows (OR):\n")
print(round(data.frame(M2 = exp(coef(m2_3)),
                       M3 = exp(coef(m3)[names(coef(m2_3))])), 3))

cat("\nM3 multicollinearity (GVIF):\n")
print(round(car::vif(lm(update(delete.response(terms(m3)), as.numeric(stflife) ~ .),
                        data = d3, weights = w)), 2))

# M3b: M3 without stfeco and stfhlth
# both are satisfaction ratings close to the outcome (shared tendency to
# answer positively), in M3 stfeco absorbs trstprl and flips Latvia
m3b <- update(m3, . ~ . - stfeco - stfhlth)

cat("\nModel fit on the", nrow(d3), "rows:\n")
print(round(rbind(M2 = fit3(m2_3), M3b = fit3(m3b), M3 = fit3(m3)), 3))
cat("\nLR test M3b vs M2:\n"); print(signif(lr_test(m2_3, m3b), 3))
cat("LR test M3 vs M3b:\n");   print(signif(lr_test(m3b, m3), 3))

cat("\nM3b odds ratios:\n")
print(or_table(m3b), digits = 3)

cat("\nM2 variables, M2 vs M3b vs M3 on the same rows (OR):\n")
print(round(data.frame(M2  = exp(coef(m2_3)),
                       M3b = exp(coef(m3b)[names(coef(m2_3))]),
                       M3  = exp(coef(m3)[names(coef(m2_3))])), 3))
# decision: M2 = main-variable model, M3b = full interpretive model,
# M3 only for prediction (stfeco absorbs trstprl and flips Latvia)


# 8. M4: interactions, each block added to M2 on its own (all 10,151 rows)
# A: do income, health and trust matter equally in each country?
# B: does the age pattern differ by country after the controls?
# blocks chosen in advance, with block C (step 13) Bonferroni for 3 blocks:
# p < 0.05 / 3 = 0.0167, the same blocks on M3b in step 13b
m4a <- update(m2, . ~ . + cntry:hincfel + cntry:health + cntry:trstprl)
m4b <- update(m2, . ~ . + cntry:age5)

cat("\nModel fit on the", nrow(d), "rows:\n")
print(round(rbind(M2 = fit_stats(m2), M4A = fit_stats(m4a), M4B = fit_stats(m4b)), 3))
cat("\nLR test M4A vs M2:\n"); print(signif(lr_test(m2, m4a), 3))
cat("LR test M4B vs M2:\n");   print(signif(lr_test(m2, m4b), 3))

# effect of a term in each country = main effect + country interaction
# OR with 95% CI, SE from the sum of the coefficients
by_country <- function(m, term) {
  b <- coef(m); V <- vcov(m)
  sapply(levels(d$cntry), function(cn) {
    nm <- term
    if (cn != levels(d$cntry)[1]) {
      int <- c(paste0("cntry", cn, ":", term), paste0(term, ":cntry", cn))
      nm  <- c(term, int[int %in% names(b)])
    }
    est <- sum(b[nm]); se <- sqrt(sum(V[nm, nm]))
    round(exp(c(OR = est, lo = est - 1.96 * se, hi = est + 1.96 * se)), 3)
  })
}

cat("\nBlock A, OR per country:\n")
for (v in c("hincfel", "health", "trstprl")) {
  cat("\n", v, ":\n", sep = ""); print(by_country(m4a, v))
}

cat("\nBlock B, age group OR (vs 15-29) per country:\n")
for (a in paste0("age5", levels(d$age5)[-1])) {
  cat("\n", a, ":\n", sep = ""); print(by_country(m4b, a))
}
# decision: no interactions in the main models (final check on M3b in 13b)
# block A: income and trust equal in all countries, health stronger in LT
# block B: deeper mid-life dip in LT, older EE more satisfied
# both pass LR (p 0.003, 0.003) and lower AIC by about 8, but the differences
# are small and one effect per variable is easier to interpret


# 9. robustness checks

# 9a. without Latvia R10 (self-completion survey, less comparable)
# weights rescaled on the smaller sample, the round R10 effect now comes
# from LT and EE only
d_nolv10 <- d %>%
  filter(!(cntry == "LV" & round == "R10 2020-22")) %>%
  mutate(w = anweight / mean(anweight))

cat("\nRows without Latvia R10:", nrow(d_nolv10), "of", nrow(d), "\n")

# lv_r10 is all 0 here, so it is dropped from the formula
m1_nolv10 <- update(m1, . ~ . - lv_r10, data = d_nolv10)
m2_nolv10 <- update(m2, . ~ . - lv_r10, data = d_nolv10)

cat("\nM1, all rows vs without Latvia R10 (OR):\n")
print(round(data.frame(all = exp(coef(m1)), no_LV_R10 = exp(coef(m1_nolv10)[names(coef(m1))])), 3))

# z of the difference, in units of the full-sample SE (rough guide)
rob <- data.frame(all       = exp(coef(m2)),
                  no_LV_R10 = exp(coef(m2_nolv10)[names(coef(m2))]))
rob$diff_se <- (coef(m2_nolv10)[names(coef(m2))] - coef(m2)) /
  sqrt(diag(vcov(m2)))[names(coef(m2))]
cat("\nM2, all rows vs without Latvia R10 (OR):\n")
print(round(rob, 3))
cat("\nSignificant at 0.05 without Latvia R10:\n")
print(or_table(m2_nolv10)[, c("term", "OR", "lo", "hi")], digits = 3)
# decision: lv_r10 indicator added to M1 and M2 (and so to all later models),
# individual-level effects robust, Latvia and R10 effects depended on LV R10

# 9b. age + age^2 instead of age5
# gives the age with the lowest life satisfaction, -b1 / (2 b2), to compare
# with the literature (about 52 in post-communist countries)
# age in decades so polr stays numerically stable
d <- d %>% mutate(age10 = agea / 10)

m1_q <- update(m1, . ~ . - age5 + age10 + I(age10^2))
m2_q <- update(m2, . ~ . - age5 + age10 + I(age10^2))

# lowest point with a 95% CI from the delta method, in years
min_age <- function(m) {
  b <- coef(m)[c("age10", "I(age10^2)")]
  V <- vcov(m)[names(b), names(b)]
  est  <- -b[1] / (2 * b[2])
  grad <- c(-1 / (2 * b[2]), b[1] / (2 * b[2]^2))
  se   <- sqrt(drop(t(grad) %*% V %*% grad))
  round(10 * c(min_age = unname(est), lo = unname(est - 1.96 * se),
               hi = unname(est + 1.96 * se)), 1)
}

cat("\n9b. Age + age^2, lowest point (years):\n")
print(rbind(M1 = min_age(m1_q), M2 = min_age(m2_q)))

cat("\nage5 vs age + age^2 (same rows, AIC):\n")
print(round(c(M1_age5 = AIC(m1), M1_quad = AIC(m1_q),
              M2_age5 = AIC(m2), M2_quad = AIC(m2_q)), 1))

cat("\nAge terms (log-odds):\n")
print(round(rbind(M1 = coef(m1_q)[c("age10", "I(age10^2)")],
                  M2 = coef(m2_q)[c("age10", "I(age10^2)")]), 3))

# other M2 effects should not depend on how age is coded
shared <- setdiff(names(coef(m2)), grep("^age5", names(coef(m2)), value = TRUE))
cat("\nM2 other effects, age5 vs age + age^2 (OR):\n")
print(round(data.frame(age5 = exp(coef(m2)[shared]),
                       quad = exp(coef(m2_q)[shared])), 3))


# 10. design-based standard errors
# polr SEs treat the rows as a simple random sample, svyolr uses the ESS
# design (PSUs, strata, anweight) as in survey_design.R
# coefficients are the same as polr, only the SEs change
# the design is built on the full data and then subset to the model rows,
# so the strata keep all their PSUs (a design built on the subset would not)
# polr is still used for AIC, LR and the checks (svyolr has no likelihood)
library(survey)
options(survey.lonely.psu = "adjust")

full <- readRDS("data/ess_baltic_clean.rds")

# model rows matched to the full data by country, round and idno
key <- function(x) paste(x$cntry, x$round, x$idno)

# design with the model data attached, flags mark the rows of each sample
design_for <- function(dm) {
  dm_cols <- dm %>% mutate(key = key(dm), in_model = TRUE) %>%
    select(-idno, -cntry, -round, -anweight, -w)
  dat <- full %>%
    select(idno, cntry, round, psu_id, strat_id, anweight) %>%
    mutate(key = key(.)) %>%
    left_join(dm_cols, by = "key") %>%
    mutate(in_model = !is.na(in_model))
  des <- svydesign(ids = ~psu_id, strata = ~strat_id, weights = ~anweight,
                   data = dat, nest = TRUE)
  subset(des, in_model)
}

des  <- design_for(d)
des3 <- des                     # same rows (see step 7)
stopifnot(nrow(des) == nrow(d))

s1  <- svyolr(formula(m1),  design = des)
s2  <- svyolr(formula(m2),  design = des)
s3b <- svyolr(formula(m3b), design = des3)

# OR table from svyolr, plus how much larger the design SE is than polr's
svy_table <- function(s, m) {
  b  <- coef(s)[names(coef(m))]
  se <- sqrt(diag(vcov(s)))[names(coef(m))]
  data.frame(term = names(b),
             OR   = exp(b),
             lo   = exp(b - 1.96 * se),
             hi   = exp(b + 1.96 * se),
             p    = 2 * pnorm(-abs(b / se)),
             se_ratio = se / sqrt(diag(vcov(m)))[names(coef(m))],
             row.names = NULL)
}

cat("\n10. Design-based results (svyolr)\n")
cat("\nCoefficients same as polr, largest difference M2:",
    signif(max(abs(coef(s2)[names(coef(m2))] - coef(m2))), 2), "\n")

cat("\nM1:\n");  print(svy_table(s1, m1),  digits = 3)
cat("\nM2:\n");  print(svy_table(s2, m2),  digits = 3)
cat("\nM3b:\n"); print(svy_table(s3b, m3b), digits = 3)

# design-based Wald tests for the interaction blocks (replace the LR tests)
s4a <- svyolr(formula(m4a), design = des)
s4b <- svyolr(formula(m4b), design = des)
cat("\nBlock A, design-based Wald test:\n")
print(regTermTest(s4a, ~ cntry:hincfel + cntry:health + cntry:trstprl, method = "Wald"))
cat("\nBlock B, design-based Wald test:\n")
print(regTermTest(s4b, ~ cntry:age5, method = "Wald"))
# decision: svyolr CIs and p-values reported for M1, M2, M3b, Wald tests
# for the blocks (on M2: A p = 0.004, B p = 0.009), polr kept for AIC/LR/checks


# 11. cross-validation on the train folds
# each model is fitted on 9 folds and predicts the 10th
# prediction = expected score, sum of k x P(score = k), comparable with OLS
# and the ML models, metrics weighted with anweight on all out-of-fold rows
# log-loss = minus log probability of the true score (ordered logit only)
cv_polr <- function(m, data) {
  scores <- 0:10
  oof <- lapply(sort(unique(data$fold)), function(k) {
    fit <- update(m, data = data[data$fold != k, ])
    te  <- data[data$fold == k, ]
    p   <- predict(fit, newdata = te, type = "probs")
    y   <- as.numeric(as.character(te$stflife))
    data.frame(fold = k, y = y, pred = drop(p %*% scores),
               p_true = p[cbind(seq_len(nrow(te)), y + 1)],
               wt = te$anweight)
  })
  do.call(rbind, oof)
}

# weighted metrics, R2 against the weighted mean of the true scores
cv_metrics <- function(o) {
  ybar <- weighted.mean(o$y, o$wt)
  c(R2      = 1 - sum(o$wt * (o$y - o$pred)^2) / sum(o$wt * (o$y - ybar)^2),
    RMSE    = sqrt(weighted.mean((o$y - o$pred)^2, o$wt)),
    MAE     = weighted.mean(abs(o$y - o$pred), o$wt),
    logloss = weighted.mean(-log(o$p_true), o$wt))
}

# R2 in each fold, to see how stable it is
fold_r2 <- function(o) sapply(split(o, o$fold), function(x) cv_metrics(x)["R2"])

cv_summary <- function(models, data) {
  oofs <- lapply(models, cv_polr, data = data)
  res  <- t(sapply(oofs, cv_metrics))
  r2s  <- sapply(oofs, fold_r2)
  cbind(res, R2_fold_sd = apply(r2s, 2, sd),
        R2_fold_min = apply(r2s, 2, min), R2_fold_max = apply(r2s, 2, max))
}

set.seed(2026)
cat("\n11. CV, 10 folds, on the", nrow(d), "rows:\n")
cv_a <- cv_summary(list(M1 = m1, M2 = m2, M3b = m3b, M3 = m3), d)
print(round(cv_a, 3))

# calibration: predicted vs actual share of each score (M2, out-of-fold)
oof_m2 <- cv_polr(m2, d)
pred_share <- sapply(sort(unique(oof_m2$fold)), function(k) {
  fit <- update(m2, data = d[d$fold != k, ])
  te  <- d[d$fold == k, ]
  colSums(predict(fit, newdata = te, type = "probs") * te$anweight)
})
calib <- rbind(actual    = tapply(oof_m2$wt, factor(oof_m2$y, levels = 0:10), sum),
               predicted = rowSums(pred_share))
calib <- 100 * calib / sum(oof_m2$wt)
cat("\nM2 calibration, % of people per score (weighted, out-of-fold):\n")
print(round(calib, 1))
# decision: final models M2 and M3b (interpretation), M3 (prediction only)


# 11b. test set, only for the final models, no tuning on it
# (first scored with complete cases on 2026-11-18, re-scored after imputation
# was added in 03, models unchanged)
# models fitted on the full train set (above), same coding via load_vars
# and code_vars, metrics weighted with anweight as in the CV
test_d <- code_vars(load_vars(readRDS("data/ess_test.rds")))

# test predictions in the same format as cv_polr, so cv_metrics can be reused
test_pred <- function(m, te) {
  p <- predict(m, newdata = te, type = "probs")
  y <- as.numeric(as.character(te$stflife))
  data.frame(y = y, pred = drop(p %*% 0:10),
             p_true = p[cbind(seq_len(nrow(te)), y + 1)], wt = te$anweight)
}

# all test rows, missing values filled with the train values in 03
stopifnot(!anyNA(test_d[c(main_vars, other_vars)]))
cat("\n11b. Test set:", nrow(test_d), "rows, imputed:", sum(test_d$imputed), "\n")

test_a <- t(sapply(list(M1 = m1, M2 = m2, M3b = m3b, M3 = m3),
                   function(m) cv_metrics(test_pred(m, test_d))))

cat("\nTest metrics:\n")
print(round(cbind(test_a, CV_R2 = cv_a[rownames(test_a), "R2"]), 3))

# calibration on the test set, M2
p_te  <- predict(m2, newdata = test_d, type = "probs")
calib_te <- rbind(actual    = tapply(test_d$anweight, test_d$stflife, sum),
                  predicted = colSums(p_te * test_d$anweight))
calib_te <- 100 * calib_te / sum(test_d$anweight)
cat("\nM2 calibration on the test set, % of people per score:\n")
print(round(calib_te, 1))


# 12. partial proportional odds (robustness check for 6a), on M3b
# the variables that broke proportional odds in 6a get a separate effect at
# each cut-off (nominal), all others keep one effect
# if the single effects (income, health, trust ...) barely change, the
# violation does not bias them
library(ordinal)

# all four variables at once give crossing cut-offs (invalid fit, sclmeet adds
# 6 levels x 10 cut-offs), so two partial fits: country + age + activity,
# and sclmeet alone, each is valid
f_po <- formula(m3b)
c_po <- clm(f_po, data = d3, weights = w)   # same model as m3b, as a check

ppo_fit <- function(vars) {
  f <- update(f_po, as.formula(paste(". ~ . -", paste(vars, collapse = " - "))))
  clm(f, nominal = as.formula(paste("~", paste(vars, collapse = " + "))),
      data = d3, weights = w)
}
ppo <- list(PPO_a = ppo_fit(c("cntry", "age5", "activity_grp")),
            PPO_b = ppo_fit("sclmeet"))

cat("\n12. Partial proportional odds on M3b\n")
cat("clm vs polr, largest coefficient difference:",
    signif(max(abs(coef(c_po)[names(coef(m3b))] - coef(m3b))), 2), "\n")
cat("Valid fits (code 0):", sapply(ppo, function(m) m$convergence$code == 0), "\n")

cat("\nFit vs M3b:\n")
print(round(t(sapply(c(list(M3b = c_po), ppo), function(m) c(
  df = length(coef(m)), logLik = as.numeric(logLik(m)), AIC = AIC(m),
  LR_p = if (identical(m, c_po)) NA else pchisq(
    2 * (as.numeric(logLik(m)) - as.numeric(logLik(c_po))),
    length(coef(m)) - length(coef(c_po)), lower.tail = FALSE)))), 4))

# single-effect variables: those that keep one effect in both partial fits
shared_ppo <- intersect(names(ppo$PPO_a$beta), names(ppo$PPO_b$beta))
ors <- sapply(c(list(M3b = c_po), ppo), function(m) exp(m$beta[shared_ppo]))
chg <- 100 * (ors[, -1] / ors[, "M3b"] - 1)
colnames(chg) <- paste0(colnames(chg), "_chg_pct")
cat("\nSingle-effect variables, M3b vs partial fits (OR, change in %):\n")
print(round(cbind(ors, chg), 3))
# decision: proportional-odds violations do not bias the other effects,
# single-effect ORs change < 5% (largest eisced1 -4.8%, key determinants < 1%),
# M3b stays


# 13. M4 block C: do income, health and trust matter equally for each age group?
# the mirror of block A (country), answers "for each age group" in the
# report's introduction, Angelini et al. expect health to matter more for older people
# added to M2 like blocks A and B, Bonferroni now for 3 blocks: p < 0.05 / 3 = 0.0167
m4c <- update(m2, . ~ . + age5:hincfel + age5:health + age5:trstprl)

cat("\n13. Block C, age group x income, health, trust\n")
print(round(rbind(M2 = fit_stats(m2), M4C = fit_stats(m4c)), 3))
cat("LR test M4C vs M2:\n"); print(signif(lr_test(m2, m4c), 3))

s4c <- svyolr(formula(m4c), design = des)
cat("\nBlock C, design-based Wald test:\n")
print(regTermTest(s4c, ~ age5:hincfel + age5:health + age5:trstprl, method = "Wald"))

# effect of a term in each age group = main effect + age interaction,
# OR with 95% CI from the design-based covariance
by_age <- function(s, term) {
  b <- coef(s); V <- vcov(s)
  sapply(levels(d$age5), function(a) {
    nm <- term
    if (a != levels(d$age5)[1]) {
      int <- c(paste0("age5", a, ":", term), paste0(term, ":age5", a))
      nm  <- c(term, int[int %in% names(b)])
    }
    est <- sum(b[nm]); se <- sqrt(sum(V[nm, nm]))
    round(exp(c(OR = est, lo = est - 1.96 * se, hi = est + 1.96 * se)), 3)
  })
}

cat("\nBlock C, OR per age group (design-based CI):\n")
for (v in c("hincfel", "health", "trstprl")) {
  cat("\n", v, ":\n", sep = ""); print(by_age(s4c, v))
}
# decision: no evidence that income, health or trust matter differently by
# age group (design Wald p = 0.40), no age interactions in the models


# 13b. blocks A, B and C on the final model M3b
# M3b is the interpretive model, so the "no interactions" decision is checked
# on it too, same rows as M2 (imputed data), the M2 versions above are kept
# for comparison, Bonferroni for 3 blocks: p < 0.05 / 3 = 0.0167
int_terms <- list(A = ~ cntry:hincfel + cntry:health + cntry:trstprl,
                  B = ~ cntry:age5,
                  C = ~ age5:hincfel + age5:health + age5:trstprl)
m5 <- lapply(int_terms, function(f)
  update(m3b, as.formula(paste(". ~ . +", deparse(f[[2]], width.cutoff = 500)))))
stopifnot(all(sapply(m5, function(m) length(coef(m)) > length(coef(m3b)))))

cat("\n13b. Blocks A, B, C added to M3b\n")
print(round(rbind(M3b = fit3(m3b), t(sapply(m5, fit3))), 3))

# do.call puts the formula itself in the stored call, regTermTest re-evaluates it
s5 <- lapply(m5, function(m) do.call(svyolr, list(formula = formula(m), design = des)))
wald5 <- t(sapply(names(s5), function(k) {
  w <- regTermTest(s5[[k]], int_terms[[k]], method = "Wald")
  c(F = unname(w$Ftest), df = w$df, p = unname(w$p))
}))
cat("\nDesign-based Wald tests on M3b (M2 versions: steps 10 and 13):\n")
print(signif(wald5, 3))

cat("\nBlock A on M3b, OR per country (design-based CI):\n")
for (v in c("hincfel", "health", "trstprl")) {
  cat("\n", v, ":\n", sep = ""); print(by_country(s5$A, v))
}
cat("\nBlock B on M3b, age group OR (vs 15-29) per country (design-based CI):\n")
for (a in paste0("age5", levels(d$age5)[-1])) {
  cat("\n", a, ":\n", sep = ""); print(by_country(s5$B, a))
}
cat("\nBlock C on M3b, OR per age group (design-based CI):\n")
for (v in c("hincfel", "health", "trstprl")) {
  cat("\n", v, ":\n", sep = ""); print(by_age(s5$C, v))
}
# decision: no interactions in M3b, blocks reported as tests
# A significant (p = 0.0006): income and trust equal in all countries, health
# stronger in LT (OR 0.55 vs 0.68 in EE, CIs do not overlap)
# B not significant on M3b (p = 0.024 > 0.0167, was 0.009 on M2), same
# direction as on M2, C not significant (p = 0.41)
# AIC gain small (A -12, B -5, C +3 on about 38,960)


# 14. importance of each variable in M3b (drop-one)
# refit M3b without one variable (factors as a whole), the loss in logLik
# is that variable's unique contribution, shown as a share of all that
# M3b explains (its logLik gain over the null model)
# unique = shared parts with correlated variables (e.g. sclmeet and sclact)
# count for neither, so related variables can look smaller than together
# Wald F per variable from svyolr, the design-based test of the whole variable
vars_m3b <- attr(terms(m3b), "term.labels")
ll_m3b   <- as.numeric(logLik(m3b))
explained <- ll_m3b - as.numeric(logLik(m0_3))


imp <- do.call(rbind, lapply(vars_m3b, function(v) {
  m_drop <- update(m3b, as.formula(paste(". ~ . -", v)))
  loss   <- ll_m3b - as.numeric(logLik(m_drop))
  wt     <- regTermTest(s3b, as.formula(paste("~", v)), method = "Wald")
  data.frame(variable  = v,
             df        = length(coef(m3b)) - length(coef(m_drop)),
             LR        = 2 * loss,
             dAIC      = AIC(m_drop) - AIC(m3b),
             share_pct = 100 * loss / explained,
             Wald_p    = as.numeric(wt$p))
}))
imp <- imp[order(-imp$LR), ]

cat("\n14. Importance in M3b, drop-one (sorted by LR):\n")
print(imp, digits = 3, row.names = FALSE)
cat("\nSum of unique shares:", round(sum(imp$share_pct), 1),
    "% (the rest is shared between correlated variables)\n")

# 14b. importance by concept (Dolan et al. areas), each group dropped as a block
# a group's share includes what its own variables share with each other,
# so related variables (e.g. the social ones) are no longer split up
groups <- list(
  Income        = "hincfel",
  Health        = "health",
  Work          = c("activity_grp", "ever_unemployed"),
  Education     = "eisced",
  Relationships = c("partner", "sclmeet", "sclact", "inprdsc", "hhmmb"),
  Trust         = c("trstprl", "ppltrst"),
  Religiosity   = "rlgdgr",
  Personal      = c("age5", "female", "russian_home"),
  Environment   = c("domicil", "aesfdrk"),
  Country_time  = c("cntry", "round", "lv_r10"))
stopifnot(setequal(unlist(groups), vars_m3b))

imp_g <- do.call(rbind, lapply(names(groups), function(g) {
  v      <- groups[[g]]
  m_drop <- update(m3b, as.formula(paste(". ~ . -", paste(v, collapse = " - "))))
  loss   <- ll_m3b - as.numeric(logLik(m_drop))
  wt     <- regTermTest(s3b, as.formula(paste("~", paste(v, collapse = " + "))),
                        method = "Wald")
  data.frame(group     = g,
             df        = length(coef(m3b)) - length(coef(m_drop)),
             LR        = 2 * loss,
             dAIC      = AIC(m_drop) - AIC(m3b),
             share_pct = 100 * loss / explained,
             Wald_p    = as.numeric(wt$p))
}))
imp_g <- imp_g[order(-imp_g$LR), ]

cat("\n14b. Importance by group in M3b, drop-one (sorted by LR):\n")
print(imp_g, digits = 3, row.names = FALSE)
cat("\nSum of group shares:", round(sum(imp_g$share_pct), 1),
    "% (the rest is shared between groups)\n")


# 15. predicted probabilities
# average predicted probability of high life satisfaction (score 8 or more):
# set a variable to one value for everyone, keep all else as observed,
# average over people with anweight (a population average, no "typical person")
# 95% CI by drawing coefficients and cut-points from the design-based
# covariance of svyolr (1,000 draws)
# lv_r10 set to 0 for everyone, so countries and rounds compare like-for-like

# P(score >= 8) and expected score for every person, for one set of parameters
pred_people <- function(m, par, nd) {
  X    <- model.matrix(delete.response(terms(m)), data = nd, xlev = m$xlevels)[, -1]
  b    <- par[colnames(X)]
  zeta <- par[grep("\\|", names(par))]
  eta  <- drop(X %*% b)
  cum  <- plogis(outer(-eta, zeta, "+"))          # P(score <= k), k = 0..9
  p    <- cbind(cum, 1) - cbind(0, cum)           # P(score = k), k = 0..10
  list(p8 = 1 - cum[, "7|8"], score = drop(p %*% 0:10))
}

# population averages for each scenario, with simulation CIs
scenario_probs <- function(m, s, data, var, values) {
  set.seed(2026)
  draws <- MASS::mvrnorm(1000, coef(s), vcov(s))
  data$lv_r10 <- 0L
  res <- lapply(values, function(val) {
    nd <- data
    nd[[var]] <- if (is.factor(data[[var]]))
      factor(val, levels = levels(data[[var]])) else val
    est  <- pred_people(m, coef(s), nd)
    sims <- apply(draws, 1, function(par)
      weighted.mean(pred_people(m, par, nd)$p8, nd$anweight))
    data.frame(variable = var, value = as.character(val),
               P8 = 100 * weighted.mean(est$p8, nd$anweight),
               lo = 100 * quantile(sims, 0.025), hi = 100 * quantile(sims, 0.975),
               score = weighted.mean(est$score, nd$anweight),
               row.names = NULL)
  })
  do.call(rbind, res)
}

# 15a. key determinants in M3b
scen <- list(hincfel      = 1:4,
             health       = 1:5,
             trstprl      = c(0, 5, 10),
             ppltrst      = c(0, 5, 10),
             partner      = 0:1,
             activity_grp = c("Working", "Unemployed"),
             sclmeet      = c("1", "5"),
             female       = 0:1,
             russian_home = 0:1)

pp <- do.call(rbind, lapply(names(scen), function(v)
  scenario_probs(m3b, s3b, d3, v, scen[[v]])))

cat("\n15a. M3b, % with life satisfaction 8 or more (95% CI) and mean score:\n")
print(pp, digits = 3, row.names = FALSE)

# 15b. country and age, round and mode only (M1) vs all variables (M3b)
s1_3 <- s1                      # same rows (see step 7)
m1_3 <- m1

ca <- function(var, values) {
  rbind(cbind(model = "M1", scenario_probs(m1_3, s1_3, d3, var, values)),
        cbind(model = "M3b", scenario_probs(m3b, s3b, d3, var, values)))
}
cat("\n15b. Country and age, M1 (round and mode only) vs M3b (all variables):\n")
print(rbind(ca("cntry", levels(d3$cntry)), ca("age5", levels(d3$age5))),
      digits = 3, row.names = FALSE)

