# Ordered logit for life satisfaction (stflife 0-10), how the final model
# M3 was built and the final fit on all data
# steps 1-7, train set: M1 (country, round, age), M2 (+ main variables),
#   M3 (+ other variables, the final model), LR tests, CV and test
# steps 8-9, all data (train + test): the final M3 with design-based odds
#   ratios for the report, the country and age gaps it explains, and the
#   interaction tests
# M3 on the train set for the group comparison is in 06a_ordered_logit_full_model.R,
# the assumption and robustness checks are in 06c_ordered_logit_checks.R
# earlier versions (M3b, checks on M2, importance, predicted probabilities on
# M3b) are in git history, commit 0cef0da
# run 01_clean.R and 03_train_test_split.R first, working directory = repo folder
# input:  data/ess_train.rds (with fold and imputed columns), data/ess_test.rds,
#         data/ess_baltic_clean.rds (survey design, step 8),
#         outputs/tables/ologit_check_robustness.csv (06c, optional, to check step 9)
# output: outputs/tables/ologit_final_coefficients.csv (step 9a, the final M3
#         on all data, design-based ORs), everything else printed only
# packages: dplyr, MASS, survey
#   install.packages(c("dplyr", "survey"))  # MASS comes with R
# runs several minutes (mostly the simulations in step 9b)
# results checked with R 4.5.2, dplyr 1.1.4, MASS 7.3-65, survey 4.5

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
# as a function, so the test set (steps 7b and 9) is loaded the same way
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
# CIs from polr, they ignore the survey design (design-based SEs in step 9)
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
# lv_r10 in all models, Latvia and R10 then compare like-for-like rounds
# (06c checks that dropping the Latvia R10 rows gives the same results)
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


# 6. M3: M2 plus the other variables, the final model
# all 22 group predictors, the same model as 06a, here on the train set to
# show what the other variables add, the final fit on all data is in step 9
m3 <- update(m2, . ~ . + eisced + domicil + russian_home + inprdsc +
               sclact + hhmmb + ppltrst + stfeco + stfhlth + aesfdrk +
               ever_unemployed)

cat("\nModel fit on the", nrow(d), "rows:\n")
print(round(rbind(M2 = fit_stats(m2), M3 = fit_stats(m3)), 3))
cat("\nLR test M3 vs M2:\n"); print(signif(lr_test(m2, m3), 3))

cat("\nM3 odds ratios:\n")
print(or_table(m3), digits = 3)

# how the M2 variables change once the other variables are added
# stfeco and stfhlth absorb part of the trust, income and health effects
cat("\nM2 variables, M2 vs M3 on the same rows (OR):\n")
print(round(data.frame(M2 = exp(coef(m2)),
                       M3 = exp(coef(m3)[names(coef(m2))])), 3))
# assumption and robustness checks of M3 are in 06c_ordered_logit_checks.R


# 7. cross-validation on the train folds
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
cat("\n7. CV, 10 folds, on the", nrow(d), "rows:\n")
cv_a <- cv_summary(list(M1 = m1, M2 = m2, M3 = m3), d)
print(round(cv_a, 3))

# 7b. test set, no tuning on it
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
cat("\n7b. Test set:", nrow(test_d), "rows, imputed:", sum(test_d$imputed), "\n")

test_a <- t(sapply(list(M1 = m1, M2 = m2, M3 = m3),
                   function(m) cv_metrics(test_pred(m, test_d))))

cat("\nTest metrics:\n")
print(round(cbind(test_a, CV_R2 = cv_a[rownames(test_a), "R2"]), 3))
# M3 test metrics are the same as 06a (ologit_metrics.csv)


# 8. helpers for the final model
library(survey)
options(survey.lonely.psu = "adjust")

# design-based standard errors: polr SEs treat the rows as a simple random
# sample, svyolr uses the ESS design (PSUs, strata, anweight) as in
# survey_design.R, the coefficients are the same as polr, only the SEs change
# the design is built on the full data and then subset to the model rows,
# so the strata keep all their PSUs (a design built on the subset would not)
# polr is still used for AIC and LR (svyolr has no likelihood)
full <- readRDS("data/ess_baltic_clean.rds")

# model rows matched to the full data by country, round and idno
key <- function(x) paste(x$cntry, x$round, x$idno)

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

# predicted probabilities: average predicted probability of high life
# satisfaction (score 8 or more), set a variable to one value for everyone,
# keep all else as observed, average over people with anweight (a population
# average, no "typical person"), 95% CI by drawing coefficients and cut-points
# from the design-based covariance of svyolr (1,000 draws)
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

# effect of a term in each country (age group) = main effect + interaction,
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

# interaction blocks, chosen in advance:
# A: do income, health and trust matter equally in each country?
# B: is the age pattern the same in each country?
# C: do income, health and trust matter equally for each age group?
int_terms <- list(A = ~ cntry:hincfel + cntry:health + cntry:trstprl,
                  B = ~ cntry:age5,
                  C = ~ age5:hincfel + age5:health + age5:trstprl)


# 9. the final model M3 on all data: OR table, differences by country and age
# the group chose M3 (with stfeco and stfhlth) as the final ordered logit,
# fitted on all data (train + test) for interpretation, the test metrics stay
# from the train fit (06a, step 7b), checks of this fit are in 06c
# steps 1-7 above are on the train set
# M3 controls for stfeco, which itself differs by country, so its country
# effects are the gaps left among people who rate the economy equally

# all data, loaded and coded as in steps 1-2, weights rescaled to mean 1
test_raw <- readRDS("data/ess_test.rds")
dF <- bind_rows(load_vars(train), load_vars(test_raw)) %>%
  mutate(w = anweight / mean(anweight)) %>%
  code_vars()
stopifnot(nrow(dF) == nrow(train) + nrow(test_raw),
          !anyNA(dF[c(main_vars, other_vars)]),
          !anyDuplicated(key(dF)))
desF <- design_for(dF)
stopifnot(nrow(desF) == nrow(dF))
cat("\n9. All data:", nrow(dF), "rows\n")

m1F <- polr(formula(m1), data = dF, weights = w, Hess = TRUE)
m3F <- polr(formula(m3), data = dF, weights = w, Hess = TRUE)
s1F <- svyolr(formula(m1), design = desF)
s3F <- svyolr(formula(m3), design = desF)
stopifnot(max(abs(coef(s3F)[names(coef(m3F))] - coef(m3F))) < 1e-3)

# the fit must match the reference of 06c (same data, same model)
rob <- "outputs/tables/ologit_check_robustness.csv"
if (file.exists(rob)) {
  r <- subset(read.csv(rob), check == "train_only")
  stopifnot(max(abs(r$coef_ref - coef(s3F)[r$term])) < 1e-3)
}

# 9a. OR table of the final model, design-based SEs, the same columns as
# outputs/tables/ologit_coefficients.csv from 06a (that one is the train fit)
bF <- coef(s3F)
seF <- sqrt(diag(vcov(s3F)))[names(bF)]
final_tab <- data.frame(term  = names(bF),
                        type  = ifelse(names(bF) %in% names(m3F$zeta), "cutpoint", "coefficient"),
                        coef  = round(bF, 4),
                        se    = round(seF, 4),
                        p     = signif(2 * pnorm(-abs(bF / seF)), 3),
                        OR    = round(exp(bF), 4),
                        OR_lo = round(exp(bF - 1.96 * seF), 4),
                        OR_hi = round(exp(bF + 1.96 * seF), 4),
                        row.names = NULL)
final_tab[final_tab$type == "cutpoint", c("OR", "OR_lo", "OR_hi")] <- NA
cat("\n9a. Final model M3, all data, design-based ORs:\n")
print(final_tab, row.names = FALSE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(final_tab, "outputs/tables/ologit_final_coefficients.csv", row.names = FALSE)

# 9b. how much of the country and age gaps do the variables explain?
# M1 (country, round, mode, age only) = the raw gaps, M3 = the gaps at equal
# values of all other variables, % with score 8 or more and mean score
caF <- function(var, values) {
  rbind(cbind(model = "M1", scenario_probs(m1F, s1F, dF, var, values)),
        cbind(model = "M3", scenario_probs(m3F, s3F, dF, var, values)))
}
cat("\n9b. Country and age, M1 (round and mode only) vs M3 (all variables):\n")
print(rbind(caF("cntry", levels(dF$cntry)), caF("age5", levels(dF$age5))),
      digits = 3, row.names = FALSE)

cat("\nCountry and age ORs, M1 vs M3 (design-based CI):\n")
print(merge(svy_table(s1F, m1F)[, c("term", "OR", "lo", "hi", "p")],
            svy_table(s3F, m3F)[, c("term", "OR", "lo", "hi", "p")],
            by = "term", suffixes = c("_M1", "_M3")),
      digits = 3, row.names = FALSE)

# 9c. do the determinants work differently by country and age? blocks A, B
# and C (chosen in advance, step 8) added to M3, design-based Wald
# tests, Bonferroni for 3 blocks: p < 0.05 / 3 = 0.0167
# polr refitted directly (update() would re-evaluate data = d, the train set)
m6 <- lapply(int_terms, function(f)
  polr(update(formula(m3), as.formula(paste(". ~ . +", deparse(f[[2]], width.cutoff = 500)))),
       data = dF, weights = w, Hess = TRUE))
stopifnot(all(sapply(m6, function(m) length(coef(m)) > length(coef(m3F)))))

fitF <- function(m) c(df = length(coef(m)), logLik = as.numeric(logLik(m)), AIC = AIC(m))
cat("\n9c. Blocks A, B, C added to M3\n")
print(round(rbind(M3 = fitF(m3F), t(sapply(m6, fitF))), 1))

# do.call puts the formula itself in the stored call, regTermTest re-evaluates it
s6 <- lapply(m6, function(m) do.call(svyolr, list(formula = formula(m), design = desF)))
wald6 <- t(sapply(names(s6), function(k) {
  w <- regTermTest(s6[[k]], int_terms[[k]], method = "Wald")
  c(F = unname(w$Ftest), df = w$df, p = unname(w$p))
}))
cat("\nDesign-based Wald tests on M3, all data:\n")
print(signif(wald6, 3))

cat("\nBlock A on M3, OR per country (design-based CI):\n")
for (v in c("hincfel", "health", "trstprl")) {
  cat("\n", v, ":\n", sep = ""); print(by_country(s6$A, v))
}
cat("\nBlock B on M3, age group OR (vs 15-29) per country (design-based CI):\n")
for (a in paste0("age5", levels(dF$age5)[-1])) {
  cat("\n", a, ":\n", sep = ""); print(by_country(s6$B, a))
}
cat("\nBlock C on M3, OR per age group (design-based CI):\n")
for (v in c("hincfel", "health", "trstprl")) {
  cat("\n", v, ":\n", sep = ""); print(by_age(s6$C, v))
}
# result (all data, 2026-11-27):
# 9b: raw (M1) % with 8+: LT 43.8, LV 41.2, EE 49.9, at equal values of all
# other variables (M3): LT 39.4, LV 46.9, EE 41.2, so Estonia's lead is
# explained by circumstances and Latvia is the most satisfied
# raw age gap (15-29 55.1, 60-74 35.4) mostly explained, at equal values
# 15-74 are 40-44 and 75+ 48.2 (CI overlaps 15-29), at least as satisfied
# 9c: A significant (p = 0.006 < 0.0167): income and trust equal in all
# countries, health stronger in LT (OR 0.59 vs 0.71 in EE, CIs do not
# overlap), B (p = 0.044) and C (p = 0.105) not significant after Bonferroni
# decision: no interactions in the final model, block A reported as a
# separate finding
