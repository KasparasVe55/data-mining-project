# Checks of the final ordered logit M3: assumptions (A) and robustness (B)
# the reference is M3 fitted on all data (train + test, 12,688 rows), the
# model reported for interpretation
# the test metrics stay from the train fit in 06a (out of sample), the
# all-data fit has no test metrics, it has seen every row
# the checks cannot prove the model is right, they show whether its
# assumptions hold well enough and whether the conclusions depend on choices
#
# A. assumption checks, on the all-data M3
# A1 proportional odds: binary logits at cut-offs 5, 7, 8, 9, one effect per
#    variable should fit every cut-off
# A2 partial proportional odds: the variables that break A1 get an effect per
#    cut-off, do the other odds ratios change?
# A3 linearity: each numeric variable as a factor instead, does it fit better?
# A4 multicollinearity: GVIF
# (independence of observations is handled by the design-based SEs)
#
# B. robustness checks, M3 refitted on other rows and compared with the
#    all-data M3, the refits share most rows with it, so they are not
#    independent and the differences are only described, not tested
# check 1: all data vs the train set only (the 06a fit)
#   if the odds ratios barely change, the estimates do not depend on the
#   80/20 split
# check 2: all data (imputed in 03) vs the complete cases only
#   shows whether filling in missing predictors changes the estimates
# check 3: all data vs all data without the Latvia R10 rows (self-completion
#   survey mode), shows whether the lv_r10 indicator handles the mode change
# run 01_clean.R, 03_train_test_split.R and 06a first, working directory = repo folder
# input:  data/ess_train.rds, data/ess_test.rds (both imputed in 03),
#         data/ess_baltic_clean.rds (survey design for the standard errors),
#         outputs/tables/ologit_coefficients.csv (from 06a, to check the train fit)
# output: outputs/tables/ologit_check_po.csv (A1, coefficients per cut-off)
#         outputs/tables/ologit_check_ppo.csv (A2, odds ratios with and
#           without partial proportional odds)
#         outputs/tables/ologit_check_linearity.csv (A3, effect per level,
#           factor vs linear, with the test per variable)
#         outputs/tables/ologit_check_linearity_other.csv (A3b, other odds
#           ratios with the non-linear scales as factors)
#         outputs/tables/ologit_check_gvif.csv (A4)
#         outputs/tables/ologit_check_robustness.csv (B, one row per check
#           and term, ref = M3 on all data, alt = the refitted M3)
# packages: dplyr, MASS, survey, car, ordinal
#   install.packages(c("dplyr", "survey", "car", "ordinal"))  # MASS comes with R

library(dplyr)
library(MASS, exclude = "select")
library(survey)

train <- readRDS("data/ess_train.rds")
test  <- readRDS("data/ess_test.rds")

# same predictors as in 03_train_test_split.R and 06a
predictors <- c("agea", "female", "hincfel", "activity_grp", "health", "sclmeet",
                "partner", "trstprl", "rlgdgr", "eisced", "domicil", "russian_home",
                "inprdsc", "sclact", "hhmmb", "ppltrst", "stfeco", "stfhlth",
                "aesfdrk", "ever_unemployed", "cntry", "round")

dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)


# 1. data and coding, the same as prep() in 06a
prep <- function(x) {
  x %>%
    select(idno, stflife, essround, all_of(predictors), anweight, imputed) %>%
    mutate(stflife = factor(stflife, levels = 0:10, ordered = TRUE),
           age5    = cut(agea, breaks = c(14, 29, 44, 59, 74, Inf),
                         labels = c("15-29", "30-44", "45-59", "60-74", "75+")),
           domicil = factor(domicil, levels = 1:5),
           sclact  = factor(sclact,  levels = 1:5),
           sclmeet = factor(sclmeet, levels = 1:7),
           eisced  = relevel(factor(eisced, levels = 1:7), ref = "4"),
           hhmmb   = factor(pmin(hhmmb, 6), levels = 1:6, labels = c(1:5, "6+")),
           lv_r10  = as.integer(cntry == "LV" & round == "R10 2020-22"))
}

# weights rescaled to mean 1 within each data set, as in 06a
d_train <- prep(train) %>% mutate(w = anweight / mean(anweight))
d_all   <- bind_rows(prep(train), prep(test)) %>% mutate(w = anweight / mean(anweight))
stopifnot(!anyNA(d_all[predictors]),
          nrow(d_all) == nrow(train) + nrow(test),
          !anyDuplicated(paste(d_all$cntry, d_all$round, d_all$idno)))


# 2. M3 with design-based standard errors
# the same formula as 06a, fitted with polr, then svyolr for the SEs
# the design is built on the full cleaned data and subset to the rows used,
# so the strata keep all their PSUs
options(survey.lonely.psu = "adjust")
full <- readRDS("data/ess_baltic_clean.rds")
key  <- function(x) paste(x$cntry, x$round, x$idno)

f_m3 <- stflife ~ cntry + round + lv_r10 + age5 + female + hincfel + activity_grp +
  health + sclmeet + partner + trstprl + rlgdgr +
  eisced + domicil + russian_home + inprdsc + sclact + hhmmb +
  ppltrst + stfeco + stfhlth + aesfdrk + ever_unemployed

# f can be changed when a check drops a term, the default is M3
fit_m3 <- function(d, f = f_m3) {
  m <- polr(f, data = d, weights = w, Hess = TRUE)

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

  s  <- svyolr(f, design = des)
  b  <- coef(s)
  se <- sqrt(diag(vcov(s)))[names(b)]
  tab <- data.frame(term = names(b),
                    type = ifelse(names(b) %in% names(m$zeta), "cutpoint", "coefficient"),
                    coef = b, se = se, p = 2 * pnorm(-abs(b / se)),
                    row.names = NULL)
  attr(tab, "polr") <- m   # the polr fit, used by the assumption checks
  tab
}

# reference: M3 on all data, the final model for interpretation
res_ref <- fit_m3(d_all)

# the train fit (check 1) must reproduce the saved 06a coefficients
res_train <- fit_m3(d_train)
saved <- read.csv("outputs/tables/ologit_coefficients.csv")
stopifnot(identical(saved$term, res_train$term),
          max(abs(saved$coef - res_train$coef)) < 1e-3)

m_all   <- attr(res_ref, "polr")     # polr fits for the assumption checks
m_train <- attr(res_train, "polr")
rhs     <- delete.response(terms(f_m3))
ll      <- function(m) as.numeric(logLik(m))

# which variable each model column belongs to (factors have several columns)
X        <- model.matrix(rhs, data = d_all)
term_var <- setNames(attr(terms(rhs), "term.labels")[attr(X, "assign")[-1]],
                     colnames(X)[-1])
stopifnot(setequal(names(term_var), names(coef(m_all))))


# ===================== A. assumption checks ==================================

# A1. proportional odds
# binary logits P(stflife >= k) at a few cut-offs, if the assumption holds
# their coefficients are close to M3's single coefficient
# spread = largest minus smallest cut-off coefficient (log-odds scale)
# quasibinomial = same estimates as binomial, no warning for non-integer weights
cutoffs <- c(5, 7, 8, 9)
po <- sapply(cutoffs, function(k) {
  dk <- d_all %>% mutate(y = as.integer(as.numeric(as.character(stflife)) >= k))
  coef(glm(update(rhs, y ~ .), data = dk, weights = w, family = quasibinomial))[-1]
})
colnames(po) <- paste0("ge", cutoffs)
po_tab <- data.frame(term = rownames(po), variable = term_var[rownames(po)],
                     M3 = coef(m_all)[rownames(po)], po, row.names = NULL)
po_tab$spread <- apply(po, 1, function(x) max(x) - min(x))

cat("\nA1. Proportional odds, share of rows at or above each cut-off:",
    round(sapply(cutoffs, function(k)
      weighted.mean(as.numeric(as.character(d_all$stflife)) >= k, d_all$w)), 3), "\n")
cat("Largest spread per variable (log-odds):\n")
po_var <- po_tab %>% group_by(variable) %>%
  summarise(max_spread = max(spread), term = term[which.max(spread)]) %>%
  arrange(desc(max_spread))
print(as.data.frame(po_var), digits = 3, row.names = FALSE)


# A2. partial proportional odds
# each variable with a large A1 spread (> 0.5 log-odds) gets a separate effect
# at each cut-off (nominal) while all others keep one effect, one variable at
# a time (several at once give crossing cut-offs, an invalid fit)
# if the other single effects barely change, the violation does not bias them
library(ordinal)
c_m3 <- clm(f_m3, data = d_all, weights = w)   # same model as m_all, as a check
stopifnot(max(abs(c_m3$beta - coef(m_all)[names(c_m3$beta)])) < 1e-3)

ppo_vars <- po_var$variable[po_var$max_spread > 0.5]
ppo <- lapply(setNames(ppo_vars, ppo_vars), function(v) {
  clm(update(f_m3, as.formula(paste(". ~ . -", v))),
      nominal = as.formula(paste("~", v)), data = d_all, weights = w)
})
ppo_fit <- data.frame(variable = ppo_vars,
                      valid    = sapply(ppo, function(m) m$convergence$code == 0),
                      extra_df = sapply(ppo, function(m) length(coef(m)) - length(coef(c_m3))),
                      dAIC     = sapply(ppo, function(m) AIC(c_m3) - AIC(m)),  # > 0 = better
                      row.names = NULL)
cat("\nA2. Partial proportional odds, one variable at a time:\n")
print(ppo_fit, digits = 3, row.names = FALSE)

# OR change of every other term in each valid partial fit, vs M3
ppo_tab <- do.call(rbind, lapply(ppo_vars[ppo_fit$valid], function(v) {
  b <- ppo[[v]]$beta
  data.frame(nominal = v, term = names(b), variable = term_var[names(b)],
             OR_M3 = exp(c_m3$beta[names(b)]), OR_PPO = exp(b), row.names = NULL)
})) %>% mutate(OR_change_pct = 100 * (OR_PPO / OR_M3 - 1))
cat("Other ORs, median |change| %:", round(median(abs(ppo_tab$OR_change_pct)), 2),
    ", largest:", round(max(abs(ppo_tab$OR_change_pct)), 2), "\n")
print(ppo_tab %>% filter(abs(OR_change_pct) > 3), digits = 3, row.names = FALSE)


# A3. linearity of the numeric variables
# refit M3 with one numeric variable as a factor, if the factor fits much
# better the equal-steps assumption fails for that variable
# effect of each level vs the lowest: from the factor fit, and as the linear
# slope implies it, max_gap = largest difference between the two (log-odds)
# weighted fits give a pseudo-likelihood, so LR and AIC compare models on the
# same rows and weights only
num_vars <- c("hincfel", "health", "aesfdrk", "inprdsc", "trstprl",
              "ppltrst", "rlgdgr", "stfeco", "stfhlth")
lin <- do.call(rbind, lapply(num_vars, function(v) {
  f   <- update(f_m3, as.formula(paste(". ~ . -", v, "+ factor(", v, ")")))
  m_f <- polr(f, data = d_all, weights = w)
  lev <- sort(unique(d_all[[v]]))
  fac <- c(0, coef(m_f)[paste0("factor(", v, ")", lev[-1])])
  lr  <- 2 * (ll(m_f) - ll(m_all))
  df  <- length(coef(m_f)) - length(coef(m_all))
  data.frame(variable = v, level = lev,
             factor_effect = unname(fac),
             linear_effect = unname(coef(m_all)[v] * (lev - lev[1])),
             dAIC = AIC(m_all) - AIC(m_f), LR = lr, df = df,
             p = pchisq(lr, df, lower.tail = FALSE))
}))
lin$gap <- lin$factor_effect - lin$linear_effect

lin_test <- lin %>% group_by(variable) %>%
  summarise(dAIC = first(dAIC), LR = first(LR), df = first(df), p = first(p),
            max_gap = max(abs(gap)), slope_total = abs(last(linear_effect))) %>%
  arrange(desc(dAIC))
cat("\nA3. Linearity, factor vs numeric (dAIC > 0 = factor fits better),",
    "max_gap vs the whole linear effect (slope_total), log-odds:\n")
print(as.data.frame(lin_test), digits = 3, row.names = FALSE)

# A3b. do the non-linear scales bias the other effects?
# all numeric variables where the factor fits clearly better (dAIC > 10) are
# entered as factors at once, the ORs of the other terms are compared with M3
nonlin <- lin_test$variable[lin_test$dAIC > 10]
f_fac  <- update(f_m3, as.formula(paste(". ~ . -", paste(nonlin, collapse = " - "), "+",
                                        paste0("factor(", nonlin, ")", collapse = " + "))))
m_fac  <- polr(f_fac, data = d_all, weights = w)
keep   <- setdiff(names(coef(m_all)), nonlin)
lin_other <- data.frame(term = keep, variable = term_var[keep],
                        OR_M3 = exp(coef(m_all)[keep]),
                        OR_factors = exp(coef(m_fac)[keep]), row.names = NULL) %>%
  mutate(OR_change_pct = 100 * (OR_factors / OR_M3 - 1))
cat("\nA3b. Scales as factors:", nonlin, "\n  dAIC vs M3:",
    round(AIC(m_all) - AIC(m_fac), 1), "for", length(coef(m_fac)) - length(coef(m_all)),
    "extra df\n  other ORs, median |change| %:", round(median(abs(lin_other$OR_change_pct)), 2),
    ", largest:", round(max(abs(lin_other$OR_change_pct)), 2), "\n")
print(lin_other %>% filter(abs(OR_change_pct) > 3), digits = 3, row.names = FALSE)
# decision (2026-11-27): the scales stay numeric, their ORs are reported as
# average trends and the non-linearity as a limitation
# shape: flat from 0 to about 7, a jump at 8-10, largest at 10 (likely a
# response style shared with stflife), key determinants change < 4% (A3b)


# A4. multicollinearity
# VIF depends only on the predictors, so from a weighted linear model with
# the same right-hand side, GVIF^(1/(2 df)) makes factors comparable with
# 1-df terms (rule of thumb: problem above sqrt(5) = 2.2)
gvif <- car::vif(lm(update(rhs, as.numeric(stflife) ~ .), data = d_all, weights = w))
gvif <- data.frame(variable = rownames(gvif), GVIF = gvif[, "GVIF"], df = gvif[, "Df"],
                   GVIF_adj = gvif[, "GVIF^(1/(2*Df))"], row.names = NULL) %>%
  arrange(desc(GVIF_adj))
cat("\nA4. Multicollinearity, highest GVIF^(1/(2 df)):\n")
print(head(gvif, 6), digits = 3, row.names = FALSE)


# ===================== B. robustness checks ==================================


# B0. comparison per term, used by every check
# ref = M3 on all data, alt = the M3 refitted in the check
# OR change in %, SE ratio (alt / ref), whether the alt estimate lies in the
# ref 95% CI, CI overlap, sign and significance changes at 0.05
# cut-points have no odds ratio, they are compared on the coefficient scale
compare <- function(ref, alt, check) {
  cmp <- ref %>%
    inner_join(alt, by = c("term", "type"), suffix = c("_ref", "_alt")) %>%
    mutate(OR_ref  = ifelse(type == "coefficient", exp(coef_ref), NA),
           OR_alt  = ifelse(type == "coefficient", exp(coef_alt), NA),
           OR_change_pct = 100 * (OR_alt / OR_ref - 1),
           se_ratio = se_alt / se_ref,
           lo_ref = coef_ref - 1.96 * se_ref, hi_ref = coef_ref + 1.96 * se_ref,
           lo_alt = coef_alt - 1.96 * se_alt, hi_alt = coef_alt + 1.96 * se_alt,
           alt_in_ref_ci = coef_alt >= lo_ref & coef_alt <= hi_ref,
           ci_overlap    = lo_alt <= hi_ref & lo_ref <= hi_alt,
           sign_change   = sign(coef_ref) != sign(coef_alt),
           sig_change    = (p_ref < 0.05) != (p_alt < 0.05))
  # every alt term must be in the ref model, the alt model may drop terms
  # (check 3 drops lv_r10), those terms are listed and left out
  stopifnot(nrow(cmp) == nrow(alt))
  dropped <- setdiff(ref$term, alt$term)
  if (length(dropped) > 0) cat("\nTerms not in the", check, "model:", dropped, "\n")

  out <- cmp %>%
    transmute(check, term, type,
              coef_ref = round(coef_ref, 4), coef_alt = round(coef_alt, 4),
              OR_ref = round(OR_ref, 4), OR_alt = round(OR_alt, 4),
              OR_change_pct = round(OR_change_pct, 2),
              se_ref = round(se_ref, 4), se_alt = round(se_alt, 4),
              se_ratio = round(se_ratio, 3),
              p_ref = signif(p_ref, 3), p_alt = signif(p_alt, 3),
              alt_in_ref_ci, ci_overlap, sign_change, sig_change)

  # summary over the coefficients (cut-points left out)
  coefs <- filter(out, type == "coefficient")
  cut   <- filter(out, type == "cutpoint")
  cat("\n=== Check:", check, "===",
      "\nCoefficients:", nrow(coefs),
      "\nmedian |OR change| %:", round(median(abs(coefs$OR_change_pct)), 2),
      "\nmax |OR change| %:", round(max(abs(coefs$OR_change_pct)), 2),
      "(", coefs$term[which.max(abs(coefs$OR_change_pct))], ")",
      "\nmedian SE ratio:", round(median(coefs$se_ratio), 3),
      "\nalt estimate inside ref 95% CI:", sum(coefs$alt_in_ref_ci), "of", nrow(coefs),
      "\nsign changes:", sum(coefs$sign_change),
      "\nsignificance changes at 0.05:", sum(coefs$sig_change),
      "\ncut-point shift (alt - ref), range:",
      round(range(cut$coef_alt - cut$coef_ref), 3), "\n\n")
  print(filter(coefs, sig_change | sign_change | !alt_in_ref_ci) %>%
          select(term, OR_ref, OR_alt, OR_change_pct, p_ref, p_alt, alt_in_ref_ci),
        row.names = FALSE)
  out
}


# B1. check 1: all data vs the train set only (the 06a fit)
# the smaller n alone gives an SE ratio of about sqrt(12688 / 10151) = 1.12
check_train <- compare(res_ref, res_train, "train_only")


# B2. check 2: imputed vs complete cases
# 03 filled missing predictors with the train median / mode and flagged the
# rows (imputed = TRUE, 1,348 of all rows), the complete-case fit drops them
# imputed train rows are less satisfied (weighted mean 6.30 vs 6.79), so
# dropping them can shift the cut-points and the effects linked to item
# non-response
d_cc <- d_all %>% filter(!imputed) %>% mutate(w = anweight / mean(anweight))
stopifnot(nrow(d_cc) == sum(!train$imputed) + sum(!test$imputed))
check_cc <- compare(res_ref, fit_m3(d_cc), "complete_cases")


# B3. check 3: without Latvia R10
# Latvia R10 was a self-completion survey, M3 absorbs it with the lv_r10
# indicator, which only shifts the level of stflife in that cell
# the mode can also change the effects and the other 0-10 ratings
# (stfeco, stfhlth, trstprl, ...), so here the cell is dropped instead
# lv_r10 is then always 0 and is left out of the formula
# with lv_r10 in M3, cntryLV and round R10 are already estimated from the
# other cells, so they should barely move, the informative terms are the
# slopes (stfeco, hincfel, health, ...) and the cut-points
d_nolv10 <- d_all %>%
  filter(lv_r10 == 0) %>%
  mutate(w = anweight / mean(anweight))
stopifnot(nrow(d_nolv10) == nrow(d_all) - sum(d_all$lv_r10))
check_nolv10 <- compare(res_ref, fit_m3(d_nolv10, update(f_m3, . ~ . - lv_r10)),
                        "without_lv_r10")


rob <- bind_rows(check_train, check_cc, check_nolv10)


# ===================== C. summary ============================================
# one line per check, the numbers behind the report's validation paragraph
key_terms <- c("hincfel", "health", "stfeco", "partner", "female",
               "activity_grpUnemployed", "cntryLV")
rob_c  <- filter(rob, type == "coefficient")
rob_k  <- filter(rob_c, term %in% key_terms)
summ <- data.frame(
  check = c("A1 proportional odds", "A2 partial proportional odds",
            "A3 linearity", "A3b scales as factors", "A4 multicollinearity",
            "B1-B3 robustness"),
  result = c(
    paste0("violated (spread > 0.5) for ", paste(ppo_vars, collapse = ", ")),
    paste0("other ORs change median ", round(median(abs(ppo_tab$OR_change_pct)), 1),
           "%, max ", round(max(abs(ppo_tab$OR_change_pct)), 1), "%"),
    paste0("factor fits better (dAIC > 10) for ", paste(nonlin, collapse = ", ")),
    paste0("other ORs change median ", round(median(abs(lin_other$OR_change_pct)), 1),
           "%, key terms max ",
           round(max(abs(lin_other$OR_change_pct[lin_other$term %in% key_terms])), 1), "%"),
    paste0("highest GVIF^(1/(2 df)) ", round(max(gvif$GVIF_adj), 2), " (", gvif$variable[1], ")"),
    paste0(sum(rob_c$alt_in_ref_ci), " of ", nrow(rob_c),
           " estimates inside the all-data 95% CI, key terms max change ",
           round(max(abs(rob_k$OR_change_pct)), 1), "%")))
cat("\nC. Summary of the checks on the final M3 (all data):\n")
print(summ, right = FALSE, row.names = FALSE)


# ===================== save ==================================================
write.csv(po_tab,  "outputs/tables/ologit_check_po.csv",          row.names = FALSE)
write.csv(ppo_tab, "outputs/tables/ologit_check_ppo.csv",         row.names = FALSE)
write.csv(lin_other, "outputs/tables/ologit_check_linearity_other.csv", row.names = FALSE)
write.csv(lin,     "outputs/tables/ologit_check_linearity.csv",   row.names = FALSE)
write.csv(gvif,    "outputs/tables/ologit_check_gvif.csv",        row.names = FALSE)
write.csv(rob,     "outputs/tables/ologit_check_robustness.csv",  row.names = FALSE)
