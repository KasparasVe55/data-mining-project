# Train/test split 80:20 and CV folds in the train set
# run 01_clean.R first, working directory = repo folder
# output: data/ess_train.rds (with fold and imputed columns), data/ess_test.rds
#         (not uploaded, everyone makes them)
# same seed = everyone gets the same split and folds
# missing predictors are filled with typical values from the train set,
# imputed = TRUE marks rows where something was filled (for complete case checks)

library(dplyr)

# number of CV folds, not decided yet (10 or 4)
# changing it changes only the folds, not the train/test split
n_folds <- 10

# predictors used in the models (all of them, reduced later by backward selection)
predictors <- c("agea", "female", "hincfel", "activity_grp", "health", "sclmeet",
                "partner", "trstprl", "rlgdgr", "eisced", "domicil", "russian_home",
                "inprdsc", "sclact", "hhmmb", "ppltrst", "stfeco", "stfhlth",
                "aesfdrk", "ever_unemployed", "cntry", "round")

# weighted mean and weighted % in each group of variable v, for the checks
wmean  <- function(x, v) weighted.mean(x[[v]], x$anweight, na.rm = TRUE)
wshare <- function(x, v) round(100 * tapply(x$anweight, x[[v]], sum) / sum(x$anweight), 1)

d <- readRDS("data/ess_baltic_clean.rds")

# rows without stflife can't be used for models
d <- d %>% filter(!is.na(stflife))


# 1. split
# stratified by stflife x country x round, so each of the 99 groups
# is split 80:20 and train/test have the same proportions of these
set.seed(2026)

d <- d %>%
  group_by(stflife, cntry, round) %>%
  mutate(test = row_number() %in% sample(n(), round(0.2 * n()))) %>%
  ungroup()

train <- d %>% filter(!test) %>% select(-test)
test  <- d %>% filter(test)  %>% select(-test)


# 2. CV folds
# stratified the same way, each group is spread evenly over the folds
# folds are shuffled in each group, so small groups don't always land in fold 1
train <- train %>%
  group_by(stflife, cntry, round) %>%
  mutate(fold = sample(rep_len(sample(n_folds), n()))) %>%
  ungroup()


# 3. missing predictors
# mark rows with at least one missing predictor before filling them,
# complete cases later = filter(!imputed), same split and folds
train$imputed <- !complete.cases(train[predictors])
test$imputed  <- !complete.cases(test[predictors])

# typical value from the train set only (median for numbers,
# most common category for factors), the same values are used for test
typical <- function(x) {
  if (is.numeric(x)) round(median(x, na.rm = TRUE)) else names(which.max(table(x)))
}
fill_values <- lapply(train[predictors], typical)

for (v in predictors) {
  train[[v]][is.na(train[[v]])] <- fill_values[[v]]
  test[[v]][is.na(test[[v]])]   <- fill_values[[v]]
}


# 4. checks
stopifnot(nrow(train) + nrow(test) == nrow(d),
          all(train$fold %in% 1:n_folds),
          !anyNA(train[predictors]),
          !anyNA(test[predictors]))

cat("Train:", nrow(train), " Test:", nrow(test),
    " Test share:", round(nrow(test) / nrow(d), 3), "\n")
cat("Rows with imputed values - train:", sum(train$imputed),
    " test:", sum(test$imputed), "\n\n")

cat("Rows per fold:\n")
print(table(train$fold))
cat("Weighted mean stflife per fold:\n")
print(round(sapply(split(train, train$fold), wmean, v = "stflife"), 3))
cat("\n")

# other variables are not stratified, they should be close by chance
# weighted with anweight, like the report
main_num <- c("stflife", "agea", "female", "hincfel", "health",
              "sclmeet", "partner", "trstprl", "rlgdgr")

balance <- data.frame(
  variable = main_num,
  train    = sapply(main_num, wmean, x = train),
  test     = sapply(main_num, wmean, x = test)
)
balance$diff <- balance$test - balance$train
cat("Weighted means of main variables:\n")
print(balance, row.names = FALSE, digits = 3)

cat("\nCountry (weighted %):\n")
print(rbind(train = wshare(train, "cntry"), test = wshare(test, "cntry")))

cat("\nAge groups (weighted %):\n")
print(rbind(train = wshare(train, "age5"), test = wshare(test, "age5")))

cat("\nActivity groups (weighted %):\n")
print(rbind(train = wshare(train, "activity_grp"), test = wshare(test, "activity_grp")))

saveRDS(train, "data/ess_train.rds")
saveRDS(test,  "data/ess_test.rds")
