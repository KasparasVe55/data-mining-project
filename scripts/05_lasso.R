#lasso regression for life satisfaction
#run 01_clean.R and 03_train_test_split.R first, working directory = repo folder
#input:  data/ess_train.rds (with fold column), data/ess_test.rds
#output: outputs/figures/models/lasso_*.pdf
#        outputs/tables/lasso_metrics.csv (test set results)
#        outputs/tables/lasso_coefficients.csv (all coefficients, 0 = dropped by lasso)
#        outputs/predictions/lasso_test.csv (test predictions, for comparing all models)
#        data/lasso_model.rds (fitted model, not uploaded)

library(dplyr)
library(glmnet)

train <- readRDS("data/ess_train.rds")
test  <- readRDS("data/ess_test.rds")

#same predictors as in 03_train_test_split.R
predictors <- c("agea", "female", "hincfel", "activity_grp", "health", "sclmeet",
                "partner", "trstprl", "rlgdgr", "eisced", "domicil", "russian_home",
                "inprdsc", "sclact", "hhmmb", "ppltrst", "stfeco", "stfhlth",
                "aesfdrk", "ever_unemployed", "cntry", "round")

dir.create("outputs/figures/models", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/predictions", showWarnings = FALSE, recursive = TRUE)


#1. x matrix
#glmnet wants a numeric matrix, not a formula
#numbers stay numbers, a factor gets one 0/1 column for every category
#(no reference category, so the lasso itself decides which categories matter,
#with a reference category the result would depend on which one we pick)
#categories are taken from train, so test gets exactly the same columns
cat_vars <- predictors[!sapply(train[predictors], function(x) is.numeric(x) || is.logical(x))]
cat_levels <- lapply(train[cat_vars], function(x) levels(factor(x)))

make_x <- function(df) {
  df <- as.data.frame(df[predictors])
  for (v in predictors) {
    if (v %in% cat_vars) df[[v]] <- factor(df[[v]], levels = cat_levels[[v]], ordered = FALSE)
    else df[[v]] <- as.numeric(df[[v]])
  }
  dummies <- if (length(cat_vars) > 0) lapply(df[cat_vars], contrasts, contrasts = FALSE)
  model.matrix(~ ., data = df, contrasts.arg = dummies)[, -1]   #-1 drops the intercept column, glmnet adds its own
}

x_train <- make_x(train)
x_test  <- make_x(test)
y_train <- train$stflife

#no rows lost and the same columns in train and test
stopifnot(nrow(x_train) == nrow(train), nrow(x_test) == nrow(test),
          identical(colnames(x_train), colnames(x_test)))
dim(x_train)
colnames(x_train)


#2. lasso with cv
#lambda = how hard the coefficients are pushed to 0, chosen by cv
#small lambda = almost normal ols, big lambda = everything 0 (only the mean is left)
#alpha = 1 is lasso (alpha = 0 would be ridge)
#foldid = the folds from 03_train_test_split.R, so lasso is tuned on the same folds as the other models
#weights = anweight, so the fit and the cv error are weighted like the report
#glmnet standardizes the columns itself before the penalty (age in years and 0/1 dummies
#are treated fairly), coefficients come back on the original scale
#only main effects: country/round interactions were tried in cv (cv rmse 1.797 vs 1.804),
#the gain was smaller than 1 se and needed 77 terms instead of 27, so not worth it
set.seed(2026)
cv_lasso <- cv.glmnet(x_train, y_train,
                      weights = train$anweight,
                      alpha = 1,
                      foldid = train$fold,
                      type.measure = "mse")

cv_lasso
#lambda.min = lowest cv error, this is the model we compare with the others
#(the usual choice when the goal is the best prediction)
#lambda.1se = the simplest model whose cv error is within 1 se of the lowest, kept as a check
lambda_min <- cv_lasso$lambda.min
lambda_1se <- cv_lasso$lambda.1se

#cv results of both lambdas (rmse = sqrt of the weighted cv mse), to compare with the test results later
i_min <- which(cv_lasso$lambda == lambda_min)
i_1se <- which(cv_lasso$lambda == lambda_1se)
cv_res <- data.frame(Model   = c("Lasso", "Lasso 1se"),
                     lambda  = c(lambda_min, lambda_1se),
                     RMSE_cv = sqrt(cv_lasso$cvm[c(i_min, i_1se)]),
                     nonzero = cv_lasso$nzero[c(i_min, i_1se)],
                     row.names = NULL)
cv_res

pdf("outputs/figures/models/lasso_cv.pdf", width = 7, height = 5)
plot(cv_lasso)                         #cv error for each lambda, numbers on top = how many coefficients are not 0
title("Lasso: CV error", line = 2.5)  #left dashed line = lambda.min, right = lambda.1se
dev.off()

pdf("outputs/figures/models/lasso_path.pdf", width = 7, height = 5)
plot(cv_lasso$glmnet.fit, xvar = "lambda")   #every line = one coefficient, going to 0 as lambda grows
abline(v = log(c(lambda_min, lambda_1se)), lty = 2)
title("Lasso: coefficient paths", line = 2.5)
dev.off()


#3. coefficients
#coef = effect on the 0-10 scale for one unit more of x
#for categories compare them with each other (a category with 0 has no own effect)
#std_coef = coef x weighted sd of the column, so age, 0-10 scales and dummies can be compared
#(same standardization that glmnet uses inside), this is our variable importance
w <- train$anweight / sum(train$anweight)
wsd <- apply(x_train, 2, function(x) sqrt(sum(w * (x - sum(w * x))^2)))

coef_min <- as.matrix(coef(cv_lasso, s = "lambda.min"))[, 1]
coef_1se <- as.matrix(coef(cv_lasso, s = "lambda.1se"))[, 1]

coefs <- data.frame(term     = names(coef_min),
                    coef     = coef_min,
                    std_coef = c(NA, coef_min[-1] * wsd),
                    coef_1se = coef_1se,
                    row.names = NULL) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

coefs %>% filter(coef != 0) %>% arrange(desc(abs(std_coef)))   #kept by lasso
coefs$term[coefs$coef == 0]                                     #dropped by lasso (pushed to exactly 0)

pdf("outputs/figures/models/lasso_coef.pdf", width = 7, height = 8)
imp <- coefs %>% filter(term != "(Intercept)", coef != 0) %>% arrange(abs(std_coef))
par(mar = c(5, 11, 4, 2))
barplot(imp$std_coef, names.arg = imp$term, horiz = TRUE, las = 1, cex.names = 0.7,
        col = ifelse(imp$std_coef > 0, "steelblue", "tomato"),
        xlab = "Standardized coefficient (change in life satisfaction for 1 sd)",
        main = "Lasso: coefficients at lambda.min")
abline(v = 0)
dev.off()


#4. results on the test set
#weighted with anweight, like the report
wmetrics <- function(obs, pred, w) {
  c(RMSE     = sqrt(sum(w * (obs - pred)^2) / sum(w)),
    MAE      = sum(w * abs(obs - pred)) / sum(w),
    Rsquared = 1 - sum(w * (obs - pred)^2) /
                   sum(w * (obs - weighted.mean(obs, w))^2))
}

pred_lasso     <- as.vector(predict(cv_lasso, newx = x_test, s = "lambda.min"))
pred_lasso_1se <- as.vector(predict(cv_lasso, newx = x_test, s = "lambda.1se"))

#predictions should stay within the 0-10 scale
#lasso is a straight line so it can go a bit above 10 or below 0, those are cut to 0-10
range(pred_lasso)
sum(pred_lasso > 10 | pred_lasso < 0)
pred_lasso     <- pmin(pmax(pred_lasso, 0), 10)
pred_lasso_1se <- pmin(pmax(pred_lasso_1se, 0), 10)

#observed vs predicted: one box of predictions for each observed answer 0-10
#red line = perfect prediction
pdf("outputs/figures/models/lasso_obs_pred.pdf", width = 10, height = 5)
par(mfrow = c(1, 2))
boxplot(pred_lasso ~ test$stflife, at = sort(unique(test$stflife)),
        xlim = c(-0.5, 10.5), ylim = c(0, 10),
        xlab = "Observed", ylab = "Predicted", main = "Lasso: Obs vs Pred")
abline(0, 1, col = "red", lty = 2)
boxplot(pred_lasso_1se ~ test$stflife, at = sort(unique(test$stflife)),
        xlim = c(-0.5, 10.5), ylim = c(0, 10),
        xlab = "Observed", ylab = "Predicted", main = "Lasso 1se: Obs vs Pred")
abline(0, 1, col = "red", lty = 2)
par(mfrow = c(1, 1))
dev.off()

#residual checks on the train set: residuals vs fitted and qq plot
#no strong pattern in residuals vs fitted = ok, the stripes come from y being whole numbers 0-10
fit_train <- as.vector(predict(cv_lasso, newx = x_train, s = "lambda.min"))
res_train <- y_train - fit_train
pdf("outputs/figures/models/lasso_residuals.pdf", width = 10, height = 5)
par(mfrow = c(1, 2))
plot(fit_train, res_train, pch = 16, cex = 0.4, col = rgb(0, 0, 0, 0.25),
     xlab = "Fitted", ylab = "Residual", main = "Lasso: residuals vs fitted")
abline(h = 0, col = "red", lty = 2)
lines(lowess(fit_train, res_train), col = "blue", lwd = 2)
qqnorm(res_train, pch = 16, cex = 0.4, main = "Lasso: normal QQ plot of residuals")
qqline(res_train, col = "red", lty = 2)
par(mfrow = c(1, 1))
dev.off()

metrics <- rbind(Lasso       = wmetrics(test$stflife, pred_lasso, test$anweight),
                 `Lasso 1se` = wmetrics(test$stflife, pred_lasso_1se, test$anweight))
metrics <- data.frame(Model = rownames(metrics), round(metrics, 4), row.names = NULL)
print(metrics)

#cv results of the chosen lambdas, to compare with the test results
cv_res

#check: same metrics only on test rows where nothing was filled in (complete cases)
cc <- !test$imputed
round(wmetrics(test$stflife[cc], pred_lasso[cc], test$anweight[cc]), 4)


#5. save
write.csv(metrics, "outputs/tables/lasso_metrics.csv", row.names = FALSE)
write.csv(coefs, "outputs/tables/lasso_coefficients.csv", row.names = FALSE)

#test predictions with ids, so the group can join all models on the same rows
predictions <- data.frame(idno     = test$idno,
                          cntry    = test$cntry,
                          essround = test$essround,
                          stflife  = test$stflife,
                          anweight = test$anweight,
                          imputed  = test$imputed,
                          pred_lasso     = pred_lasso,
                          pred_lasso_1se = pred_lasso_1se)
write.csv(predictions, "outputs/predictions/lasso_test.csv", row.names = FALSE)

saveRDS(list(lasso = cv_lasso, cat_levels = cat_levels, x_columns = colnames(x_train)),
        "data/lasso_model.rds")
