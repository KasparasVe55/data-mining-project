#load packages
for (p in c("tidyverse", "gridExtra", "survey")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(tidyverse)
library(gridExtra)
library(grid)

DATA_FILE <- "data/ess_baltic_R9-R11.csv"
OUT <- "outputs/ESS_output"
AGE_BREAKS <- c(14, 29, 44, 59, 74, Inf)

raw <- read_csv(
  DATA_FILE,
  show_col_types = FALSE,
  col_types = cols(
    .default = col_double(),
    name = col_character(),
    proddate = col_character(),
    cntry = col_character(),
    lnghom1 = col_character()
  )
) %>%
  mutate(cntry = factor(cntry, c("EE", "LV", "LT")))

#check for duplicates
stopifnot(!anyDuplicated(raw[c("cntry", "essround", "idno")]))

#variable metadata and labels
info <- tribble(
  ~variable, ~group, ~type, ~label, ~scale,
  "stflife", "target", "integer", "Life satisfaction", "0 dissatisfied - 10 satisfied",
  "agea", "main", "integer", "Age", "years",
  "age5", "main", "categorical", "Age group", "made from agea",
  "female", "main", "binary", "Female", "from gndr, 1 = female",
  "hincfel", "main", "ordinal", "Feeling about income", "1 comfortable - 4 very difficult",
  "activity", "main", "categorical", "Main activity (last 7 days)", "from mnactic",
  "health", "main", "ordinal", "General health", "1 very good - 5 very bad",
  "sclmeet", "main", "ordinal", "Meets friends/family", "1 never - 7 every day",
  "partner", "main", "binary", "Lives with partner", "from rshpsts, 1 = yes",
  "trstprl", "main", "integer", "Trust in parliament", "0 no trust - 10 complete trust",
  "rlgdgr", "main", "integer", "How religious", "0 not religious - 10 very religious",
  "eisced", "other", "ordinal", "Education level", "1 lowest - 7 highest",
  "domicil", "other", "categorical", "Type of area", "1 big city - 5 farm/countryside",
  "russian_home", "other", "binary", "Russian spoken at home", "from lnghom1, 1 = yes",
  "inprdsc", "other", "ordinal", "Confidants (intimate matters)", "0 none - 6 more than 10",
  "sclact", "other", "ordinal", "Social activities vs peers", "1 much less - 5 much more",
  "hhmmb", "other", "integer", "Household size", "persons",
  "ppltrst", "other", "integer", "Trust in most people", "0 careful - 10 trusting",
  "stfeco", "other", "integer", "Satisfaction with economy", "0 dissatisfied - 10 satisfied",
  "stfhlth", "other", "integer", "Satisfaction: health services", "0 bad - 10 good",
  "aesfdrk", "other", "ordinal", "Feels safe after dark", "1 very safe - 4 very unsafe",
  "uemp3m", "other", "categorical", "Ever unemployed 3+ months", "1 yes, 2 no"
)

lbl <- function(v) info$label[match(v, info$variable)]
scl <- function(v) info$scale[match(v, info$variable)]

int_vars <- info$variable[info$type %in% c("integer", "ordinal")]
cat_vars <- info$variable[info$type %in% c("categorical", "ordinal", "binary")]
num_vars <- info$variable[info$type %in% c("integer", "ordinal", "binary")]

lab <- list(
  female = c("0" = "Male", "1" = "Female"),
  partner = c("0" = "No partner", "1" = "Lives with partner"),
  russian_home = c("0" = "Other language", "1" = "Russian at home"),
  uemp3m = c("1" = "Yes", "2" = "No"),
  domicil = c("1" = "Big city", "2" = "Suburbs", "3" = "Town", "4" = "Village", "5" = "Farm/countryside"),
  activity = c(
    "1" = "Paid work", "2" = "Education", "3" = "Unemployed, looking",
    "4" = "Unemployed, not looking", "5" = "Sick/disabled", "6" = "Retired",
    "7" = "Community/military", "8" = "Housework/care", "9" = "Other"
  )
)

#missing value codes (7/77/777=refusal, 8/88/888=don't know, 9/99/999=no answer)
a <- c(7, 8, 9)
b <- c(77, 88, 99)
miss_codes <- list(
  stflife = b, ppltrst = b, trstprl = b, stfeco = b, stfhlth = b, rlgdgr = b, hinctnta = b,
  inprdsc = b, rlgatnd = b, sclmeet = b, hhmmb = b,
  mnactic = b,
  rshpsts = b,
  aesfdrk = a, health = a, hlthhmp = a, sclact = a, domicil = a, hincfel = a, uemp3m = a,
  chldhhe = a,
  gndr = 9, agea = 999, lnghom1 = c(777, 888, 999),
  eisced = c(55, 77, 88, 99),
  wkhtot = c(666, 777, 888, 999)
)

dropped <- c("hinctnta", "wkhtot", "chldhhe", "hlthhmp", "rlgatnd")
used_raw <- setdiff(names(miss_codes), dropped)

#helper to expand data across all round and country combinations
views <- function(x) {
  r <- bind_rows(mutate(x, round = "All"), mutate(x, round = as.character(essround)))
  bind_rows(mutate(r, country = "All"), mutate(r, country = as.character(cntry))) %>%
    mutate(
      round = factor(round, c("All", "9", "10", "11")),
      country = factor(country, c("EE", "LV", "LT", "All"))
    )
}

#count missing values before cleaning
miss_long <- map_dfr(used_raw, function(v) {
  codes <- as.character(miss_codes[[v]])
  views(raw %>% mutate(value = as.character(.data[[v]]))) %>%
    group_by(Variable = v, round, country) %>%
    summarise(
      N = n(),
      Missing = sum(value %in% codes),
      Refusal = sum(value %in% intersect(codes, c("7", "77", "777"))),
      `Don't know` = sum(value %in% intersect(codes, c("8", "88", "888"))),
      `No answer` = sum(value %in% intersect(codes, c("9", "99", "999"))),
      .groups = "drop"
    ) %>%
    mutate(
      Other = Missing - Refusal - `Don't know` - `No answer`,
      `Missing %` = round(100 * Missing / N, 1),
      Codes = paste(codes, collapse = "/")
    )
})

#replace missing codes with NA and create new variables
d <- raw
for (v in names(miss_codes)) {
  d[[v]][as.character(raw[[v]]) %in% as.character(miss_codes[[v]])] <- NA
}
d$wkhtot[!(raw$mnactic %in% 1)] <- NA

lo <- AGE_BREAKS[-length(AGE_BREAKS)] + 1
hi <- AGE_BREAKS[-1]

d <- d %>%
  mutate(
    age5 = cut(agea, AGE_BREAKS, labels = ifelse(is.infinite(hi), paste0(lo, "+"), paste0(lo, "-", hi))),
    female = as.numeric(gndr == 2),
    partner = if_else(rshpsts == 66, 0, 1),
    russian_home = as.numeric(lnghom1 == "RUS"),
    activity = mnactic,
    strat = interaction(cntry, essround, stratum, drop = TRUE)
  )

summary(d$stflife)
tapply(d$stflife, d$cntry, summary)

#weighted stats functions
wmean <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
wsd <- function(x, w) {
  k <- !is.na(x)
  x <- x[k]
  w <- w[k]
  m <- sum(w * x) / sum(w)
  sqrt(sum(w * (x - m)^2) / (sum(w) - sum(w^2) / sum(w)))
}
wq <- function(x, w, p) {
  k <- !is.na(x)
  x <- x[k]
  w <- w[k]
  o <- order(x)
  x[o][which(cumsum(w[o]) / sum(w) >= p - 1e-12)[1]]
}

#survey design for confidence intervals
options(survey.lonely.psu = "adjust")
des <- survey::svydesign(ids = ~psu, strata = ~strat, weights = ~anweight, data = d, nest = TRUE)
mean_ci <- function(by) {
  as_tibble(survey::svyby(~stflife, by, des, survey::svymean, na.rm = TRUE, na.rm.by = TRUE, vartype = "ci"))
}

#output folders and table/plot saving functions
dirs <- function(kind, sub) {
  p <- file.path(OUT, kind, sub)
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}

tt <- ttheme_minimal(
  base_size = 8.5,
  padding = unit(c(2.2, 1.6), "mm"),
  core = list(bg_params = list(fill = c("white", "grey94"), col = NA)),
  colhead = list(
    fg_params = list(col = "white", fontface = "bold"),
    bg_params = list(fill = "#2C3E50", col = NA)
  )
)

save_table <- function(df, sub, name, title) {
  write_csv(df, file.path(dirs("csv", sub), paste0(name, ".csv")))
  shown <- df %>%
    mutate(across(where(is.numeric), ~ ifelse(
      is.na(.x), "-",
      if (grepl("%$", cur_column())) sprintf("%.1f", .x)
      else if (grepl("(Mean|SD|low|high)$", cur_column())) sprintf("%.2f", .x)
      else format(.x, big.mark = ",", trim = TRUE)
    )))
  g <- tableGrob(shown, rows = NULL, theme = tt)
  left <- which(names(df) %in% c("Variable", "Category", "Country", "Type", "Label", "Scale", "Codes", "Round", "Group", "Reason", "Age group"))
  for (k in which(g$layout$name %in% c("core-fg", "colhead-fg") & g$layout$l %in% left)) {
    g$grobs[[k]]$x <- unit(2, "mm")
    g$grobs[[k]]$hjust <- 0
  }
  png(tempfile(), 600, 600)
  w <- convertWidth(sum(g$widths), "in", TRUE)
  h <- convertHeight(sum(g$heights), "in", TRUE)
  dev.off()
  w <- max(w, 3.6)
  ttl <- paste(strwrap(title, floor(w * 11.5)), collapse = "\n")
  th <- .22 * (str_count(ttl, "\n") + 1) + .12
  full <- arrangeGrob(
    textGrob(ttl, x = 0, hjust = 0, gp = gpar(fontsize = 10.5, fontface = "bold", col = "#2C3E50")),
    g,
    widths = unit(w, "in"),
    heights = unit(c(th, h), "in"),
    padding = unit(.15, "in"),
    vp = viewport(x = 0, just = "left", width = unit(w + .3, "in"))
  )
  png(file.path(dirs("tables_png", sub), paste0(name, ".png")), w + .3, th + h + .3, units = "in", res = 200)
  grid.draw(full)
  dev.off()
  attr(full, "in_h") <- th + h + .3
  invisible(full)
}

cols <- c(EE = "#0072B2", LV = "#D55E00", LT = "#009E73")
ctry <- c(EE = "Estonia", LV = "Latvia", LT = "Lithuania")
round_lab <- c("9" = "R9 2018/19", "10" = "R10 2020-22", "11" = "R11 2023/24")
CAP <- "Source: ESS rounds 9-11, valid answers only, weighted with anweight. Latvia round 10 = self-completion survey (less comparable)."

theme_set(
  theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.title.position = "plot",
      plot.subtitle = element_text(colour = "grey30"),
      plot.caption = element_text(colour = "grey45", size = 7.5, hjust = 0),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
)

txt <- function(title, sub = NULL, cap = CAP, ...) {
  labs(
    title = str_wrap(title, 70),
    subtitle = str_wrap(sub, 95),
    caption = str_wrap(cap, 110),
    ...
  )
}

save_plot <- function(p, sub, name, w = 7, h = 4.5) {
  ggsave(file.path(dirs("graphs_png", sub), paste0(name, ".png")), p, width = w, height = h, dpi = 200, bg = "white")
  p
}

#generate tables
G <- list()

#overview tables
sample_tbl <- d %>%
  views() %>%
  count(round, country) %>%
  pivot_wider(names_from = country, values_from = n) %>%
  arrange(round == "All") %>%
  transmute(
    Round = recode(as.character(round), All = "All rounds"),
    Estonia = EE, Latvia = LV, Lithuania = LT, `All countries` = All
  )
G$sample <- save_table(sample_tbl, "overview", "sample_sizes", "Number of respondents by country and round")

excluded <- tribble(
  ~Variable, ~Label, ~Reason,
  "hinctnta", "Household income decile", "Mostly young people missing; hincfel is used instead",
  "wkhtot", "Weekly work hours", "Paid workers only, impossible values (168 h)",
  "chldhhe", "Children in household", "Almost no relation with life satisfaction",
  "hlthhmp", "Hampered by illness", "Duplicates health",
  "rlgatnd", "Religious services", "Duplicates rlgdgr"
) %>%
  mutate(`Missing %` = round(100 * map_dbl(Variable, ~ mean(is.na(d[[.x]]))), 1), .before = Reason)
G$excl <- save_table(excluded, "overview", "variables_left_out", "Variables left out of the analysis")

src <- c(female = "gndr", partner = "rshpsts", russian_home = "lnghom1", activity = "mnactic")
m_cty <- miss_long %>%
  filter(round == "All", country != "All") %>%
  select(Variable, country, `Missing %`) %>%
  pivot_wider(names_from = country, values_from = `Missing %`, names_glue = "{country} %")

miss_tbl <- miss_long %>%
  filter(round == "All", country == "All") %>%
  select(Variable, Codes, Refusal, `Don't know`, `No answer`, Other, Missing, `Missing %`) %>%
  left_join(m_cty, by = "Variable")
G$miss <- save_table(miss_tbl, "overview", "missing_values", "Missing values per variable (excluded from all other tables)")

miss_cr <- miss_long %>% filter(round != "All", country != "All")
write_csv(
  miss_cr %>% select(Variable, Round = round, Country = country, N, Missing, `Missing %`, Refusal, `Don't know`, `No answer`, Other),
  file.path(dirs("csv", "overview"), "missing_by_country_round_long.csv")
)

save_table(
  miss_cr %>%
    mutate(col = paste0(country, " R", round)) %>%
    select(Variable, col, `Missing %`) %>%
    pivot_wider(names_from = col, values_from = `Missing %`) %>%
    select(Variable, all_of(paste(rep(names(ctry), each = 3), paste0("R", c(9, 10, 11))))),
  "overview", "missing_pct_by_country_round", "Missing values (%) by country and round"
)

#numeric variable tables
num_all <- map_dfr(num_vars, function(v) {
  d %>%
    select(essround, cntry, w = anweight, x = all_of(v)) %>%
    views() %>%
    group_by(Round = round, Country = fct_recode(country, "All countries" = "All")) %>%
    summarise(
      n = sum(!is.na(x)),
      Missing = sum(is.na(x)),
      `Missing %` = round(100 * mean(is.na(x)), 1),
      Mean = round(mean(x, na.rm = TRUE), 2),
      SD = round(sd(x, na.rm = TRUE), 2),
      Min = min(x, na.rm = TRUE),
      Q1 = quantile(x, .25, na.rm = TRUE, names = FALSE),
      Median = median(x, na.rm = TRUE),
      Q3 = quantile(x, .75, na.rm = TRUE, names = FALSE),
      Max = max(x, na.rm = TRUE),
      `W Mean` = round(wmean(x, w), 2),
      `W SD` = round(wsd(x, w), 2),
      `W Q1` = wq(x, w, .25),
      `W Median` = wq(x, w, .5),
      `W Q3` = wq(x, w, .75),
      .groups = "drop"
    ) %>%
    mutate(Variable = v, .before = 1)
})

for (v in int_vars) {
  t <- filter(num_all, Variable == v) %>% select(-Variable)
  G[[paste0(v, "_by_country")]] <- save_table(
    filter(t, Round == "All") %>% select(-Round),
    "integer", paste0(v, "_by_country"),
    paste0(lbl(v), " [", v, "]: summary by country, all rounds (W = weighted)")
  )
  G[[paste0(v, "_by_round")]] <- save_table(
    filter(t, Round != "All") %>% mutate(Round = as.character(Round)),
    "integer", paste0(v, "_by_round"),
    paste0(lbl(v), " [", v, "]: summary by round and country (W = weighted)")
  )
}

all_c <- filter(num_all, Round == "All")
G$int_all <- save_table(
  filter(all_c, Country == "All countries", Variable %in% int_vars) %>% select(Variable, n, `Missing %`, Mean:`W Q3`),
  "overview", "integer_all_countries", "Integer and ordinal variables, all countries, all rounds (W = weighted)"
)

sm <- function(x, who) as_tibble_row(round(unclass(summary(x)), 2)) %>% mutate(Country = who, .before = 1)
G$summ <- save_table(
  bind_rows(sm(d$stflife, "All countries"), map_dfr(names(ctry), ~ sm(d$stflife[d$cntry == .x], .x))),
  "overview", "stflife_summary_function", "Life satisfaction: output of R's summary() function (raw)"
)

#master summary tables
master <- info %>%
  select(Variable = variable, Label = label, Group = group, Type = type) %>%
  mutate(
    n = map_int(Variable, ~ sum(!is.na(d[[.x]]))),
    `Missing %` = round(100 * (1 - n / nrow(d)), 1)
  ) %>%
  left_join(filter(all_c, Country == "All countries") %>% select(Variable, Mean, `W Mean`, SD, Median), by = "Variable")

G$master <- save_table(
  master, "overview", "master_table_all_variables",
  "Master table: all variables (mean of a 0/1 variable = share; no mean for nominal variables)"
)

G$mean_cty <- save_table(
  all_c %>%
    mutate(Country = recode(Country, "All countries" = "All")) %>%
    select(Variable, Country, Mean, `W Mean`) %>%
    pivot_wider(names_from = Country, values_from = c(Mean, `W Mean`), names_glue = "{Country} {.value}") %>%
    select(Variable, starts_with("EE"), starts_with("LV"), starts_with("LT"), starts_with("All")),
  "overview", "master_mean_by_country", "Mean by country, all rounds: raw and weighted (W)"
)

G$mean_rnd <- save_table(
  filter(num_all, Round != "All", Country != "All countries") %>%
    mutate(col = paste0(Country, " R", Round)) %>%
    select(Variable, col, `W Mean`) %>%
    pivot_wider(names_from = col, values_from = `W Mean`) %>%
    select(Variable, all_of(paste(rep(names(ctry), each = 3), paste0("R", c(9, 10, 11))))),
  "overview", "master_weighted_mean_by_country_round", "Weighted mean by country and round"
)

#categorical variable tables
cat_long <- function(v) {
  x <- d %>% select(essround, cntry, w = anweight, cat = all_of(v)) %>% drop_na(cat)
  x$cat <- if (v %in% names(lab)) factor(x$cat, names(lab[[v]]), lab[[v]]) else factor(x$cat)
  views(x) %>%
    group_by(round, country, cat) %>%
    summarise(n = n(), nw = sum(w), .groups = "drop") %>%
    complete(round, country, cat, fill = list(n = 0, nw = 0)) %>%
    group_by(round, country) %>%
    mutate(`%` = round(100 * n / sum(n), 1), `W %` = round(100 * nw / sum(nw), 1)) %>%
    ungroup() %>%
    select(-nw)
}

cat_tbl <- function(v) {
  t <- cat_long(v) %>%
    pivot_wider(names_from = country, values_from = c(n, `%`, `W %`), names_glue = "{country} {.value}") %>%
    select(Round = round, Category = cat, starts_with("EE"), starts_with("LV"), starts_with("LT"), starts_with("All"))
  tot <- t %>%
    group_by(Round) %>%
    summarise(across(-Category, sum), .groups = "drop") %>%
    mutate(Category = "Total valid", across(ends_with("%"), ~ 100))
  bind_rows(mutate(t, Category = as.character(Category)), tot) %>%
    arrange(Round, Category == "Total valid") %>%
    mutate(Round = as.character(Round))
}

for (v in cat_vars) {
  t <- cat_tbl(v)
  G[[paste0(v, "_by_country")]] <- save_table(
    filter(t, Round == "All") %>% select(-Round),
    "categorical", paste0(v, "_by_country"),
    paste0(lbl(v), " [", v, "]: count and % by country, all rounds (W % = weighted)")
  )
  save_table(
    filter(t, Round != "All"),
    "categorical", paste0(v, "_by_round"),
    paste0(lbl(v), " [", v, "]: count and % by round and country (W % = weighted)")
  )
}

#plots
#life satisfaction by age group
age_ci <- mean_ci(~cntry + essround + age5)
age_tbl <- d %>%
  drop_na(stflife, age5) %>%
  group_by(Country = cntry, Round = essround, `Age group` = age5) %>%
  summarise(n = n(), Mean = mean(stflife), .groups = "drop") %>%
  left_join(
    age_ci %>% transmute(Country = cntry, Round = essround, `Age group` = age5, `W Mean` = stflife, `W CI low` = ci_l, `W CI high` = ci_u),
    by = c("Country", "Round", "Age group")
  ) %>%
  mutate(
    Round = as.character(Round),
    Country = as.character(Country),
    `Age group` = as.character(`Age group`),
    across(where(is.double), ~ round(.x, 2))
  )

G$age_tbl <- save_table(
  age_tbl, "overview", "stflife_by_age_group_country_round",
  "Life satisfaction by age group, country and round: raw mean, weighted mean (W) and 95% confidence interval"
)

p_age <- age_ci %>%
  ggplot(aes(age5, stflife, colour = factor(essround), group = essround)) +
  geom_line(position = position_dodge(.3)) +
  geom_pointrange(aes(ymin = ci_l, ymax = ci_u), position = position_dodge(.3), size = .35) +
  facet_wrap(~cntry, nrow = 1, labeller = labeller(cntry = ctry)) +
  scale_colour_manual(values = scales::hue_pal()(3), labels = round_lab, name = "ESS round") +
  txt(
    "Life satisfaction by age group, country and round",
    "Weighted mean with 95% confidence interval (survey design: psu, stratum). Latvia round 10 was a self-completion survey.",
    x = "Age group", y = "Mean life satisfaction (0-10)"
  )
save_plot(p_age, "overview", "stflife_by_age_group", 8, 4)

#life satisfaction trend by round
p_trend <- mean_ci(~cntry + essround) %>%
  ggplot(aes(factor(essround), stflife, colour = cntry, group = cntry, shape = cntry == "LV" & essround == 10)) +
  geom_line(position = position_dodge(.3)) +
  geom_pointrange(aes(ymin = ci_l, ymax = ci_u), position = position_dodge(.3)) +
  scale_colour_manual(values = cols, labels = ctry, name = NULL) +
  scale_shape_manual(values = c(16, 17), guide = "none") +
  scale_x_discrete(labels = round_lab) +
  txt(
    "Life satisfaction: mean by country and round",
    "Weighted mean on the 0-10 scale with 95% confidence interval. Triangle = Latvia round 10 (self-completion).",
    x = "ESS round", y = "Mean life satisfaction (0-10)"
  )
save_plot(p_trend, "overview", "stflife_mean_by_round")

#histograms for integer variables
hist_data <- function(v, bw = 1, by = "cntry") {
  d %>%
    select(all_of(by), w = anweight, x = all_of(v)) %>%
    drop_na(x) %>%
    mutate(x = floor(x / bw) * bw + if (bw > 1) bw / 2 else 0, variable = v) %>%
    group_by(across(all_of(c(by, "variable", "x")))) %>%
    summarise(nw = sum(w), .groups = "drop_last") %>%
    mutate(pct = 100 * nw / sum(nw)) %>%
    ungroup()
}

hist_plot <- function(v, bw = 1, by_round = FALSE) {
  by <- c("cntry", if (by_round) "essround")
  m <- d %>% group_by(across(all_of(by))) %>% summarise(m = wmean(.data[[v]], anweight), .groups = "drop")
  ggplot(hist_data(v, bw, by), aes(x, pct, fill = cntry)) +
    geom_col(width = bw, colour = "white", show.legend = FALSE) +
    geom_vline(data = m, aes(xintercept = m), linetype = "dashed") +
    geom_text(data = m, aes(x = -Inf, y = Inf, label = sprintf("mean %.1f", m)), hjust = -0.15, vjust = 1.6, size = 3, inherit.aes = FALSE) +
    facet_grid(
      if (by_round) essround ~ cntry else . ~ cntry,
      labeller = labeller(cntry = ctry, essround = function(x) paste("Round", x))
    ) +
    scale_fill_manual(values = cols) +
    scale_x_continuous(breaks = scales::breaks_width(if (bw == 1) 2 else 20)) +
    txt(
      paste0(lbl(v), ": distribution", if (by_round) " by country and round" else " by country"),
      "Weighted % of respondents. Dashed line = weighted mean.",
      x = scl(v), y = "% of respondents"
    )
}

int_only <- info$variable[info$type == "integer"]
for (v in int_only) {
  save_plot(hist_plot(v, bw = if (v == "agea") 5 else 1), "histograms", paste0("hist_", v), 7, 3.6)
}

p_hist_rounds <- save_plot(hist_plot("stflife", by_round = TRUE), "overview", "stflife_hist_country_by_round", 7, 6)

s10 <- c("stflife", "ppltrst", "trstprl", "stfeco", "stfhlth", "rlgdgr")
save_plot(
  map_dfr(s10, hist_data) %>%
    mutate(variable = factor(lbl(variable), lbl(s10))) %>%
    ggplot(aes(x, pct, fill = cntry)) +
    geom_col(width = 1, colour = "white", show.legend = FALSE) +
    facet_grid(variable ~ cntry, labeller = labeller(cntry = ctry, variable = label_wrap_gen(16))) +
    scale_fill_manual(values = cols) +
    scale_x_continuous(breaks = c(0, 5, 10)) +
    txt(
      "The 0-10 scales: distribution by country",
      "Weighted % of respondents giving each score (0 = lowest, 10 = highest).",
      x = "Score", y = "% of respondents"
    ) +
    theme(strip.text.y = element_text(angle = 0, hjust = 0)),
  "overview", "scales_0_10_histograms", 7, 8
)

#medians and quartiles plot
p_q <- all_c %>%
  filter(Variable %in% int_only, Country %in% names(ctry)) %>%
  mutate(Country = droplevels(Country), Variable = factor(lbl(Variable), lbl(int_only))) %>%
  ggplot(aes(Country, `W Median`, colour = Country)) +
  geom_pointrange(aes(ymin = `W Q1`, ymax = `W Q3`), show.legend = FALSE) +
  facet_wrap(~Variable, scales = "free_y", ncol = 4, labeller = label_wrap_gen(18)) +
  scale_colour_manual(values = cols) +
  txt("Integer variables: weighted median and quartiles by country", "Dot = weighted median, line = Q1 to Q3.", x = NULL, y = NULL)
save_plot(p_q, "overview", "integer_median_quartiles", 7, 5)

#bar plots for categorical variables
bar_plot <- function(v) {
  b <- cat_long(v) %>% filter(round == "All", country != "All")
  flip <- nlevels(b$cat) > 6
  b$g <- if (flip) fct_rev(b$country) else b$country
  p <- ggplot(b, aes(cat, `W %`, fill = country, group = g)) +
    geom_col(position = position_dodge(.8), width = .75) +
    scale_fill_manual(values = cols, labels = ctry, name = NULL) +
    txt(
      paste0(lbl(v), ": % by category and country"),
      "Weighted % of valid answers, all rounds.",
      x = if (info$type[info$variable == v] == "ordinal") scl(v),
      y = "% of valid answers"
    )
  if (flip) p + coord_flip() + scale_x_discrete(limits = rev) else p
}

for (v in cat_vars) {
  save_plot(bar_plot(v), "bars", paste0("bar_", v), 7, if (nlevels(cat_long(v)$cat) > 6) 5 else 4)
}

key <- c("female", "age5", "partner", "russian_home")
p_key <- map_dfr(key, ~ cat_long(.x) %>% mutate(variable = lbl(.x))) %>%
  filter(round == "All", country != "All") %>%
  mutate(variable = factor(variable, lbl(key))) %>%
  ggplot(aes(cat, `W %`, fill = country)) +
  geom_col(position = position_dodge(.8), width = .75) +
  facet_wrap(~variable, scales = "free_x", ncol = 2) +
  scale_fill_manual(values = cols, labels = ctry, name = NULL) +
  scale_x_discrete(labels = scales::label_wrap(10)) +
  txt("Main categorical variables: comparison of countries", "Weighted % of valid answers, all rounds.", x = NULL, y = "% of valid answers")
save_plot(p_key, "overview", "categorical_key_variables", 7, 6)

#missing values heatmap
p_heat <- miss_long %>%
  filter(round != "All", country != "All") %>%
  mutate(
    cell = factor(paste(country, round), paste(rep(names(ctry), each = 3), c(9, 10, 11))),
    Variable = factor(Variable, rev(used_raw))
  ) %>%
  ggplot(aes(cell, Variable, fill = `Missing %`)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = ifelse(`Missing %` > 0, `Missing %`, "")), size = 2.6) +
  scale_fill_gradient(low = "white", high = "#C0392B", limits = c(0, NA), name = "% missing") +
  scale_x_discrete(position = "top") +
  theme(legend.position = "right", panel.grid = element_blank()) +
  txt(
    "Missing values: % of respondents per variable, country and round",
    "Columns = country and round, e.g. 'LV 10' = Latvia round 10. Darker = more missing.",
    cap = "Source: ESS rounds 9-11. Refusal, don't know and no answer (unweighted counts).",
    x = NULL, y = NULL
  )
save_plot(p_heat, "overview", "missing_heatmap", 7, 6.5)

#build pdf report
n_page <- 0
gp_ <- function(p, h) {
  g <- ggplotGrob(p)
  attr(g, "in_h") <- h
  g
}

page <- function(title, note, ...) {
  items <- list(...)
  n_page <<- n_page + 1
  title <- paste(strwrap(title, 52), collapse = "\n")
  note <- paste(strwrap(note, 110), collapse = "\n")
  th <- .36 * (str_count(title, "\n") + 1) + .1
  nh <- .16 * (str_count(note, "\n") + 1) + .15
  grid.newpage()
  pushViewport(viewport(width = unit(7.3, "in"), height = unit(10.7, "in")))
  grid.draw(arrangeGrob(
    grobs = c(
      list(
        textGrob(title, x = 0, y = 1, hjust = 0, vjust = 1, gp = gpar(fontsize = 16, fontface = "bold", col = "#2C3E50")),
        textGrob(note, x = 0, y = 1, hjust = 0, vjust = 1, gp = gpar(fontsize = 9, col = "grey30"))
      ),
      items,
      list(nullGrob())
    ),
    ncol = 1,
    heights = unit.c(unit(c(th, nh, map_dbl(items, ~ attr(.x, "in_h"))), "in"), unit(1, "null"))
  ))
  popViewport()
  grid.text(
    sprintf("ESS rounds 9-11  |  page %d", n_page),
    x = unit(1, "npc") - unit(.6, "in"),
    y = unit(.35, "in"),
    hjust = 1,
    gp = gpar(fontsize = 8, col = "grey50")
  )
}

pdf(file.path(OUT, "ESS_report.pdf"), width = 8.27, height = 11.69)

page(
  "ESS: life satisfaction in Estonia, Latvia and Lithuania",
  paste(
    "Descriptive overview of ESS rounds 9, 10 and 11. Valid answers only: missing codes (refusal, don't know, no answer) are excluded and listed separately.",
    "Tables show raw numbers and, next to them, weighted numbers (W = weighted with the ESS analysis weight anweight, which corrects for groups that",
    "answer more often; in 'All countries' it also weights countries by population size). Graphs are weighted. Latvia round 10 is the ESS self-completion",
    "survey (web/paper): kept in the data but flagged as less comparable. Variable types: integer = a real number (mean, sd); categorical = a category code",
    "(count, %); ordinal = ordered categories (both); binary = 0/1. All tables and graphs are in the csv/, tables_png/ and graphs_png/ folders."
  ),
  G$sample, G$excl
)

page(
  "Master table: all variables",
  paste(
    "Group: target = life satisfaction, main = ten key variables, other = the rest. n and missing % use all 12,791 respondents.",
    "W Mean = weighted mean. Derived variables: age5 from agea, female from gndr, partner from rshpsts, russian_home from lnghom1, activity from mnactic."
  ),
  G$master
)

page(
  "Master table: means by country and round",
  "Mean of every numeric variable (for 0/1 variables: the share). W = weighted. R9 = round 9 etc.",
  G$mean_cty, G$mean_rnd
)

page(
  "Missing values",
  paste(
    "Missing answers are excluded from all other tables, not replaced. The codes differ per variable (e.g. 77/88/99). 'Other' = code 55 in eisced.",
    "Source variables: gndr = female, rshpsts = partner, lnghom1 = russian_home, mnactic = activity. Based on 12,791 respondents.",
    "To include them later: keep them as an own category (categorical) or impute them, e.g. with the mice package (numeric)."
  ),
  G$miss
)

page(
  "Missing values by country and round",
  "Darker = more missing answers. Latvia round 10 (self-completion, no 'don't know' option) has more missing answers in several variables.",
  gp_(p_heat, 7.2)
)

page(
  "Life satisfaction: tables",
  "Target variable, scale 0-10. W = weighted. The summary() table shows the raw min, quartiles, median, mean and max.",
  G$stflife_by_country, G$stflife_by_round, G$summ
)

page(
  "Life satisfaction: age groups and rounds",
  "Weighted means with 95% confidence intervals (design-based). Latvia round 10 was a self-completion survey.",
  gp_(p_age, 3.8), gp_(p_trend, 4.3)
)

page(
  "Life satisfaction: histograms",
  "Weighted % of respondents per score by country and round, dashed line = weighted mean.",
  gp_(p_hist_rounds, 6.6)
)

page(
  "Integer and ordinal variables",
  "n, missing %, mean, sd, min, quartiles (Q1, Q3), median and max, raw and weighted (W). A weighted median/quartile is the first value where the weighted share reaches 50%/25%/75%, so on a 0-10 scale it can differ by one point from the raw one. Tables per variable, country and round are in the csv/integer folder.",
  G$int_all, gp_(p_q, 4.4)
)

page(
  "Categorical variables: count and %",
  "Valid answers only, all rounds; W % = weighted %. Tables for every categorical variable, also by round, are in the csv/categorical folder.",
  G$female_by_country, G$age5_by_country, G$partner_by_country, G$russian_home_by_country
)

page(
  "Categorical variables: graphs",
  "Weighted % of valid answers by category, all rounds. One graph per variable is in graphs_png/bars.",
  gp_(p_key, 5.4), gp_(bar_plot("activity"), 3.8)
)

invisible(dev.off())
message("Done. Everything is in: ", normalizePath(OUT))