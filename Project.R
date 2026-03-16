# ---- packages ----
pkgs <- c("haven", "dplyr", "ggplot2", "stringr", "tidyr")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- helper: read xpt ----
read_xpt_safe <- function(fname) {
  if (!file.exists(fname)) stop("Missing file: ", fname)
  haven::read_xpt(fname)
}

# ---- helper: pick first existing variable name ----
pick_var <- function(df, candidates) {
  nms <- names(df)
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) NA_character_ else hit[1]
}

# ---- helper: safe numeric conversion (haven labels -> numeric) ----
as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

demo <- read_xpt_safe("DEMO_L.xpt")
bpxo <- read_xpt_safe("BPXO_L.xpt")
bmx  <- read_xpt_safe("BMX_L.xpt")
dr1  <- read_xpt_safe("DR1TOT_L.xpt")
dr2  <- read_xpt_safe("DR2TOT_L.xpt")
vid  <- read_xpt_safe("VID_L.xpt")
hdl  <- read_xpt_safe("HDL_L.xpt")
smq  <- read_xpt_safe("SMQ_L.xpt")

# ---- DEMO ----
age_var  <- pick_var(demo, c("RIDAGEYR"))
sex_var  <- pick_var(demo, c("RIAGENDR"))
race_var <- pick_var(demo, c("RIDRETH3", "RIDRETH1"))
mon_var  <- pick_var(demo, c("RIDEXMON"))
wt_var   <- pick_var(demo, c("WTMEC2YR"))
psu_var  <- pick_var(demo, c("SDMVPSU"))
str_var  <- pick_var(demo, c("SDMVSTRA"))

demo_s <- demo %>%
  transmute(
    SEQN,
    age = as_num(.data[[age_var]]),
    sex = .data[[sex_var]],
    race = .data[[race_var]],
    exam_month = if (!is.na(mon_var)) as_num(.data[[mon_var]]) else NA_real_,
    wt_mec2yr = if (!is.na(wt_var)) as_num(.data[[wt_var]]) else NA_real_,
    sdmvpsu = if (!is.na(psu_var)) as_num(.data[[psu_var]]) else NA_real_,
    sdmvstra = if (!is.na(str_var)) as_num(.data[[str_var]]) else NA_real_
  ) %>%
  mutate(
    sex = factor(as_num(sex), levels = c(1,2), labels = c("Male","Female")),
    race = factor(as_num(race))
  )

# ---- BPXO (blood pressure) ----
sbp_vars <- c("BPXOSY1","BPXOSY2","BPXOSY3")
dbp_vars <- c("BPXODI1","BPXODI2","BPXODI3")

bpxo_s <- bpxo %>%
  transmute(
    SEQN,
    sbp = rowMeans(cbind(as_num(BPXOSY1), as_num(BPXOSY2), as_num(BPXOSY3)), na.rm = TRUE),
    dbp = rowMeans(cbind(as_num(BPXODI1), as_num(BPXODI2), as_num(BPXODI3)), na.rm = TRUE)
  )

# ---- BMX (BMI) ----
bmi_var <- pick_var(bmx, c("BMXBMI"))
if (is.na(bmi_var)) stop("Could not find BMXBMI in BMX_L.xpt")

bmx_s <- bmx %>%
  transmute(SEQN, bmi = as_num(.data[[bmi_var]]))

# ---- DR1/DR2 TOT (diet) ----
# sodium (mg), calories (kcal), fiber (g), protein (g)
dr1_sod <- pick_var(dr1, c("DR1TSODI"))
dr2_sod <- pick_var(dr2, c("DR2TSODI"))
dr1_kc  <- pick_var(dr1, c("DR1TKCAL"))
dr2_kc  <- pick_var(dr2, c("DR2TKCAL"))
dr1_fib <- pick_var(dr1, c("DR1TFIBE"))
dr2_fib <- pick_var(dr2, c("DR2TFIBE"))
dr1_pro <- pick_var(dr1, c("DR1TPROT"))
dr2_pro <- pick_var(dr2, c("DR2TPROT"))

diet_s <- dr1 %>%
  transmute(
    SEQN,
    sod1 = as_num(.data[[dr1_sod]]),
    kcal1 = as_num(.data[[dr1_kc]]),
    fiber1 = as_num(.data[[dr1_fib]]),
    prot1 = as_num(.data[[dr1_pro]])
  ) %>%
  left_join(
    dr2 %>% transmute(
      SEQN,
      sod2 = as_num(.data[[dr2_sod]]),
      kcal2 = as_num(.data[[dr2_kc]]),
      fiber2 = as_num(.data[[dr2_fib]]),
      prot2 = as_num(.data[[dr2_pro]])
    ),
    by = "SEQN"
  ) %>%
  mutate(
    sodium_mg = rowMeans(cbind(sod1, sod2), na.rm = TRUE),
    kcal = rowMeans(cbind(kcal1, kcal2), na.rm = TRUE),
    fiber_g = rowMeans(cbind(fiber1, fiber2), na.rm = TRUE),
    protein_g = rowMeans(cbind(prot1, prot2), na.rm = TRUE)
  ) %>%
  select(SEQN, sodium_mg, kcal, fiber_g, protein_g)

# ---- VID (vitamin D) ----
vitd_var <- pick_var(vid, c("LBXVIDMS"))
if (is.na(vitd_var)) stop("Could not find LBXVIDMS in VID_L.xpt")

vid_s <- vid %>%
  transmute(SEQN, vitd_nmol = as_num(.data[[vitd_var]]))

# ---- HDL ----
hdl_var <- pick_var(hdl, c("LBDHDD"))
if (is.na(hdl_var)) stop("Could not find LBDHDD in HDL_L.xpt")

hdl_s <- hdl %>%
  transmute(SEQN, hdl_mgdl = as_num(.data[[hdl_var]]))

# ---- SMQ (smoking status) ----
# Common variables:
# SMQ020: smoked at least 100 cigarettes (1 yes, 2 no)
# SMD641: do you now smoke cigarettes (1 every day, 2 some days, 3 not at all)
# If SMD641 missing, fallback to SMQ040 (now smoke every day/some days/not at all)
ever100_var <- pick_var(smq, c("SMQ020"))
now_var     <- pick_var(smq, c("SMQ040"))

if (is.na(ever100_var) || is.na(now_var)) {
  stop("Could not find smoking variables in SMQ_L.xpt (need SMQ020 and SMD641 or SMQ040).")
}

smq_s <- smq %>%
  transmute(
    SEQN,
    ever100   = as_num(.data[[ever100_var]]),
    now_smoke = as_num(.data[[now_var]])
  ) %>%
  mutate(
    smoking = case_when(
      ever100 == 2 ~ "Never",
      ever100 == 1 & now_smoke %in% c(1, 2) ~ "Current",
      ever100 == 1 & now_smoke == 3 ~ "Former",
      TRUE ~ NA_character_
    ),
    smoking = factor(smoking, levels = c("Never", "Former", "Current"))
  ) %>%
  select(SEQN, smoking)

# ============================================================
# 3) Merge
# ============================================================
dat <- demo_s %>%
  left_join(bpxo_s, by = "SEQN") %>%
  left_join(bmx_s,  by = "SEQN") %>%
  left_join(diet_s, by = "SEQN") %>%
  left_join(vid_s,  by = "SEQN") %>%
  left_join(hdl_s,  by = "SEQN") %>%
  left_join(smq_s,  by = "SEQN")

# ============================================================
# 4) Clean + derived variables
# ============================================================
dat2 <- dat %>%
  filter(!is.na(sbp), !is.na(dbp), !is.na(age), !is.na(sex), !is.na(bmi)) %>%
  mutate(
    sodium_g = sodium_mg / 1000,
    hypertension = factor(ifelse(sbp >= 130 | dbp >= 80, 1, 0),
                          levels = c(0,1), labels = c("No","Yes"))
  )

# ============================================================
# 5) EDA
# ============================================================

# ----- basic sample size -----
cat("N (after basic cleaning):", nrow(dat2), "\n")

# ----- missingness for key vars -----
key_vars <- c("sbp","dbp","bmi","sodium_mg","kcal","fiber_g","protein_g",
              "vitd_nmol","hdl_mgdl","smoking","age","sex")
miss <- sapply(dat2[, key_vars], function(x) mean(is.na(x)))
miss <- sort(miss, decreasing = TRUE)
print(miss)

# ----- summary stats -----
print(summary(dat2[, c("sbp","dbp","bmi","sodium_mg","kcal","fiber_g","protein_g","vitd_nmol","hdl_mgdl","age")]))

# ----- prevalence of hypertension -----
print(prop.table(table(dat2$hypertension)))

# ----- plots -----
ggplot(dat2, aes(x = sbp)) + geom_histogram(bins = 30) +
  labs(title = "SBP distribution", x = "SBP (mmHg)", y = "Count")

ggplot(dat2, aes(x = dbp)) + geom_histogram(bins = 30) +
  labs(title = "DBP distribution", x = "DBP (mmHg)", y = "Count")

ggplot(dat2, aes(x = sodium_g, y = sbp)) + geom_point(alpha = 0.3) +
  labs(title = "SBP vs sodium intake", x = "Sodium (g/day)", y = "SBP (mmHg)")

ggplot(dat2, aes(x = smoking, y = sbp)) + geom_boxplot() +
  labs(title = "SBP by smoking status", x = "Smoking", y = "SBP (mmHg)")

ggplot(dat2, aes(x = bmi, y = sbp)) + geom_point(alpha = 0.3) +
  labs(title = "SBP vs BMI", x = "BMI (kg/m^2)", y = "SBP (mmHg)")

# ============================================================
# 6) Model
# ============================================================

# (A) Linear regression: SBP as continuous outcome
m1 <- lm(sbp ~ sodium_g + bmi + age + sex + smoking + vitd_nmol + hdl_mgdl + kcal,
         data = dat2)
cat("\n===== Linear model (SBP) =====\n")
print(summary(m1))

# (B) Logistic regression for hypertension
m2 <- glm(hypertension ~ sodium_g + bmi + age + sex + smoking + vitd_nmol + hdl_mgdl + kcal,
          data = dat2, family = binomial())
cat("\n===== Logistic model (Hypertension) =====\n")
print(summary(m2))

# Odds ratios + 95% CI
or <- exp(coef(m2))
ci <- suppressWarnings(exp(confint(m2)))
or_tab <- data.frame(term = names(or), OR = as.numeric(or),
                     LCL = ci[,1], UCL = ci[,2], row.names = NULL)
print(or_tab)

# ============================================================
# 7) Model with smoking
# ============================================================
# crude model (without smoking)
m_crude <- glm(hypertension ~ age + sex + bmi,
               data = dat2,
               family = binomial())

# Extract estimates
est_crude <- coef(m_crude)

# Odds ratios
or_crude <- exp(est_crude)

# Confidence intervals
ci_crude <- exp(confint(m_crude))

# Create table
table_crude <- data.frame(
  Variable = names(or_crude)[-1],
  Estimate = round(est_crude[-1], 3),
  OR = round(or_crude[-1], 2),
  CI = paste0("(", round(ci_crude[-1,1],3), ", ", round(ci_crude[-1,2],3), ")")
)

table_crude

# adjusted model
m_adj <- glm(hypertension ~ smoking + age + sex + bmi,
             data = dat2,
             family = binomial())

# Extract estimates
est_adj <- coef(m_adj)

# Odds ratios
or_adj <- exp(est_adj)

# Confidence intervals
ci_adj <- exp(confint(m_adj))

# Create table
table_adj <- data.frame(
  Variable = names(or_adj)[-1],
  Estimate = round(est_adj[-1], 3),
  OR = round(or_adj[-1], 2),
  CI = paste0("(", round(ci_adj[-1,1],3), ", ", round(ci_adj[-1,2],3), ")")
)

table_adj

library(ggplot2)

# Create BMI sequence
bmi_seq <- seq(min(dat2$bmi, na.rm = TRUE),
               max(dat2$bmi, na.rm = TRUE),
               length.out = 100)
bmi_seq <- seq(18, 45, length.out = 100)
# Create prediction dataset
newdata <- expand.grid(
  bmi = bmi_seq,
  smoking = levels(dat2$smoking),
  age = mean(dat2$age, na.rm = TRUE),
  sex = levels(dat2$sex)[1]   # use reference level
)

# Predict probabilities from the adjusted model
newdata$pred_prob <- predict(m_adj,
                             newdata = newdata,
                             type = "response")

# Plot predicted probability vs BMI
ggplot(newdata, aes(x = bmi, y = pred_prob, color = smoking)) +
  geom_line(size = 1.3) +
  labs(
    x = "BMI",
    y = "Predicted probability of hypertension",
    color = "Smoking status",
    title = "Predicted Hypertension Probability vs BMI by Smoking Status"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )


## Model focusing on Middle Age Group
dat_mid <- dat2 %>%
  dplyr::filter(age >= 40 & age <= 65)
m_mid <- glm(hypertension ~ smoking + age + sex + bmi,
             data = dat_mid, family = binomial())

summary(m_mid)

# OR + CI
or_mid <- exp(coef(m_mid))
ci_mid <- exp(confint(m_mid))

data.frame(
  Variable = names(or_mid),
  OR = round(or_mid, 3),
  LCL = round(ci_mid[,1], 3),
  UCL = round(ci_mid[,2], 3)
)

# ============================================================
# 8) Model with smoking Diagnosis
# ============================================================
library(car)
vif(m_adj)

library(ggplot2)
ggplot(dat2, aes(age, as.numeric(hypertension)-1)) +
  geom_smooth(method="loess")

infl <- influence.measures(m_adj)
# rows flagged by any diagnostic
which(apply(infl$is.inf, 1, any))

# Cook's distance values
cooks <- cooks.distance(m_adj)
# threshold
threshold_cook <- 4 / nobs(m_adj)
threshold_cook
# observations exceeding threshold
which(cooks > threshold_cook)

hat <- hatvalues(m_adj)
which(hat > 2 * mean(hat))

plot(m_adj, which = 5)

library(pROC)
mf <- model.frame(m_adj)                
y  <- mf$hypertension             
phat <- fitted(m_adj)        

roc_obj <- roc(y, phat, levels = c("No","Yes"), direction = "<")
auc(roc_obj)
plot(roc_obj, col="#2C7FB8", lwd=3,
     main="ROC Curve for Hypertension Model")
abline(a=0, b=1, lty=2, col="gray")

### EDA
library(dplyr)

dat_cc <- dat2 %>%
  select(hypertension, smoking, age, sex, bmi, sbp) %>%   # keep what you need
  filter(complete.cases(.)) %>%                           # complete cases only
  mutate(smoking = factor(smoking, levels = c("Never","Former","Current")))

N <- nrow(dat_cc)

overview <- dat_cc %>%
  summarise(
    mean_age = mean(age),
    female_pct = mean(sex == "Female") * 100,
    mean_bmi = mean(bmi),
    hbp_prev = mean(hypertension == "Yes") * 100
  )

smoking_dist <- dat_cc %>%
  count(smoking) %>%
  mutate(percent = 100 * n / sum(n))

cat("Analysis sample size (complete cases):", N, "\n")
cat("Mean age:", round(overview$mean_age, 1), "\n")
cat("Female:", round(overview$female_pct, 1), "%\n")
cat("Mean BMI:", round(overview$mean_bmi, 1), "\n")
cat("Hypertension prevalence:", round(overview$hbp_prev, 1), "%\n\n")

cat("Smoking status distribution:\n")
print(smoking_dist)


######################################################
dat_plot <- dat2 %>% dplyr::filter(!is.na(smoking))

ggplot(dat_plot, aes(smoking, fill = hypertension)) +
  geom_bar(position = "fill") +
  labs(
    x = "Smoking Status",
    y = "Proportion",
    fill = "Hypertension",
    title = "Hypertension prevalence by smoking status"
  ) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
  theme_minimal(base_size = 14)


dat2_clean <- dat2 %>%
  filter(!is.na(smoking)) %>%                
  mutate(smoking = factor(smoking,
                          levels = c("Never", "Former", "Current")))  

ggplot(dat2_clean, aes(x = smoking, y = sbp)) +
  geom_boxplot() +
  labs(title = "Systolic Blood Pressure by Smoking Status",
       x = "Smoking status",
       y = "SBP (mmHg)") +
  theme_minimal()