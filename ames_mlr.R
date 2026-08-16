# Project 3 — Ames Housing Multiple Linear Regression Analysis

# ============================================================================
# TABLE OF CONTENTS
# ============================================================================
# A. Setup
#    1. Packages
#    2. Brand colors and portfolio plot theme
#    3. Plot-saving helper
#
# B. Import and initial housekeeping
#    1. Import Ames Housing data
#    2. Remove identifier columns and clean column names
#
# C. Inspect and clean data
#    1. Encode ordinal variables
#    2. Convert remaining categorical variables to factors
#    3. Create absence / presence indicators
#    4. Validate and correct year variables
#    5. Remove redundant predictors and handle missing values
#
# D. Exploratory data analysis
#    1. Exclude abnormal sales and very large houses
#    2. SalePrice and log(SalePrice) distributions
#    3. Correlation analysis
#
# E. Multiple linear regression
#    1. Train / test split
#    2. LASSO variable selection
#    3. OLS refit
#    4. AIC and BIC backward selection
#    5. Model comparison
#
# F. Model diagnostics
#    1. Residuals vs fitted
#    2. Normal Q-Q
#    3. Scale-location
#    4. Residuals vs leverage
#    5. Cook's distance, residuals, and VIF
#    6. Residuals by sale condition
#    7. Breusch-Pagan test
#    8. High-leverage observations
#    9. Rare design-matrix columns
#   10. Residual spread by sale condition
#
# G. Results and visualizations
#    1. Coefficient table with percent effects
#    2. Predicted vs actual — test set
#    3. Standardized coefficient plot
#    4. Error metrics by SalePrice tercile
#    5. Baseline model
#
# H. Pre-registered exploratory comparison
#    1. Training-only top-20 rankings and test evaluation
# ============================================================================

# ============================================================================
# A. Setup
# ============================================================================

# ----------------------------------------------------------------------------
# 1. Packages
# ----------------------------------------------------------------------------
library(ggplot2)      # plotting and portfolio theme
library(dplyr)        # data manipulation (filter, mutate, select, etc.)
library(readxl)       # import the AmesHousing.xls file
library(broom)        # tidy model output if needed
library(car)          # VIF and other regression diagnostics
library(lmtest)       # Breusch-Pagan and other specification tests
library(Matrix)       # required by glmnet
library(glmnet)       # LASSO and Elastic Net
library(patchwork)    # combine multiple ggplots into a 2x2 grid

# ----------------------------------------------------------------------------
# 2. Brand colors and portfolio plot theme
# ----------------------------------------------------------------------------
# Brand colors
brand_colors = c(
  field = "#041F2C",
  blue = "#08415C",
  plum = "#511730",
  brick = "#8E443D",
  salmon = "#F7A399",
  sand = "#E0D68A",
  plate = "#F4F0E6",
  plum_ink = "#3A0F21"
)

# Portfolio plot theme
portfolio_theme = theme_minimal(base_size = 15) +
  theme(
    text = element_text(
      family = "Montserrat",
      color = brand_colors["field"]
    ),
    plot.title = element_text(
      family = "Archivo Black",
      color = brand_colors["plum"],
      size = 18
    ),
    plot.subtitle = element_text(
      family = "Montserrat",
      color = brand_colors["field"],
      size = 13
    ),
    plot.caption = element_text(
      family = "Source Code Pro",
      color = brand_colors["brick"],
      size = 11
    ),
    axis.title = element_text(
      family = "Montserrat",
      color = brand_colors["field"],
      size = 13
    ),
    axis.text = element_text(
      family = "Montserrat",
      color = brand_colors["field"],
      size = 12
    ),
    panel.grid.major = element_line(
      color = brand_colors["salmon"],
      linewidth = 0.25
    ),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(
      fill = brand_colors["plate"],
      color = NA
    ),
    panel.background = element_rect(
      fill = brand_colors["plate"],
      color = NA
    )
  )
# ----------------------------------------------------------------------------
# 3. Plot-saving helper
# ----------------------------------------------------------------------------
save_plot <- function(plot, file, width = 7, height = 7) {
  # Wrap long ggplot titles so they stay inside the saved image.
  # Patchwork figures are left alone because their title comes from plot_annotation().
  if (!inherits(plot, "patchwork") && !is.null(plot$labels$title)) {
    plot <- plot + labs(
      title = paste(strwrap(plot$labels$title, width = 50), collapse = "\n")
    )
  }
  
  ggsave(file, plot, width = width, height = height, dpi = 200)
}


# ============================================================================
# B. Import and initial housekeeping
# ============================================================================

# ----------------------------------------------------------------------------
# 1. Import Ames Housing data
# ----------------------------------------------------------------------------
setwd("~/project_3_housing")
data = read_excel("AmesHousing.xls")

# ----------------------------------------------------------------------------
# 2. Remove identifier columns and clean column names
# ----------------------------------------------------------------------------
# Drop pure identifier columns ~ listed in docs, not useful for MLR
data$Order = NULL
data$PID = NULL
names(data)

# Replace every space in column names with an underscore
names(data) = gsub(" ", "_", names(data))
names(data)


# ============================================================================
# C. Inspect and clean data
# ============================================================================

# ----------------------------------------------------------------------------
# 1. Encode ordinal variables
# ----------------------------------------------------------------------------
# Get character columns
char_cols = names(data)[sapply(data, is.character)]

# Quality-coded ordinal variables
# automating the first set of ordinals
# Detect columns that contain quality codes
qual_levels = c("Ex", "Gd", "TA", "Fa", "Po")
qual_cols = char_cols[sapply(data[char_cols], function(x) {
  any(as.character(x) %in% qual_levels)
})]

# Inspect what was detected
lapply(data[qual_cols], function(x) sort(unique(as.character(x))))

# Handle Bsmt_Exposure first (its own scale)
exposure_map = c(
  "No" = 1,
  "Mn" = 2,
  "Av" = 3,
  "Gd" = 4,
  "NA" = 0
)
data$Bsmt_Exposure = unname(exposure_map[as.character(data$Bsmt_Exposure)])

# Check Bsmt_Exposure
table(data$Bsmt_Exposure, useNA = "ifany")

# Handle the remaining quality columns
qual_map = c(
  "Po" = 1,
  "Fa" = 2,
  "TA" = 3,
  "Gd" = 4,
  "Ex" = 5,
  "NA" = 0
)
other_qual_cols = setdiff(qual_cols, "Bsmt_Exposure")
data[other_qual_cols] = lapply(data[other_qual_cols], function(x) {
  unname(qual_map[as.character(x)])
})

# Final check of all quality columns
lapply(data[qual_cols], table, useNA = "ifany")

# Remaining Char columns - these are one-by-one based on the info provided in the DataDocumentation.txt
# Character columns that have not been converted yet
# Convert the next group of ordinals

# Lot_Shape
lotshape_map = c(
  "IR3" = 1,
  "IR2" = 2,
  "IR1" = 3,
  "Reg" = 4
)
data$Lot_Shape = unname(lotshape_map[as.character(data$Lot_Shape)])

# Land_Slope
landslope_map = c(
  "Sev" = 1,
  "Mod" = 2,
  "Gtl" = 3
)
data$Land_Slope = unname(landslope_map[as.character(data$Land_Slope)])

# Utilities
utilities_map = c(
  "ELO" = 1,
  "NoSeWa" = 2,
  "NoSewr" = 3,
  "AllPub" = 4
)
data$Utilities = unname(utilities_map[as.character(data$Utilities)])

# BsmtFin_Type_1 and BsmtFin_Type_2
bsmtfin_map = c(
  "Unf" = 1,
  "LwQ" = 2,
  "Rec" = 3,
  "BLQ" = 4,
  "ALQ" = 5,
  "GLQ" = 6,
  "NA" = 0
)
data$BsmtFin_Type_1 = unname(bsmtfin_map[as.character(data$BsmtFin_Type_1)])
data$BsmtFin_Type_2 = unname(bsmtfin_map[as.character(data$BsmtFin_Type_2)])

# Electrical
electrical_map = c(
  "FuseP" = 1,
  "FuseF" = 2,
  "FuseA" = 3,
  "Mix" = 4,
  "SBrkr" = 5
)
data$Electrical = unname(electrical_map[as.character(data$Electrical)])

# Functional
functional_map = c(
  "Sal" = 1,
  "Sev" = 2,
  "Maj2" = 3,
  "Maj1" = 4,
  "Mod" = 5,
  "Min2" = 6,
  "Min1" = 7,
  "Typ" = 8
)
data$Functional = unname(functional_map[as.character(data$Functional)])

# Garage_Finish
garage_finish_map = c(
  "Unf" = 1,
  "RFn" = 2,
  "Fin" = 3,
  "NA" = 0
)
data$Garage_Finish = unname(garage_finish_map[as.character(data$Garage_Finish)])

# Paved_Drive
paved_map = c(
  "N" = 1,
  "P" = 2,
  "Y" = 3
)
data$Paved_Drive = unname(paved_map[as.character(data$Paved_Drive)])

# Fence
fence_map = c(
  "MnWw" = 1,
  "GdWo" = 2,
  "MnPrv" = 3,
  "GdPrv" = 4,
  "NA" = 0
)
data$Fence = unname(fence_map[as.character(data$Fence)])

remaining_char = setdiff(char_cols, c(
  qual_cols,
  "Lot_Shape", "Land_Slope", "Utilities",
  "BsmtFin_Type_1", "BsmtFin_Type_2",
  "Electrical", "Functional", "Garage_Finish",
  "Paved_Drive", "Fence"
))

# verifying no ordinals remain:
lapply(data[remaining_char], function(x) sort(unique(as.character(x))))
# confirmed.

# ----------------------------------------------------------------------------
# 2. Convert remaining categorical variables to factors
# ----------------------------------------------------------------------------
#The rest are non-ordinals so they will be treated as factors
data[remaining_char] = lapply(data[remaining_char], as.factor)
table(sapply(data, class))
names(data)[sapply(data, is.character)]

# ----------------------------------------------------------------------------
# 3. Create absence / presence indicators
# ----------------------------------------------------------------------------
data$Has_Basement = as.integer(data$Bsmt_Qual > 0)
data$Has_Garage = as.integer(data$Garage_Qual > 0)
data$Has_Fireplace = as.integer(data$Fireplace_Qu > 0)
data$Has_Pool = as.integer(data$Pool_QC > 0)
data$Has_Fence = as.integer(data$Fence > 0)
data$Has_Alley = as.integer(data$Alley != "NA") # still a factor
data$Has_MasVnr = as.integer(data$Mas_Vnr_Type != "None")

# Validate them
table(data$Has_Basement, useNA = "ifany")
table(data$Has_Garage, useNA = "ifany")
table(data$Has_Fireplace, useNA = "ifany")
table(data$Has_Pool, useNA = "ifany")
table(data$Has_Fence, useNA = "ifany")

# Cross-check against the original quality columns
table(data$Has_Basement, data$Bsmt_Qual == 0)
table(data$Has_Garage, data$Garage_Qual == 0)
table(data$Has_Fireplace, data$Fireplace_Qu == 0)
table(data$Has_Pool, data$Pool_QC == 0)
table(data$Has_Fence, data$Fence == 0)

# ----------------------------------------------------------------------------
# 4. Validate and correct year variables
# ----------------------------------------------------------------------------
# Checking year variables.
# Year validation checks
# Basic ranges (with correct backticks)
summary(data$Year_Built)
summary(data$`Year_Remod/Add`)
summary(data$Garage_Yr_Blt)
summary(data$Yr_Sold)

# Logical checks
# Remodel year should not be before year built
table(data$`Year_Remod/Add` < data$Year_Built)

# Garage year should not be before year built (when garage exists)
table(data$Garage_Yr_Blt < data$Year_Built, useNA = "ifany")

# Sold year should not be before year built
table(data$Yr_Sold < data$Year_Built)

# Sold year should not be before remodel year
table(data$Yr_Sold < data$`Year_Remod/Add`)

# Sold year should not be before garage year
table(data$Yr_Sold < data$Garage_Yr_Blt, useNA = "ifany")

# Garage_Yr_Blt should be NA exactly when there is no garage
table(is.na(data$Garage_Yr_Blt), data$Has_Garage == 0)

# Show problem rows with a row identifier
data %>%
  mutate(row = row_number()) %>%
  filter(
    `Year_Remod/Add` < Year_Built |
      Garage_Yr_Blt < Year_Built |
      Yr_Sold < Year_Built |
      Yr_Sold < `Year_Remod/Add` |
      Yr_Sold < Garage_Yr_Blt
  ) %>%
  select(row, Year_Built, `Year_Remod/Add`, Garage_Yr_Blt, Yr_Sold, Has_Garage) %>%
  print(n = 30)

# Fix the obvious typo (2207 → 2007)
data$Garage_Yr_Blt[data$Garage_Yr_Blt == 2207] = 2007

# When garage year is before house year, set it equal to Year_Built
# (common and reasonable assumption: garage not older than the house)
data$Garage_Yr_Blt = ifelse(
  !is.na(data$Garage_Yr_Blt) & data$Garage_Yr_Blt < data$Year_Built,
  data$Year_Built,
  data$Garage_Yr_Blt
)

# Check the remaining logical problems
data %>%
  mutate(row = row_number()) %>%
  filter(
    `Year_Remod/Add` < Year_Built |
      Yr_Sold < Year_Built |
      Yr_Sold < `Year_Remod/Add` |
      Yr_Sold < Garage_Yr_Blt
  ) %>%
  select(row, Year_Built, `Year_Remod/Add`, Garage_Yr_Blt, Yr_Sold)

# Row 851: Remodel year before built year → set Remodel = Built
data$`Year_Remod/Add`[data$`Year_Remod/Add` < data$Year_Built] =
  data$Year_Built[data$`Year_Remod/Add` < data$Year_Built]

# The other three rows have Yr_Sold before Year_Remod/Add (and one before Garage)
# Most conservative: set the remodel/garage year to the sold year when sold comes first
data$`Year_Remod/Add` = ifelse(
  data$Yr_Sold < data$`Year_Remod/Add`,
  data$Yr_Sold,
  data$`Year_Remod/Add`
)

data$Garage_Yr_Blt = ifelse(
  !is.na(data$Garage_Yr_Blt) & data$Yr_Sold < data$Garage_Yr_Blt,
  data$Yr_Sold,
  data$Garage_Yr_Blt
)

data %>%
  mutate(row = row_number()) %>%
  filter(
    `Year_Remod/Add` < Year_Built |
      Yr_Sold < Year_Built |
      Yr_Sold < `Year_Remod/Add` |
      Yr_Sold < Garage_Yr_Blt
  ) %>%
  select(row, Year_Built, `Year_Remod/Add`, Garage_Yr_Blt, Yr_Sold)

# one left with built date after sale date. Checking it:
data %>%
  mutate(row = row_number()) %>%
  filter(row == 2181) %>%
  select(row, Year_Built, `Year_Remod/Add`, Garage_Yr_Blt, Yr_Sold, Sale_Condition, Sale_Type)
# Sale_Condition = Partial and Sale_Type = New explains it completely.
# Leave the years exactly as they are. No fix needed.

# ----------------------------------------------------------------------------
# 5. Remove redundant predictors and handle missing values
# ----------------------------------------------------------------------------
# Choose the candidate pool explicitly
# Garage_Yr_Blt — 159 NAs, kills every garage-less house ~ dropping.
# -- it is redundant, and the year a garage is built is not likely
# a strong predictor of house price
# Losing parts for SF, and parts for Bsmt to avoid perfect multi-colinearity
# Drop definitionally redundant / highly collinear components
# Keep the totals instead
data$`1st_Flr_SF` = NULL
data$`2nd_Flr_SF` = NULL
data$Low_Qual_Fin_SF = NULL
data$BsmtFin_SF_1 = NULL
data$BsmtFin_SF_2 = NULL
data$Bsmt_Unf_SF = NULL
data$TotRms_AbvGrd = NULL
data$Garage_Yr_Blt = NULL

# How many rows still contain at least one NA?
sum(!complete.cases(data))

na_counts = colSums(is.na(data))
na_counts[na_counts > 0]
data$Lot_Frontage = NULL # var 490 NAs + weak signal low cor with SalePrice
# Fix the 23 masonry veneer missings, assumption: no recorded Vnr = no Vnr.
# Note this is an inference, and possibly wrong, but the damage is only 23 rows
data$Mas_Vnr_Type[is.na(data$Mas_Vnr_Type)] = "None"
data$Mas_Vnr_Area[is.na(data$Mas_Vnr_Area)] = 0
# Rebuild the indicator after the fix
data$Has_MasVnr = as.integer(data$Mas_Vnr_Type != "None")
sum(!complete.cases(data)) # 9 remaining NAs dropping them now:
# Drop the final 9 incomplete rows
data = data[complete.cases(data), ]
nrow(data)


# ============================================================================
# D. Exploratory data analysis
# ============================================================================

# ----------------------------------------------------------------------------
# 1. Exclude abnormal sales and very large houses
# ----------------------------------------------------------------------------
# A very low sale price exists on the log transformed SalePrice, looking to see if exclusion is justified
data %>%
  mutate(row = row_number()) %>%
  filter(SalePrice < 20000) %>%
  select(row, SalePrice, Sale_Condition, Sale_Type, Gr_Liv_Area, Overall_Qual, Year_Built)

# Exclude abnormal sales (not arm's-length transactions)
data = data %>% filter(Sale_Condition != "Abnorml")
nrow(data)
summary(data$SalePrice)


# Exclude very large houses (Gr_Liv_Area > 4000)
# De Cock (dataset author) recommends this exclusion because these are
# typically Partial sales of unfinished homes that do not represent
# normal market transactions and create extreme residuals.
data = data %>% filter(Gr_Liv_Area <= 4000)
nrow(data)


# ----------------------------------------------------------------------------
# 2. SalePrice and log(SalePrice) distributions
# ----------------------------------------------------------------------------
# Distribution of SalePrice
plot_saleprice = ggplot(data, aes(x = SalePrice)) +
  geom_histogram(bins = 40, fill = brand_colors["blue"], color = "white") +
  portfolio_theme +
  labs(title = "Distribution of SalePrice", x = "Sale Price", y = "Count")
save_plot(plot_saleprice, "Distribution of SalePrice.png")

# Log version (usually much better behaved)
plot_log_saleprice = ggplot(data, aes(x = log(SalePrice))) +
  geom_histogram(bins = 40, fill = brand_colors["plum"], color = "white") +
  portfolio_theme +
  labs(title = "Distribution of log(SalePrice)", x = "log(Sale Price)", y = "Count")
save_plot(plot_log_saleprice, "Distribution of log(SalePrice).png")


# ----------------------------------------------------------------------------
# 3. Correlation analysis
# ----------------------------------------------------------------------------
# All numeric columns (continuous, ordinal, and indicators)
numeric_vars = data[sapply(data, is.numeric)]

# Remove SalePrice from the predictor set
numeric_predictors = numeric_vars[, names(numeric_vars) != "SalePrice"]

# Correlations with log(SalePrice), strongest first
log_price = log(data$SalePrice)
cor_with_logprice = sort(
  cor(numeric_predictors, log_price)[, 1],
  decreasing = TRUE
)
head(cor_with_logprice, 10)

# finding the constant
sds = sapply(numeric_vars, function(x) sd(x, na.rm = TRUE))
sds[is.na(sds) | sds < 1e-10]

# Turn into a data frame for ggplot
cor_df = data.frame(
  variable = names(cor_with_logprice),
  correlation = as.numeric(cor_with_logprice)
)

# Keep the top 20 for readability
cor_df = head(cor_df, 20)

# Add a simple color group
cor_df$color_group = ifelse(cor_df$correlation >= 0.60, "Top", "Other")

plot_top_correlations = ggplot(cor_df, aes(x = reorder(variable, correlation), y = correlation, fill = color_group)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(
    "Top" = unname(brand_colors["plum"]),
    "Other" = "grey70"
  )) +
  portfolio_theme +
  labs(
    title = "Top Correlations with log(SalePrice)",
    x = NULL,
    y = "Correlation",
    fill = NULL
  ) +
  theme(legend.position = "none")
save_plot(plot_top_correlations, "Top Correlations with log(SalePrice).png")


# Variables with correlation > 0.60 to log(SalePrice)
top_vars = names(cor_with_logprice)[cor_with_logprice > 0.60]

# Check how many you got
length(top_vars)
top_vars

# Correlation matrix of the top variables
top_cor = cor(numeric_vars[, top_vars], use = "complete.obs")

# Convert to long format for ggplot
cor_long = as.data.frame(as.table(top_cor))
names(cor_long) = c("Var1", "Var2", "Correlation")

# Plot
plot_top_predictor_correlations = ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3, color = "black") +
  scale_fill_gradient2(
    low = brand_colors["plum"],
    mid = "white",
    high = brand_colors["blue"],
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  portfolio_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(margin = margin(l = 36))
  ) +
  labs(
    title = "Correlations among top predictors (r > 0.60)",
    x = NULL,
    y = NULL,
    fill = "r"
  ) +
  coord_fixed()
save_plot(plot_top_predictor_correlations, "Correlations among top predictors (r > 0.60).png")


# ============================================================================
# E. Multiple linear regression
# ============================================================================

# ----------------------------------------------------------------------------
# 1. Train / test split
# ----------------------------------------------------------------------------
set.seed(42)
n <- nrow(data)
train_idx <- sample(seq_len(n), size = floor(0.80 * n))

train <- data[train_idx, ]
test  <- data[-train_idx, ]

# Save the two piles for Python
write.csv(train, "train.csv", row.names = FALSE)
write.csv(test,  "test.csv",  row.names = FALSE)

nrow(train)
nrow(test)

# ----------------------------------------------------------------------------
# 2. LASSO variable selection
# ----------------------------------------------------------------------------
# Model selection is performed on the training set only
# Build the design matrix and response from the training data
x_train <- model.matrix(log(SalePrice) ~ . - 1, data = train)
y_train <- log(train$SalePrice)

set.seed(42)
cv_lasso <- cv.glmnet(
  x = x_train,
  y = y_train,
  alpha = 1,
  nfolds = 10
)

# Coefficients at lambda.1se
coef_1se <- coef(cv_lasso, s = "lambda.1se")
selected_1se <- coef_1se[coef_1se[, 1] != 0, , drop = FALSE]

cat("lambda.1se kept:", nrow(selected_1se) - 1, "predictors\n")

# ----------------------------------------------------------------------------
# 3. OLS refit
# ----------------------------------------------------------------------------
selected_vars <- setdiff(rownames(selected_1se), "(Intercept)")
x_clean <- x_train[, selected_vars, drop = FALSE]

# Clean ugly names
colnames(x_clean) <- gsub("`", "", colnames(x_clean))
colnames(x_clean) <- gsub("Year_Remod/Add", "Year_Remod_Add", colnames(x_clean))
colnames(x_clean) <- gsub("MS_ZoningC \\(all\\)", "MS_Zoning_C_all", colnames(x_clean))

ols_1se <- lm(y_train ~ ., data = as.data.frame(x_clean))
summary(ols_1se)

# ----------------------------------------------------------------------------
# 4. AIC and BIC backward selection
# ----------------------------------------------------------------------------
ols_aic <- step(ols_1se, direction = "backward", trace = 0)
ols_bic <- step(ols_1se, direction = "backward", k = log(nrow(x_clean)), trace = 0)

cat("AIC stepwise predictors:", length(coef(ols_aic)) - 1, "\n")
cat("BIC stepwise predictors:", length(coef(ols_bic)) - 1, "\n")

summary(ols_aic)
summary(ols_bic)

# ----------------------------------------------------------------------------
# 5. Model comparison
# ----------------------------------------------------------------------------
# Evaluation function
get_metrics <- function(model, name, test_data) {
  
  s <- summary(model)
  
  # Test design matrix
  x_test_full <- model.matrix(log(SalePrice) ~ . - 1, data = test_data)
  
  # Clean names the same way we cleaned the training matrix
  colnames(x_test_full) <- gsub("`", "", colnames(x_test_full))
  colnames(x_test_full) <- gsub("Year_Remod/Add", "Year_Remod_Add", colnames(x_test_full))
  colnames(x_test_full) <- gsub("MS_ZoningC \\(all\\)", "MS_Zoning_C_all", colnames(x_test_full))
  
  model_vars <- setdiff(names(coef(model)), "(Intercept)")
  x_test <- x_test_full[, model_vars, drop = FALSE]
  
  test_log_pred <- predict(model, newdata = as.data.frame(x_test))
  test_pred     <- exp(test_log_pred)
  test_actual   <- test_data$SalePrice
  
  data.frame(
    Model       = name,
    Predictors  = length(coef(model)) - 1,
    
    # Training-fit metrics
    Adj_R2      = s$adj.r.squared,
    Residual_SE = s$sigma,
    AIC         = AIC(model),
    BIC         = BIC(model),
    
    # Test (original dollar scale)
    Test_RMSE   = sqrt(mean((test_actual - test_pred)^2)),
    Test_MAE    = mean(abs(test_actual - test_pred)),
    Test_MAPE   = mean(abs((test_actual - test_pred) / test_actual)) * 100
  )
}

# Build the final comparison table
comparison <- rbind(
  get_metrics(ols_1se, "LASSO 1se + OLS",  test),
  get_metrics(ols_aic, "LASSO + AIC back", test),
  get_metrics(ols_bic, "LASSO + BIC back", test)
)

print(comparison, digits = 5, row.names = FALSE)


# ============================================================================
# F. Model diagnostics
# ============================================================================

# Choose the model to diagnose BIC
final_model <- ols_bic

# Prepare diagnostic data
diag_df <- data.frame(
  obs        = seq_len(nrow(model.frame(final_model))),
  fitted     = fitted(final_model),
  residuals  = residuals(final_model),
  std_resid  = rstandard(final_model),
  leverage   = hatvalues(final_model),
  cooksd     = cooks.distance(final_model)
)

# Identify the most extreme points for labeling
finite_resid  <- is.finite(diag_df$std_resid)
high_lev_idx  <- which.max(replace(diag_df$leverage, !finite_resid, -Inf))
high_cook_idx <- which.max(replace(diag_df$cooksd,   !finite_resid, -Inf))
label_idx     <- unique(c(high_lev_idx, high_cook_idx))
label_df      <- diag_df[label_idx, ]

# ----------------------------------------------------------------------------
# 1. Residuals vs fitted
# ----------------------------------------------------------------------------
p1 <- ggplot(diag_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.5, color = brand_colors["blue"]) +
  geom_hline(yintercept = 0, linetype = "dashed", color = brand_colors["plum"]) +
  geom_smooth(se = FALSE, color = brand_colors["brick"], linewidth = 0.8) +
  geom_text(data = label_df, aes(label = obs), nudge_y = 0.03, size = 3.5) +
  portfolio_theme +
  labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")
save_plot(p1, "Residuals vs Fitted.png")

# ----------------------------------------------------------------------------
# 2. Normal Q-Q
# ----------------------------------------------------------------------------
qq_df <- diag_df[order(diag_df$std_resid), ]
qq_df$theoretical <- qnorm(ppoints(nrow(qq_df)))
qq_label <- qq_df[qq_df$obs %in% label_df$obs, ]

p2 <- ggplot(qq_df, aes(x = theoretical, y = std_resid)) +
  geom_point(alpha = 0.5, color = brand_colors["blue"]) +
  geom_abline(slope = 1, intercept = 0, color = brand_colors["plum"]) +
  geom_text(data = qq_label, aes(label = obs), nudge_y = 0.25, size = 3.5) +
  portfolio_theme +
  labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals")
save_plot(p2, "Normal Q-Q.png")

# ----------------------------------------------------------------------------
# 3. Scale-location
# ----------------------------------------------------------------------------
p3 <- ggplot(diag_df, aes(x = fitted, y = sqrt(abs(std_resid)))) +
  geom_point(alpha = 0.5, color = brand_colors["blue"]) +
  geom_smooth(se = FALSE, color = brand_colors["brick"], linewidth = 0.8) +
  geom_text(data = transform(label_df, y = sqrt(abs(std_resid))),
            aes(x = fitted, y = y, label = obs), nudge_y = 0.05, size = 3.5) +
  portfolio_theme +
  labs(title = "Scale-Location", x = "Fitted values", y = expression(sqrt("|Std. Residuals|")))
save_plot(p3, "Scale-Location.png")

# ----------------------------------------------------------------------------
# 4. Residuals vs leverage
# ----------------------------------------------------------------------------
p4 <- ggplot(diag_df, aes(x = leverage, y = std_resid)) +
  geom_point(alpha = 0.5, color = brand_colors["blue"]) +
  geom_hline(yintercept = 0, linetype = "dashed", color = brand_colors["plum"]) +
  geom_text(data = label_df, aes(label = obs), nudge_y = 0.3, hjust = 1, size = 3.5) +
  portfolio_theme +
  labs(title = "Residuals vs Leverage", x = "Leverage", y = "Standardized Residuals")
save_plot(p4, "Residuals vs Leverage.png")

# Combine into 2x2
diagnostics_plot = (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title = "Diagnostics — LASSO + BIC back",
    theme = portfolio_theme
  )

save_plot(
  diagnostics_plot,
  "Diagnostics — LASSO + BIC back.png",
  width = 10,
  height = 10
)

# ----------------------------------------------------------------------------
# 5. Cook's distance, residuals, and VIF
# ----------------------------------------------------------------------------
# Highest Cook's distances
finite <- is.finite(diag_df$cooksd)
head(diag_df[finite, ][order(-diag_df$cooksd[finite]), ], 10)

# Worst (most negative) residuals
worst <- order(diag_df$residuals)[1:10]
train[worst, c("SalePrice", "Gr_Liv_Area", "Overall_Qual", "Overall_Cond",
               "Neighborhood", "Sale_Condition", "Sale_Type", "Year_Built")]

# Highest VIFs
sort(vif(final_model), decreasing = TRUE)[1:10]


# Cook's distance plot
n <- nrow(diag_df)

# Keep only the 6 highest Cook's D points for labeling
top6 <- diag_df[order(-diag_df$cooksd), ][1:6, ]

cook_plot = ggplot(diag_df, aes(x = obs, y = cooksd)) +
  geom_point(alpha = 0.6, color = brand_colors["blue"]) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = brand_colors["plum"]) +
  geom_text(
    data = top6,
    aes(label = obs),
    nudge_y = 0.008,
    size = 3.5
  ) +
  coord_cartesian(ylim = c(0, 0.55)) +
  portfolio_theme +
  labs(
    title = "Cook's Distance — LASSO + BIC back",
    x = "Observation index",
    y = "Cook's Distance"
  ) +
  annotate("text", x = n * 0.75, y = 0.52,
           label = "Cook's D = 0.5",
           color = brand_colors["plum"], size = 3.5)
save_plot(cook_plot, "Cook's Distance — LASSO + BIC back.png")

# ----------------------------------------------------------------------------
# 6. Residuals by sale condition
# ----------------------------------------------------------------------------
plot_df <- data.frame(
  residuals = diag_df$residuals,
  Sale_Condition = train$Sale_Condition
)

counts <- table(plot_df$Sale_Condition)
label_levels <- paste0(names(counts), "\n(n = ", counts, ")")

plot_df$Sale_Condition_label <- factor(
  plot_df$Sale_Condition,
  levels = names(counts),
  labels = label_levels
)

# Unnamed vector — assigned by position to the factor levels
condition_colors <- c(
  brand_colors["blue"],
  brand_colors["plum"],
  brand_colors["brick"],
  brand_colors["field"],
  brand_colors["salmon"]
)

plot_residuals_by_condition = ggplot(plot_df, aes(x = Sale_Condition_label, y = residuals, fill = Sale_Condition_label)) +
  geom_boxplot(
    color = brand_colors["field"],
    alpha = 0.85,
    outlier.color = brand_colors["brick"],
    outlier.alpha = 0.7,
    outlier.size = 1.8
  ) +
  scale_fill_manual(values = unname(condition_colors)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = brand_colors["plum"], linewidth = 0.9) +
  portfolio_theme +
  labs(
    title = "Residuals by Sale Condition — LASSO + BIC back",
    x = "Sale Condition",
    y = "Residuals (log scale)"
  ) +
  theme(
    legend.position = "none",
    plot.title.position = "plot"
  )
save_plot(plot_residuals_by_condition, "Residuals by Sale Condition — LASSO + BIC back.png")

# ----------------------------------------------------------------------------
# 7. Breusch-Pagan test
# ----------------------------------------------------------------------------

# Test for heteroskedasticity in the final regression model
bptest(final_model)


# ----------------------------------------------------------------------------
# 8. High-leverage observations
# ----------------------------------------------------------------------------

# Examine leverage values from the already-fitted BIC model
hv <- hatvalues(ols_bic)

# Count observations with leverage greater than 0.99
sum(hv > 0.99)

# Identify those high-leverage observations
which(hv > 0.99)

# Inspect their standardized residuals
rstandard(ols_bic)[hv > 0.99]


# ----------------------------------------------------------------------------
# 9. Rare design-matrix columns
# ----------------------------------------------------------------------------

# Inspect the design matrix from the already-fitted BIC model
X <- model.matrix(ols_bic)

# Sum each design-matrix column
cs <- colSums(X)

# Show rare indicator columns supported by 1 to 9 observations
sort(cs[cs > 0 & cs <= 9])


# ----------------------------------------------------------------------------
# 10. Residual spread by sale condition
# ----------------------------------------------------------------------------

# Match model residuals to the corresponding rows in the training data
model_rows <- match(
  names(resid(ols_bic)),
  rownames(train)
)

# Compare residual dispersion across sale-condition groups
tapply(
  resid(ols_bic),
  train$Sale_Condition[model_rows],
  IQR,
  na.rm = TRUE
)




# ============================================================================
# G. Results and visualizations
# ============================================================================

# ----------------------------------------------------------------------------
# 1. Coefficient table with percent effects
# ----------------------------------------------------------------------------
coef_table <- broom::tidy(final_model) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    pct_effect = (exp(estimate) - 1) * 100
  ) %>%
  arrange(desc(abs(estimate)))

print(
  coef_table[, c("term", "estimate", "pct_effect")],
  digits = 4,
  row.names = FALSE
)

# ----------------------------------------------------------------------------
# 2. Predicted vs actual — test set
# ----------------------------------------------------------------------------
x_test_full <- model.matrix(log(SalePrice) ~ . - 1, data = test)
colnames(x_test_full) <- gsub("`", "", colnames(x_test_full))
colnames(x_test_full) <- gsub("Year_Remod/Add", "Year_Remod_Add", colnames(x_test_full))
colnames(x_test_full) <- gsub("MS_ZoningC \\(all\\)", "MS_Zoning_C_all", colnames(x_test_full))

model_vars <- setdiff(names(coef(final_model)), "(Intercept)")
x_test <- x_test_full[, model_vars, drop = FALSE]

test_log_pred   <- predict(final_model, newdata = as.data.frame(x_test))
test_log_actual <- log(test$SalePrice)

test_pred_dollars   <- exp(test_log_pred)
test_actual_dollars <- test$SalePrice

test_rmse <- sqrt(mean((test_actual_dollars - test_pred_dollars)^2))
test_mape <- mean(abs(test_actual_dollars - test_pred_dollars) / test_actual_dollars) * 100

test_fit_df <- data.frame(
  observed  = test_log_actual,
  predicted = test_log_pred
)

plot_predicted_vs_actual = ggplot(test_fit_df, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.45, color = brand_colors["blue"]) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed",
    color = brand_colors["brick"],
    linewidth = 0.8
  ) +
  portfolio_theme +
  labs(
    title = "Predicted vs Actual — Test Set",
    subtitle = paste0(
      "Test RMSE = $", format(round(test_rmse), big.mark = ","),
      "  |  Test MAPE = ", round(test_mape, 1), "%",
      "\nVisual spread reflects proportional error"
    ),
    x = "Observed log(SalePrice)",
    y = "Predicted log(SalePrice)"
  )
save_plot(plot_predicted_vs_actual, "Predicted vs Actual (Test Set, log scale) — LASSO + BIC back.png")

# ----------------------------------------------------------------------------
# 3. Standardized coefficient plot
# ----------------------------------------------------------------------------
coef_model <- final_model
coef_x <- model.matrix(coef_model)[, -1, drop = FALSE]
coef_y <- model.response(model.frame(coef_model))

coef_std <- broom::tidy(coef_model) %>%
  filter(term != "(Intercept)")

x_sd <- apply(coef_x, 2, sd)
y_sd <- sd(coef_y)

coef_std$std_estimate <- coef_std$estimate * x_sd[coef_std$term] / y_sd
coef_std$std_se       <- coef_std$std.error * x_sd[coef_std$term] / y_sd
coef_std$lower        <- coef_std$std_estimate - 1.96 * coef_std$std_se
coef_std$upper        <- coef_std$std_estimate + 1.96 * coef_std$std_se
coef_std$abs_std      <- abs(coef_std$std_estimate)

coef_plot <- coef_std %>%
  arrange(desc(abs_std)) %>%
  slice_head(n = 15)

coef_plot$term <- factor(coef_plot$term, levels = rev(coef_plot$term))

plot_strongest_predictors = ggplot(coef_plot, aes(x = term, y = std_estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = brand_colors["field"]) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.20, color = brand_colors["brick"]) +
  geom_point(size = 3, color = brand_colors["plum"]) +
  coord_flip() +
  portfolio_theme +
  theme(
    plot.title.position = "plot"
  ) +
  labs(
    title = "Strongest Predictors in the Final BIC Model",
    subtitle = "Top 15 standardized coefficients",
    x = NULL,
    y = "Standardized coefficient"
  )
save_plot(plot_strongest_predictors, "Strongest Predictors in the Final BIC Model.png")

# ----------------------------------------------------------------------------
# 4. Error metrics by SalePrice tercile
# ----------------------------------------------------------------------------
binned_df <- data.frame(
  actual = test_actual_dollars,
  pred   = test_pred_dollars
)

breaks <- quantile(binned_df$actual, probs = c(0, 1/3, 2/3, 1))

binned_df$tercile <- cut(
  binned_df$actual,
  breaks = breaks,
  include.lowest = TRUE,
  labels = c("Low", "Mid", "High")
)

binned_metrics <- binned_df %>%
  group_by(tercile) %>%
  summarise(
    n          = n(),
    price_min  = min(actual),
    price_max  = max(actual),
    RMSE       = sqrt(mean((actual - pred)^2)),
    MAE        = mean(abs(actual - pred)),
    MAPE       = mean(abs(actual - pred) / actual) * 100,
    .groups = "drop"
  )

print(binned_metrics, digits = 4)


# ----------------------------------------------------------------------------
# 5. Baseline model
# ----------------------------------------------------------------------------
# Two-variable model: Overall_Qual + Gr_Liv_Area
baseline <- lm(log(SalePrice) ~ Overall_Qual + Gr_Liv_Area, data = train)

# Predict on test set
baseline_log_pred <- predict(baseline, newdata = test)
baseline_pred     <- exp(baseline_log_pred)
baseline_actual   <- test$SalePrice

baseline_rmse <- sqrt(mean((baseline_actual - baseline_pred)^2))
baseline_mape <- mean(abs(baseline_actual - baseline_pred) / baseline_actual) * 100

cat("Baseline (Overall_Qual + Gr_Liv_Area)\n")
cat("Test RMSE: $", format(round(baseline_rmse), big.mark = ","), "\n")
cat("Test MAPE: ", round(baseline_mape, 1), "%\n", sep = "")

# ============================================================================
# H. Pre-registered exploratory comparison
# ============================================================================

# ----------------------------------------------------------------------------
# 1. Training-only top-20 rankings and test evaluation
# ----------------------------------------------------------------------------

# This exploratory comparison was specified after the main analysis was
# complete but before these additional models were run.
#
# All three predictor rankings below are computed from the training data only.
# None of the ranking rules uses test-set performance.
# The ranking and evaluation rule was specified before running this section.
# Therefore, the three additional test-set evaluations are legitimate
# out-of-sample estimates rather than protocol violations.
#
# This section is read-only with respect to the completed analysis:
# it does not modify train, test, ols_bic, or any other existing object.
# All temporary objects are created inside a local environment and disappear
# after this section finishes.

local({
  
  # --------------------------------------------------------------------------
  # Rebuild the full training design matrix used by the original LASSO stage
  # --------------------------------------------------------------------------
  
  # Use the same cleaned predictor pool and model-matrix specification as the
  # original LASSO analysis. These are design-matrix columns, including the
  # dummy variables created from categorical predictors.
  explore_x_train <- model.matrix(
    log(SalePrice) ~ . - 1,
    data = train
  )
  
  explore_y_train <- log(train$SalePrice)
  
  # Stop rather than silently changing the ranking rule if the design matrix
  # contains duplicate column names.
  if (anyDuplicated(colnames(explore_x_train)) > 0) {
    stop(
      "Exploratory comparison stopped: the training design matrix contains ",
      "duplicate predictor names."
    )
  }
  
  # Pearson correlation and standardized coefficients are undefined for
  # predictors with zero variance. Report them and stop rather than dropping
  # them or substituting another ranking rule.
  explore_sd <- apply(explore_x_train, 2, sd)
  
  if (any(!is.finite(explore_sd) | explore_sd == 0)) {
    
    explore_bad_sd <- names(
      explore_sd[!is.finite(explore_sd) | explore_sd == 0]
    )
    
    cat(
      "\nExploratory comparison stopped.\n",
      "The following full-design-matrix predictors have zero or undefined ",
      "training-set variance:\n\n",
      sep = ""
    )
    
    print(explore_bad_sd)
    
    stop(
      "The requested marginal-correlation and standardized-coefficient ",
      "rankings cannot be computed as specified. No alternate rule was used."
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Ranking 1: marginal correlation
  # --------------------------------------------------------------------------
  
  # Compute the absolute Pearson correlation of every training predictor with
  # log(SalePrice), rank from largest to smallest, and retain the top 20.
  explore_cor <- cor(
    explore_x_train,
    explore_y_train,
    method = "pearson"
  )[, 1]
  
  # Stop if any correlation is undefined rather than substituting another rule.
  if (any(!is.finite(explore_cor))) {
    
    explore_bad_cor <- names(explore_cor[!is.finite(explore_cor)])
    
    cat(
      "\nExploratory comparison stopped.\n",
      "Pearson correlation is undefined for:\n\n",
      sep = ""
    )
    
    print(explore_bad_cor)
    
    stop(
      "Marginal-correlation ranking cannot be computed as specified. ",
      "No alternate ranking rule was used."
    )
  }
  
  explore_cor_rank <- sort(
    abs(explore_cor),
    decreasing = TRUE
  )
  
  # A tie exactly at the 20-variable cutoff would make the requested top-20
  # set ambiguous because no further tie-breaking rule was specified.
  if (
    length(explore_cor_rank) > 20 &&
    explore_cor_rank[20] == explore_cor_rank[21]
  ) {
    stop(
      "Exploratory comparison stopped: marginal correlation has an exact ",
      "tie at the 20-variable cutoff, and no additional tie rule was specified."
    )
  }
  
  explore_top20_cor <- names(explore_cor_rank)[1:20]
  
  
  # --------------------------------------------------------------------------
  # Ranking 2: LASSO entry order
  # --------------------------------------------------------------------------
  
  # Fit a standard LASSO path using training data only.
  # No cross-validation is used for this ranking.
  explore_lasso_path <- glmnet::glmnet(
    x = explore_x_train,
    y = explore_y_train,
    alpha = 1
  )
  
  explore_beta <- as.matrix(explore_lasso_path$beta)
  
  # Record the first time each predictor receives a non-zero coefficient as
  # lambda decreases. Within the same lambda step, rank newly entering
  # predictors by absolute coefficient magnitude at that step.
  explore_entered <- character(0)
  
  for (explore_j in seq_len(ncol(explore_beta))) {
    
    explore_nonzero <- rownames(explore_beta)[
      explore_beta[, explore_j] != 0
    ]
    
    explore_new <- setdiff(
      explore_nonzero,
      explore_entered
    )
    
    if (length(explore_new) > 0) {
      
      explore_new_abs <- abs(
        explore_beta[explore_new, explore_j]
      )
      
      explore_new <- explore_new[
        order(explore_new_abs, decreasing = TRUE)
      ]
      
      # If an unresolved coefficient-magnitude tie would cross the top-20
      # boundary, stop instead of inventing an additional ranking rule.
      explore_remaining <- 20 - length(explore_entered)
      
      if (
        explore_remaining > 0 &&
        length(explore_new) > explore_remaining
      ) {
        
        explore_ordered_abs <- sort(
          explore_new_abs,
          decreasing = TRUE
        )
        
        if (
          explore_ordered_abs[explore_remaining] ==
          explore_ordered_abs[explore_remaining + 1]
        ) {
          stop(
            "Exploratory comparison stopped: the LASSO entry ranking has ",
            "an unresolved coefficient-magnitude tie at the top-20 cutoff."
          )
        }
      }
      
      explore_entered <- c(
        explore_entered,
        explore_new
      )
    }
    
    if (length(explore_entered) >= 20) {
      break
    }
  }
  
  if (length(explore_entered) < 20) {
    stop(
      "Exploratory comparison stopped: fewer than 20 predictors entered ",
      "the LASSO path."
    )
  }
  
  explore_top20_lasso <- explore_entered[1:20]
  
  
  # --------------------------------------------------------------------------
  # Ranking 3: standardized coefficient magnitude
  # --------------------------------------------------------------------------
  
  # Standardize every predictor from the full cleaned training design matrix.
  explore_x_std <- scale(explore_x_train)
  
  # Fit OLS using all standardized training predictors.
  explore_std_design <- cbind(
    `(Intercept)` = 1,
    explore_x_std
  )
  
  explore_std_fit <- lm.fit(
    x = explore_std_design,
    y = explore_y_train
  )
  
  # A rank-deficient fit would make individual standardized coefficients
  # non-unique. Stop rather than changing the model or ranking rule.
  if (explore_std_fit$rank < ncol(explore_std_design)) {
    stop(
      "Exploratory comparison stopped: full standardized OLS is rank ",
      "deficient, so standardized coefficient magnitudes cannot be ranked ",
      "uniquely as specified."
    )
  }
  
  explore_std_coef <- explore_std_fit$coefficients[-1]
  
  if (any(!is.finite(explore_std_coef))) {
    stop(
      "Exploratory comparison stopped: at least one standardized OLS ",
      "coefficient is not finite."
    )
  }
  
  explore_std_rank <- sort(
    abs(explore_std_coef),
    decreasing = TRUE
  )
  
  # Stop on an unresolved exact tie at the top-20 cutoff.
  if (
    length(explore_std_rank) > 20 &&
    explore_std_rank[20] == explore_std_rank[21]
  ) {
    stop(
      "Exploratory comparison stopped: standardized coefficient magnitude ",
      "has an exact tie at the 20-variable cutoff."
    )
  }
  
  explore_top20_std <- names(explore_std_rank)[1:20]
  
  
  # --------------------------------------------------------------------------
  # Lock the three training-only rankings before examining the test set
  # --------------------------------------------------------------------------
  
  cat("\n============================================================\n")
  cat("TOP 20 — MARGINAL CORRELATION\n")
  cat("============================================================\n")
  cat(
    paste0(
      seq_along(explore_top20_cor),
      ". ",
      explore_top20_cor
    ),
    sep = "\n"
  )
  cat("\n")
  
  
  cat("\n============================================================\n")
  cat("TOP 20 — LASSO ENTRY ORDER\n")
  cat("============================================================\n")
  cat(
    paste0(
      seq_along(explore_top20_lasso),
      ". ",
      explore_top20_lasso
    ),
    sep = "\n"
  )
  cat("\n")
  
  
  cat("\n============================================================\n")
  cat("TOP 20 — STANDARDIZED COEFFICIENT MAGNITUDE\n")
  cat("============================================================\n")
  cat(
    paste0(
      seq_along(explore_top20_std),
      ". ",
      explore_top20_std
    ),
    sep = "\n"
  )
  cat("\n")
  
  
  # --------------------------------------------------------------------------
  # Pairwise overlap and Overall_Cond membership
  # --------------------------------------------------------------------------
  
  explore_overlap <- data.frame(
    Comparison = c(
      "Marginal correlation vs LASSO entry",
      "Marginal correlation vs standardized coefficient",
      "LASSO entry vs standardized coefficient"
    ),
    Overlap = c(
      length(intersect(explore_top20_cor, explore_top20_lasso)),
      length(intersect(explore_top20_cor, explore_top20_std)),
      length(intersect(explore_top20_lasso, explore_top20_std))
    )
  )
  
  cat("\n============================================================\n")
  cat("PAIRWISE TOP-20 OVERLAP\n")
  cat("============================================================\n")
  print(
    explore_overlap,
    row.names = FALSE
  )
  
  
  explore_overall_cond <- data.frame(
    Ranking = c(
      "Marginal correlation",
      "LASSO entry order",
      "Standardized coefficient magnitude"
    ),
    Overall_Cond = c(
      "Overall_Cond" %in% explore_top20_cor,
      "Overall_Cond" %in% explore_top20_lasso,
      "Overall_Cond" %in% explore_top20_std
    )
  )
  
  cat("\n============================================================\n")
  cat("OVERALL_COND IN TOP 20\n")
  cat("============================================================\n")
  print(
    explore_overall_cond,
    row.names = FALSE
  )
  
  
  # --------------------------------------------------------------------------
  # Test-set evaluation
  # --------------------------------------------------------------------------
  
  # The rankings are now fixed. Build the test design matrix using exactly the
  # same model-matrix specification as the training data.
  explore_x_test <- model.matrix(
    log(SalePrice) ~ . - 1,
    data = test
  )
  
  # Stop if the training and test matrices are not structurally identical.
  if (!identical(colnames(explore_x_train), colnames(explore_x_test))) {
    stop(
      "Exploratory comparison stopped: training and test design matrices ",
      "do not contain identical predictor columns."
    )
  }
  
  explore_y_test_dollars <- test$SalePrice
  
  
  # Fit plain OLS on the original unstandardized predictor columns selected by
  # each training-only ranking and return dollar-scale test RMSE and MAPE.
  explore_score_model <- function(explore_vars, explore_name) {
    
    explore_train_20 <- explore_x_train[
      ,
      explore_vars,
      drop = FALSE
    ]
    
    explore_test_20 <- explore_x_test[
      ,
      explore_vars,
      drop = FALSE
    ]
    
    # Include an intercept and fit ordinary least squares.
    explore_design_train <- cbind(
      `(Intercept)` = 1,
      explore_train_20
    )
    
    explore_fit <- lm.fit(
      x = explore_design_train,
      y = explore_y_train
    )
    
    # Stop if one of the requested 20-predictor OLS models is rank deficient.
    if (explore_fit$rank < ncol(explore_design_train)) {
      stop(
        paste0(
          "Exploratory comparison stopped: the ",
          explore_name,
          " 20-predictor OLS model is rank deficient."
        )
      )
    }
    
    explore_design_test <- cbind(
      `(Intercept)` = 1,
      explore_test_20
    )
    
    explore_log_pred <- drop(
      explore_design_test %*% explore_fit$coefficients
    )
    
    # Back-transform predictions to dollars before computing test metrics.
    explore_pred_dollars <- exp(explore_log_pred)
    
    explore_rmse <- sqrt(
      mean(
        (explore_y_test_dollars - explore_pred_dollars)^2
      )
    )
    
    explore_mape <- mean(
      abs(
        (explore_y_test_dollars - explore_pred_dollars) /
          explore_y_test_dollars
      )
    ) * 100
    
    data.frame(
      Model = explore_name,
      Test_RMSE = explore_rmse,
      Test_MAPE = explore_mape
    )
  }
  
  
  # Evaluate exactly the three pre-specified top-20 models.
  explore_results <- rbind(
    explore_score_model(
      explore_top20_cor,
      "Top 20 marginal correlation"
    ),
    explore_score_model(
      explore_top20_lasso,
      "Top 20 LASSO entry order"
    ),
    explore_score_model(
      explore_top20_std,
      "Top 20 standardized coefficient"
    )
  )
  
  
  cat("\n============================================================\n")
  cat("TEST-SET PERFORMANCE — THREE PRE-SPECIFIED TOP-20 MODELS\n")
  cat("============================================================\n")
  
  print(
    explore_results,
    digits = 6,
    row.names = FALSE
  )
  
})
