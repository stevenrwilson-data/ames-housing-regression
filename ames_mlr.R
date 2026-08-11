# Project 3 - Housing dataset for MLR analysis
# = is used in place of <- intentionally
# Load packages
library(ggplot2)      # plotting and portfolio theme
library(dplyr)        # data manipulation (filter, mutate, select, etc.)
library(readxl)       # import the AmesHousing.xls file
library(broom)        # tidy model output if needed
library(car)          # VIF and other regression diagnostics
library(corrplot)     # correlation matrix visualization
library(GGally)       # optional pairwise plots
library(lmtest)       # Breusch-Pagan and other specification tests
library(performance)  # additional model performance / check functions
library(Matrix)       # required by glmnet
library(glmnet)       # LASSO and Elastic Net
library(tidyr)        # reshaping data for ggplot correlation heatmap
library(patchwork)    # combine multiple ggplots into a 2x2 grid

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
portfolio_theme = theme_minimal() +
  theme(
    text = element_text(
      family = "Montserrat",
      color = brand_colors["field"]
    ),
    plot.title = element_text(
      family = "Archivo Black",
      color = brand_colors["plum"]
    ),
    plot.caption = element_text(
      family = "Source Code Pro",
      color = brand_colors["brick"]
    ),
    axis.title = element_text(
      color = brand_colors["field"]
    ),
    axis.text = element_text(
      color = brand_colors["field"]
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

###
# Import data
###
setwd("~/project_3_housing")
data = read_excel("AmesHousing.xls")

##
## Quick housekeeping
##
# Drop pure identifier columns ~ listed in docs, not useful for MLR
data$Order = NULL
data$PID = NULL
names(data)

# Replace every space in column names with an underscore
names(data) = gsub(" ", "_", names(data))
names(data)


###
# Inspect and Clean data
###
# Get character columns
char_cols = names(data)[sapply(data, is.character)]

###
# Start with Ordinals
###
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

## Remaining Non-Ordinal char vectors
#The rest are non-ordinals so they will be treated as factors
data[remaining_char] = lapply(data[remaining_char], as.factor)
table(sapply(data, class))
names(data)[sapply(data, is.character)]

# Absence / presence indicators
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





###
# Exploratory data analysis
###

# A very low sale price exists on the log transformed SalePrice, looking to see if exclusion is justified
data %>%
  mutate(row = row_number()) %>%
  filter(SalePrice < 20000) %>%
  select(row, SalePrice, Sale_Condition, Sale_Type, Gr_Liv_Area, Overall_Qual, Year_Built)

# Exclude abnormal sales (not arm's-length transactions)
data = data %>% filter(Sale_Condition != "Abnorml")
nrow(data)
summary(data$SalePrice)


# This is being added here after looking at residual plot that did not account 
# for some anomalies at the high end, I am preserving the data set with those
# anomalies to see the problems with the data on the diagnostic plots. 
# Preserve the dataset that still contains the large Partial sales
# (used for the diagnostic plots that revealed the problem)
data_with_large_homes = data

# Exclude very large houses (Gr_Liv_Area > 4000)
# De Cock (dataset author) recommends this exclusion because these are
# typically Partial sales of unfinished homes that do not represent
# normal market transactions and create extreme residuals.
data = data %>% filter(Gr_Liv_Area <= 4000)
nrow(data)


# Distribution of SalePrice
ggplot(data, aes(x = SalePrice)) +
  geom_histogram(bins = 40, fill = brand_colors["blue"], color = "white") +
  portfolio_theme +
  labs(title = "Distribution of SalePrice", x = "Sale Price", y = "Count")

# Log version (usually much better behaved)
ggplot(data, aes(x = log(SalePrice))) +
  geom_histogram(bins = 40, fill = brand_colors["plum"], color = "white") +
  portfolio_theme +
  labs(title = "Distribution of log(SalePrice)", x = "log(Sale Price)", y = "Count")


# All numeric columns (continuous, ordinal, and indicators)
numeric_vars = data[sapply(data, is.numeric)]

# Remove SalePrice from the predictor set
numeric_predictors = numeric_vars[, names(numeric_vars) != "SalePrice"]

# Correlation matrix
# suppressWarnings because Has_Garage is constant among rows
# where Garage_Yr_Blt is non-missing
cor_matrix = suppressWarnings(
  cor(numeric_predictors, use = "pairwise.complete.obs")
)

# Correlations with log(SalePrice), strongest first
log_price = log(data$SalePrice)

cor_with_logprice = sort(
  cor(numeric_predictors, log_price, use = "pairwise.complete.obs")[, 1],
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

ggplot(cor_df, aes(x = reorder(variable, correlation), y = correlation, fill = color_group)) +
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


# Variables with correlation > 0.60 to log(SalePrice)
top_vars = names(cor_with_logprice)[cor_with_logprice > 0.60]

# Check how many you got
length(top_vars)
top_vars

# Correlation matrix of the top variables
top_cor = cor(numeric_vars[, top_vars], use = "pairwise.complete.obs")

# Convert to long format for ggplot
cor_long = as.data.frame(as.table(top_cor))
names(cor_long) = c("Var1", "Var2", "Correlation")

# Plot
ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
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
    panel.grid = element_blank()
  ) +
  labs(
    title = "Correlations among top predictors (r > 0.60)",
    x = NULL,
    y = NULL,
    fill = "r"
  ) +
  coord_fixed()








###
# Multiple linear regression
###

# Build the model matrix first (this drops incomplete rows)
x = model.matrix(log(SalePrice) ~ . - 1, data = data)
# Create y from the same rows that remain in x
# model.matrix keeps the row names / order of the complete cases
y = log(data$SalePrice[complete.cases(data)])

set.seed(42)
cv_lasso = cv.glmnet(
  x = x,
  y = y,
  alpha = 1,
  nfolds = 10
)

# Plot the CV curve
plot(cv_lasso)

## ggplot version
# Extract the CV results into a data frame
cv_df = data.frame(
  lambda = cv_lasso$lambda,
  cvm = cv_lasso$cvm,
  cvsd = cv_lasso$cvsd,
  nzero = cv_lasso$nzero
)

# Add the two special lambdas
lambda_min = cv_lasso$lambda.min
lambda_1se = cv_lasso$lambda.1se

ggplot(cv_df, aes(x = -log(lambda), y = cvm)) +
  geom_ribbon(aes(ymin = cvm - cvsd, ymax = cvm + cvsd), fill = "grey80", alpha = 0.5) +
  geom_line(color = brand_colors["field"]) +
  geom_point(color = brand_colors["plum"], size = 1.5) +
  geom_vline(xintercept = -log(lambda_min), linetype = "dashed", color = brand_colors["blue"]) +
  geom_vline(xintercept = -log(lambda_1se), linetype = "dashed", color = brand_colors["brick"]) +
  portfolio_theme +
  labs(
    title = "LASSO Cross-Validation",
    x = "-log(λ)",
    y = "Mean Squared Error"
  )

# Coefficients at lambda.min (best predictive accuracy)
coef_min = coef(cv_lasso, s = "lambda.min")
selected_min = coef_min[coef_min[,1] != 0, , drop = FALSE]
selected_min

# Coefficients at lambda.1se (preferred simpler model)
coef_1se = coef(cv_lasso, s = "lambda.1se")
selected_1se = coef_1se[coef_1se[,1] != 0, , drop = FALSE]
selected_1se

# Quick comparison of how many variables each kept
cat("lambda.min kept:", nrow(selected_min) - 1, "predictors\n")  # subtract intercept
cat("lambda.1se kept:", nrow(selected_1se) - 1, "predictors\n")


# Quick comparison of how many variables each kept
cat("lambda.min kept:", nrow(selected_min) - 1, "predictors\n")
cat("lambda.1se kept:", nrow(selected_1se) - 1, "predictors\n")

# Create the full model matrix again
x_full = model.matrix(log(SalePrice) ~ . - 1, data = data)
y = log(data$SalePrice[complete.cases(data)])

# Keep only the columns that LASSO 1se selected
selected_vars = setdiff(rownames(selected_1se), "(Intercept)")
x_1se = x_full[, selected_vars, drop = FALSE]

# Refit OLS on the selected columns
ols_1se = lm(y ~ x_1se)

summary(ols_1se)

# clean up ugly variable names
# Get the selected variable names from LASSO
selected_vars = setdiff(rownames(selected_1se), "(Intercept)")

# Create a clean model matrix with only those columns
x_clean = x_full[, selected_vars]
colnames(x_clean) = selected_vars   # keep the original glmnet names but drop the x_1se prefix later if desired

# Refit with cleaner call
ols_1se = lm(y ~ ., data = as.data.frame(x_clean))

summary(ols_1se)

## fixing last straggler ugly variables
# Clean the problematic names
colnames(x_clean) = gsub("`", "", colnames(x_clean))
colnames(x_clean) = gsub("Year_Remod/Add", "Year_Remod_Add", colnames(x_clean))
colnames(x_clean) = gsub("1st_Flr_SF", "First_Flr_SF", colnames(x_clean))
colnames(x_clean) = gsub("MS_ZoningC \\(all\\)", "MS_Zoning_C_all", colnames(x_clean))

# Refit one more time with the cleaned names
ols_1se = lm(y ~ ., data = as.data.frame(x_clean))
summary(ols_1se)

# Start with the 39-variable OLS model
# Backward stepwise using AIC (common and reproducible)
ols_small = step(ols_1se, direction = "backward", trace = 0)

# See what it kept
summary(ols_small)

# How many predictors remain?
length(coef(ols_small)) - 1
# only drops 3 variables

# Trying BIC
ols_small = step(ols_1se, direction = "backward", k = log(nrow(x_clean)), trace = 0)

summary(ols_small)
length(coef(ols_small)) - 1

###
## comparing the 3 models
###
# Make sure we have all three models
# ols_1se  = 39-variable model (LASSO 1se + OLS)
# ols_aic  = AIC stepwise version
# ols_small = BIC stepwise version (already created)

# Recreate the AIC version for completeness
ols_aic = step(ols_1se, direction = "backward", trace = 0)

# Side-by-side comparison
comparison = data.frame(
  Model = c("LASSO 1se + OLS", "AIC stepwise", "BIC stepwise"),
  Predictors = c(
    length(coef(ols_1se)) - 1,
    length(coef(ols_aic)) - 1,
    length(coef(ols_small)) - 1
  ),
  Adj_R2 = c(
    summary(ols_1se)$adj.r.squared,
    summary(ols_aic)$adj.r.squared,
    summary(ols_small)$adj.r.squared
  ),
  Residual_SE = c(
    summary(ols_1se)$sigma,
    summary(ols_aic)$sigma,
    summary(ols_small)$sigma
  )
)

print(comparison)



###
# Model diagnostics
###

# List of models
models = list(
  "LASSO 1se (39)" = ols_1se,
  "AIC (36)" = ols_aic,
  "BIC (30)" = ols_small
)

# Function to create diagnostic data
get_diag = function(model) {
  data.frame(
    obs = rownames(model.frame(model)),
    fitted = fitted(model),
    residuals = residuals(model),
    std_resid = rstandard(model),
    leverage = hatvalues(model)
  )
}

# Generate diagnostic plots for each model
for (name in names(models)) {
  diag_df = get_diag(models[[name]])
  
  # Residuals vs Fitted
  p1 = ggplot(diag_df, aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.5, color = brand_colors["blue"]) +
    geom_hline(yintercept = 0, linetype = "dashed", color = brand_colors["plum"]) +
    geom_smooth(se = FALSE, color = brand_colors["brick"]) +
    portfolio_theme +
    labs(title = paste("Residuals vs Fitted -", name),
         x = "Fitted values", y = "Residuals")
  print(p1)
  
  # Normal Q-Q
  p2 = ggplot(diag_df, aes(sample = std_resid)) +
    stat_qq(alpha = 0.5, color = brand_colors["blue"]) +
    stat_qq_line(color = brand_colors["plum"]) +
    portfolio_theme +
    labs(title = paste("Normal Q-Q -", name),
         x = "Theoretical quantiles", y = "Standardized residuals")
  print(p2)
}

## using patchwork to make 2x2 ggplots
# Generate 2x2 diagnostic grids for each model
for (name in names(models)) {
  
  diag_df = get_diag(models[[name]])
  
  # identify the single highest-leverage observation
  target_idx = which.max(diag_df$leverage)
  target_obs = diag_df$obs[target_idx]
  label_df = diag_df[target_idx, ]
  
  # data for QQ plot so the same observation can be labeled there too
  qq_df = diag_df[order(diag_df$std_resid), ]
  qq_df$theoretical = qnorm(ppoints(nrow(qq_df)))
  
  qq_label_df = qq_df[qq_df$obs == target_obs, ]
  
  # 1. Residuals vs Fitted
  p1 = ggplot(diag_df, aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.5, color = brand_colors["blue"]) +
    geom_hline(yintercept = 0, linetype = "dashed", color = brand_colors["plum"]) +
    geom_smooth(se = FALSE, color = brand_colors["brick"], linewidth = 0.8) +
    geom_text(
      data = label_df,
      aes(label = obs),
      nudge_y = 0.02,
      size = 4
    ) +
    portfolio_theme +
    labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")
  
  # 2. Normal Q-Q
  p2 = ggplot(qq_df, aes(x = theoretical, y = std_resid)) +
    geom_point(alpha = 0.5, color = brand_colors["blue"]) +
    geom_abline(slope = 1, intercept = 0, color = brand_colors["plum"]) +
    geom_text(
      data = qq_label_df,
      aes(x = theoretical, y = std_resid, label = obs),
      nudge_y = 0.2,
      size = 4
    ) +
    portfolio_theme +
    labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals")
  
  # 3. Scale-Location
  p3 = ggplot(diag_df, aes(x = fitted, y = sqrt(abs(std_resid)))) +
    geom_point(alpha = 0.5, color = brand_colors["blue"]) +
    geom_smooth(se = FALSE, color = brand_colors["brick"], linewidth = 0.8) +
    geom_text(
      data = transform(label_df, sl_y = sqrt(abs(std_resid))),
      aes(x = fitted, y = sl_y, label = obs),
      nudge_y = 0.05,
      size = 4
    ) +
    portfolio_theme +
    labs(title = "Scale-Location", x = "Fitted values", y = "√|Std. Residuals|")
  
  # 4. Residuals vs Leverage
  p4 = ggplot(diag_df, aes(x = leverage, y = std_resid)) +
    geom_point(alpha = 0.5, color = brand_colors["blue"]) +
    geom_hline(yintercept = 0, linetype = "dashed", color = brand_colors["plum"]) +
    geom_text(
      data = label_df,
      aes(label = obs),
      nudge_y = 0.25,
      hjust = 1,
      size = 4
    ) +
    portfolio_theme +
    labs(title = "Residuals vs Leverage", x = "Leverage", y = "Standardized Residuals")
  
  # 2x2 grid for this model
  grid = (p1 + p2) / (p3 + p4) +
    plot_annotation(title = paste("Diagnostics -", name))
  
  print(grid)
}


###
# Results and visualizations
###

# Standardized coefficients for the BIC model
library(lm.beta)   # or use the version below if you don’t want another package

# Base R version (no extra package)
std_coefs = coef(final_model)[-1] * sapply(as.data.frame(x_clean)[names(coef(final_model))[-1]], sd) / sd(y)

# Rank them
std_coefs_sorted = sort(abs(std_coefs), decreasing = TRUE)
std_coefs_sorted