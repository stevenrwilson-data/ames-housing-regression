# ============================================================
# TABLE OF CONTENTS
# ============================================================
#
# A. Load and prepare train/test data
#    1. Load the already-clean train and test sets
#    2. Separate the response variable
#    3. Verify data against the R reference values
#    4. Temporarily combine predictors for consistent encoding
#    5. Split back into the original train and test sets
#    6. Verify alignment
#
# B. LASSO variable selection
#    1. Standardize predictors using the training set only
#    2. Run 10-fold cross-validated LASSO
#    3. Apply the 1-SE rule
#    4. Fit LASSO at the 1-SE alpha
#    5. Identify selected predictors
#
# C. OLS refit on LASSO-selected predictors
#    1. Remove the exact duplicate predictor
#    2. Build the OLS design matrix
#    3. Check matrix rank before fitting
#    4. Fit ordinary least squares
#    5. Report basic model statistics
#
# D. Backward AIC and BIC model selection
#    1. Define the backward-selection function
#    2. Run backward AIC selection
#    3. Run backward BIC selection
#    4. Compare the selected models
#
# E. Test-set model performance
#    1. Define a function for test-set predictions
#    2. Evaluate all three models
#    3. Report out-of-sample performance
#
# F. Paired bootstrap comparison of test performance
#    1. Generate dollar-scale predictions for each model
#    2. Define metric calculations
#    3. Run paired bootstrap resampling
#    4. Calculate 95% bootstrap confidence intervals
#    5. Report paired performance differences
#
# G. Final model selection
#    1. Designate the BIC model as the final model
#
# ============================================================

# ============================================================
# A. Load and prepare train/test data
# ============================================================

import numpy as np
import pandas as pd
import statsmodels.api as sm
from sklearn.linear_model import Lasso, LassoCV
from sklearn.model_selection import KFold
from sklearn.preprocessing import StandardScaler



# ------------------------------------------------------------
# 1. Load the already-clean train and test sets
# ------------------------------------------------------------

train = pd.read_csv("train.csv", keep_default_na=False)
test = pd.read_csv("test.csv", keep_default_na=False)


# ------------------------------------------------------------
# 2. Separate the response variable
# ------------------------------------------------------------

y_train = np.log(train["SalePrice"])
y_test = np.log(test["SalePrice"])

# ------------------------------------------------------------
# 3. Verify data against the R reference values
# ------------------------------------------------------------

# These values were recorded from the final R train/test datasets.
# They verify that Python is using the exact same observations.
expected_train_shape = (2182, 78)
expected_test_shape = (546, 78)

expected_train_log_sum = 26283.13468112
expected_test_log_sum = 6561.66730467

if train.shape != expected_train_shape:
    raise ValueError(
        f"Unexpected train shape: {train.shape}; "
        f"expected {expected_train_shape}"
    )

if test.shape != expected_test_shape:
    raise ValueError(
        f"Unexpected test shape: {test.shape}; "
        f"expected {expected_test_shape}"
    )

if not np.isclose(
    y_train.sum(),
    expected_train_log_sum,
    rtol=0.0,
    atol=1e-8
):
    raise ValueError(
        "Training log(SalePrice) checksum does not match R."
    )

if not np.isclose(
    y_test.sum(),
    expected_test_log_sum,
    rtol=0.0,
    atol=1e-8
):
    raise ValueError(
        "Test log(SalePrice) checksum does not match R."
    )

print("\nDATA VERIFICATION")
print("Train/test dimensions match R.")
print("log(SalePrice) checksums match R.")

X_train_raw = train.drop(columns="SalePrice")
X_test_raw = test.drop(columns="SalePrice")
# MS_SubClass contains category codes, not a continuous numeric measurement.
# R retained it as a factor; CSV export loses that factor metadata, so restore
# its categorical meaning explicitly before one-hot encoding.
X_train_raw["MS_SubClass"] = X_train_raw["MS_SubClass"].astype(str)
X_test_raw["MS_SubClass"] = X_test_raw["MS_SubClass"].astype(str)

# ------------------------------------------------------------
# 4. Temporarily combine predictors for consistent encoding
# ------------------------------------------------------------

combined = pd.concat(
    [X_train_raw, X_test_raw],
    keys=["train", "test"]
)

combined_encoded = pd.get_dummies(
    combined,
    drop_first=True,
    dtype=float
)


# ------------------------------------------------------------
# 5. Split back into the original train and test sets
# ------------------------------------------------------------

X_train = combined_encoded.loc["train"].copy()
X_test = combined_encoded.loc["test"].copy()


# ------------------------------------------------------------
# 6. Verify alignment
# ------------------------------------------------------------

print("Train design matrix:", X_train.shape)
print("Test design matrix: ", X_test.shape)
print("Columns identical:   ", X_train.columns.equals(X_test.columns))



# ============================================================
# B. LASSO variable selection
# ============================================================

# ------------------------------------------------------------
# 1. Standardize predictors using the training set only
# ------------------------------------------------------------

scaler = StandardScaler()

# Only the training matrix is scaled. Scaling exists so LASSO penalizes
# predictors on comparable scales; the OLS refit and all predictions use
# the original unscaled values.
X_train_scaled = scaler.fit_transform(X_train)


# ------------------------------------------------------------
# 2. Run 10-fold cross-validated LASSO
# ------------------------------------------------------------

# Shuffle the training rows before assigning folds so the folds
# behave more like the randomized folds used by cv.glmnet in R.
cv = KFold(
    n_splits=10,
    shuffle=True,
    random_state=42
)

lasso_cv = LassoCV(
    cv=cv,
    max_iter=100000,
    n_jobs=-1
)

lasso_cv.fit(X_train_scaled, y_train)


# ------------------------------------------------------------
# 3. Apply the 1-SE rule
# ------------------------------------------------------------

# Mean validation error for each tested alpha.
mean_mse = lasso_cv.mse_path_.mean(axis=1)

# Standard error of validation error for each tested alpha.
se_mse = (
    lasso_cv.mse_path_.std(axis=1, ddof=1)
    / np.sqrt(lasso_cv.mse_path_.shape[1])
)

# Locate the alpha with minimum cross-validation error.
min_index = np.argmin(mean_mse)

# The 1-SE threshold is one standard error above the minimum error.
one_se_threshold = mean_mse[min_index] + se_mse[min_index]

# LassoCV stores alphas from largest penalty to smallest penalty.
# The 1-SE rule chooses the largest alpha whose error is still
# within one standard error of the minimum.
eligible = np.where(mean_mse <= one_se_threshold)[0]
one_se_index = eligible[0]
alpha_1se = lasso_cv.alphas_[one_se_index]


# ------------------------------------------------------------
# 4. Fit LASSO at the 1-SE alpha
# ------------------------------------------------------------

lasso_1se = Lasso(
    alpha=alpha_1se,
    max_iter=100000
)

lasso_1se.fit(X_train_scaled, y_train)


# ------------------------------------------------------------
# 5. Identify selected predictors
# ------------------------------------------------------------

selected_mask = lasso_1se.coef_ != 0
selected_predictors = X_train.columns[selected_mask]

print("\nLASSO RESULTS")
print(f"CV-minimum alpha: {lasso_cv.alpha_:.8f}")
print(f"1-SE alpha:       {alpha_1se:.8f}")
print(f"Predictors kept:  {len(selected_predictors)}")

print("\nSelected predictors")
for predictor in selected_predictors:
    print(predictor)

# ============================================================
# C. OLS refit on LASSO-selected predictors
# ============================================================

# ------------------------------------------------------------
# 1. Remove the exact duplicate predictor
# ------------------------------------------------------------

# Exterior_1st_PreCast and Exterior_2nd_PreCast are identical
# across the training data. Keeping both makes the OLS design
# matrix rank-deficient, so retain only Exterior_1st_PreCast.
ols_predictors = selected_predictors.drop("Exterior_2nd_PreCast")


# ------------------------------------------------------------
# 2. Build the OLS design matrix
# ------------------------------------------------------------

# Use the original, unscaled predictor values for OLS.
X_ols = X_train.loc[:, ols_predictors].copy()

# Add an intercept explicitly for statsmodels.
X_ols = sm.add_constant(X_ols, has_constant="add")


# ------------------------------------------------------------
# 3. Check matrix rank before fitting
# ------------------------------------------------------------

matrix_rank = np.linalg.matrix_rank(X_ols.to_numpy())
matrix_columns = X_ols.shape[1]

print("\nOLS DESIGN MATRIX")
print(f"Columns including intercept: {matrix_columns}")
print(f"Matrix rank:                 {matrix_rank}")
print(f"Full rank:                   {matrix_rank == matrix_columns}")


# ------------------------------------------------------------
# 4. Fit ordinary least squares
# ------------------------------------------------------------

ols_1se = sm.OLS(y_train, X_ols).fit()


# ------------------------------------------------------------
# 5. Report basic model statistics
# ------------------------------------------------------------

print("\nOLS REFIT — LASSO 1-SE VARIABLES")
print(f"Predictors:       {len(ols_predictors)}")
print(f"Adjusted R²:      {ols_1se.rsquared_adj:.5f}")
print(f"Residual SE:      {np.sqrt(ols_1se.mse_resid):.5f}")
print(f"AIC:              {ols_1se.aic:.2f}")
print(f"BIC:              {ols_1se.bic:.2f}")

# ============================================================
# D. Backward AIC and BIC model selection
# ============================================================

# ------------------------------------------------------------
# 1. Define the backward-selection function
# ------------------------------------------------------------

def backward_selection(X, y, criterion):
    """
    Repeatedly remove one predictor at a time when doing so
    improves the selected information criterion.

    Lower AIC or BIC is better.
    """

    criterion = criterion.upper()

    if criterion not in {"AIC", "BIC"}:
        raise ValueError("criterion must be 'AIC' or 'BIC'")

    remaining = list(X.columns)

    # Helper function for fitting OLS with an intercept.
    def fit_model(columns):
        X_model = sm.add_constant(
            X.loc[:, columns],
            has_constant="add"
        )

        return sm.OLS(y, X_model).fit()

    # Begin with the complete LASSO-selected OLS model.
    current_model = fit_model(remaining)

    if criterion == "AIC":
        current_score = current_model.aic
    else:
        current_score = current_model.bic

    # At each step, try dropping every remaining predictor.
    while len(remaining) > 1:

        candidate_results = []

        for predictor in remaining:

            trial_predictors = [
                column for column in remaining
                if column != predictor
            ]

            trial_model = fit_model(trial_predictors)

            if criterion == "AIC":
                trial_score = trial_model.aic
            else:
                trial_score = trial_model.bic

            candidate_results.append(
                (trial_score, predictor, trial_model)
            )

        # Find the single deletion producing the lowest score.
        best_score, predictor_to_drop, best_model = min(
            candidate_results,
            key=lambda result: result[0]
        )

        # Keep the deletion only if it improves the criterion.
        if best_score < current_score:

            remaining.remove(predictor_to_drop)

            current_model = best_model
            current_score = best_score

        else:
            break

    return current_model, remaining


# ------------------------------------------------------------
# 2. Run backward AIC selection
# ------------------------------------------------------------

X_stepwise = X_train.loc[:, ols_predictors].copy()

ols_aic, aic_predictors = backward_selection(
    X_stepwise,
    y_train,
    criterion="AIC"
)


# ------------------------------------------------------------
# 3. Run backward BIC selection
# ------------------------------------------------------------

ols_bic, bic_predictors = backward_selection(
    X_stepwise,
    y_train,
    criterion="BIC"
)


# ------------------------------------------------------------
# 4. Compare the selected models
# ------------------------------------------------------------

print("\nBACKWARD AIC/BIC RESULTS")

print("\nLASSO 1-SE + OLS")
print(f"Predictors:       {len(ols_predictors)}")
print(f"Adjusted R²:      {ols_1se.rsquared_adj:.5f}")
print(f"Residual SE:      {np.sqrt(ols_1se.mse_resid):.5f}")

print("\nAIC model")
print(f"Predictors:       {len(aic_predictors)}")
print(f"Adjusted R²:      {ols_aic.rsquared_adj:.5f}")
print(f"Residual SE:      {np.sqrt(ols_aic.mse_resid):.5f}")
print(f"AIC:              {ols_aic.aic:.2f}")
print(f"BIC:              {ols_aic.bic:.2f}")

print("\nBIC model")
print(f"Predictors:       {len(bic_predictors)}")
print(f"Adjusted R²:      {ols_bic.rsquared_adj:.5f}")
print(f"Residual SE:      {np.sqrt(ols_bic.mse_resid):.5f}")
print(f"AIC:              {ols_bic.aic:.2f}")
print(f"BIC:              {ols_bic.bic:.2f}")


# ============================================================
# E. Test-set model performance
# ============================================================

# ------------------------------------------------------------
# 1. Define a function for test-set predictions
# ------------------------------------------------------------

def evaluate_model(model, predictors, X_test, y_test_log):
    """
    Evaluate an OLS model on the untouched test set.

    Predictions are made on the log-price scale and then
    transformed back to dollars for interpretation.
    """

    # Build the test design matrix using exactly the predictors
    # retained by the corresponding training model.
    X_test_model = X_test.loc[:, predictors].copy()

    # Add the intercept expected by statsmodels.
    X_test_model = sm.add_constant(
        X_test_model,
        has_constant="add"
    )

    # Predict log(SalePrice).
    predicted_log_price = model.predict(X_test_model)

    # Convert both actual and predicted values back to dollars.
    actual_price = np.exp(y_test_log)
    predicted_price = np.exp(predicted_log_price)

    # Root mean squared error penalizes large prediction errors.
    rmse = np.sqrt(
        np.mean((actual_price - predicted_price) ** 2)
    )

    # Mean absolute error gives the average dollar error.
    mae = np.mean(
        np.abs(actual_price - predicted_price)
    )

    # Mean absolute percentage error expresses error relative
    # to the actual selling price.
    mape = np.mean(
        np.abs(
            (actual_price - predicted_price)
            / actual_price
        )
    ) * 100

    return rmse, mae, mape


# ------------------------------------------------------------
# 2. Evaluate all three models
# ------------------------------------------------------------

lasso_rmse, lasso_mae, lasso_mape = evaluate_model(
    ols_1se,
    ols_predictors,
    X_test,
    y_test
)

aic_rmse, aic_mae, aic_mape = evaluate_model(
    ols_aic,
    aic_predictors,
    X_test,
    y_test
)

bic_rmse, bic_mae, bic_mape = evaluate_model(
    ols_bic,
    bic_predictors,
    X_test,
    y_test
)


# ------------------------------------------------------------
# 3. Report out-of-sample performance
# ------------------------------------------------------------

print("\nTEST-SET PERFORMANCE")

print("\nLASSO 1-SE + OLS")
print(f"Predictors: {len(ols_predictors)}")
print(f"RMSE:       ${lasso_rmse:,.0f}")
print(f"MAE:        ${lasso_mae:,.0f}")
print(f"MAPE:       {lasso_mape:.2f}%")

print("\nAIC model")
print(f"Predictors: {len(aic_predictors)}")
print(f"RMSE:       ${aic_rmse:,.0f}")
print(f"MAE:        ${aic_mae:,.0f}")
print(f"MAPE:       {aic_mape:.2f}%")

print("\nBIC model")
print(f"Predictors: {len(bic_predictors)}")
print(f"RMSE:       ${bic_rmse:,.0f}")
print(f"MAE:        ${bic_mae:,.0f}")
print(f"MAPE:       {bic_mape:.2f}%")

# ============================================================
# F. Paired bootstrap comparison of test performance
# ============================================================

# ------------------------------------------------------------
# 1. Generate dollar-scale predictions for each model
# ------------------------------------------------------------

def predict_prices(model, predictors, X_test):
    """
    Generate SalePrice predictions in dollars using the exact
    predictors retained by a fitted statsmodels OLS model.
    """

    X_model = X_test.loc[:, predictors].copy()

    X_model = sm.add_constant(
        X_model,
        has_constant="add"
    )

    predicted_log_price = model.predict(X_model)

    return np.exp(predicted_log_price)


actual_price = np.exp(y_test.to_numpy())

pred_lasso = predict_prices(
    ols_1se,
    ols_predictors,
    X_test
)

pred_aic = predict_prices(
    ols_aic,
    aic_predictors,
    X_test
)

pred_bic = predict_prices(
    ols_bic,
    bic_predictors,
    X_test
)


# ------------------------------------------------------------
# 2. Define metric calculations
# ------------------------------------------------------------

def calculate_metrics(actual, predicted):
    """
    Calculate RMSE, MAE, and MAPE on the dollar scale.
    """

    errors = actual - predicted

    rmse = np.sqrt(np.mean(errors ** 2))
    mae = np.mean(np.abs(errors))
    mape = np.mean(np.abs(errors / actual)) * 100

    return rmse, mae, mape


# ------------------------------------------------------------
# 3. Run paired bootstrap resampling
# ------------------------------------------------------------

# Each bootstrap sample resamples house indices rather than
# resampling each model separately. This preserves the paired
# comparison because every model is evaluated on the same houses.
n_bootstrap = 10000

rng = np.random.default_rng(42)

# Store AIC - LASSO and BIC - LASSO differences.
aic_minus_lasso = np.empty((n_bootstrap, 3))
bic_minus_lasso = np.empty((n_bootstrap, 3))

n_test = len(actual_price)

for i in range(n_bootstrap):

    # Sample 546 test houses with replacement.
    sample_index = rng.integers(
        0,
        n_test,
        size=n_test
    )

    actual_sample = actual_price[sample_index]

    lasso_metrics = calculate_metrics(
        actual_sample,
        pred_lasso[sample_index]
    )

    aic_metrics = calculate_metrics(
        actual_sample,
        pred_aic[sample_index]
    )

    bic_metrics = calculate_metrics(
        actual_sample,
        pred_bic[sample_index]
    )

    aic_minus_lasso[i] = (
        np.array(aic_metrics)
        - np.array(lasso_metrics)
    )

    bic_minus_lasso[i] = (
        np.array(bic_metrics)
        - np.array(lasso_metrics)
    )


# ------------------------------------------------------------
# 4. Calculate 95% bootstrap confidence intervals
# ------------------------------------------------------------

def bootstrap_interval(values):
    """
    Return the central 95% bootstrap percentile interval.
    """

    lower = np.percentile(values, 2.5)
    upper = np.percentile(values, 97.5)

    return lower, upper


metric_names = ["RMSE", "MAE", "MAPE"]


# ------------------------------------------------------------
# 5. Report paired performance differences
# ------------------------------------------------------------

print("\nPAIRED BOOTSTRAP MODEL COMPARISON")
print(f"Bootstrap samples: {n_bootstrap:,}")

print("\nAIC minus LASSO")
for j, metric in enumerate(metric_names):

    lower, upper = bootstrap_interval(
        aic_minus_lasso[:, j]
    )

    observed = [
        aic_rmse - lasso_rmse,
        aic_mae - lasso_mae,
        aic_mape - lasso_mape
    ][j]

    if metric in {"RMSE", "MAE"}:
        print(
            f"{metric}: "
            f"${observed:,.0f} "
            f"(95% CI: ${lower:,.0f} to ${upper:,.0f})"
        )
    else:
        print(
            f"{metric}: "
            f"{observed:.3f} percentage points "
            f"(95% CI: {lower:.3f} to {upper:.3f})"
        )


print("\nBIC minus LASSO")
for j, metric in enumerate(metric_names):

    lower, upper = bootstrap_interval(
        bic_minus_lasso[:, j]
    )

    observed = [
        bic_rmse - lasso_rmse,
        bic_mae - lasso_mae,
        bic_mape - lasso_mape
    ][j]

    if metric in {"RMSE", "MAE"}:
        print(
            f"{metric}: "
            f"${observed:,.0f} "
            f"(95% CI: ${lower:,.0f} to ${upper:,.0f})"
        )
    else:
        print(
            f"{metric}: "
            f"{observed:.3f} percentage points "
            f"(95% CI: {lower:.3f} to {upper:.3f})"
        )

# ============================================================
# G. Final model selection
# ============================================================

# ------------------------------------------------------------
# 1. Designate the BIC model as the final model
# ------------------------------------------------------------

# The BIC model retains substantially fewer predictors while the
# paired bootstrap shows no statistically detectable degradation
# in test-set RMSE, MAE, or MAPE relative to the larger model.
final_model = ols_bic
final_predictors = bic_predictors

print("\nFINAL MODEL")
print("Selection criterion: BIC")
print(f"Predictors retained: {len(final_predictors)}")

print("\nFinal predictors")
for predictor in final_predictors:
    print(predictor)