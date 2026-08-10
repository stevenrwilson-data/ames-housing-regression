# Project 3 - Housing dataset for MLR analysis

# Load packages
library(ggplot2)
library(dplyr)
library(readr)
library(readxl)
library(broom)
library(car)
library(corrplot)
library(GGally)
library(lmtest)
library(performance)

# Brand colors
brand_colors <- c(
  field    = "#041F2C",
  blue     = "#08415C",
  plum     = "#511730",
  brick    = "#8E443D",
  salmon   = "#F7A399",
  sand     = "#E0D68A",
  plate    = "#F4F0E6",
  plum_ink = "#3A0F21"
)

# Portfolio plot theme
portfolio_theme <- theme_minimal() +
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

setwd("~/project_3_housing")
data <- read_excel("AmesHousing.xls")
# Drop pure identifier columns ~ listed in docs, not useful for MLR
data$Order <- NULL
data$PID <- NULL
names(data)

# Replace every space in column names with an underscore
names(data) <- gsub(" ", "_", names(data))
names(data)

# Get character columns
char_cols <- names(data)[sapply(data, is.character)]

# automating the first set of ordinals
# Detect columns that contain quality codes
qual_levels <- c("Ex", "Gd", "TA", "Fa", "Po")
qual_cols <- char_cols[sapply(data[char_cols], function(x) {
  any(as.character(x) %in% qual_levels)
})]

# Inspect what was detected
lapply(data[qual_cols], function(x) sort(unique(as.character(x))))

# Handle Bsmt_Exposure first (its own scale)
exposure_map <- c(
  "No" = 1,
  "Mn" = 2,
  "Av" = 3,
  "Gd" = 4,
  "NA" = 0
)
data$Bsmt_Exposure <- unname(exposure_map[as.character(data$Bsmt_Exposure)])

# Check Bsmt_Exposure
table(data$Bsmt_Exposure, useNA = "ifany")

# Handle the remaining quality columns
qual_map <- c(
  "Po" = 1,
  "Fa" = 2,
  "TA" = 3,
  "Gd" = 4,
  "Ex" = 5,
  "NA" = 0
)

other_qual_cols <- setdiff(qual_cols, "Bsmt_Exposure")

data[other_qual_cols] <- lapply(data[other_qual_cols], function(x) {
  unname(qual_map[as.character(x)])
})

# Final check of all quality columns
lapply(data[qual_cols], table, useNA = "ifany")

# Remaining Char columns
# Character columns that have not been converted yet
remaining_char <- setdiff(char_cols, qual_cols)

# Inspect their unique values
lapply(data[remaining_char], function(x) sort(unique(as.character(x))))

# Import data

# Inspect data

# Clean data

# Exploratory data analysis

# Multiple linear regression

# Model diagnostics

# Results and visualizations

