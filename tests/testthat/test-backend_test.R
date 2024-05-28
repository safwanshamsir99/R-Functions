library(testthat)
library(here)
source(here("app", "weighting_code.R"))

test_that("test calc_weight", {
  df <- read.csv("test_weighting.csv", check.names = FALSE)
  sample_list <- list(~Gender, ~IncomeGroup)
  gender.dist <- data.frame(
    
    Gender = c("Male", "Female"),
    freq = nrow(df) * c(0.4985, 0.5015)
    
  )
  incomegroup.dist <- data.frame(
    
    IncomeGroup = c("B40", "M40", "T20"),
    freq = nrow(df) * c(0.40, 0.40, 0.20)
    
  )
  pop_list <- list(gender.dist, incomegroup.dist)
  lower_limit <- 0.3
  upper_limit <- 3
  
  result <- calc_weight(df, sample_list, pop_list, lower_limit, upper_limit)

  expect_is(result, "data.frame")

  expect_equal(colnames(result), c(
    "1. [LIKERT] Opinions" ,
    "2. What is your dream job field?",
    "Gender",
    "IncomeGroup", 
    "untrimmed_weight", 
    "trimmed_weight"))

  expect_true(all(result$trimmed_weight >= lower_limit))
  expect_true(all(result$trimmed_weight <= upper_limit))
})