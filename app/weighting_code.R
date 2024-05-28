library(survey)
library(tidyverse)

calc_weight <- function(
    df, 
    sample_list, 
    pop_list,
    lower_limit,
    upper_limit){
  
  # Create survey design
  svy.unweighted <- svydesign(ids = ~1, data = df)
  
  # Raking
  svy.rake <- rake(
    design = svy.unweighted,
    sample.margins = sample_list,
    population.margins = pop_list,
    control = list(
      maxit = 100, 
      epsilon = 1, 
      verbose = FALSE
      )
  )
  
  # Append untrimmed weight to the data frame
  df$untrimmed_weight <- weights(svy.rake)
  
  # Trim weights
  svy.rake.trim <- trimWeights(
    svy.rake, 
    lower = lower_limit, 
    upper = upper_limit, 
    strict = TRUE
    )
  
  # Append trimmed weights
  df$trimmed_weight <- weights(svy.rake.trim)
  
  return(df)
}


