library(readxl)

# Load streamlit dataframe into R dataframe
# df: Streamlit dataframe
load <- function(df) {
  # Get the file extension
  df_name <- tools::file_ext(df)
  
  # Check file type and read them accordingly
  if (df_name == 'csv') {
    df <- read.csv(df, na.strings = "", stringsAsFactors = FALSE)
  } else {
    df <- readxl::read_excel(df, na = "")
  }
  
  return(df)
}

# Helper function to auto-detect demography columns
# df: Whole data frame
demography <- function(df) {
  default_demo <- c('age', 'gender', 'eth', 'income', 'urban')
  data_list <- colnames(df)
  pattern <- paste(default_demo, collapse = '|')
  default_demo <- data_list[grepl(pattern, data_list, ignore.case = TRUE)]
  return(default_demo)
}

# Helper function to sort the list of the unique value in the selected strata column.
# demo: Column name of the strata you have selected [str]
# df: Whole data frame
sorter <- function(demo, df) {
  unique_values <- unique(df[[demo]])
  
  if (grepl("age", demo, ignore.case = TRUE)) {
    return(sort(unique_values))
    
  } else if (grepl("gender", demo, ignore.case = TRUE)) {
    return(sort(unique_values, decreasing = TRUE))
    
  } else if (grepl("eth", demo, ignore.case = TRUE)) {
    return(sort(unique_values, decreasing = TRUE))
    
  } else if (grepl("income", demo, ignore.case = TRUE)) {
    return(sort(unique_values))
    
  } else if (grepl("urban", demo, ignore.case = TRUE)) {
    return(sort(unique_values, decreasing = TRUE))
  } else {
    return(sort(unique_values))
  }
}






