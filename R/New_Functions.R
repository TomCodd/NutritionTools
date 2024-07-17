# SOP_std_creator ----
# ¬ Taken from FAO-Fisheries (paper branch)/functions/summary_table_functions.R ----


#Dependency:

check_columns <- function(dataset, columns) {
  #' Check presence of columns required for a calculation in a given dataset.
  #' Column names are case sensitive and error is thrown if not found.
  #' @param dataset :Required (FCT dataset to be checked)
  #' @param columns (Columns to be checked)
  #' @return dataset
  #' @examples
  for (column in columns) {
    if (column %in% names(dataset)) {

    } else {
      stop(
        paste0(
          "Error: variable ",
          column,
          " not found, halting execution. Please fix your input data and try again"
        )
      )
    }
  }
}


SOP_std_creator <- function(dataset) {
  #' @description Calculates SOPg_standardised = (WATERg + PROCNTg + FAT_g_standardised + CHOAVLg + #` FIBTGg_std + ALCg +ASHg).
  #' Column names are case sensitive and error is thrown if not found.
  #' @param dataset :Required (FCT dataset to be checked)
  #' @param SOPg_standardised Sum of Proximate in g per 100g EP as reported in the original FCT
  #' @param WATERg Water/ moisture content in g per 100g of EP
  #' @param PROCNTg Protein in g per 100g of EP, as reported in the original FCT and assumed to be calculated from nitrogen (NTg) content
  #' @param FAT_g_standardised fat content unknown method of calculation in g per 100g of EP
  #' @param CHOAVLDFg_standardised Available carbohydrates calculated by difference in g per 100g of EP
  #' @param FIBTGg_std Fibre content from combined Tagnames, with preference of Total dietary fibre by AOAC Prosky method, expressed in g per 100g of EP
  #' @param ALCg Alcohol in g per 100g
  #' @param ASHg Ashes in g per 100g of EP
  #' @return Original FCT dataset with SOPg_standardised column added
  #' @examples
  # Check presence of required columns
  columns <- c(
    "WATERg",
    "PROCNTg",
    "FAT_g_standardised",
    "CHOAVLDFg_standardised",
    "FIBTGg_std",
    "ALCg",
    "ASHg"
  )
  check_columns(dataset = dataset, columns = columns)

  tryCatch(
    dataset %>%
      as_tibble() %>%
      mutate_at(.vars = columns, .funs = as.numeric) %>%
      # ! Create a temp row with the count of NAs in the required columns
      mutate(temp = rowSums(is.na(
        dataset %>%
          select(all_of(columns))
      ))) %>%
      # Rowwise allows for per row evaluations.
      rowwise() %>%
      # ! If all the rows are NA then output is NA.
      # ! Else do the calculation and omit NAs.
      mutate(SOPg_standardised = ifelse(
        temp == length(columns),
        NA,
        sum(
          WATERg,
          PROCNTg,
          FAT_g_standardised,
          CHOAVLDFg_standardised,
          FIBTGg_std,
          ALCg,
          ASHg,
          na.rm = TRUE
        )
      )) %>%
      # ! remove the temp column
      select(-temp) %>%
      ungroup(),
    error = function(e) {
      print(
        paste0(
          "Error : Required columns i.e. ",
          columns,
          " should be numeric. The SOPg_standardised will not be calculated"
        )
      )
    }
  )
}




SOP_std_creator_2 <- function(dataset) {
  # Check presence of required columns
  columns <- c(
    "WATERg",
    "PROCNTg",
    "FAT_g_standardised",
    # Change FAT_g to FAT_g_standardised
    "CHOAVLg",
    "FIBTGg",
    "ALCg",
    "ASHg_std" # change ASHg to ASHg_std
  )

  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(dataset))
  stopifnot("The WATERg is not a column name in df - please input a string that is a column name in df, e.g. 'column one'" = "WATERg" %in% colnames(dataset)) #Checks to see if the group_ID_col is in the list of column names for the df
  stopifnot("The PROCNTg is not a column name in df - please input a string that is a column name in df, e.g. 'column one'" = "PROCNTg" %in% colnames(dataset))
  stopifnot("The FAT_g_standardised is not a column name in df - please input a string that is a column name in df, e.g. 'column one'" = "FAT_g_standardised" %in% colnames(dataset))
  stopifnot("The CHOAVLg is not a column name in df - please input a string that is a column name in df, e.g. 'column one'" = "CHOAVLg" %in% colnames(dataset))
  stopifnot("The FIBTGg is not a column name in df - please input a string that is a column name in df, e.g. 'column one'" = "FIBTGg" %in% colnames(dataset))
  stopifnot("The ALCg is not a column name in df - please input a string that is a column name in df, e.g. 'column one'" = "ALCg" %in% colnames(dataset))
  stopifnot("The ASHg_std is not a column name in df - please input a string that is a column name in df, e.g. 'column one'" = "ASHg_std" %in% colnames(dataset))


  # Try the calculation
  tryCatch(
    dataset |>
      as_tibble() |>
      mutate_at(.vars = columns, .funs = as.numeric) |>
      # ! Create a temp row with the number of NAs across the required
      # column
      mutate(temp = rowSums(is.na(
        dataset |>
          select(all_of(columns))
      ))) |>
      rowwise() |>
      # ! Check if all the rows are NA then output NA else do the
      # calculation and omit NAs
      mutate(SOP_std = ifelse(
        temp == length(columns),
        NA,
        sum(
          WATERg,
          PROCNTg,
          FAT_g_standardised,
          CHOAVLg,
          FIBTGg,
          ALCg,
          ASHg_std,
          na.rm = TRUE
        )
      )) |>
      # ! remove the temp column
      select(-temp) |>
      ungroup(),
    error = function(e) {
      print(
        paste0(
          "Error : Required columns i.e. ",
          columns,
          " should be numeric. The SOP_std will not be calculated"
        )
      )
    }
  )
}



# ¬ New Version ----


SOP_std_creator_New <- function(df,
                                WATERg_column = "WATERg",
                                PROCNTg_column = "PROCNTg",
                                FAT_g_standardised_column = "FAT_g_standardised",
                                CHOAVLDFg_standardised_column = "CHOAVLDFg_standardised",
                                FIBTGg_standardised_column = "FIBTGg_standardised",
                                ALCg_column = "ALCg",
                                ASHg_column = "ASHg",
                                comment = T,
                                comment_col = "comments") {

  #' @title Sum of Proximate Calculator
  #' @description Calculates SOPg_standardised = (WATERg + PROCNTg + FAT_g_standardised + CHOAVLDFg_standardised + FIBTGg_standardised_column + ALCg +ASHg).
  #' Column names are case sensitive and an error is returned if not found.
  #' @param df Required - the data.frame the data is currently stored in.
  #' @param WATERg_column Required - default: \code{'WATERg'} - The name of the column containing Water/moisture content in grams per 100g of Edible Portion (EP).
  #' @param PROCNTg_column Required - default: \code{'PROCNTg'} - Protein in grams per 100g of Edible Portion (EP), as reported in the original FCT and assumed to be calculated from nitrogen (NTg) content.
  #' @param FAT_g_standardised_column Required - default: \code{'FAT_g_standardised'} - Fat content, unknown method of calculation, in grams per 100g of Edible Portion (EP).
  #' @param CHOAVLDFg_standardised_column Required - default: \code{'CHOAVLDFg_standardised'} - Available carbohydrates calculated by difference, in grams per 100g of Edible Portion (EP).
  #' @param FIBTGg_standardised_column Required - default: \code{'FIBTGg_standardised'} - Fibre content from combined Tagnames, with preference of Total dietary fibre by AOAC Prosky method, expressed in grams per 100g of Edible Portion (EP).
  #' @param ALCg_column Required - default: \code{'ALCg'} - Alcohol in grams per 100g of Edible Portion (EP).
  #' @param ASHg_column Required - default: \code{'ASHg'} - Ashes in grams per 100g of Edible Portion (EP).
  #' @param comment Optional - default: \code{T} - \code{TRUE} or \code{FALSE}.If comment is set to \code{TRUE} (as it is by default), when the function is
  #'   run a comment describing the source of the \code{SOPg_standardised} column is added to the comment_col. If no comment_col is selected, and \code{comment
  #'   = T}, one is created, called \code{comments}.
  #' @param comment_col Optional - default: \code{'comments'} - A potential input
  #'   variable; the column which contains the metadata comments for the food item
  #'   in question. Not required if the comment parameter is set to \code{FALSE}.
  #' @return Original FCT dataset with a new SOPg_standardised column.
  #' @examples
  #'



  # Check presence of required columns

  # Input checks - goes through each input column name, and checks if its a column in the df. If it isn't, it prints an error message and stops.

  # This check makes sure the entered df is a data frame.
  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df))

  #This block of checks throws an error if the entry for the columns is not present in the df.
  stopifnot("The WATERg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column one'" = WATERg_column %in% colnames(df))
  stopifnot("The PROCNTg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column two'" = PROCNTg_column %in% colnames(df))
  stopifnot("The FAT_g_standardised is not a column name in df - please input a string that is a column name in df, e.g. 'column three'" = FAT_g_standardised_column %in% colnames(df))
  stopifnot("The CHOAVLDFg_standardised_column is not a column name in df - please input a string that is a column name in df, e.g. 'column four'" = CHOAVLDFg_standardised_column %in% colnames(df))
  stopifnot("The FIBTGg_std_column is not a column name in df - please input a string that is a column name in df, e.g. 'column five'" = FIBTGg_standardised_column %in% colnames(df))
  stopifnot("The ALCg_columnis not a column name in df - please input a string that is a column name in df, e.g. 'column six'" = ALCg_column %in% colnames(df))
  stopifnot("The ASHg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column seven'" = ASHg_column %in% colnames(df))

  #This block of checks makes sure the columns that are meant to be numeric are numeric.
  stopifnot("The WATERg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[WATERg_column]]))
  stopifnot("The PROCNTg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[PROCNTg_column]]))
  stopifnot("The FAT_g_standardised_column is not numeric. Please ensure it is numeric." = is.numeric(df[[FAT_g_standardised_column]]))
  stopifnot("The CHOAVLDFg_standardised_column is not numeric. Please ensure it is numeric." = is.numeric(df[[CHOAVLDFg_standardised_column]]))
  stopifnot("The FIBTGg_standardised_column is not numeric. Please ensure it is numeric." = is.numeric(df[[FIBTGg_standardised_column]]))
  stopifnot("The ALCg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[ALCg_column]]))
  stopifnot("The ASHg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[ASHg_column]]))

  df$SOPg_standardised <- NA #This row creates the SOPg_standardised column, and fills it with NA values

  #This adds all the columns together, ignoring NA results
  df$SOPg_standardised <- rowSums(df[, c(
    WATERg_column,
    PROCNTg_column,
    FAT_g_standardised_column,
    CHOAVLDFg_standardised_column,
    FIBTGg_standardised_column,
    ALCg_column,
    ASHg_column
  )], na.rm = T)

  # This checks if any rows were entirely NA values, and sets the SOPg_standardised to NA if so.
  df[is.na(df[[WATERg_column]]) &
       is.na(df[[PROCNTg_column]]) &
       is.na(df[[FAT_g_standardised_column]]) &
       is.na(df[[CHOAVLDFg_standardised_column]]) &
       is.na(df[[FIBTGg_standardised_column]]) &
       is.na(df[[ALCg_column]]) &
       is.na(df[[ASHg_column]]), "SOPg_standardised"] <- NA

  # Inserting comment here

  comment_message <- "SOPg_standardised calculated from constituents"

  if (comment == T) {
    if(!(comment_col %in% colnames(df))){
      df[[comment_col]] <- comment_message #If the comment column isn't present yet, but comments are set to True, then it creates the comment column
    }

    #If comment == T and there is already a comment col in the df, then this appends the message to the existing comments.
    df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col], "; ", comment_message)

    #If comment == T and there is already a comment col in the df, but its empty, then this becomes the first entry into the column.
    df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]), comment_col] <- paste0(comment_message)
  }

  return(df)

}

# ¬ Testing ----

test_df <- read.csv("~/GitHub/UoN-FAO/Output/Global_nct_imitation_v1.0.2.csv")

test_df$CHOAVLDFg_standardised <- test_df$CHOAVLg

colnames(test_df)[colnames(test_df) == 'FATg'] <- 'FAT_g_standardised'
colnames(test_df)[colnames(test_df) == 'FIBTGg_standardised'] <- 'FIBTGg_std'

library(dplyr)
time_1 <- Sys.time()

old_method_output <- SOP_std_creator(test_df)

time_2 <- Sys.time()

new_method_output <- SOP_std_creator_New(test_df, FIBTGg_standardised_column = "FIBTGg_std")

time_3 <- Sys.time()

isitadf <- function(df){
  stopifnot("Its not a df" = is.data.frame(df))
}

time_2 - time_1

time_3 - time_2

parity_test <- signif(old_method_output$SOPg_standardised, 10) == signif(new_method_output$SOPg_standardised, 10)

print(table(parity_test))

results_comparison <- old_method_output[, c(
  "WATERg",
  "PROCNTg",
  "FAT_g_standardised",
  "CHOAVLDFg_standardised",
  "FIBTGg_std",
  "ALCg",
  "ASHg",
  "SOPg_standardised"
)]

results_comparison$new_method_SOPg_standardised <- new_method_output$SOPg_standardised

results_comparison$difference <- results_comparison$SOPg_standardised - results_comparison$new_method_SOPg_standardised

# ¬ Test results ----

#switching to stopifnot gives us more control, and has no affect on benchmarking.

# New method seems to be 72-131x faster, before comments col added.
# Old method results: 0.136317 , 0.1248879 , 0.2551999
# New method results: 0.001843929 , 0.001731157 , 0.001951218

#With addition of comments its still ~24x faster



# nutri_combiner ----
# ¬ Taken from FAO-Fisheries (paper branch)/functions/nutri_combiner.R ----


## This function can be used to combine more than one Tagname of the
## same nutrient. It adds information to the "comments" variable on the
## Tagname used.


nutri_combiner <-  function(data.df, var1, var2, var3, new_var){

  text <- paste0(new_var, " equals to ") # metadata in comments variable

  data.df[, new_var] <- NA

  # Loop that prioritise in the other of the variables defined above (1->3)
  for(i in 1:nrow(data.df)){
    print(i)
    if (!is.na(data.df[i, var1])) {
      print(!is.na(data.df[i, var1]))
      data.df[i, new_var] <- data.df[i, var1]
      data.df[i, "comments"] <- ifelse(!is.na(data.df[i, "comments"]),
                                       paste0(data.df[i, "comments"], ";", text, var1),
                                       paste0(text, var1))


    }  else if (is.na(data.df[i, var1]) & !is.na(data.df[i, var2])) {
      data.df[i, new_var] <- data.df[i, var2]
      data.df[i, "comments"] <- ifelse(!is.na(data.df[i, "comments"]),
                                       paste0(data.df[i, "comments"], ";", text, var2),
                                       paste0(text, var2))

    }
    if (is.na(data.df[i, var1]) & is.na(data.df[i, var2]) & !is.na(data.df[i, var3])) {
      data.df[i, new_var] <- data.df[i, var3]
      data.df[i, "comments"] <- ifelse(!is.na(data.df[i, "comments"]),
                                       paste0(data.df[i, "comments"], ";", text, var3),
                                       paste0(text, var3))

    }
    if (is.na(data.df[i, var1]) & is.na(data.df[i, var2]) & is.na(data.df[i, var3])) {
      data.df[i, new_var] <- NA
    }
    print(data.df[i, new_var])
  }

  return(data.df)

}


# CHOAVLDFg_std_creator ----
# ¬ Taken from FAO-Fisheries (paper branch)/functions/summary_table_functions.R ----

CHOAVLDFg_std_creator <- function(dataset) {
  #' @title CHOAVLDFg_std_creator
  #' @description Calculates CHOAVLDFg_std = (100 - (WATERg + PROTg + FATg_standardised + FBGTg + ASHg + ALCg)).
  #' Column names are case sensitive and error is thrown if not found.
  #' @param dataset :Required (FCT dataset to be checked)
  #' @param CHOAVLDFg_standardised Available carbohydrates calculated by difference
  #' @param WATERg Water/ moisture content in g per 100g of EP
  #' @param PROCNTg Protein in g per 100g of EP, as reported in the original FCT and assumed to be calculated from nitrogen (NTg) content
  #' @param FAT_g_standardised fat content unknown method of calculation in g per 100g of EP
  #' @param FIBTGg_std Fibre content from combined Tagnames, with preference of Total dietary fibre by AOAC Prosky method, expressed in g per 100g of EP
  #' @param ALCg Alcohol in g per 100g
  #' @param ASHg Ashes in g per 100g of EP
  #' @return Original FCT dataset with SOPg_standardised column added
  #' @examples

  columns <- c(
    "WATERg",
    "PROCNTg",
    "FAT_g_standardised",
    "FIBTGg_std",
    "ASHg",
    "ALCg"
  )
  check_columns(dataset = dataset, columns = columns)
  tryCatch(
    dataset %>%
      as_tibble() %>%
      mutate_at(.vars = columns, .funs = as.numeric) %>%
      # ! Create a temp row with a count of number of NAs in req columns
      mutate(temp = rowSums(is.na(
        dataset %>%
          select(all_of(columns))
      ))) %>%
      rowwise() %>%
      # ! Check if all the rows are NA then output NA else do the calculation and omit NAs
      mutate(CHOAVLDFg_standardised = ifelse(
        temp == length(columns),
        NA,
        sum(
          100,
          -WATERg,
          -PROCNTg,
          -FAT_g_standardised,
          -FIBTGg_std,
          -ASHg,
          -ALCg,
          na.rm = TRUE
        )
      )) %>%
      # ! remove the temp column
      select(-temp) %>%
      ungroup(),
    error = function(e) {
      print("Error : Required columns not found i.e :")
      print(columns)
      print("The CHOAVLDFg_standardised will not be calculated")
    }
  )
}


# ¬ New Version ----


CHOAVLDFg_std_creator_New <- function(df,
                                WATERg_column = "WATERg",
                                PROCNTg_column = "PROCNTg",
                                FAT_g_standardised_column = "FAT_g_standardised",
                                FIBTGg_standardised_column = "FIBTGg_standardised",
                                ALCg_column = "ALCg",
                                ASHg_column = "ASHg",
                                comment = T,
                                comment_col = "comments") {

  #' @title Carbohydrates (calculated by difference) Calculator
  #' @description Calculates CHOAVLDFg_std = (100 - (WATERg + PROTg + FATg_standardised + FBGTg + ASHg + ALCg)).
  #' Column names are case sensitive and an error is returned if not found.
  #' @param df Required - the data.frame the data is currently stored in.
  #' @param WATERg_column Required - default: \code{'WATERg'} - The name of the column containing Water/moisture content in grams per 100g of Edible Portion (EP).
  #' @param PROCNTg_column Required - default: \code{'PROCNTg'} - Protein in grams per 100g of Edible Portion (EP), as reported in the original FCT and assumed to be calculated from nitrogen (NTg) content.
  #' @param FAT_g_standardised_column Required - default: \code{'FAT_g_standardised'} - Fat content, unknown method of calculation, in grams per 100g of Edible Portion (EP).
  #' @param FIBTGg_standardised_column Required - default: \code{'FIBTGg_standardised'} - Fibre content from combined Tagnames, with preference of Total dietary fibre by AOAC Prosky method, expressed in grams per 100g of Edible Portion (EP).
  #' @param ALCg_column Required - default: \code{'ALCg'} - Alcohol in grams per 100g of Edible Portion (EP).
  #' @param ASHg_column Required - default: \code{'ASHg'} - Ashes in grams per 100g of Edible Portion (EP).
  #' @param comment Optional - default: \code{T} - \code{TRUE} or \code{FALSE}.If comment is set to \code{TRUE} (as it is by default), when the function is
  #'   run a comment describing the source of the \code{CHOAVLDFg_standardised} column is added to the comment_col. If no comment_col is selected, and \code{comment
  #'   = T}, one is created, called \code{comments}.
  #' @param comment_col Optional - default: \code{'comments'} - A potential input
  #'   variable; the column which contains the metadata comments for the food item
  #'   in question. Not required if the comment parameter is set to \code{FALSE}.
  #' @return Original FCT dataset with a new CHOAVLDFg_standardised column.
  #'
  #'
  #'
  #'
  # Check presence of required columns

  # Input checks - goes through each input column name, and checks if its a column in the df. If it isn't, it prints an error message and stops.

  # This check makes sure the entered df is a data frame.
  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df))

  #This block of checks throws an error if the entry for the columns is not present in the df.
  stopifnot("The WATERg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column one'" = WATERg_column %in% colnames(df))
  stopifnot("The PROCNTg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column two'" = PROCNTg_column %in% colnames(df))
  stopifnot("The FAT_g_standardised is not a column name in df - please input a string that is a column name in df, e.g. 'column three'" = FAT_g_standardised_column %in% colnames(df))
  stopifnot("The FIBTGg_std_column is not a column name in df - please input a string that is a column name in df, e.g. 'column five'" = FIBTGg_standardised_column %in% colnames(df))
  stopifnot("The ALCg_columnis not a column name in df - please input a string that is a column name in df, e.g. 'column six'" = ALCg_column %in% colnames(df))
  stopifnot("The ASHg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column seven'" = ASHg_column %in% colnames(df))

  #This block of checks makes sure the columns that are meant to be numeric are numeric.
  stopifnot("The WATERg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[WATERg_column]]))
  stopifnot("The PROCNTg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[PROCNTg_column]]))
  stopifnot("The FAT_g_standardised_column is not numeric. Please ensure it is numeric." = is.numeric(df[[FAT_g_standardised_column]]))
  stopifnot("The FIBTGg_standardised_column is not numeric. Please ensure it is numeric." = is.numeric(df[[FIBTGg_standardised_column]]))
  stopifnot("The ALCg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[ALCg_column]]))
  stopifnot("The ASHg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[ASHg_column]]))

  df$CHOAVLDFg_standardised <- NA #This row creates the CHOAVLDFg_standardised column, and fills it with NA values

  #This adds all the columns together, ignoring NA results
  df$CHOAVLDFg_standardised <- 100 - rowSums(df[, c(
    WATERg_column,
    PROCNTg_column,
    FAT_g_standardised_column,
    FIBTGg_standardised_column,
    ALCg_column,
    ASHg_column
  )], na.rm = T)

  # This checks if any rows were entirely NA values, and sets the CHOAVLDFg_standardised to NA if so.
  df[is.na(df[[WATERg_column]]) &
       is.na(df[[PROCNTg_column]]) &
       is.na(df[[FAT_g_standardised_column]]) &
       is.na(df[[FIBTGg_standardised_column]]) &
       is.na(df[[ALCg_column]]) &
       is.na(df[[ASHg_column]]), "CHOAVLDFg_standardised"] <- NA

  # Inserting comment here

  comment_message <- "CHOAVLDFg_standardised calculated from 100-[constituents]"

  if (comment == T) {
    if(!(comment_col %in% colnames(df))){
      df[[comment_col]] <- comment_message #If the comment column isn't present yet, but comments are set to True, then it creates the comment column
    }

    #If comment == T and there is already a comment col in the df, then this appends the message to the existing comments.
    df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col], "; ", comment_message)

    #If comment == T and there is already a comment col in the df, but its empty, then this becomes the first entry into the column.
    df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]), comment_col] <- paste0(comment_message)
  }

  return(df)

}

# ¬ Testing ----

test_df <- read.csv("~/GitHub/UoN-FAO/Output/Global_nct_imitation_v1.0.2.csv")

colnames(test_df)[colnames(test_df) == 'FATg'] <- 'FAT_g_standardised'
colnames(test_df)[colnames(test_df) == 'FIBTGg_standardised'] <- 'FIBTGg_std'

library(dplyr)
time_1 <- Sys.time()

old_method_output <- CHOAVLDFg_std_creator(test_df)

time_2 <- Sys.time()

new_method_output <- CHOAVLDFg_std_creator_New(test_df, FIBTGg_standardised_column = "FIBTGg_std")

time_3 <- Sys.time()

isitadf <- function(df){
  stopifnot("Its not a df" = is.data.frame(df))
}

time_2 - time_1

time_3 - time_2

parity_test <- signif(old_method_output$CHOAVLDFg_standardised, 10) == signif(new_method_output$CHOAVLDFg_standardised, 10)

print(table(parity_test))

results_comparison <- old_method_output[, c(
  "WATERg",
  "PROCNTg",
  "FAT_g_standardised",
  "FIBTGg_std",
  "ALCg",
  "ASHg",
  "CHOAVLDFg_standardised"
)]

results_comparison$CHOAVLDFg_standardised_New <- new_method_output$CHOAVLDFg_standardised

results_comparison$difference <- results_comparison$CHOAVLDFg_standardised - results_comparison$CHOAVLDFg_standardised_New

different_results <- results_comparison[!(signif(results_comparison$CHOAVLDFg_standardised, 10) == signif(results_comparison$CHOAVLDFg_standardised_New, 10)),]

print(paste0("Largest difference is ", max(results_comparison$difference)))

# ¬¬ Results ----
# Differences are in the order of 10^-14 out. So insignificant.



# VITAmcg_std_creator ----
# ¬ Taken from FAO-Fisheries (paper branch)/functions/summary_table_functions.R ----

VITAmcg_std_creator <- function(dataset) {
  #' @title Vitamin A, retinol calculator
  #' @description Calculates weighted sum of VITAmcg_std (Vitamin A (Retinol Eq. (RE) in mcg per 100g of EP) using the eq. VITAmcg_std = RETOLmcg + 1 / 6 * CARTBEQmcg_std
  #' @param VITAmcg_std Vitamin A (Retinol Eq. (RE) in mcg per 100g of EP
  #' @param RETOLmcg Retinol in mcg per 100g of EP
  #' @param CARTBEQmcg_std Beta-carotene equivalents, expressed in mcg per 100g of EP
  #' @return Original dataset with the added column

  columns <- c("RETOLmcg", "CARTBEQmcg_std")
  check_columns(dataset = dataset, columns = columns)
  # Try the calculation
  tryCatch(
    dataset %>%
      as_tibble() %>%
      mutate_at(.vars = columns, .funs = as.numeric) %>%
      # ! Create a temp row with the number of NAs across the required
      # column
      mutate(temp = rowSums(is.na(
        dataset %>%
          select(all_of(columns))
      ))) %>%
      rowwise() %>%
      # ! Check if all the rows are NA then output NA else do the calculation and omit NAs
      mutate(VITAmcg_std = ifelse(
        temp == length(columns), NA, sum(RETOLmcg, (1 / 6 * CARTBEQmcg_std), na.rm = TRUE)
      )) %>%
      # ! remove the temp column
      select(-temp) %>%
      ungroup(),
    error = function(e) {
      print("Error : Required columns not found i.e :")
      print(columns)
      print("The VITAmcg_standardised will not be calculated")
    }
  )
}



# VITA_RAEmcg_std_creator ----
# ¬ Taken from FAO-Fisheries (paper branch)/functions/summary_table_functions.R ----

VITA_RAEmcg_std_creator <- function(dataset) {
  #' @title Vitamin A, retinol activity
  #' @description Calculates a wighted sum of VITA_RAEmcg_std - Vitamin A (Retinol Activity Eq. (RAE)) in mcg per 100g of EP using (RETOLmcg + 1 / 12 * CARTBEQmcg_std)
  #' @param dataset - FCT dataset
  #' @param VITA_RAEmcg_std - Vitamin A (Retinol Activity Eq. (RAE)) in mcg per 100g of EP
  #' @param RETOLmcg Retinol in mcg per 100g of EP
  #' @param CARTBEQmcg_std Beta-carotene equivalents, expressed in mcg per 100g of EP
  #' @return Original dataset with the added column
  columns <- c("RETOLmcg", "CARTBEQmcg_std")
  check_columns(dataset = dataset, columns = columns)
  tryCatch(
    dataset %>%
      as_tibble() %>%
      mutate_at(.vars = columns, .funs = as.numeric) %>%
      # ! Create a temp row with the number of NAs across the required
      # column
      mutate(temp = rowSums(is.na(
        dataset %>%
          select(all_of(columns))
      ))) %>%
      rowwise() %>%
      # ! Check if all the rows are NA then output NA else do the calculation and omit NAs
      mutate(VITA_RAEmcg_std = ifelse(
        temp == length(columns), NA, sum(RETOLmcg, (1 / 12 * CARTBEQmcg_std), na.rm = TRUE)
      )) %>% # ! remove the temp column
      select(-temp) %>%
      ungroup(),
    error = function(e) {
      print("Error : Required columns not found i.e :")
      print(columns)
      print("The VITA_RAEmcg_standardised will not be calculated")
    }
  )
}


# nia_calculator ----
# ¬ Problem - not found, not referenced ----


# THIAmg_std_creator  ----
# ¬ Taken from FAO-Fisheries (paper branch)/functions/summary_table_functions.R ----

THIAmg_std_creator <- function(dataset) {
  #' @title THIAmg_std_creator
  #' @description Thiamin variable combinations: In absence of THIAmg, use values of THIAHCLmg i.e. THIAmg_std = THIAmg OR THIAHCLmg
  #' @param THIAmg Thiamin, vitamin B1 analysed and expressed as thiamin in mg per 100g of EP
  #' @param THIAHCLmg Thiamin hydrochloride, vitamin B1 analysed and expressed as thiamin hydrochloride in mg per 100g of EP

  columns <- c("THIAmg", "THIAHCLmg")
  check_columns(dataset = dataset, columns = columns)
  # Try the calculation
  tryCatch(
    dataset %>%
      as_tibble() %>%
      mutate_at(.vars = columns, .funs = as.numeric) %>%
      mutate(comments = case_when(
        !is.na(THIAmg) & !is.na(comments) ~  paste0(comments,
                                                    " ; THIAmg_standardised equals to THIAmg"),
        !is.na(THIAmg) & is.na(comments) ~ "THIAmg_standardised equals to THIAmg",
        is.na(THIAmg) & !is.na(THIAHCLmg) & !is.na(comments) ~  paste0(comments,
                                                                       " ; THIAmg_standardised equals to THIAHCLmg"),
        is.na(THIAmg) & !is.na(THIAHCLmg) ~  "THIAmg_standardised equals to THIAHCLmg"),
        THIAmg_std = case_when(
          !is.na(THIAmg) ~ THIAmg,
          is.na(THIAmg) ~ THIAHCLmg
        )),
    error = function(e) {
      print("Error : Required columns not found i.e :")
      print(columns)
    }
  )
}
