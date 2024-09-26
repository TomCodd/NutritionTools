#---
# Title: Column Name Comparer
# Author: Thomas Codd - https://github.com/TomCodd
# Version: V1.0.0
# Changelog:
# Github: https://github.com/TomCodd/NutritionTools
#---

#' @title Column Name Comparer
#' @description Compares the column names of the two input data.frames,
#'   \code{df1} and \code{df2}, and displays whether they are the same or not.
#'   Useful ahead of an rbind function. If the columns are uneven, the function
#'   will create dummy columns to make up the numbers for the comparison.
#' @param df1 Required - the first data.frame to be compared.
#' @param df2 Required - the second data.frame to be compared.
#' @param Summary_Message Optional - default: \code{TRUE} - \code{TRUE} or
#'   \code{FALSE}. Decides whether a summary message should be included.
#' @return A comparison data.frame, listing the names of the columns from df1
#'   and df2.
#' @examples
#' # Example data.frames have been created to give an example of using the
#' # Column_Comparison to compare the column names.
#'
#' breakfast_df
#' #
#' breakfast_df_nonstandard
#'
#' # We will start with two data.frames who's names do not line up.
#'
#' Col_Comparison <- Column_Comparison(breakfast_df, breakfast_df_nonstandard)
#'
#' Col_Comparison
#'
#' # Note how most of the columns do not match in their names, and the function
#' # tells you so.
#'
#'
#' # If the data.frames don't line up in terms of column numbers, then filler
#' # columns will be added.
#'
#' breakfast_df_2 <- breakfast_df
#' breakfast_df_2$THIAmg <- NULL
#' breakfast_df_2$TRPmg <- NULL
#' breakfast_df_2$FAT_g <- NULL
#'
#' Col_Comparison <- Column_Comparison(breakfast_df, breakfast_df_2)
#'
#' Col_Comparison
#'
#' # Note how new columns are added to make up the numbers - however, these
#' # columns are added to the end of the data.frame, causing a shift.
#'
#' # If the columns line up perfectly however:
#'
#' breakfast_df_copy <- breakfast_df
#'
#' Col_Comparison <- Column_Comparison(breakfast_df, breakfast_df_copy)
#'
#' Col_Comparison
#'
#' # Then the Output shows this. It is possible to turn off the Summary
#' # Messages, however:
#'
#'
#' Col_Comparison <- Column_Comparison(breakfast_df, breakfast_df_copy,
#' Summary_Message = FALSE)
#'
#' Col_Comparison
#'
#' @export


Column_Comparison <- function(df1, df2, Summary_Message = TRUE){
  stopifnot("df1 is not a data.frame - please ensure it is a data.frame." = is.data.frame(df1)) #Checks the inputs are data.frames
  stopifnot("df2 is not a data.frame - please ensure it is a data.frame." = is.data.frame(df2)) #Checks the inputs are data.frames
  stopifnot("Summary_Message is not TRUE or FALSE - please ensure it is TRUE or FALSE." = is.logical(Summary_Message)) #Checks the Summary_Message input is True or False

  df1_name <- deparse(substitute(df1)) #finds the input df name for df1
  df2_name <- deparse(substitute(df2)) #finds the input df name for df2

  df1 <- df1[1,] #We only need the column names, so this trims the df to the first row to make things faster
  df2 <- df1[2,] #We only need the column names, so this trims the df to the first row to make things faster

  number_of_cols_1 <- length(df1) - length(df2) #Finds the column difference - the number of extra columns df2 needs to meet df1's
  number_of_cols_2 <- length(df2) - length(df1) #Finds the column difference - the number of extra columns df1 needs to meet df2's

  if(number_of_cols_1 > 0){ #Creates the extra columns required for df2
    if(Summary_Message == TRUE){
      message(paste0(df1_name, " is ", number_of_cols_1, " columns wider than ", df2_name))
    }
    df2[c(paste0("filler_col_", 1:number_of_cols_1))] <- NA
  }

  if(number_of_cols_2 > 0){ #Creates the extra columns required for df1
    if(Summary_Message == TRUE){
      message(paste0(df2_name, " is ", number_of_cols_2, " columns wider than ", df1_name))
    }
    df1[c(paste0("filler_col_", 1:number_of_cols_2))] <- NA
  }

  column_name_comparison <- data.frame("From_df1" = colnames(df1), #Creates a data.frame of the colnames in line with each other
                                       "From_df2" = colnames(df2))

  column_name_comparison$df1_equals_df2 <- column_name_comparison$From_df1 == column_name_comparison$From_df2

  colnames(column_name_comparison)[1] <- paste0("From_", df1_name) #Renames to make the answer clearer
  colnames(column_name_comparison)[2] <- paste0("From_", df2_name) #Renames to make the answer clearer

  if(Summary_Message == TRUE){ #Outputs a summary message of the results.

    message("---------------------------")
    message()
    message("Number of times column names match:")
    print(table(column_name_comparison$df1_equals_df2)) #A pretty acceptable detail message
    message()
    if(FALSE %in% column_name_comparison$df1_equals_df2){
      message("Columns do not match")
    } else {
      message("Columns match")
    }
    message()
    message("---------------------------")
  }

  return(column_name_comparison) #returns the column name comparison df
}
