#---
# Title: VITAmcg_calculator
# Author: Thomas Codd - https://github.com/TomCodd
# Contributor: Lucia Segovia de la Revilla  - https://github.com/LuciaSegovia
# Version: V1.0.1
# Changelog:
# V1.0.0 -> V1.0.1: Added conversion of character classes to numeric for key
# columns, if needed.
# Github: https://github.com/TomCodd/NutritionTools
#---

#' @title Vitamin A Calculator
#' @description Calculates VITAmcg_calculated = (RETOLmcg + (CARTBEQmcg_combined/6)).
#' Column names are case sensitive and an error is returned if not found.
#' @param df Required - the data.frame the data is currently stored in.
#' @param RETOLmcg_column Required - default: \code{'RETOLmcg'} - The name of the
#'   column containing Retinol in mcg per 100g of Edible Portion (EP).
#' @param CARTBEQmcg_combined_column Required - default: \code{'CARTBEQmcg_combined'} -
#'   Beta-carotene equivalents, in mcg per 100g of Edible Portion (EP).
#' @param comment Required - default: \code{TRUE} - \code{TRUE} or \code{FALSE}.
#'   If \code{comment} is set to \code{TRUE} (as it is by default), when the
#'   function is run a comment describing the calculation used to find the
#'   VITA_mcg_calculated value is added to the \code{comment_col}.
#'   If no \code{comment_col} is selected, and \code{comment = TRUE}, one is
#'   created.
#' @param comment_col Optional - default: \code{'comments'} - A potential
#'   input variable; the column which contains the metadata comments for the
#'   food item in question. Not required if \code{comment} is set to
#'   \code{FALSE}. If \code{comment} is set to \code{TRUE}, and the
#'   \code{comment_col} input is not the name of a column found in the
#'   \code{df}, the function will create a column with the name of the
#'   \code{comment_col} input to store comments in.
#' @return Original data.frame with a new \code{VITAmcg_calculated} column, and
#'   (depending on the options selected) an additional comment/comments column
#'   and comment.
#' @examples
#' # We will go through two examples of the VITAmcg_calculator, one using standard
#' # names, and another with non-standard names.
#' breakfast_df <- breakfast_df[,c("food_code", "food_name", "RETOLmcg",
#' "CARTBEQmcg_combined", "comments")]
#' breakfast_df
#'
#' # This is the first data.frame; you can see it has the food item information,
#' # the required columns for calculation, and a comments column. Everything
#' # needed to run the VITAmcg_calculator.
#'
#' VitA_results <- VITAmcg_calculator(breakfast_df)
#'
#' VitA_results
#'
#' # You can see how the data.frame has been returned with a new column (VITAmcg_calculated)
#' # and an additional comment in the comments column, detailing the calculation used.
#'
#' # The second example uses non-standard names, to show how to set the input parameters
#' # if the data.frame is not using the suggested TAGNAMEunit naming system.
#'
#' breakfast_df_nonstandard <- breakfast_df_nonstandard[,c("food_code",
#' "food_name", "Retinol_micrograms", "Beta_Carotene_Equivalents_micrograms",
#' "comments_column")]
#'
#' breakfast_df_nonstandard
#' # You can see this is the same dataset as used previously, but with differing
#' # column names. This will mean the function will not know what the required
#' # column names are, and will need the user to name them.
#'
#' VitA_results_nonstandard <- VITAmcg_calculator(breakfast_df_nonstandard,
#' RETOLmcg_column = "Retinol_micrograms",
#' CARTBEQmcg_combined_column = "Beta_Carotene_Equivalents_micrograms",
#' comment_col = "comments_column")
#'
#' VitA_results_nonstandard
#'
#' # You can see how the results are the same as calculated above, regardless of
#' # the changed column names.
#'
#' @export


VITAmcg_calculator <- function(df,
                               RETOLmcg_column = "RETOLmcg",
                               CARTBEQmcg_combined_column = "CARTBEQmcg_combined",
                               comment = TRUE,
                               comment_col = "comments") {


  # Check presence of required columns

  # Input checks - goes through each input column name, and checks if its a column in the df. If it isn't, it prints an error message and stops.

  # This check makes sure the entered df is a data frame.
  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df))

  #This block of checks throws an error if the entry for the columns is not present in the df.
  stopifnot("The RETOLmcg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column one'." = RETOLmcg_column %in% colnames(df))
  stopifnot("The CARTBEQmcg_combined_column is not a column name in df - please input a string that is a column name in df, e.g. 'column two'." = CARTBEQmcg_combined_column %in% colnames(df))


  #This converts the columns to numeric
  if("character" %in% sapply(df[,c(RETOLmcg_column, CARTBEQmcg_combined_column)], class)){ #Checks to see if character class is detected
    char_cols <- colnames(df[, sapply(df, class) == "character"]) #creates list of all character classes
    input_char_cols <- char_cols[char_cols %in% c(RETOLmcg_column, CARTBEQmcg_combined_column)] #selects character classes which are also input columns
    message("Character class detected in input columns. Attempting to convert following columns to Numeric: ", paste0(input_char_cols, collapse = ", ")) #prints message, listing erroneous columns
    df[[input_char_cols]] <- sapply(df[[input_char_cols]],as.numeric) #attempts to convert to numeric
  }

  #This block of checks makes sure the columns that are meant to be numeric are numeric.
  stopifnot("The RETOLmcg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[RETOLmcg_column]]))
  stopifnot("The CARTBEQmcg_combined_column is not numeric. Please ensure it is numeric." = is.numeric(df[[CARTBEQmcg_combined_column]]))

  #This block checks to make sure logical entries are True or False.
  stopifnot("The comment parameter is not set to TRUE or FALSE - please use TRUE or FALSE." = is.logical(comment))



  df$VITAmcg_calculated <- NA #This row creates the VITAmcg_calculated column, and fills it with NA values
  df$TEMPsixthCARTBEQ <- df[[CARTBEQmcg_combined_column]]/6 #This creates a column for CARTBEQ divided by 6

  #This adds the CARTBEQ/6 and RETOL columns together, ignoring NA results
  df$VITAmcg_calculated <- rowSums(df[, c(
    RETOLmcg_column,
    "TEMPsixthCARTBEQ"
  )], na.rm = TRUE)

  # This checks if any rows were entirely NA values, and sets the VITAmcg_calculated to NA if so.
  df[is.na(df[[RETOLmcg_column]]) &
       is.na(df[[CARTBEQmcg_combined_column]]), "VITAmcg_calculated"] <- NA

  # This deletes the temporary TEMPsixthCARTBEQ column

  df$TEMPsixthCARTBEQ <- NULL

  # Comments process
  if (comment == TRUE) {
    #Creates comments_message if comments are true
    comment_message <- "VITAmcg_calculated value calculated from Retinol + 1/6 Beta-Carotene Equivalents"

    if(!(comment_col %in% colnames(df))){
      df[[comment_col]] <- comment_message #If the comment column isn't present yet, but comments are set to True, then it creates the comment column
    }

    #If comment == TRUE and there is already a comment in the df, then this appends the message to the existing comments.
    df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col], "; ", comment_message)

    #If comment == TRUE and the comment_col is empty, then this becomes the first entry into the column.
    df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]), comment_col] <- paste0(comment_message)

  }

  return(df)

}
