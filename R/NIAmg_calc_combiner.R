
#---
# Title: Niacin Calculator and Combiner
# Author: Thomas Codd - https://github.com/TomCodd
# Contributor: Lucia Segovia de la Revilla  - https://github.com/LuciaSegovia
# Version: V1.0.1
# Changelog:
# V1.0.0 -> V1.0.1: Added conversion of character classes to numeric for key
# columns, if needed.
# Github: https://github.com/TomCodd/NutritionTools
#---

#' @title Niacin Calculator and Combiner
#' @description This function calculates potential values for Niacin, and then
#'   combines the most appropriate ones for each item into a new value. Inputs must
#'   cover at least two of the following combinations: NIAmg_column,
#'   NIAEQmg_column + NIATRPmg_column, NIAEQmg_column + TRPmg_column. This is
#'   because the function can then find Niacin values in multiple ways, and
#'   select the most appropriate.
#'
#'   The priority for use is 1st:  \code{Niacin}, 2nd: \code{Total
#'   Niacin Equivalents} - \code{(Tryptophan/60)}, 3rd: \code{Total Niacin
#'   Equivalents} - \code{Niacin Equivalents from Tryptophan}.
#' @param df Required - the data.frame the data is currently stored in.
#' @param NIAEQmg_column Required - default: \code{'NIAEQmg'} - The name of the
#'   column containing Niacin equivalents, total (preformed Niacin as well as
#'   Niacin equivalents from Tryptophan) in mg per 100g of Edible Portion
#'   (EP). The only required input as its impossible to get 2 or more ways of
#'   calculating \code{NIAmg_combined} without it, which is required for
#'   the function to work.
#' @param NIAmg_column Optional - default: \code{'NIAmg'} - The name of the
#'   column containing Niacin (preformed) in mg per 100g of Edible Portion (EP).
#'   If unavailable, set input to \code{NA}.
#' @param TRPmg_column Optional - default: \code{'TRPmg'} - Tryptophan, in mg
#'   per 100g of Edible Portion (EP). If unavailable, set input to \code{NA}.
#' @param NIATRPmg_column Optional - default: \code{'NIATRPmg'} - The name of the
#'   column containing Niacin equivalents from Tryptophan, in mg per 100g of
#'   Edible Portion (EP). If unavailable, set input to \code{NA}.
#' @param comment Required - default: \code{TRUE} - \code{TRUE} or \code{FALSE}.
#'   If \code{comment} is set to \code{TRUE} (as it is by default), when the
#'   function is run a comment describing the calculation used to find the
#'   NIAmg_combined value is added to the \code{comment_col}.
#'   If no \code{comment_col} is selected, and \code{comment = TRUE}, one is
#'   created.
#' @param comment_col Optional - default: \code{'comments'} - A potential
#'   input variable; the column which contains the metadata comments for the
#'   food item in question. Not required if \code{comment} is set to
#'   \code{FALSE}. If \code{comment} is set to \code{TRUE}, and the
#'   \code{comment_col} input is not the name of a column found in the
#'   \code{df}, the function will create a column with the name of the
#'   \code{comment_col} input to store comments in.
#' @return Original data.frame with a new \code{NIAmg_combined} column, and
#'   (depending on the options selected) an additional comment/comments column
#'   and comment.
#'
#' @examples
#' # We will go through three examples of the NIAmg_calc_combiner, two using
#' # standard names, and another with non-standard names.
#' breakfast_df <- breakfast_df[,c("food_code", "food_name", "NIAmg", "TRPmg",
#' "NIAEQmg", "NIATRPmg", "comments")]
#' breakfast_df
#'
#' # This is the first data.frame; you can see it has the food item information,
#' # the required columns for calculation, and a comments column. Everything
#' # needed to run the NIAmg_calc_combiner
#'
#' NIA_results <- NIAmg_calc_combiner(breakfast_df)
#'
#' NIA_results
#'
#' # You can see how the data.frame has been returned with a new column (NIAmg_combined)
#' # and an additional comment in the comments column, detailing the calculation used.
#'
#' # However, what if you don't have data for all of these nutrients? In that case,
#' # The nutrient in question should be set to NA. The function assumes you have
#' # all nutrients it needs available, and assumes the standard INFOODS TAGNAMEunit
#' # naming system. This is why the above example works without the need to specify
#' # which column is which. However, when it a column is missing, this means that
#' # the default input needs to be overridden by an NA value, to tell the function that
#' # its missing.
#'
#' breakfast_df$TRPmg <- NULL
#'
#' breakfast_df
#'
#' # We can see the breakfast_df is now missing an input column; NIATRPmg. To run
#' # this function without this input, the command would be:
#'
#' NIA_results_noTRP <- NIAmg_calc_combiner(breakfast_df, TRPmg_column = NA)
#' NIA_results_noTRP
#'
#' # You can see the results have shifted. The values that had been calculated using
#' # TRP are now calculated using NIATRP. (Please don't worry about the discrepancy
#' # between the previous results and these; these nutritional figures are entirely
#' # fictional, and so the new calculation may shift NIAmg_combined massively. This
#' # is not an issue.)
#'
#' # If too few inputs for calculation are inputted, then the function will show a
#' # warning, and not run.
#'
#'
#' # The third example uses non-standard names, to show how to set the input parameters
#' # if the data.frame is not using the suggested TAGNAMEunit naming system.
#'
#' breakfast_df_nonstandard <- breakfast_df_nonstandard[,c("food_code",
#' "food_name", "Niacin_milligrams", "Tryptophan_milligrams", "Niacin_eq_milligrams",
#' "Niacine_from_TRP_mg", "comments_column")]
#'
#' breakfast_df_nonstandard
#' # You can see this is the same dataset as used previously, but with differing
#' # column names. This will mean the function will not know what the required
#' # column names are, and will need the user to name them.
#'
#' NIA_results_nonstandard <- NIAmg_calc_combiner(breakfast_df_nonstandard,
#' NIAmg_column = "Niacin_milligrams",
#' TRPmg_column = "Tryptophan_milligrams",
#' NIAEQmg_column = "Niacin_eq_milligrams",
#' NIATRPmg_column = "Niacine_from_TRP_mg",
#' comment_col = "comments_column")
#'
#' NIA_results_nonstandard
#'
#' # You can see how the results are the same as calculated above, regardless of
#' # the changed column names.
#'
#' @export


NIAmg_calc_combiner <- function(df,
                                NIAEQmg_column = "NIAEQmg",
                                NIAmg_column = "NIAmg",
                                TRPmg_column = "TRPmg",
                                NIATRPmg_column = "NIATRPmg",
                                comment = TRUE,
                                comment_col = "comments") {

  # Check presence of required columns

  # Input checks - goes through each input column name, and checks if its a column in the df. If it isn't, it prints an error message and stops.

  # This check makes sure the entered df is a data frame.
  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df))

  #This block of checks throws an error if the entry for the columns is not present in the df.
  stopifnot("The NIAEQmg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column one'." = NIAEQmg_column %in% colnames(df))

  #This converts the columns to numeric, if needed

  present_columns <- c(NIAEQmg_column, TRPmg_column, NIAmg_column, NIATRPmg_column)
  present_columns <- present_columns[!is.na(present_columns)]

  if("character" %in% sapply(df[, present_columns], class)){ #Checks to see if character class is detected
    char_cols <- colnames(df[, sapply(df, class) == "character"]) #creates list of all character classes
    input_char_cols <- char_cols[char_cols %in% present_columns] #selects character classes which are also input columns
    message("Character class detected in input columns. Attempting to convert following columns to Numeric: ", paste0(input_char_cols, collapse = ", ")) #prints message, listing erroneous columns
    df[, input_char_cols] <- sapply(df[, input_char_cols], as.numeric) #attempts to convert to numeric
  }

  #This block of checks makes sure the columns that are meant to be numeric are numeric.
  stopifnot("The NIAEQmg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[NIAEQmg_column]]))

  #This block checks to make sure logical entries are True or False.
  stopifnot("The comment parameter is not set to TRUE or FALSE - please use TRUE or FALSE." = is.logical(comment))

  #This block makes sure that enough options are present for NIAmg_combined to be calculated in 2+ ways.
  #It also performs input checks on non-NA inputs, and pre-calculations for if they require them, e.g. TRP, followed by the actual calculations.

  #1st way: NIAmg
  #2nd way: NIAEQmg_column - (TRPmg/60)
  #3rd way: NIAEQmg_column - NIATRPmg

  NumberOfWays <- 0

  if(!is.na(NIAmg_column)){
    NumberOfWays <- NumberOfWays+1
    stopifnot("The NIAmg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column two'. If there is no column for NIAmg, please set the NIAmg_column input to NA." = NIAmg_column %in% colnames(df))
    stopifnot("The NIAmg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[NIAmg_column]]))
  }

  if(!is.na(TRPmg_column)){
    NumberOfWays <- NumberOfWays+1
    stopifnot("The TRPmg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column three'. If there is no column for TRPmg, please set the TRPmg_column input to NA." = TRPmg_column %in% colnames(df))
    stopifnot("The TRPmg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[TRPmg_column]]))
    df$TEMP_NIAmg_std_NIAEQ_TRP <- df[[NIAEQmg_column]]-(df[[TRPmg_column]]/60)
  }

  if(!is.na(NIATRPmg_column)){
    NumberOfWays <- NumberOfWays+1
    stopifnot("The NIATRPmg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column four'. If there is no column for NIATRPmg, please set the NIATRPmg_column input to NA." = NIATRPmg_column %in% colnames(df))
    stopifnot("The NIATRPmg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[NIATRPmg_column]]))
    df$TEMP_NIAmg_std_NIAEQ_NIATRP <- df[[NIAEQmg_column]]-df[[NIATRPmg_column]]

  }

  if(NumberOfWays < 2){
    stop(
      "Not enough ways to calculate NIAmg_combined found. Please make sure you have included inputs for at least 2 of the following parameters: NIAmg_column, TRPmg_column, NIATRPmg_column. please run '?NIAmg_calc_standardiser()' to learn more."
    )
  }

  #Standardisation

  df$NIAmg_combined <- NA #This row creates the NIAmg_combined column, and fills it with NA values
  if(comment == TRUE){
    df$TEMP_NIA_comment <- "No suitable value for NIAmg_combined found"
  }

  # The least wanted input is NIAEQmg - NIATRPmg, so we'll do that one first.

  if(!is.na(NIATRPmg_column)){
    df[!(df$TEMP_NIAmg_std_NIAEQ_NIATRP %in% "" | is.na(df$TEMP_NIAmg_std_NIAEQ_NIATRP)), "NIAmg_combined"] <- df[!(df$TEMP_NIAmg_std_NIAEQ_NIATRP %in% "" | is.na(df$TEMP_NIAmg_std_NIAEQ_NIATRP)), "TEMP_NIAmg_std_NIAEQ_NIATRP"] #Where TEMP_NIAmg_std_NIAEQ_NIATRP is not NA or blank, sets new_variable to be that value.

    if(comment == TRUE){ #If comments are true, sets the relevant rows to mention they come from this calculation
      df[!(df$TEMP_NIAmg_std_NIAEQ_NIATRP %in% "" | is.na(df$TEMP_NIAmg_std_NIAEQ_NIATRP)), "TEMP_NIA_comment"] <- paste0("NIAmg_combined equal to ", NIAEQmg_column, " - ", NIATRPmg_column)
    }
  }



  # Then the second least wanted input; NIAEQmg - TRPmg/60

  if(!is.na(TRPmg_column)){
    df[!(df$TEMP_NIAmg_std_NIAEQ_TRP %in% "" | is.na(df$TEMP_NIAmg_std_NIAEQ_TRP)), "NIAmg_combined"] <- df[!(df$TEMP_NIAmg_std_NIAEQ_TRP %in% "" | is.na(df$TEMP_NIAmg_std_NIAEQ_TRP)), "TEMP_NIAmg_std_NIAEQ_TRP"] #Where TEMP_NIAmg_std_NIAEQ_TRP is not NA or blank, sets new_variable to be that value.

    if(comment == TRUE){ #If comments are true, sets the relevant rows to mention they come from this calculation
      df[!(df$TEMP_NIAmg_std_NIAEQ_TRP %in% "" | is.na(df$TEMP_NIAmg_std_NIAEQ_TRP)), "TEMP_NIA_comment"] <- paste0("NIAmg_combined equal to ", NIAEQmg_column, " - (", TRPmg_column, "/60)")
    }
  }



  # Then the most wanted input; NIAmg

  if(!is.na(NIAmg_column)){
    df[!(df[[NIAmg_column]] %in% "" | is.na(df[[NIAmg_column]])), "NIAmg_combined"] <- df[!(df[[NIAmg_column]] %in% "" | is.na(df[[NIAmg_column]])), NIAmg_column] #Where TEMP_NIAmg_std_NIAEQ_TRP is not NA or blank, sets new_variable to be that value.

    if(comment == TRUE){ #If comments are true, sets the relevant rows to mention they come from htis value
      df[!(df[[NIAmg_column]] %in% "" | is.na(df[[NIAmg_column]])), "TEMP_NIA_comment"] <- paste0("NIAmg_combined equal to ", NIAmg_column)
    }
  }



  #The sort out the comments

  if (comment == TRUE) {

    message("---------------------------")
    message()
    message("Breakdown of values used:")
    print(table(df$TEMP_NIA_comment)) #A pretty acceptable detail message
    message()
    message("---------------------------")

    if(!(comment_col %in% colnames(df))){
      df[[comment_col]] <- NA #If the comment column isn't present yet in the data frame, but comments are set to True, then it creates the comment column
    }

    df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col], "; ", df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), "TEMP_NIA_comment"])

    #If comment == T and there is already a comment col in the df, but its empty, then this becomes the first entry into the column.
    df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]), comment_col] <- df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]), "TEMP_NIA_comment"]

  }

  # Remove the temp columns
  df$TEMP_NIA_comment <- NULL
  df$TEMP_NIAmg_std_NIAEQ_NIATRP <- NULL
  df$TEMP_NIAmg_std_NIAEQ_TRP <- NULL

  return(df)

}
