
#---
# Title: Carbohydrates (calculated by difference) Calculator
# Author: Thomas Codd - https://github.com/TomCodd
# Contributor: Lucia Segovia de la Revilla  - https://github.com/LuciaSegovia
# Version: V1.0.0
# Changelog:
# Github: https://github.com/TomCodd/NutritionTools
#---

#' @title Carbohydrates (calculated by difference) Calculator
#' @description Calculates CHOAVLDFg_calculated = (100 - (WATERg + PROTg +
#'   FATg_combined + FBGTg + ASHg + ALCg)). Column names are case
#'   sensitive and an error is returned if not found.
#' @param df Required - the data.frame the data is currently stored in.
#' @param WATERg_column Required - default: \code{'WATERg'} - The name of the
#'   column containing Water/moisture content in grams per 100g of Edible
#'   Portion (EP).
#' @param PROCNTg_column Required - default: \code{'PROCNTg'} - Protein in
#'   grams per 100g of Edible Portion (EP), as reported in the original FCT
#'   and assumed to be calculated from nitrogen (NTg) content.
#' @param FAT_g_combined_column Required - default:
#'   \code{'FAT_g_combined'} - Fat content, unknown method of calculation,
#'   in grams per 100g of Edible Portion (EP).
#' @param FIBTGg_combined_column Required - default:
#'   \code{'FIBTGg_combined'} - Fibre content from combined Tagnames, with
#'   preference of Total dietary fibre by AOAC Prosky method, expressed in
#'   grams per 100g of Edible Portion (EP).
#' @param ALCg_column Required - default: \code{'ALCg'} - Alcohol in grams per
#'   100g of Edible Portion (EP).
#' @param ASHg_column Required - default: \code{'ASHg'} - Ashes in grams per
#'   100g of Edible Portion (EP).
#' @param comment Required - default: \code{TRUE} - \code{TRUE} or \code{FALSE}.
#'   If \code{comment} is set to \code{TRUE} (as it is by default), when the
#'   function is run a comment describing the source of the
#'   \code{CHOAVLDFg_calculated} column is added to the \code{comment_col}
#'   If no \code{comment_col} is selected, and \code{comment = TRUE}, one is
#'   created.
#' @param comment_col Optional - default: \code{'comments'} - A potential
#'   input variable; the column which contains the metadata comments for the
#'   food item in question. Not required if \code{comment} is set to
#'   \code{FALSE}. If \code{comment} is set to \code{TRUE}, and the
#'   \code{comment_col} input is not the name of a column found in the
#'   \code{df}, the function will create a column with the name of the
#'   \code{comment_col} input to store comments in.
#' @param NegativeValueReplacement Required - default: \code{0} - Options are
#'   \code{0}, \code{NA}, \code{'remove'}, or \code{'nothing'}. Choose what
#'   happens to negative values. If set to \code{0}, then negative values are
#'   set to 0. If set to \code{NA}, they are replaced with NA. if set to
#'   \code{'remove'}, then those entries in the \code{df} are removed. if set to
#'   \code{'nothing'}, then they are left as negative values.
#' @param NegativeValueDF Required - default: \code{FALSE} - \code{TRUE} or
#'   \code{FALSE}. If set to \code{TRUE}, Then the output switches from being
#'   a copy of the input \code{df} with the the \code{CHOAVLDFg_calculated}
#'   column to a subset of that dataframe only showing
#'   \code{CHOAVLDFg_calculated} values that are less than 0, for manual
#'   inspection.
#' @return Original data.frame with a new \code{CHOAVLDFg_calculated} column, and
#'   (depending on the options selected) an additional comment/comments column
#'   and comment.
#' @examples
#' # Two example data.frames have been prepared to illustrate the
#' # CHOAVLDFg_calculator The first is a dataset of fictional food values to
#' # illustrate the various options in the function, and the second is a dataset
#' # with non-standard column names, to show how to specify columns.
#'
#' # This is the first data.frame - before the CHOAVLDFg_calculator has been used on it.
#' breakfast_df <- breakfast_df[,c("food_code", "food_name", "WATERg",
#' "PROCNTg", "FAT_g_combined", "FIBTGg_combined", "ALCg", "ASHg",
#' "comments")]
#' breakfast_df
#' #
#' #
#' # First, an example of the standard usecase - calculate the CHOAVLDFg_calculated
#' # value, while resetting negative values to 0.
#' NegativeToZeroResults <- CHOAVLDFg_calculator(breakfast_df, NegativeValueReplacement = 0)
#' #
#' NegativeToZeroResults
#' # See the changes - the addition of the CHOAVLDFg_calculated column, and
#' # the additions to the comments column. Notice how some comments are
#' # specially formulated to show if the corresponding CHOAVLDFg_calculated
#' # value has been reset to 0.
#' #
#' #
#' # The second example shows the results when the Replacement option is set to NA
#' NegativeToNA_results <- CHOAVLDFg_calculator(breakfast_df, NegativeValueReplacement = NA)
#' #
#' NegativeToNA_results
#' # Check the CHOAVLDFg_calculated column and comments column again - see how
#' # values below zero have been replaced with NA, and a note of this change
#' # logged in the comments column.
#' #
#' #
#' # The third example shows the results when the Replacement option is set to 'remove'
#' remove_results <- CHOAVLDFg_calculator(breakfast_df, NegativeValueReplacement = "remove")
#' #
#' remove_results
#' # See how the out of bounds values have been removed.
#' #
#' #
#' # The fourth example is of the nothing results - i.e. nothing happens to the
#' # negative values.
#' NegativeNoChangeResults <- CHOAVLDFg_calculator(breakfast_df, NegativeValueReplacement = "nothing")
#' #
#' NegativeNoChangeResults
#' # Look at the CHOAVLDFg_calculator values - and see how they've been left,
#' # even if they're negative.
#' #
#' #
#' # The fifth example is of the negative dataframe - an option useful for identifying
#' # and examining negative results.
#' NegativeCarb_results <- CHOAVLDFg_calculator(breakfast_df, NegativeValueDF = TRUE)
#' #
#' NegativeCarb_results
#' # Only the out of bounds results are present, in their original form, for inspection.
#' #
#' #
#' # The sixth example is of the CHOAVLDFg_calculator working on a dataframe with
#' # non-standard column names. It uses a modified example data frame, shown below.
#' #
#' breakfast_df_nonstandard <- breakfast_df_nonstandard[,c("food_code",
#' "food_name", "Water_values_g", "PROCNT_values_g", "FAT_values_g_combined",
#' "FIBTG_values_g_combined", "ALC_values_g", "ASH_values_g", "comments_column")]
#' breakfast_df_nonstandard
#' # Notice how the column names are different, and differ from the assumed names.
#' #
#' #
#' # Because of the different names, the column names for each input must be specified.
#' nothing_results_NonStandardInput <- CHOAVLDFg_calculator(
#' breakfast_df_nonstandard,
#' WATERg_column = "Water_values_g",
#' PROCNTg_column = "PROCNT_values_g",
#' FAT_g_combined_column = "FAT_values_g_combined",
#' FIBTGg_combined_column = "FIBTG_values_g_combined",
#' ALCg_column = "ALC_values_g",
#' ASHg_column = "ASH_values_g",
#' comment_col = "comments_column"
#' )
#' nothing_results_NonStandardInput
#' #
#' # The resulting CHOAVLDFg_calculated column is the same as in the first example,
#' # despite the different names.
#' @export

CHOAVLDFg_calculator <- function(df,
                                 WATERg_column = "WATERg",
                                 PROCNTg_column = "PROCNTg",
                                 FAT_g_combined_column = "FAT_g_combined",
                                 FIBTGg_combined_column = "FIBTGg_combined",
                                 ALCg_column = "ALCg",
                                 ASHg_column = "ASHg",
                                 comment = TRUE,
                                 comment_col = "comments",
                                 NegativeValueReplacement = 0,
                                 NegativeValueDF = FALSE) {

  # Check presence of required columns

  # Input checks - goes through each input column name, and checks if its a column in the df. If it isn't, it prints an error message and stops.

  # This check makes sure the entered df is a data frame.
  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df))

  #This block of checks throws an error if the entry for the columns is not present in the df.
  stopifnot("The WATERg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column one'." = WATERg_column %in% colnames(df))
  stopifnot("The PROCNTg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column two'." = PROCNTg_column %in% colnames(df))
  stopifnot("The FAT_g_combined is not a column name in df - please input a string that is a column name in df, e.g. 'column three'." = FAT_g_combined_column %in% colnames(df))
  stopifnot("The FIBTGg_combined_column is not a column name in df - please input a string that is a column name in df, e.g. 'column five'." = FIBTGg_combined_column %in% colnames(df))
  stopifnot("The ALCg_columnis not a column name in df - please input a string that is a column name in df, e.g. 'column six'." = ALCg_column %in% colnames(df))
  stopifnot("The ASHg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column seven'." = ASHg_column %in% colnames(df))

  #This block of checks makes sure the columns that are meant to be numeric are numeric.
  stopifnot("The WATERg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[WATERg_column]]))
  stopifnot("The PROCNTg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[PROCNTg_column]]))
  stopifnot("The FAT_g_combined_column is not numeric. Please ensure it is numeric." = is.numeric(df[[FAT_g_combined_column]]))
  stopifnot("The FIBTGg_combined_column is not numeric. Please ensure it is numeric." = is.numeric(df[[FIBTGg_combined_column]]))
  stopifnot("The ALCg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[ALCg_column]]))
  stopifnot("The ASHg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[ASHg_column]]))

  #This block checks to make sure logical entries are True or False.
  stopifnot("The comment parameter is not set to TRUE or FALSE - please use TRUE or FALSE." = is.logical(comment))
  stopifnot("The NegativeValueDF parameter is not set to TRUE or FALSE - please use TRUE or FALSE." = is.logical(NegativeValueDF))

  #Special check to check the options for the NegativeValueReplacement input.
  stopifnot("The NegativeValueReplacement parameter is not set to 0, NA, 'remove' or 'nothing' - please use one of these options." = tolower(NegativeValueReplacement) %in% c(NA, 0, "nothing", "none", "n", "rm", "del", "remove", "delete"))



  if(NegativeValueDF == TRUE){ #Turns off comments if NegativeValueDF is active. This produces a subdataset, without the changes that the comments are recording.
    comment <- FALSE
  }

  df$CHOAVLDFg_calculated <- NA #This row creates the CHOAVLDFg_calculated column, and fills it with NA values

  #This adds all the columns together, ignoring NA results
  df$CHOAVLDFg_calculated <- 100 - rowSums(df[, c(
    WATERg_column,
    PROCNTg_column,
    FAT_g_combined_column,
    FIBTGg_combined_column,
    ALCg_column,
    ASHg_column
  )], na.rm = TRUE)

  # This checks if any rows were entirely NA values, and sets the CHOAVLDFg_calculated to NA if so.
  df[is.na(df[[WATERg_column]]) &
       is.na(df[[PROCNTg_column]]) &
       is.na(df[[FAT_g_combined_column]]) &
       is.na(df[[FIBTGg_combined_column]]) &
       is.na(df[[ALCg_column]]) &
       is.na(df[[ASHg_column]]), "CHOAVLDFg_calculated"] <- NA

  # Inserting comment here

  comment_message <- "CHOAVLDFg_calculated calculated from 100-[constituents]"

  if (comment == TRUE) {
    if(!(comment_col %in% colnames(df))){
      df[[comment_col]] <- comment_message #If the comment column isn't present yet, but comments are set to True, then it creates the comment column
    }

    if (tolower(NegativeValueReplacement) %in% c(0)){ #If NegativeToZero is set to 0, then a special message must appear in specific columns, detailing the original value and that it was reset to 0.

      # This is for rows with existing comments, and negative values
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0 & !is.na(df$CHOAVLDFg_calculated), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0 & !is.na(df$CHOAVLDFg_calculated), comment_col], "; ", comment_message, " - Original value of ", df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0 & !is.na(df$CHOAVLDFg_calculated), "CHOAVLDFg_calculated"], " reset to 0")

      # This is for rows without existing comments, and negative values
      df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0 & !is.na(df$CHOAVLDFg_calculated), comment_col] <- paste0(comment_message, " - Original value of ", df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0 & !is.na(df$CHOAVLDFg_calculated), "CHOAVLDFg_calculated"], " reset to 0")

      # This is for rows with existing comments, and positive values
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated >= 0 & !is.na(df$CHOAVLDFg_calculated), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated >= 0 & !is.na(df$CHOAVLDFg_calculated), comment_col], "; ", comment_message)

      #This is for rows without existing comments, and positive values
      df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated >= 0 & !is.na(df$CHOAVLDFg_calculated), comment_col] <- paste0(comment_message)


    } else if (is.na(NegativeValueReplacement)){ #If NegativeToZero is set to NA, then a special message must appear in specific columns, detailing the original value and that it was reset to NA.

      # This is for rows with existing comments, and negative values
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0 & !is.na(df$CHOAVLDFg_calculated), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0 & !is.na(df$CHOAVLDFg_calculated), comment_col], "; ", comment_message, " - Original value of ", df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0 & !is.na(df$CHOAVLDFg_calculated), "CHOAVLDFg_calculated"], " reset to NA")

      # This is for rows without existing comments, and negative values
      df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0 & !is.na(df$CHOAVLDFg_calculated), comment_col] <- paste0(comment_message, " - Original value of ", df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0, "CHOAVLDFg_calculated"], " reset to NA")

      # This is for rows with existing comments, and positive values
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated >= 0 & !is.na(df$CHOAVLDFg_calculated), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated >= 0 & !is.na(df$CHOAVLDFg_calculated), comment_col], "; ", comment_message)

      #This is for rows without existing comments, and positive values
      df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated >= 0 & !is.na(df$CHOAVLDFg_calculated), comment_col] <- paste0(comment_message)


    } else { #If NegativeValueReplacement is set to nothing, or delete (the only other valid options), the comments for the negative values don't matter. All comments are therefore the same - and negative values do not need a custom message.

      #If comment == TRUE and there is already a comment col in the df, then this appends the message to the existing comments.
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & !is.na(df$CHOAVLDFg_calculated), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & !is.na(df$CHOAVLDFg_calculated), comment_col], "; ", comment_message)

      #If comment == TRUE and there is already a comment col in the df, but its empty, then this becomes the first entry into the column.
      df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]) & !is.na(df$CHOAVLDFg_calculated), comment_col] <- paste0(comment_message)
    }
  }


  BelowZeroNumber <- length(df[df$CHOAVLDFg_calculated < 0, "CHOAVLDFg_calculated"]) #Sees how many values are less than 0.

  if(BelowZeroNumber > 0){ #Triggers a warning if they are present.

    Min_Number <- min(df$CHOAVLDFg_calculated, na.rm = TRUE) #Finds the lowest value.
    Number_Below_Minus5 <- length(df[df$CHOAVLDFg_calculated < -5 & !is.na(df$CHOAVLDFg_calculated), "CHOAVLDFg_calculated"]) #Finds the number of values less than -5.
    NA_number <- length(df[is.na(df$CHOAVLDFg_calculated), "CHOAVLDFg_calculated"])

    message("---------------------------") #Prints a warning message.
    message()
    message(BelowZeroNumber, " CHOAVLDFg_calculated values calculated to be less than 0. Minimum result: ", Min_Number, ". Number of values below -5: ", Number_Below_Minus5, ". Number of NA results: ", NA_number, ". Please rerun the function with NegativeValueDF = TRUE if you wish to inspect these values.")
    message()
    if (tolower(NegativeValueReplacement) %in% c(0)){
      message("Negative values set to 0, as per user input.")
    } else if(is.na(NegativeValueReplacement)) {
      message("Negative values set to NA, as per user input.")
    } else if(tolower(NegativeValueReplacement) %in% c("nothing", "none", "n")) {
      message("Negative values left untouched, as per user input.")
    } else { #The only other valid option is for them to be removed.
      message("Negative value rows removed, as per user input.")
    }
    message()
    message("---------------------------")
  }

  if (NegativeValueDF == TRUE){ #Implements the NegativeValueDF functionality - stripping to a a df with just negative calc Carb values.
    result_df <- df[df$CHOAVLDFg_calculated < 0,]
  } else { #Otherwise does the normal process of setting negative values to 0, if NegativeToZero is set to TRUE.
    if (tolower(NegativeValueReplacement) %in% c(0)){
      result_df <- df
      result_df[result_df$CHOAVLDFg_calculated < 0 & !is.na(df$CHOAVLDFg_calculated), "CHOAVLDFg_calculated"] <- 0
    } else if (is.na(NegativeValueReplacement)){
      result_df <- df
      result_df[result_df$CHOAVLDFg_calculated < 0  & !is.na(df$CHOAVLDFg_calculated), "CHOAVLDFg_calculated"] <- NA
    } else if (tolower(NegativeValueReplacement) %in% c("rm", "del", "remove", "delete")) {
      result_df <- df[df$CHOAVLDFg_calculated >= 0  & !is.na(df$CHOAVLDFg_calculated),] #Only outputs rows with CHOAVLDFg_calculated values over 0.
    } else { #The only valid option left is to do nothing - so nothing happens.
      result_df <- df
    }
  }

  return(result_df)

}
