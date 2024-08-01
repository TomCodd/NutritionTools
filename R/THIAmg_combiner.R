#---
# Title: Thiamine Nutrient Combiner
# Author: Thomas Codd - https://github.com/TomCodd
# Contributor: Lucia Segovia de la Revilla  - https://github.com/LuciaSegovia
# Version: V1.0.0
# Changelog:
# Github: https://github.com/TomCodd/NutritionTools
#---

#' @title Thiamine Nutrient Combiner
#' @description Combines possible values for Thiamine heirachically so the
#'   most suitable is the one used. Considers Thiamine and Thiamine
#'   Hydrochloride.
#' @param df Required - the data.frame the data is currently stored in.
#' @param THIAmg_column Required - default: \code{'THIAmg'} - The name of the
#'   column containing Thiamine, vitamin B1 analysed and expressed as Thiamine
#'   in mg per 100g of EP.
#' @param THIAHCLmg_column Required - default: \code{'THIAHCLmg'} - The name
#'   of the column containing THIAHCLmg hydrochloride, vitamin B1 analysed and
#'   expressed as Thiamine hydrochloride in mg per 100g of EP.
#' @param comment Required - default: \code{TRUE} - \code{TRUE} or \code{FALSE}.
#'   If comment is set to \code{TRUE} (as it is by default), when the function
#'   is run a comment describing the source of the
#'   \code{THIAmg_combined} column is added to the comment_col. If no
#'   comment_col is selected, and \code{comment = TRUE}, one is created.
#' @param comment_col Optional - default: \code{'comments'} - A potential
#'   input variable; the column which contains the metadata comments for the
#'   food item in question. Not required if the comment parameter is set to
#'   \code{FALSE}. If set to true, and the comment_col entry is not found in
#'   the df, it will create a column with the name of the entry.
#' @return Original data.frame with a new \code{THIAmg_combined} column, and
#'   (depending on the options selected) an additional comment/comments column
#'   and comment.
#' @examples  #' # Two example data.frames have been created to give an example of
#' # using the THIAmg_combiner; one with the expected inputs, another with custom
#' # inputs.
#'
#' breakfast_df <- breakfast_df[, c("food_code", "food_name", "THIAmg", "THIAHCLmg",
#' "comments")]
#' breakfast_df
#'
#' # We start with a data.frame containing patchy values between THIAmg and THIAHCLmg. Ideally
#' # we would like to combine these into a single 'combined' column with as few gaps
#' # as possible. This is the purpose of the THIAmg_combiner.
#'
#' THIA_results <- THIAmg_combiner(breakfast_df)
#' THIA_results
#'
#' # Note how there is now a new THIAmg_combined column, with values filled in
#' # from the THIAmg and THIAHCLmg columns, depending on which is most appropriate.
#' # A comment has also been added to the comments column detailing this change.
#'
#' # This works without any input beyond the data.frame containing the nutrition
#' # information because the function is expecting standard column names, comprised
#' # of the nutrient INFOODS Tagname and the unit. However, if the columns are not
#' # named this way, then the user must specify which column relates to which input.
#' # An example of this is shown below.
#' breakfast_df_nonstandard <- breakfast_df_nonstandard[, c("food_code", "food_name",
#'  "Thiamine_milligrams", "Thiamine_from_HCL_milligrams", "comments_column")]
#' breakfast_df_nonstandard
#'
#' # You can see how the column names are different. The values remain the same
#' # however.
#'
#' # To use these non-standard names, you must specify each input - as shown below.
#'
#' THIA_results_nonstandard <- THIAmg_combiner(breakfast_df_nonstandard,
#' THIAmg_column = "Thiamine_milligrams",
#' THIAHCLmg_column = "Thiamine_from_HCL_milligrams",
#' comment_col = "comments_column")
#'
#' THIA_results_nonstandard
#'
#' # You can see from the results that the calculation is run in exactly the same
#' # way, with the changed column names.
#'
#' @export


THIAmg_combiner <-  function(df,
                             THIAmg_column = "THIAmg",
                             THIAHCLmg_column = "THIAHCLmg",
                             comment = TRUE,
                             comment_col = "comments") {


  # This check makes sure the entered df is a data frame.
  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df))

  #This block of checks throws an error if the entry for the columns is not present in the df.
  stopifnot("The THIAmg_column is not a column name in df - please input a string that is a column name in df, e.g. 'Thiamine_mg'" = THIAmg_column %in% colnames(df))
  stopifnot("The THIAHCLmg_column is not a column name in df - please input a string that is a column name in df, e.g. 'Thiamine_HCL_mg'" = THIAHCLmg_column %in% colnames(df))

  #This checks to make sure logical entries are True or False.
  stopifnot("The comment parameter is not set to TRUE or FALSE - please use TRUE or FALSE" = is.logical(comment))


  df$THIAmg_combined <- NA #Creates the new column, and sets the value to equal to NA

  if (comment == TRUE){
    df$THIAmg_combined_comment_col_temp <- "No suitable value for THIAmg_combined found"
  }


  #Fills values in from THIAHCLmg_column if there are legitimate values there (not NA or blank)

  df[!(df[[THIAHCLmg_column]] %in% "" | is.na(df[[THIAHCLmg_column]])), "THIAmg_combined"] <- df[!(df[[THIAHCLmg_column]] %in% "" | is.na(df[[THIAHCLmg_column]])), THIAHCLmg_column]

  if(comment == TRUE){ #If comments are true, sets the relevant rows to mention they come from var 2
    df[!(df[[THIAHCLmg_column]] %in% "" | is.na(df[[THIAHCLmg_column]])), "THIAmg_combined_comment_col_temp"] <- paste0("THIAmg_combined equal to THIAHCLmg")
  }


  #Then overrides those values if better ones (THIAmg values) are available.

  df[!(df[[THIAmg_column]] %in% "" | is.na(df[[THIAmg_column]])), "THIAmg_combined"] <- df[!(df[[THIAmg_column]] %in% "" | is.na(df[[THIAmg_column]])), THIAmg_column]

  if(comment == TRUE){ #If comments are true, sets the relevant rows to mention they come from var 1
    df[!(df[[THIAmg_column]] %in% "" | is.na(df[[THIAmg_column]])), "THIAmg_combined_comment_col_temp"] <- paste0("THIAmg_combined equal to THIAmg")
  }



  #Then sorts out the comments - depending on whether there is already an existing column or not.

  if (comment == TRUE) {

    message("---------------------------")
    message()
    message("Breakdown of values used:")
    print(table(df$THIAmg_combined_comment_col_temp)) #A pretty acceptable detail message
    message()
    message("---------------------------")

    if(!(comment_col %in% colnames(df))){
      df[[comment_col]] <- comment_message #If the comment column isn't present yet in the data frame, but comments are set to True, then it creates the comment column
    }

    df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col], "; ", df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), "THIAmg_combined_comment_col_temp"])

    #If comment == T and there is already a comment col in the df, but its empty, then this becomes the first entry into the column.
    df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]), comment_col] <- df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]), "THIAmg_combined_comment_col_temp"]

    df$THIAmg_combined_comment_col_temp <- NULL # Remove the temp column
  }

  return(df)

}
