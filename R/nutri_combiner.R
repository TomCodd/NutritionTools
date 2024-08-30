#---
# Title: Multi-column Nutrient Combiner
# Author: Thomas Codd - https://github.com/TomCodd
# Contributor: Lucia Segovia de la Revilla  - https://github.com/LuciaSegovia
# Version: V1.1.0
# Changelog:
# V1.0.0 -> V1.0.1: Changed due to error in the documentation examples.
# V1.0.1 -> V1.1.0: Added the fill_missing functionality.
# Github: https://github.com/TomCodd/NutritionTools
#---

#' @title Multi-column Nutrient Combiner
#' @description Combines nutrients or variables that are spread out over
#'   multiple columns into a single new column \code{new_var}, depending on a
#'   user-set hierarchy. The hierarchy is set so that \code{var1_column} is
#'   the main variable, and the priority. If no values for \code{var1_column}
#'   are available (i.e. the \code{var1_column} has blanks, or NA values),
#'   then values from \code{var2_column} are used instead. If there are still
#'   blanks, then values from \code{var3_column} are used, then
#'   \code{var4_column}, then \code{var5_column} and finally
#'   \code{var6_column}. Please note - the use of \code{var3_column} -
#'   \code{var6_column} are optional, however \code{var1_column} and
#'   \code{var2_column} must be present. Comments can also be used to record
#'   the origin of these values.
#' @param df Required - the data.frame the data is currently stored in.
#' @param var1_column Required - The column name of the primary variable to
#'   pull values from. This should be the variable you most want to use.
#' @param var2_column Required - The column name of the secondary variable to
#'   pull values from. This should be the variable you most want to use, if
#'   you can't use \code{var1_column}.
#' @param var3_column Optional - The column name of the tertiary variable to
#'   pull values from. This should be the variable you most want to use, if
#'   you can't use \code{var1_column} or \code{var2_column}.
#' @param var4_column Optional - The column name of the fourth most
#'   appropriate variable to pull values from. This should be the next most
#'   appropriate variable after the ones selected for \code{var1_column},
#'   \code{var2_column}, and \code{var3_column}.
#' @param var5_column Optional - The column name of the fifth most appropriate
#'   variable to pull values from, after the columns selected for
#'   \code{var1_column} to \code{var4_column}.
#' @param var6_column Optional - The column name of the sixth variable. This
#'   should be the least appropriate variable to use, as it will only be used
#'   if a value cannot be found using \code{var1_column} to
#'   \code{var5_column}.
#' @param new_var Required - The name of the new column that will be created
#'   by combining the variable columns. It is recommended to use the
#'   nutrient's INFOODS Tagname, followed by the units - e.g. Thiamine in
#'   milligrams would be 'THIAmg'. The suffix '_combined' is automatically
#'   attached to the inputted name.
#' @param fill_missing Optional - default: \code{FALSE} - \code{TRUE} or
#'   \code{FALSE}. If set to \code{TRUE}, this will cause the nutri_combiner to
#'   check for missing columns (or inputs that don't match columns in the df).
#'   If it finds them, instead of throwing an error as it normally would, the
#'   function removes the ones which aren't valid, and then fills in the
#'   variables in the correct order out of the remaining valid column names.
#' @param comment Required - default: \code{TRUE} - \code{TRUE} or \code{FALSE}.
#'   If comment is set to \code{TRUE} (as it is by default), when the function
#'   is run a comment describing the source of \code{new_var} column is added
#'   to the comment_col. If no comment_col is selected, and \code{comment =
#'   TRUE}, one is created.
#' @param comment_col Optional - default: \code{'comments'} - A potential
#'   input variable; the column which contains the metadata comments for the
#'   food item in question. Not required if the comment parameter is set to
#'   \code{FALSE}. If set to true, and the comment_col entry is not found in
#'   the df, it will create a column with the name of the entry.
#' @return Original data.frame with a new \code{_combined} column, and
#'   (depending on the options selected) an additional comment/comments column
#'   and comment.
#' @examples
#' # An example data.frame has been created to give an example of using the
#' # nutri_combiner to combine FAT values.
#' breakfast_df <- breakfast_df[, c("food_code", "food_name", "FATg", "FAT_g",
#' "FATCEg", "comments")]
#' breakfast_df
#'
#' # We start with a data.frame containing multiple patchy values for fat. Ideally
#' # we would like to combine these into a single 'combined' column with as few gaps
#' # as possible. We would like to use FATg as the main value, and then fill in with
#' # FAT_g as a second choice, and then FATCEg as a last resort. We would like the
#' # new column to be called 'FAT_g_combined'.
#' #
#' # In this case, the following nutri_combiner input would be used:
#'
#' Fat_combined_results <- nutri_combiner(
#'   breakfast_df,
#'   "FATg",
#'   "FAT_g",
#'   "FATCEg",
#'   new_var = "FAT_g")
#'
#' Fat_combined_results
#'
#' # Note how the values are filled in according to the priority order - with
#' # a note added to the comments column showing the origins for each.
#'
#' # As an example of the fill_missing function, see what happens when the
#' # function is run with an incorrect column entered:
#'
#' Fat_combined_results_2 <- nutri_combiner(
#'   breakfast_df,
#'   "FATg",
#'   "FAT_g",
#'   "NonExistant_Fat_Value",
#'   "FATCEg",
#'   new_var = "FAT_g",
#'   fill_missing = TRUE)
#'
#' Fat_combined_results_2
#'
#' # See how the columns reorder, with notice.
#'
#' @export

nutri_combiner <-  function(df,
                            var1_column,
                            var2_column,
                            var3_column,
                            var4_column,
                            var5_column,
                            var6_column,
                            new_var,
                            fill_missing = FALSE,
                            comment = TRUE,
                            comment_col = "comments") {


  # This check makes sure the entered df is a data frame.
  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df))

  #This checks to see if the two required columns are present.
  stopifnot("var1_column is missing. Please put the name of a column in df as the input for var1_column" = !missing(var1_column))
  stopifnot("var2_column is missing. Please put the name of a column in df as the input for var2_column" = !missing(var2_column))


  #This checks if the columns need to be reordered. This only happens if fill_missing = TRUE.
  #If true, it will fill the columns if one is missing. e.g. if the entry for var2 isn't present in
  #the df, then var3 will move to fill in var2, if available.

  if(fill_missing == TRUE){ #checks if this input is selected - its not by default.

    variable_list <- c(var1_column, var2_column) #creates list of the two columns that must be there.

    #These lines check if the optional columns are present, and fills them if so.
    if(!missing(var3_column)){
      variable_list <- c(variable_list, var3_column)
    }
    if(!missing(var4_column)){
      variable_list <- c(variable_list, var4_column)
    }
    if(!missing(var5_column)){
      variable_list <- c(variable_list, var5_column)
    }
    if(!missing(var6_column)){
      variable_list <- c(variable_list, var6_column)
    }


    #Checks inputs against the column names in df - creates a list of inputs which don't match, and inputs that do
    columnnames <- colnames(df)
    missing_variables <- variable_list[!variable_list %in% columnnames]
    present_variables <- variable_list[variable_list %in% columnnames]

    if(length(missing_variables)>0){ #If any inputs don't match, gives a warning message, then tries to fill gaps
      message(paste0("Error - the following columns are not present in df. nutri-combiner will attempt to shift variables to fill the gap in the heirachy, if present."))
      message(paste0(missing_variables, collapse = ", "))

      if(length(present_variables)<2){
        stop("Error - less than 2 valid column names found. nutri_combiner needs at least 2 valid columns to run.")
      }

      for(i in 1:length(present_variables)){ #loops through the number of correctly assigned variables
        eval(parse(text = paste0("var", i, "_column <- present_variables[", i, "]"))) #And overwrites the input variables with the new value
      }

      unused_variable_placements <- 6-length(present_variables) #works out how many variables are meant to be empty/missing in the new order

      for(i in 1:unused_variable_placements){
        eval(parse(text = paste0("var", length(present_variables)+i, "_column <- quote(expr = )"))) #Sets the variable to missing
      }
    }
  }


  #This block of checks throws an error if the entry for the columns is not present in the df.
  stopifnot("The var1_column is not a column name in df - please input a string that is a column name in df, e.g. 'THIAmg'" = var1_column %in% colnames(df))
  stopifnot("The var2_column is not a column name in df - please input a string that is a column name in df, e.g. 'THIAHCLmg'" = var2_column %in% colnames(df))

  #This checks to make sure logical entries are True or False.
  stopifnot("The comment parameter is not set to TRUE or FALSE - please use TRUE or FALSE" = is.logical(comment))

  #This makes sure the new variable isn't missing.
  stopifnot("The new_var variable is not set. Please use it, inputting the name of the new column you would like to combine the values into; e.g. 'THIAmg'. Please note that '_combined' is automatically included as a suffix; e.g. 'THIAmg' would result in 'THIAmg_combined'" = !missing(new_var))

  new_var <- paste0(new_var, "_combined")

  df[[new_var]] <- NA #Creates the new column, and sets the value to equal to NA

  if (comment == TRUE){
    df$nutri_combiner_comment_col_temp <- paste0("No suitable value for ", new_var, " found")
  }

  if(!missing(var6_column)){ #Starts with Var 6, if present
    stopifnot("The var6_column is in use, but the input is not a column name in df - please input a string that is a column name in df, e.g. 'FATRNPg'" = var6_column %in% colnames(df)) #Checks to see if the column name is in the df
    stopifnot("var6_column is in use, but var3_column is missing. Please use var3_column first." = !missing(var3_column)) #Checks to make sure all previous variables are filled
    stopifnot("var6_column is in use, but var4_column is missing. Please use var4_column first." = !missing(var4_column))
    stopifnot("var6_column is in use, but var5_column is missing. Please use var5_column first." = !missing(var5_column))


    df[!(df[[var6_column]] %in% "" | is.na(df[[var6_column]])), new_var] <- df[!(df[[var6_column]] %in% "" | is.na(df[[var6_column]])), var6_column] #Where var 6 is not NA or blank, sets new_variable to be that value.

    if(comment == TRUE){ #If comments are true, sets the relevant rows to mention they come from var 6
      df[!(df[[var6_column]] %in% "" | is.na(df[[var6_column]])), "nutri_combiner_comment_col_temp"] <- paste0(new_var, " equal to ", var6_column)
    }
  }

  if(!missing(var5_column)){ #Then does Var 5, if present
    stopifnot("The var5_column is in use, but the input is not a column name in df - please input a string that is a column name in df, e.g. 'FATRNg'" = var5_column %in% colnames(df)) #Checks to see if the column name is in the df
    stopifnot("var5_column is in use, but var3_column is missing. Please use var3_column first." = !missing(var3_column)) #Checks to make sure all previous variables are filled
    stopifnot("var5_column is in use, but var4_column is missing. Please use var4_column first." = !missing(var4_column))


    df[!(df[[var5_column]] %in% "" | is.na(df[[var5_column]])), new_var] <- df[!(df[[var5_column]] %in% "" | is.na(df[[var5_column]])), var5_column] #Where var 5 is not NA or blank, sets new_variable to be that value.

    if(comment == TRUE){ #If comments are true, sets the relevant rows to mention they come from var 5
      df[!(df[[var5_column]] %in% "" | is.na(df[[var5_column]])), "nutri_combiner_comment_col_temp"] <- paste0(new_var, " equal to ", var5_column)
    }
  }

  if(!missing(var4_column)){ #Then does Var 4, if present
    stopifnot("The var4_column is in use, but the input is not a column name in df - please input a string that is a column name in df, e.g. 'FASATg'" = var4_column %in% colnames(df)) #Checks to see if the column name is in the df
    stopifnot("var4_column is in use, but var3_column is missing. Please use var3_column first." = !missing(var3_column)) #Checks to make sure the previous variable is filled


    df[!(df[[var4_column]] %in% "" | is.na(df[[var4_column]])), new_var] <- df[!(df[[var4_column]] %in% "" | is.na(df[[var4_column]])), var4_column] #Where var 4 is not NA or blank, sets new_variable to be that value.

    if(comment == TRUE){ #If comments are true, sets the relevant rows to mention they come from var 4
      df[!(df[[var4_column]] %in% "" | is.na(df[[var4_column]])), "nutri_combiner_comment_col_temp"] <- paste0(new_var, " equal to ", var4_column)
    }
  }

  if(!missing(var3_column)){ #Then does Var 3, if present
    stopifnot("The var3_column is in use, but the input is not a column name in df - please input a string that is a column name in df, e.g. 'FAT_g'" = var3_column %in% colnames(df)) #Checks to see if the column name is in the df

    df[!(df[[var3_column]] %in% "" | is.na(df[[var3_column]])), new_var] <- df[!(df[[var3_column]] %in% "" | is.na(df[[var3_column]])), var3_column] #Where var 3 is not NA or blank, sets new_variable to be that value.

    if(comment == TRUE){ #If comments are true, sets the relevant rows to mention they come from var 3
      df[!(df[[var3_column]] %in% "" | is.na(df[[var3_column]])), "nutri_combiner_comment_col_temp"] <- paste0(new_var, " equal to ", var3_column)
    }
  }

  #Then does Var 2 - which must be there.

  df[!(df[[var2_column]] %in% "" | is.na(df[[var2_column]])), new_var] <- df[!(df[[var2_column]] %in% "" | is.na(df[[var2_column]])), var2_column] #Where var 2 is not NA or blank, sets new_variable to be that value.

  if(comment == TRUE){ #If comments are true, sets the relevant rows to mention they come from var 2
    df[!(df[[var2_column]] %in% "" | is.na(df[[var2_column]])), "nutri_combiner_comment_col_temp"] <- paste0(new_var, " equal to ", var2_column)
  }


  #Then does Var 1 - which must be there.

  df[!(df[[var1_column]] %in% "" | is.na(df[[var1_column]])), new_var] <- df[!(df[[var1_column]] %in% "" | is.na(df[[var1_column]])), var1_column] #Where var 1 is not NA or blank, sets new_variable to be that value.

  if(comment == TRUE){ #If comments are true, sets the relevant rows to mention they come from var 1
    df[!(df[[var1_column]] %in% "" | is.na(df[[var1_column]])), "nutri_combiner_comment_col_temp"] <- paste0(new_var, " equal to ", var1_column)
  }


  #Then sorts out the comments - depending on whether there is already an existing column or not.

  if (comment == TRUE) {

    message("---------------------------")
    message()
    message("Breakdown of values used:")
    print(table(df$nutri_combiner_comment_col_temp)) #A pretty acceptable detail message
    message()
    message("---------------------------")

    if(!(comment_col %in% colnames(df))){
      df[[comment_col]] <- NA #If the comment column isn't present yet in the data frame, but comments are set to True, then it creates the comment column
    }

    df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col], "; ", df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), "nutri_combiner_comment_col_temp"])

    #If comment == T and there is already a comment col in the df, but its empty, then this becomes the first entry into the column.
    df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]), comment_col] <- df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]), "nutri_combiner_comment_col_temp"]

    df$nutri_combiner_comment_col_temp <- NULL # Remove the temp column
  }
  return(df)
}
