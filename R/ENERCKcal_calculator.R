#---
#Title: ENERCKcal_calculator
#Author: Thomas Codd - https://github.com/TomCodd
#Contributor: Lucia Segovia de la Revilla  - https://github.com/LuciaSegovia
#Version: V1.0.0
#Changelog:
#
#
#Github: https://github.com/TomCodd/NutritionTools
#---

#' @title ENERCKcal calculator
#' A function used to calculate Energy values in kcal
#'
#' @description This function has two modes. Either it works as a basic
#'   calculator - The values for Total Protein in grams, Total Fat in grams
#'   (ideally combined), Available Carbohydrate in grams, Fibre, Total Dietary
#'   in grams and Alcohol in grams are combined to find the Energy in kcal.
#'
#'   Alternatively, this function works as a data.frame calculator. If the
#'   \code{'df'} input is used, then the Nutrient inputs should be the names of
#'   the columns in \code{'df'} which contains the details of their namesakes.
#'   The result of this function will be a new column in df with the
#'   ENERCKcal_calculated values.
#'
#' @param Protein Required - default: \code{"PROTg"} - If using \code{'df'},
#'   then this should be the name of the column that contains Protein values in grams. If
#'   NOT using \code{'df'}, this should be the number or list of numbers
#'   representing Protein values you are looking to calculate an Energy value
#'   using.
#' @param Fat Required - default: \code{"FATg_combined"} - If using \code{'df'},
#'   then this should be the name of the column that contains Total Fat values
#'   in grams. If NOT using \code{'df'}, this should be the number or list of
#'   numbers representing Fat values you are looking to calculate an Energy
#'   value using.
#' @param Carbohydrate Required - default: \code{"CHOAVLg"} - If using
#'   \code{'df'}, then this should be the name of the column that contains
#'   Available Carbohydrate values in grams. If NOT using \code{'df'}, this
#'   should be the number or list of numbers representing Available Carbohydrate
#'   values you are looking to calculate an Energy value using.
#' @param Fibre Required - default: \code{"FIBTGg"} - If using
#'   \code{'df'}, then this should be the name of the column that contains Fibre
#'   values in grams. If NOT using \code{'df'}, this should be the number or
#'   list of numbers representing Fibre values you are looking to calculate an
#'   Energy value using.
#' @param Alcohol Required - default: \code{"ALCg"} - If using \code{'df'}, then
#'   this should be the name of the column that contains Alcohol values in
#'   grams. If NOT using \code{'df'}, this should be the number or list of
#'   numbers representing Alcohol values you are looking to calculate an Energy
#'   value using. Note: This is the only Optional input. All items in all other
#'   nutrient parameters must have a valid value for the calculation - however,
#'   if Stop_If_Missing is set to \code{FALSE} (as it is by default), then
#'   ALcohol values can contain blanks, which will be reset to 0.
#' @param df Optional - If using on a data.frame, then this input should be the
#'   data.frame in question. If left blank, then the standard calculation will
#'   take place instead.
#' @param comment_col Optional - default: \code{"comments"} - only relevant if
#'   \code{'df'} is in use. The name of the comment column in the df. If there
#'   isn't one present, then the function will create one to record the
#'   calculation details.
#' @param Stop_If_Missing Required - default: \code{TRUE} - Either \code{TRUE}
#'   or \code{FALSE}. If set to \code{TRUE}, then the function will stop if it
#'   detects missing values in any of the columns or input values. If set to
#'   \{FALSE}, then the function will not stop if it detects missing values.
#'   Missing values will lead to NA ENERCKcal_calculated values.
#' @param Assume_Zero_Alcohol Required - default: \code{FALSE} - Either
#'   \code{TRUE} or \code{FALSE}. If set to \code{TRUE}, and
#'   \code{'Stop_If_Missing'} is set to \code{FALSE}, then missing alcohol
#'   values will be set to 0, and a comment added if running the data.frame
#'   calculation. if set to \code{FALSE}, then the Alcohol value will not
#'   change.
#' @param Show_NA_Outputs Required - default: \code{TRUE} - Either \code{TRUE}
#'   or \code{FALSE}. If set to \code{TRUE} then if any NA values are generated
#'   for ENERCKcal_calculated, a data.frame of these values will be shown.
#' @return If using the data.frame input, the return will be a data.frame with a
#'   newly generated ENERCKcal_calculated column. If not using the data.frame
#'   input, then the return will be ENERCKcal_calculated values calculated from
#'   the input values put into the function.
#' @export
#'
#' @examples
#' # data.frame calculation: ----
#'
#' # For this example, we will use the breakfast_df example dataset. However,
#' that dataset has been designed to test a range of functions, and in its raw
#' form is incompatable with ENERCKcal_calculator - therefore some changes will
#' have to be made before it can be used.
#'
#' modified_breakfast_df <- breakfast_df[c(1:2, 4:8),] # removing rows with many empty nutrient values
#'
#' modified_breakfast_df
#'
#' modified_breakfast_df <- ENERCKcal_calculator(
#'   df = modified_breakfast_df,
#'   Protein = "PROCNTg",
#'   Fat = "FAT_g_combined",
#'   Carbohydrate = "CHOAVLg",
#'   Fibre = "FIBTGg_combined",
#'   Alcohol = "ALCg"
#' )
#'
#' modified_breakfast_df
#'
#'# However, if some of the options are changed, the function can accept any
#'# input - however, this is not advised. This will result in only
#'# partial coverage of the data.frame, as the things that stopped the function
#'# running before will lead to NA results now.
#'
#'# Before:
#'
#' breakfast_df_1 <- ENERCKcal_calculator(
#'   df = breakfast_df,
#'   Protein = "PROCNTg",
#'   Fat = "FAT_g_combined",
#'   Carbohydrate = "CHOAVLg",
#'   Fibre = "FIBTGg_combined",
#'   Alcohol = "ALCg"
#' )
#'
#' breakfast_df_1
#'
#'# In this case, the missing Alcohol values are preventing the function
#'# running. They have been returned as the df, to allow you to fix these issues
#'# and then run it again, and to allow you to see where this issue is occurring.
#'
#'# After:
#'
#' breakfast_df_2 <- ENERCKcal_calculator(
#'   df = breakfast_df,
#'   Protein = "PROCNTg",
#'   Fat = "FAT_g_combined",
#'   Carbohydrate = "CHOAVLg",
#'   Fibre = "FIBTGg_combined",
#'   Alcohol = "ALCg",
#'   Stop_If_Missing = FALSE
#' )
#'
#' breakfast_df_2
#'
#'# There are NA values for where Alcohol is missing, and where Fat is missing.
#'
#'# This can be further modified, however. If there are missing Alcohol values,
#'# but the rest of the values are there, it is possible to set the function to
#'# set missing alcohol values to 0, through Stop_If_Missing = FALSE and
#'# Assume_Zero_Alcohol = TRUE.
#'
#' modified_breakfast_df_2 <- breakfast_df
#' modified_breakfast_df_2[5, "ALCg"] <- NA
#'
#'# Here we have artificially created this situation. When running the function
#'# with only Stop_If_Missing = FALSE, this is the result:
#'
#'modified_breakfast_df_2_output_1 <- ENERCKcal_calculator(
#'   df = modified_breakfast_df_2,
#'   Protein = "PROCNTg",
#'   Fat = "FAT_g_combined",
#'   Carbohydrate = "CHOAVLg",
#'   Fibre = "FIBTGg_combined",
#'   Alcohol = "ALCg",
#'   Stop_If_Missing = FALSE
#' )
#'
#' modified_breakfast_df_2_output_1
#'
#' # However, if we change the other option as well:
#'
#'modified_breakfast_df_2_output_2 <- ENERCKcal_calculator(
#'   df = modified_breakfast_df_2,
#'   Protein = "PROCNTg",
#'   Fat = "FAT_g_combined",
#'   Carbohydrate = "CHOAVLg",
#'   Fibre = "FIBTGg_combined",
#'   Alcohol = "ALCg",
#'   Stop_If_Missing = FALSE,
#'   Assume_Zero_Alcohol = TRUE
#' )
#'
#' modified_breakfast_df_2_output_2
#'
#' # We can see that the function has run, and the row with the missing Alcohol
#' # value has been able to generate a result - as well as having its ALcohol
#' # value changed. An additional comment has been added to record this change.
#'
#'
#'
#' #Single calculation: ----
#'
#' #Bread, wheat, white, unfortified
#'
#' Protein_value <- 7.5
#' Fat_value <- 1.3
#' Carb_value <- 50.5
#' Fibre_value <- 2.9
#' Alcohol_value <- 0
#'
#' ENERCKcal_calculated <- ENERCKcal_calculator(Protein = Protein_value, Fat = Fat_value,
#' Carbohydrate = Carb_value, Fibre = Fibre_value, Alcohol = Alcohol_value)
#'
#' ENERCKcal_calculated
#'
#' #alternatively:
#'
#' ENERCKcal_calculated_2 <- ENERCKcal_calculator(7.5, 1.3, 50.5, 2.9, 0)
#'
#' ENERCKcal_calculated_2
#'
#' # Or, for multiple values (secondary values are fictional):
#'
#' ENERCKcal_calculated_3 <- ENERCKcal_calculator(c(7.5, 5), c(1.3, 2), c(50.5, 51.5), c(2.9, 5), c(0, 1))
#'
#' ENERCKcal_calculated_3
#'
#' # However, if there are blank values, then the function will not work
#'
#' ENERCKcal_calculated_4 <- ENERCKcal_calculator(c(7.5, 5), c(1.3, 2), c(50.5, 51.5), c(2.9, 5), c(0, NA))
#'
#' # Unless other options are selected - such as Stop_If_Missing is turned off
#'
#' ENERCKcal_calculated_5 <- ENERCKcal_calculator(c(7.5, 5), c(1.3, 2), c(50.5, 51.5), c(2.9, 5), c(0, 1), Stop_If_Missing = FALSE)
#'
#' ENERCKcal_calculated_5
#'
#' # In this format we can apply Assume_Zero_Alcohol as well
#'
#' ENERCKcal_calculated_6 <- ENERCKcal_calculator(c(7.5, 5), c(1.3, 2), c(50.5, 51.5), c(2.9, 5), c(0, 1), Stop_If_Missing = FALSE, Assume_Zero_Alcohol = TRUE)
#'
#' ENERCKcal_calculated_6





ENERCKcal_calculator <- function(Protein = "PROCNTg",
                                   Fat = "FAT_g_combined",
                                   Carbohydrate = "CHOAVLg",
                                   Fibre = "FIBTGg_combined",
                                   Alcohol = "ALCg",
                                   df,
                                   comment_col = "comments",
                                   Stop_If_Missing = TRUE,
                                   Assume_Zero_Alcohol = FALSE,
                                   Show_NA_outputs = TRUE) {



  # df-based code ----

  # Finds if df is used

  if(!missing(df)){

    # And conducts the usual checks if so - make sure df is a data.frame, and the other entries are character strings, and column names.

    if(!is.data.frame(df)){
      message("df input is not a data frame. Please input a data frame.")
      return()
    }

    if(!is.character(Protein)){
      message("Protein is not a character string. Please iput a character string, which is the name of the column that contains Protein values in grams.")
      return()
    }

    if(!is.character(Fat)){
      message("Fat is not a character string. Please iput a character string, which is the name of the column that contains Fat values in grams.")
      return()
      }

    if(!is.character(Carbohydrate)){
      message("Carbohydrate is not a character string. Please iput a character string, which is the name of the column that contains Carbohydrate values in grams.")
      return()
      }

    if(!is.character(Fibre)){
      message("Fibre is not a character string. Please iput a character string, which is the name of the column that contains Fibre values in grams.")
      return()
      }

    if(!is.character(Alcohol)){
      message("Alcohol is not a character string. Please iput a character string, which is the name of the column that contains Alcohol values in grams.")
      return()
      }

    if(!(Protein %in% colnames(df))){
      message("Protein is not a column name in df. Please input the name of the column that contains Protein values in grams.")
      return()
    }

    if(!(Fat %in% colnames(df))){
      message("Protein is not a column name in df. Please input the name of the column that contains Protein values in grams.")
      return()
    }

    if(!(Carbohydrate %in% colnames(df))){
      message("Protein is not a column name in df. Please input the name of the column that contains Protein values in grams.")
      return()
    }

    if(!(Fibre %in% colnames(df))){
      message("Protein is not a column name in df. Please input the name of the column that contains Protein values in grams.")
      return()
    }

    if(!(Alcohol %in% colnames(df))){
      message("Protein is not a column name in df. Please input the name of the column that contains Protein values in grams.")
      return()
    }

    # Creates comments column if one isn't inputted

    if(!comment_col %in% colnames(df)){
      message("No comment column detected. Creating comment column.")
      df[[comment_col]] <- NA
    }

    # Convert columns to numeric
    df[, c(Protein, Fat, Carbohydrate, Fibre, Alcohol)] <- sapply(df[, c(Protein, Fat, Carbohydrate, Fibre, Alcohol)], as.numeric)

    df$TEMPnewcomment <- NA #converts temp comment column

    if(isTRUE(Stop_If_Missing)){ #Initiates check which stop the function if theres things missing, if that option is selected

      Alc_missing <- df[df[[Alcohol]] %in% c(NA, ""),] #subsets to missing values

      if (nrow(Alc_missing)>0){ #if theres anything in that subset, then sends message and stops
        message("Missing alcohol values detected. Please fill in the missing alcohol values, and try again.")
        message("")
        message("Missing alcohol values returned as df.")
        View(Alc_missing)
        return(Alc_missing)
      }

      #The other blocks are the same

      Fib_missing <- df[df[[Fibre]] %in% c(NA, ""),]

      if (nrow(Fib_missing)>0){
        message("Missing Fibre values detected. Please fill in the missing fibre values, and try again.")
        message("")
        message("Missing fibre values returned as df.")
        View(Fib_missing)
        return(Fib_missing)
      }

      Fat_missing <- df[df[[Fat]] %in% c(NA, ""),]

      if (nrow(Fat_missing)>0){
        message("Missing Fat values detected. Please fill in the missing fat values, and try again.")
        message("")
        message("Missing Fat values returned as df.")
        View(Fat_missing)
        return(Fat_missing)
      }

      Prot_missing <- df[df[[Protein]] %in% c(NA, ""),]

      if (nrow(Prot_missing)>0){
        message("Missing Protein values detected. Please fill in the missing Protein values, and try again.")
        message("")
        message("Missing Protein values returned as df.")
        View(Prot_missing)
        return(Prot_missing)
      }

      Carbs_missing <- df[df[[Carbohydrate]] %in% c(NA, ""),]

      if (nrow(Carbs_missing)>0){
        message("Missing Carbohydrate values detected. Please fill in the missing Carbohydrate values, and try again.")
        message("")
        message("Missing Carbohydrate values returned as df.")
        View(Carbs_missing)
        return(Carbs_missing)
      }
    } #stop if missing block ends


    #If a return hasn't been given above, then either the blanks should be set
    #to 0, or there aren't any All blank values should be converted to NA by the
    #conversion to numeric, so only looking for NA

    # Alcohol conversion begins
    if(isTRUE(Assume_Zero_Alcohol)){
      # Resets missing alcohol value to 0, if its blank.
      df[is.na(df[[Alcohol]]), "TEMPnewcomment"] <- "Missing Alcohol value set to 0"
      df[is.na(df[[Alcohol]]), Alcohol] <- 0
    }


    # Actual calculation takes place

    df$ENERCKcal_calculated <- df[[Protein]]*4 + df[[Fat]]*9 + df[[Carbohydrate]]*4 + df[[Fibre]]*2 + df[[Alcohol]]*7

    # adding temp comment column
    df$TEMPnewcomment <- paste0(df$TEMPnewcomment, "; ENERCKcal_calculated calculated to be ", df$ENERCKcal_calculated, " through ", Protein, "*4 + ", Fat, "*9 + ", Carbohydrate, "*4 + ", Fibre, "*2 + ", Alcohol, "*7")

    # This will introduce a comment which starts with "NA; " if theres no Alcohol reset comment.
    # Looking for comments which contain that, and removing them. Could be applied more widely if we were certain that blank comments only containted NA.
      df$TEMPnewcomment <- gsub("^NA; ", "", df$TEMPnewcomment)


    # Now need to work out which comments are blank and which aren't
    # First looking for filled comments
    df[!(df[[comment_col]] %in% c("", NA)), comment_col] <- paste0(df[!(df[[comment_col]] %in% c("", NA)), comment_col], "; ", df[!(df[[comment_col]] %in% c("", NA)), "TEMPnewcomment"])

    # Then for new comments
    df[df[[comment_col]] %in% c("", NA), comment_col] <- df[df[[comment_col]] %in% c("", NA), "TEMPnewcomment"]

    # Then deleting the temp column
    df$TEMPnewcomment <- NULL

    # This might produce erroneous values if Stop_If_Missing isn't active.
    if(isTRUE(Show_NA_outputs)){
      if(nrow(df[is.na(df$ENERCKcal_calculated),])>0){
        message("NA output values found. Viewing ENERCKcal_calculated values of NA.")
        View(df[is.na(df$ENERCKcal_calculated),])
      }
    }

    #Then returning the completed df
    return(df)

  } else {

    # non-df-based code ----

    # Converts to numeric

    if(!is.numeric(Protein)){
      message("Protein is not numeric. Attempting to convert. Please check outputs thoroughly.")
      Protein <- as.numeric(Protein)
    }

    if(!is.numeric(Fat)){
      message("Fat is not numeric. Attempting to convert. Please check outputs thoroughly.")
      Fat <- as.numeric(Fat)
    }

    if(!is.numeric(Carbohydrate)){
      message("Carbohydrate is not numeric. Attempting to convert. Please check outputs thoroughly.")
      Carbohydrate <- as.numeric(Carbohydrate)
    }

    if(!is.numeric(Fibre)){
      message("Fibre is not numeric. Attempting to convert. Please check outputs thoroughly.")
      Fibre <- as.numeric(Fibre)
    }

    if(!is.numeric(Alcohol)){
      message("Protein is not numeric. Attempting to convert. Please check outputs thoroughly.")
      Alcohol <- as.numeric(Alcohol)
    }

    if(isTRUE(Stop_If_Missing)){ #Goes through the code which stops it if components are missing
      if(NA %in% Alcohol | "" %in% Alcohol){
        message("NA/'' detected in Alcohol. Stopping. Please ensure all items have Alcohol values.")
        return()
      }
      if(NA %in% Fibre | "" %in% Fibre){
        message("NA/'' detected in Fibre. Stopping. Please ensure all items have Fibre values.")
        return()
      }
      if(NA %in% Protein | "" %in% Protein){
        message("NA/'' detected in Protein. Stopping. Please ensure all items have Protein values.")
        return()
      }
      if(NA %in% Fat | "" %in% Fat){
        message("NA/'' detected in Fat. Stopping. Please ensure all items have Fat values.")
        return()
      }
      if(NA %in% Carbohydrate | "" %in% Carbohydrate){
        message("NA/'' detected in Carbohydrate. Stopping. Please ensure all items have Carbohydrate values.")
        return()
      }
    }

    # If the function has got to this point, then blank values haven't been
    # allowed to stop it - so blanks should be removed. text and blank values
    # will have converted to NA by this point.



    if(isTRUE(Assume_Zero_Alcohol)){
      # Resets missing alcohol value to 0, if its blank.
      Alcohol[is.na(Alcohol)] <- 0
    }

    # Fibre[is.na(Fibre)] <- 0 # Leaving it as just Alcohol - the rest are too important to miss
    # Fat[is.na(Fat)] <- 0
    # Carbohydrate[is.na(Carbohydrate)] <- 0
    # Protein[is.na(Protein)] <- 0


    ENERCKcal_calculated <- Protein*4 + Fat*9 + Carbohydrate*4 + Fibre*2 + Alcohol*7
    return(ENERCKcal_calculated)
  }
}
