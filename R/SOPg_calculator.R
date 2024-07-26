SOPg_calculator <- function(df,
                           WATERg_column = "WATERg",
                           PROCNTg_column = "PROCNTg",
                           FAT_g_standardised_column = "FAT_g_standardised",
                           CHOAVLDFg_standardised_column = "CHOAVLDFg_standardised",
                           FIBTGg_standardised_column = "FIBTGg_standardised",
                           ALCg_column = "ALCg",
                           ASHg_column = "ASHg",
                           comment = T,
                           comment_col = "comments",
                           OutsideBoundsReplacement = "none",
                           LowerBound = 93,
                           UpperBound = 107,
                           OutsideBoundsDF = F) {

  #' @title Sum of Proximate Calculator
  #' @description Calculates SOPg_calculated = (WATERg + PROCNTg +
  #'   FAT_g_standardised + CHOAVLDFg_standardised + FIBTGg_standardised_column
  #'   + ALCg +ASHg). Column names are case sensitive and an error is returned
  #'   if not found.
  #' @param df Required - the data.frame the data is currently stored in.
  #' @param WATERg_column Required - default: \code{'WATERg'} - The name of the
  #'   column containing Water/moisture content in grams per 100g of Edible
  #'   Portion (EP).
  #' @param PROCNTg_column Required - default: \code{'PROCNTg'} - Protein in
  #'   grams per 100g of Edible Portion (EP), as reported in the original FCT
  #'   and assumed to be calculated from nitrogen (NTg) content.
  #' @param FAT_g_standardised_column Required - default:
  #'   \code{'FAT_g_standardised'} - Fat content, unknown method of calculation,
  #'   in grams per 100g of Edible Portion (EP).
  #' @param CHOAVLDFg_standardised_column Required - default:
  #'   \code{'CHOAVLDFg_standardised'} - Available carbohydrates calculated by
  #'   difference, in grams per 100g of Edible Portion (EP).
  #' @param FIBTGg_standardised_column Required - default:
  #'   \code{'FIBTGg_standardised'} - Fibre content from combined Tagnames, with
  #'   preference of Total dietary fibre by AOAC Prosky method, expressed in
  #'   grams per 100g of Edible Portion (EP).
  #' @param ALCg_column Required - default: \code{'ALCg'} - Alcohol in grams per
  #'   100g of Edible Portion (EP).
  #' @param ASHg_column Required - default: \code{'ASHg'} - Ashes in grams per
  #'   100g of Edible Portion (EP).
  #' @param comment Optional - default: \code{T} - \code{TRUE} or
  #'   \code{FALSE}. If comment is set to \code{TRUE} (as it is by default), when
  #'   the function is run a comment describing the source of the
  #'   \code{SOPg_calculated} column is added to the comment_col. If no
  #'   comment_col is selected, and \code{comment  T}, one is created, called
  #'   \code{comments}.
  #' @param comment_col Optional - default: \code{'comments'} - A potential
  #'   input variable; the column which contains the metadata comments for the
  #'   food item in question. Not required if the comment parameter is set to
  #'   \code{FALSE}.
  #' @return Original FCT dataset with a new SOPg_calculated column.
  #' @param OutsideBoundsReplacement Optional - default: \code{'none'} -
  #'   Options are \code{'round'}, \code{NA}, \code{'remove'}, or
  #'   \code{'none'}. Choose what happens to values that are outside of the
  #'   bounds. The ranges are set to FAO standards: 93-107 is considered
  #'   acceptable. This parameter decides what happens to those values less than
  #'   93, or over 107. If set to \code{round}, then outside of bound values are
  #'   set to the closest acceptable value (e.g. 90 -> 93, 111 -> 107. If set to
  #'   \code{NA}, they are replaced with NA. if set to \code{'remove'}, then
  #'   those rows are removed (including NA results). if set to \code{'none'},
  #'   then they are left as the out of bound values.
  #' @param LowerBound Optional - default: \code{93} - Integer value. Sets the
  #'   lower boundary for acceptable SOPg_calculated values, and therefore
  #'   determines the values affected by \code{OutsideBoundsReplacement} and
  #'   \code{OutsideBoundsDF}. FAO standards list 93 as the lower boundary for
  #'   acceptable values, and 95 as the lower boundary for preferred values.
  #' @param UpperBound Optional - default: \code{107} - Integer value. Sets the
  #'   upper boundary for acceptable SOPg_calculated values, and therefore
  #'   determines the values affected by \code{OutsideBoundsReplacement} and
  #'   \code{OutsideBoundsDF}. FAO standards list 107 as the upper boundary for
  #'   acceptable values, and 105 as the upper boundary for preferred values.
  #' @param OutsideBoundsDF Optional - default: \code{F} - \code{TRUE} or
  #'   \code{FALSE}. If set to \code{TRUE}, Then the output switches from being
  #'   a copy of the input df with the the SOPg_calculated column to a subset
  #'   of that dataframe only showing SOPg_calculated values that are out of
  #'   bounds, for manual inspection.
  #' @examples
  #' # Two example data.frames have been prepared to illustrate the
  #' # SOPg_calculator. The first is a dataset of fictional food values to
  #' # illustrate the various options in the function, and the second is a dataset
  #' # with non-standard column names, to show how to specify columns.
  #'
  #' # This is the first data.frame - before the SOPg_calculator has been used on it.
  #' SOP_example_df
  #' #
  #' #
  #' # First, an example of the standard usecase - calculate the SOPg_calculated
  #' # value, without modifying out of bounds values.
  #' nothing_results <- SOPg_calculator(SOP_example_df, OutsideBoundsReplacement = "none")
  #' # See the changes - the addition of the SOPg_calculated column, and the
  #' # additions to the comments column.
  #' #
  #' #
  #' # The second example shows the results when the Replacement option is set to NA
  #' NA_results <- SOPg_calculator(SOP_example_df, OutsideBoundsReplacement = NA)
  #' # Check the SOP column and comments column again - see how values outside of
  #' # bounds have been replaced with NA, and a note of this change logged in the
  #' # comments column.
  #' #
  #' #
  #' # The third example shows the results when the Replacement option is set to 'remove'
  #' remove_results <- SOPg_calculator(SOP_example_df, OutsideBoundsReplacement = "remove")
  #' # See how the out of bounds values have been removed.
  #' #
  #' #
  #' # The fourth example is of the rounding results.
  #' rounding_results <- SOPg_calculator(SOP_example_df, OutsideBoundsReplacement = "round")
  #' # Look at the SOP_standardised values - and see how they've been capped to the bounds
  #' # if they would have been out fo bounds, with a note of the change in the comments.
  #' #
  #' #
  #' # The fifth example is of the out of bounds dataframe - an option useful for identifying
  #' # and examining out of bounds results.
  #' OoB_DF_results <- SOPg_calculator(SOP_example_df, OutsideBoundsDF = T)
  #' # Only the out of bounds results are present, in their original form, for inspection.
  #' #
  #' #
  #' # The sixth example is of the SOPg_calculator working on a dataframe with non-standard
  #' # column names. It uses a modified example data frame, shown below.
  #' SOP_example_df_nonstandard
  #' # Notice how the column names are different, and differ from the assumed names.
  #' #
  #' #
  #' # Because of the different names, the column names for each input must be specified.
  #' nothing_results_NonStandardInput <- SOPg_calculator(
  #' SOP_example_df_nonstandard,
  #' WATERg_column = "Water_values_g",
  #' PROCNTg_column = "PROCNT_values_g",
  #' FAT_g_standardised_column = "FAT_values_g_standardised",
  #' CHOAVLDFg_standardised_column = "CHOAVLDF_values_g_standardised",
  #' FIBTGg_standardised_column = "FIBTG_values_g_standardised",
  #' ALCg_column = "ALC_values_g",
  #' ASHg_column = "ASH_values_g",
  #' comment_col = "comments_column",
  #' LowerBound = 97,
  #' UpperBound = 103,
  #' OutsideBoundsReplacement = "nothing")
  #' # The resulting SOPg_calculated column is the same as in the first example, despite the
  #' # different names - although, due to the shift in the bounds, the warning message is not.



  # Check presence of required columns

  # Input checks - goes through each input column name, and checks if its a column in the df. If it isn't, it prints an error message and stops.

  # This check makes sure the entered df is a data frame.
  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df))

  #This block of checks throws an error if the entry for the columns is not present in the df.
  stopifnot("The WATERg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column one'." = WATERg_column %in% colnames(df))
  stopifnot("The PROCNTg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column two'." = PROCNTg_column %in% colnames(df))
  stopifnot("The FAT_g_standardised is not a column name in df - please input a string that is a column name in df, e.g. 'column three'." = FAT_g_standardised_column %in% colnames(df))
  stopifnot("The CHOAVLDFg_standardised_column is not a column name in df - please input a string that is a column name in df, e.g. 'column four'." = CHOAVLDFg_standardised_column %in% colnames(df))
  stopifnot("The FIBTGg_std_column is not a column name in df - please input a string that is a column name in df, e.g. 'column five'." = FIBTGg_standardised_column %in% colnames(df))
  stopifnot("The ALCg_columnis not a column name in df - please input a string that is a column name in df, e.g. 'column six'." = ALCg_column %in% colnames(df))
  stopifnot("The ASHg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column seven'." = ASHg_column %in% colnames(df))

  #This block of checks makes sure the columns that are meant to be numeric are numeric.
  stopifnot("The WATERg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[WATERg_column]]))
  stopifnot("The PROCNTg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[PROCNTg_column]]))
  stopifnot("The FAT_g_standardised_column is not numeric. Please ensure it is numeric." = is.numeric(df[[FAT_g_standardised_column]]))
  stopifnot("The CHOAVLDFg_standardised_column is not numeric. Please ensure it is numeric." = is.numeric(df[[CHOAVLDFg_standardised_column]]))
  stopifnot("The FIBTGg_standardised_column is not numeric. Please ensure it is numeric." = is.numeric(df[[FIBTGg_standardised_column]]))
  stopifnot("The ALCg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[ALCg_column]]))
  stopifnot("The ASHg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[ASHg_column]]))

  #This block of checks makes sure the parameters that are meant to be numeric are numeric.
  stopifnot("The LowerBound parameter is not numeric. Please ensure it is numeric." = is.numeric(LowerBound))
  stopifnot("The UpperBound parameter is not numeric. Please ensure it is numeric." = is.numeric(UpperBound))

  #This block checks to make sure logical entries are True or False.
  stopifnot("The comment parameter is not set to TRUE or FALSE - please use TRUE or FALSE, or T or F." = is.logical(comment))
  stopifnot("The OutsideBoundsDF parameter is not set to TRUE or FALSE - please use TRUE or FALSE, or T or F." = is.logical(OutsideBoundsDF))

  #Special check to check the options for the OutsideBoundsReplacement input.
  stopifnot("The OutsideBoundsReplacement parameter is not set to 'round', NA, 'remove' or 'nothing' - please use one of these options." = tolower(OutsideBoundsReplacement) %in% c(NA, "round", "closest", "nearest", "nothing", "none", "n", "rm", "del", "remove", "delete"))



  if(OutsideBoundsDF == T){ #Turns off comments if OutsideBoundsDF is active. This produces a subdataset, without the changes that the comments are recording.
    comment <- F
  }

  df$SOPg_calculated <- NA #This row creates the SOPg_calculated column, and fills it with NA values

  #This adds all the columns together, ignoring NA results
  df$SOPg_calculated <- rowSums(df[, c(
    WATERg_column,
    PROCNTg_column,
    FAT_g_standardised_column,
    CHOAVLDFg_standardised_column,
    FIBTGg_standardised_column,
    ALCg_column,
    ASHg_column
  )], na.rm = T)

  # This checks if any rows were entirely NA values, and sets the SOPg_calculated to NA if so.
  df[is.na(df[[WATERg_column]]) &
       is.na(df[[PROCNTg_column]]) &
       is.na(df[[FAT_g_standardised_column]]) &
       is.na(df[[CHOAVLDFg_standardised_column]]) &
       is.na(df[[FIBTGg_standardised_column]]) &
       is.na(df[[ALCg_column]]) &
       is.na(df[[ASHg_column]]), "SOPg_calculated"] <- NA

  # Inserting comment here

  comment_message <- "SOPg_calculated calculated from adding constituents"

  if (comment == T) {
    if(!(comment_col %in% colnames(df))){
      df[[comment_col]] <- NA #If the comment column isn't present yet, but comments are set to True, then it creates the comment column
    }

    if (tolower(OutsideBoundsReplacement) %in% c("round", "closest", "nearest")){ #If OutsideBoundsReplacement is set to one of the round options, then a special message must appear in specific columns, detailing the original value and that it was reset to the value it was reset to.

      # This is for rows with existing comments, and out of bounds values to the negative
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$SOPg_calculated < LowerBound & !is.na(df$SOPg_calculated), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$SOPg_calculated < LowerBound & !is.na(df$SOPg_calculated), comment_col], "; ", comment_message, " - Original value of ", df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$SOPg_calculated < LowerBound & !is.na(df$SOPg_calculated), "SOPg_calculated"], " reset to ", LowerBound)

      # This is for rows without existing comments, and out of bounds values to the negative
      df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$SOPg_calculated < LowerBound & !is.na(df$SOPg_calculated), comment_col] <- paste0(comment_message, " - Original value of ", df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$SOPg_calculated < LowerBound & !is.na(df$SOPg_calculated), "SOPg_calculated"], " reset to ", LowerBound)

      # This is for rows with existing comments, and out of bounds values to the positive
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$SOPg_calculated > UpperBound & !is.na(df$SOPg_calculated), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$SOPg_calculated > UpperBound & !is.na(df$SOPg_calculated), comment_col], "; ", comment_message, " - Original value of ", df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$SOPg_calculated > UpperBound & !is.na(df$SOPg_calculated), "SOPg_calculated"], " reset to ", UpperBound)

      # This is for rows without existing comments, and out of bounds values to the positive
      df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$SOPg_calculated > UpperBound & !is.na(df$SOPg_calculated), comment_col] <- paste0(comment_message, " - Original value of ", df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$SOPg_calculated > UpperBound & !is.na(df$SOPg_calculated), "SOPg_calculated"], " reset to ", UpperBound)

      # This is for rows with existing comments, and in bounds values
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$SOPg_calculated >= LowerBound & df$SOPg_calculated <= UpperBound & !is.na(df$SOPg_calculated), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$SOPg_calculated >= LowerBound & df$SOPg_calculated <= UpperBound & !is.na(df$SOPg_calculated), comment_col], "; ", comment_message)

      #This is for rows without existing comments, and in bounds values (All other values will have a comment by now)
      df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col] <- paste0(comment_message)


    } else if (is.na(OutsideBoundsReplacement)){ #If OutsideBoundsReplacement is set to NA, then a special message must appear in specific columns, detailing the original value and that it was reset to NA.

      # This is for rows with existing comments, and out of bounds values
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & (df$SOPg_calculated < LowerBound | df$SOPg_calculated > UpperBound) & !is.na(df$SOPg_calculated), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & (df$SOPg_calculated < LowerBound | df$SOPg_calculated > UpperBound) & !is.na(df$SOPg_calculated), comment_col], "; ", comment_message, " - Original value of ", df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & (df$SOPg_calculated < LowerBound | df$SOPg_calculated > UpperBound) & !is.na(df$SOPg_calculated), "SOPg_calculated"], " reset to NA")

      # This is for rows without existing comments, and out of bounds values
      df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & (df$SOPg_calculated < LowerBound | df$SOPg_calculated > UpperBound) & !is.na(df$SOPg_calculated), comment_col] <- paste0(comment_message, " - Original value of ", df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]) & (df$SOPg_calculated < LowerBound | df$SOPg_calculated > UpperBound) & !is.na(df$SOPg_calculated), "SOPg_calculated"], " reset to NA")

      # This is for rows with existing comments, and in bounds values
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$SOPg_calculated >= LowerBound & df$SOPg_calculated <= UpperBound & !is.na(df$SOPg_calculated), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$SOPg_calculated >= LowerBound & df$SOPg_calculated <= UpperBound & !is.na(df$SOPg_calculated), comment_col], "; ", comment_message)

      #This is for rows without existing comments, and in bounds values (All other values will already have a comment)
      df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col] <- paste0(comment_message)


    } else { #If OutsideBoundsReplacement is set to nothing, or delete (the only other valid options), the comments for those values don't matter. All comments are therefore the same - and OoB values do not need a custom message.

      #If comment == T and there is already a comment col in the df, then this appends the message to the existing comments.
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col], "; ", comment_message)

      #If comment == T and there is already a comment col in the df, but its empty, then this becomes the first entry into the column.
      df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]), comment_col] <- paste0(comment_message)
    }
  }

  if (OutsideBoundsDF == F){ #Only produces this message if OutsideBoundsDF is False.
    OutOfBoundsValues <- df[df$SOPg_calculated < LowerBound | df$SOPg_calculated > UpperBound, "SOPg_calculated"] #Sees how many values are out of bounds.

    if(length(OutOfBoundsValues) > 0){ #Triggers a warning if they are present.

      largest_OoB <- max(abs(OutOfBoundsValues-100), na.rm = T) #Finds the highest value.

      message("---------------------------") #Prints a warning message.
      message()
      message(length(OutOfBoundsValues), " SOPg_calculated values calculated to be Out of Bounds (less than ", LowerBound, " or higher than ", UpperBound, "). Largest amount Out of Bounds: ", largest_OoB, ". Please rerun the function with OutsideBoundsDF = T if you wish to inspect these values.")
      message()
      if (tolower(OutsideBoundsReplacement) %in% c("round", "closest", "nearest")){
        message("Out of Bounds values set to closest acceptable value, as per user input.")
      } else if(is.na(OutsideBoundsReplacement)) {
        message("Out of Bounds values set to NA, as per user input.")
      } else if(tolower(OutsideBoundsReplacement) %in% c("nothing", "none", "n")) {
        message("Out of Bounds values left untouched, as per user input.")
      } else { #The only other valid option is for them to be removed.
        message("Out of Bounds value rows removed, as per user input.")
      }
      message()
      message("---------------------------")
    }
  }

  if (OutsideBoundsDF == T){ #Implements the OutsideBoundsDF functionality - stripping to a a df with just OoB SOP values.
    result_df <- df[(df$SOPg_calculated < LowerBound | df$SOPg_calculated > UpperBound) | is.na(df$SOPg_calculated),]
  } else { #Otherwise Goes through the command flow of what to do with OoB values
    if (tolower(OutsideBoundsReplacement) %in% c("round", "closest", "nearest")){
      result_df <- df
      result_df[result_df$SOPg_calculated > UpperBound & !is.na(result_df$SOPg_calculated), "SOPg_calculated"] <- UpperBound
      result_df[result_df$SOPg_calculated < LowerBound & !is.na(result_df$SOPg_calculated), "SOPg_calculated"] <- LowerBound
    } else if (is.na(OutsideBoundsReplacement)){
      result_df <- df
      result_df[(result_df$SOPg_calculated < LowerBound | result_df$SOPg_calculated > UpperBound) & !is.na(result_df$SOPg_calculated), "SOPg_calculated"] <- NA
    } else if (tolower(OutsideBoundsReplacement) %in% c("rm", "del", "remove", "delete")) {
      result_df <- df[df$SOPg_calculated >= LowerBound & df$SOPg_calculated <= UpperBound & !is.na(df$SOPg_calculated),] #Only outputs rows with SOPg_calculated values in the acceptable bounds.
    } else { #The only valid option left is to do nothing - so nothing happens.
      result_df <- df
    }
  }

  return(result_df)

}
