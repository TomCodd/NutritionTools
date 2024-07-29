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




SOP_std_creator_2 <- function(df,
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

  # Inserting comment here

  comment_message <- "SOPg_standardised calculated by adding constituents"

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



# ¬ New Version ----


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
  #' View(SOP_example_df)
  #'
  #' # First, an example of the standard usecase - calculate the SOPg_calculated
  #' # value, without modifying out of bounds values.
  #' nothing_results <- SOPg_calculator(SOP_example_df, OutsideBoundsReplacement = "none")
  #' View(nothing_results)
  #' # See the changes - the addition of the SOPg_calculated column, and the
  #' # additions to the comments column.
  #' #
  #' #
  #' # The second example shows the results when the Replacement option is set to NA
  #' NA_results <- SOPg_calculator(SOP_example_df, OutsideBoundsReplacement = NA)
  #' View(NA_results)
  #' # Check the SOP column and comments column again - see how values outside of
  #' # bounds have been replaced with NA, and a note of this change logged in the
  #' # comments column.
  #' #
  #' #
  #' # The third example shows the results when the Replacement option is set to 'remove'
  #' remove_results <- SOPg_calculator(SOP_example_df, OutsideBoundsReplacement = "remove")
  #' View(remove_results)
  #' # See how the out of bounds values have been removed.
  #' #
  #' #
  #' # The fourth example is of the rounding results.
  #' rounding_results <- SOPg_calculator(SOP_example_df, OutsideBoundsReplacement = "round")
  #' View(rounding_results)
  #' # Look at the SOP_standardised values - and see how they've been capped to the bounds
  #' # if they would have been out fo bounds, with a note of the change in the comments.
  #' #
  #' #
  #' # The fifth example is of the out of bounds dataframe - an option useful for identifying
  #' # and examining out of bounds results.
  #' OoB_DF_results <- SOPg_calculator(SOP_example_df, OutsideBoundsDF = TRUE)
  #' View(OoB_DF_results)
  #' # Only the out of bounds results are present, in their original form, for inspection.
  #' #
  #' #
  #' # The sixth example is of the SOPg_calculator working on a dataframe with non-standard
  #' # column names. It uses a modified example data frame, shown below.
  #' View(SOP_example_df_nonstandard)
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
  #' View(nothing_results_NonStandardInput)
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

# ¬ Speed Testing ----

test_df <- read.csv("~/GitHub/UoN-FAO/Output/Global_nct_imitation_v1.0.2.csv")

test_df$CHOAVLDFg_standardised <- test_df$CHOAVLg

colnames(test_df)[colnames(test_df) == 'FATg'] <- 'FAT_g_standardised'
colnames(test_df)[colnames(test_df) == 'FIBTGg_standardised'] <- 'FIBTGg_std'

library(dplyr)
time_1 <- Sys.time()

old_method_output <- SOP_std_creator(test_df)

time_2 <- Sys.time()

new_method_output <- SOPg_calculator(test_df, FIBTGg_standardised_column = "FIBTGg_std")

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

# ¬ Speed Test results ----

#switching to stopifnot gives us more control, and has no affect on benchmarking.

# New method seems to be 72-131x faster, before comments col added.
# Old method results: 0.136317 , 0.1248879 , 0.2551999
# New method results: 0.001843929 , 0.001731157 , 0.001951218

#With addition of comments its still ~24x faster

# ¬ SOPg EXAMPLES ----


SOP_example_df <- data.frame(food_code = c("F0001", "F0002", "F0003", "F0004", "F0005", "F0006", "F0007", "F0008", "F0009", "F0010"),
                                WATERg = c(10, 15, 20, 25, 30, 35, 40, NA, NA, NA),
                                PROCNTg = c(35, 20, 20, 20, 31, 50, 10, 27, NA, NA),
                                FAT_g_standardised = c(1, 2, NA, 7, 1, 3, 2, 6, NA, NA),
                                CHOAVLDFg_standardised = c(10, 1, 24, 50, 20, 10, 25, 32, NA, NA),
                                FIBTGg_standardised = c(12, 3, 8, 15, 6, 2, 9, 13, NA, NA),
                                ALCg = c(12, 43, 8, 15, 6, 2, 9, 13, NA, NA),
                                ASHg = c(12, 3, 28, 15, 6, 2, 9, 13, NA, NA),
                                comments = c("", "These are imaginary food items", NA, "With imaginary nutrient values", "", "And blanks", NA, "To test different outputs", "", "And scenarios"))

SOP_example_df_nonstandard <- data.frame(food_code = c("F0001", "F0002", "F0003", "F0004", "F0005", "F0006", "F0007", "F0008", "F0009", "F0010"),
                             Water_values_g = c(10, 15, 20, 25, 30, 35, 40, NA, NA, NA),
                             PROCNT_values_g = c(35, 20, 20, 20, 31, 50, 10, 27, NA, NA),
                             FAT_values_g_standardised = c(1, 2, NA, 7, 1, 3, 2, 6, NA, NA),
                             CHOAVLDF_values_g_standardised = c(10, 1, 24, 50, 20, 10, 25, 32, NA, NA),
                             FIBTG_values_g_standardised = c(12, 3, 8, 15, 6, 2, 9, 13, NA, NA),
                             ALC_values_g = c(12, 43, 8, 15, 6, 2, 9, 13, NA, NA),
                             ASH_values_g = c(12, 3, 28, 15, 6, 2, 9, 13, NA, NA),
                             comments_column = c("", "These are imaginary food items", NA, "With imaginary nutrient values", "", "And blanks", NA, "To test different outputs", "", "And scenarios"))


# This line creates a dataset in the correct format for a package
#usethis::use_data(SOP_example_df)
#usethis::use_data(SOP_example_df_nonstandard)



none_results_2 <- SOP_calculator(SOP_example_df, OutsideBoundsReplacement = "none")

nothing_results_NonStandardInput <- SOP_std_creator_New( #An example of using the SOPg_std_creator on a data.frame with non-standard column names. Because the column names are non-standard, the function needs telling which columns refer to which input.
  SOP_example_df_nonstandard,
  WATERg_column = "Water_values_g",
  PROCNTg_column = "PROCNT_values_g",
  FAT_g_standardised_column = "FAT_values_g_standardised",
  CHOAVLDFg_standardised_column = "CHOAVLDF_values_g_standardised",
  FIBTGg_standardised_column = "FIBTG_values_g_standardised",
  ALCg_column = "ALC_values_g",
  ASHg_column = "ASH_values_g",
  comment_col = "comments_column",
  LowerBound = 97,
  UpperBound = 103,
  OutsideBoundsReplacement = "nothing"
)


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
    #print(i)
    if (!is.na(data.df[i, var1])) {
      #print(!is.na(data.df[i, var1]))
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
    #print(data.df[i, new_var])
  }

  return(data.df)

}


# ¬ New Version ----

nutri_combiner_new <-  function(df, var1_column, var2_column, var3_column, var4_column, var5_column, var6_column, new_var, comment = TRUE, comment_col = "comments"){

  #' @title Multi-column Nutrient Combiner
  #' @description Combines nutrients or variables that are spread out over multiple columns into a single new column \code{new_var}, depending on a user-set hierarchy. The hierarchy is set so that \code{var1_column} is the main
  #' variable, and the priority. If no values for \code{var1_column} are available (i.e. the \code{var1_column} has blanks, or NA values), then values from \code{var2_column} are used instead. If there are still blanks,
  #' then values from \code{var3_column} are used, then \code{var4_column}, then \code{var5_column} and finally \code{var6_column}. Please note - the use of \code{var3_column} - \code{var6_column} are optional, however
  #' \code{var1_column} and \code{var2_column} must be present. Comments can also be used to record the origin of these values.
  #' @param df Required - the data.frame the data is currently stored in.
  #' @param var1_column Required - The column name of the primary variable to pull values from. This should be the variable you most want to use.
  #' @param var2_column Required - The column name of the secondary variable to pull values from. This should be the variable you most want to use, if you can't use \code{var1_column}.
  #' @param var3_column Optional - The column name of the tertiary variable to pull values from. This should be the variable you most want to use, if you can't use \code{var1_column} or \code{var2_column}.
  #' @param var4_column Optional - The column name of the fourth most appropriate variable to pull values from. This should be the next most appropriate variable after the ones selected for \code{var1_column}, \code{var2_column}, and \code{var3_column}.
  #' @param var5_column Optional - The column name of the fifth most appropriate variable to pull values from, after the columns selected for \code{var1_column} to \code{var4_column}.
  #' @param var6_column Optional - The column name of the sixth variable. This should be the least appropriate variable to use, as it will only be used if a value cannot be found using \code{var1_column} to \code{var5_column}.
  #' @param new_var Required - The name of the new column that will be created by combining the variable columns.
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
  #' @return Original FCT dataset with a new column.


  # This check makes sure the entered df is a data frame.
  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df))

  #This block of checks throws an error if the entry for the columns is not present in the df.
  stopifnot("The var1_column is not a column name in df - please input a string that is a column name in df, e.g. 'FATg'" = var1_column %in% colnames(df))
  stopifnot("The var2_column is not a column name in df - please input a string that is a column name in df, e.g. 'FATCEg'" = var2_column %in% colnames(df))

  #This checks to make sure logical entries are True or False.
  stopifnot("The comment parameter is not set to TRUE or FALSE - please use TRUE or FALSE" = is.logical(comment))

  #This makes sure the new variable isn't missing.
  stopifnot("The new_var variable is not set. Please use it, inputting the name of the new column you would like to combine the values into; e.g. 'FAT_g_standardised'" = !missing(new_var))

  df[[new_var]] <- NA #Creates the new column, and sets the value to equal to NA

  if (comment == TRUE){
    df$nutri_combiner_comment_col_temp <- NA
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
    if(!(comment_col %in% colnames(df))){
      df[[comment_col]] <- comment_message #If the comment column isn't present yet in the data frame, but comments are set to True, then it creates the comment column
    }

    df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col], "; ", df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), "nutri_combiner_comment_col_temp"])

    #If comment == T and there is already a comment col in the df, but its empty, then this becomes the first entry into the column.
    df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]), comment_col] <- df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]), "nutri_combiner_comment_col_temp"]

    df$nutri_combiner_comment_col_temp <- NULL # Remove the temp column
  }
  return(df)
}

# ¬ nutri-combiner testing ----

# ¬¬ Parity and speed testing ----

test_df <- read.csv("~/GitHub/UoN-FAO/Output/Global_nct_imitation_v1.0.2.csv")

time_1 <- Sys.time()

old_method_output <- nutri_combiner(test_df, "FATg", "FAT_g", "FATCEg", "old_FAT_standardisation")

time_2 <- Sys.time()

new_method_output <- nutri_combiner_new(test_df, "FATg", "FAT_g", "FATCEg", new_var = "new_FAT_standardisation")

time_3 <- Sys.time()

time_2 - time_1

time_3 - time_2

parity_test <- signif(old_method_output$old_FAT_standardisation, 10) == signif(new_method_output$new_FAT_standardisation, 10)

print(table(parity_test))

# RESULTS

# All good on the parity. All the same.
# Speed dramatically improved. Old method: 1.61-1.79 seconds, New method: 0.0320 - 0.0339 seconds. 51 times faster.

# ¬¬ Comment and heirachy testing ----






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


CHOAVLDFg_calculator <- function(df,
                                WATERg_column = "WATERg",
                                PROCNTg_column = "PROCNTg",
                                FAT_g_standardised_column = "FAT_g_standardised",
                                FIBTGg_standardised_column = "FIBTGg_standardised",
                                ALCg_column = "ALCg",
                                ASHg_column = "ASHg",
                                comment = TRUE,
                                comment_col = "comments",
                                NegativeValueReplacement = 0,
                                NegativeValueDF = FALSE) {

  #' @title Carbohydrates (calculated by difference) Calculator
  #' @description Calculates CHOAVLDFg_calculated = (100 - (WATERg + PROTg +
  #'   FATg_standardised + FBGTg + ASHg + ALCg)). Column names are case
  #'   sensitive and an error is returned if not found.
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
  #' @param FIBTGg_standardised_column Required - default:
  #'   \code{'FIBTGg_standardised'} - Fibre content from combined Tagnames, with
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
  #' @return Original FCT dataset with a new \code{CHOAVLDFg_calculated}
  #'   column.
  #' @examples
  #' # Two example data.frames have been prepared to illustrate the
  #' # CHOAVLDFg_calculator The first is a dataset of fictional food values to
  #' # illustrate the various options in the function, and the second is a dataset
  #' # with non-standard column names, to show how to specify columns.
  #'
  #' # This is the first data.frame - before the CHOAVLDFg_calculator has been used on it.
  #' SOP_example_df
  #' #
  #' #
  #' # First, an example of the standard usecase - calculate the CHOAVLDFg_calculated
  #' # value, while resetting negative values to 0.
  #' NegativeToZeroResults <- CHOAVLDFg_calculator(SOP_example_df, NegativeValueReplacement = 0)
  #' #
  #' NegativeToZeroResults
  #' # See the changes - the addition of the CHOAVLDFg_calculated column, and
  #' # the additions to the comments column. Notice how some comments are
  #' # specially formulated to show if the corresponding CHOAVLDFg_calculated
  #' # value has been reset to 0.
  #' #
  #' #
  #' # The second example shows the results when the Replacement option is set to NA
  #' NegativeToNA_results <- CHOAVLDFg_calculator(SOP_example_df, NegativeValueReplacement = NA)
  #' #
  #' NegativeToNA_results
  #' # Check the CHOAVLDFg_calculated column and comments column again - see how
  #' # values below zero have been replaced with NA, and a note of this change
  #' # logged in the comments column.
  #' #
  #' #
  #' # The third example shows the results when the Replacement option is set to 'remove'
  #' remove_results <- CHOAVLDFg_calculator(SOP_example_df, NegativeValueReplacement = "remove")
  #' #
  #' remove_results
  #' # See how the out of bounds values have been removed.
  #' #
  #' #
  #' # The fourth example is of the nothing results - i.e. nothing happens to the
  #' # negative values.
  #' NegativeNoChangeResults <- CHOAVLDFg_calculator(SOP_example_df, NegativeValueReplacement = "nothing")
  #' #
  #' NegativeNoChangeResults
  #' # Look at the CHOAVLDFg_calculator values - and see how they've been left,
  #' # even if they're negative.
  #' #
  #' #
  #' # The fifth example is of the negative dataframe - an option useful for identifying
  #' # and examining negative results.
  #' NegativeCarb_results <- CHOAVLDFg_calculator(SOP_example_df, OutsideBoundsDF = TRUE)
  #' #
  #' NegativeCarb_results
  #' # Only the out of bounds results are present, in their original form, for inspection.
  #' #
  #' #
  #' # The sixth example is of the CHOAVLDFg_calculator working on a dataframe with
  #' # non-standard column names. It uses a modified example data frame, shown below.
  #' SOP_example_df_nonstandard
  #' # Notice how the column names are different, and differ from the assumed names.
  #' #
  #' #
  #' # Because of the different names, the column names for each input must be specified.
  #' nothing_results_NonStandardInput <- CHOAVLDFg_calculator(
  #' SOP_example_df_nonstandard,
  #' WATERg_column = "Water_values_g",
  #' PROCNTg_column = "PROCNT_values_g",
  #' FAT_g_standardised_column = "FAT_values_g_standardised",
  #' FIBTGg_standardised_column = "FIBTG_values_g_standardised",
  #' ALCg_column = "ALC_values_g",
  #' ASHg_column = "ASH_values_g",
  #' comment_col = "comments_column",
  #' comments = TRUE,
  #' NegativeValueReplacement = 0,
  #' NegativeValueDF = FALSE
  #' )
  #' # The resulting CHOAVLDFg_calculated column is the same as in the first example,
  #' # despite the different names.
  #' @export



  # Check presence of required columns

  # Input checks - goes through each input column name, and checks if its a column in the df. If it isn't, it prints an error message and stops.

  # This check makes sure the entered df is a data frame.
  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df))

  #This block of checks throws an error if the entry for the columns is not present in the df.
  stopifnot("The WATERg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column one'." = WATERg_column %in% colnames(df))
  stopifnot("The PROCNTg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column two'." = PROCNTg_column %in% colnames(df))
  stopifnot("The FAT_g_standardised is not a column name in df - please input a string that is a column name in df, e.g. 'column three'." = FAT_g_standardised_column %in% colnames(df))
  stopifnot("The FIBTGg_std_column is not a column name in df - please input a string that is a column name in df, e.g. 'column five'." = FIBTGg_standardised_column %in% colnames(df))
  stopifnot("The ALCg_columnis not a column name in df - please input a string that is a column name in df, e.g. 'column six'." = ALCg_column %in% colnames(df))
  stopifnot("The ASHg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column seven'." = ASHg_column %in% colnames(df))

  #This block of checks makes sure the columns that are meant to be numeric are numeric.
  stopifnot("The WATERg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[WATERg_column]]))
  stopifnot("The PROCNTg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[PROCNTg_column]]))
  stopifnot("The FAT_g_standardised_column is not numeric. Please ensure it is numeric." = is.numeric(df[[FAT_g_standardised_column]]))
  stopifnot("The FIBTGg_standardised_column is not numeric. Please ensure it is numeric." = is.numeric(df[[FIBTGg_standardised_column]]))
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
    FAT_g_standardised_column,
    FIBTGg_standardised_column,
    ALCg_column,
    ASHg_column
  )], na.rm = T)

  # This checks if any rows were entirely NA values, and sets the CHOAVLDFg_calculated to NA if so.
  df[is.na(df[[WATERg_column]]) &
       is.na(df[[PROCNTg_column]]) &
       is.na(df[[FAT_g_standardised_column]]) &
       is.na(df[[FIBTGg_standardised_column]]) &
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
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0, comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0, comment_col], "; ", comment_message, " - Original value of ", df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0, "CHOAVLDFg_calculated"], " reset to 0")

      # This is for rows without existing comments, and negative values
      df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0, comment_col] <- paste0(comment_message, " - Original value of ", df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0, "CHOAVLDFg_calculated"], " reset to 0")

      # This is for rows with existing comments, and positive values
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated >= 0, comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated >= 0, comment_col], "; ", comment_message)

      #This is for rows without existing comments, and positive values
      df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated >= 0, comment_col] <- paste0(comment_message)


    } else if (is.na(NegativeValueReplacement)){ #If NegativeToZero is set to NA, then a special message must appear in specific columns, detailing the original value and that it was reset to NA.

      # This is for rows with existing comments, and negative values
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0, comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0, comment_col], "; ", comment_message, " - Original value of ", df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0, "CHOAVLDFg_calculated"], " reset to NA")

      # This is for rows without existing comments, and negative values
      df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0, comment_col] <- paste0(comment_message, " - Original value of ", df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated < 0, "CHOAVLDFg_calculated"], " reset to NA")

      # This is for rows with existing comments, and positive values
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated >= 0, comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated >= 0, comment_col], "; ", comment_message)

      #This is for rows without existing comments, and positive values
      df[(df[[comment_col]] %in% "" | is.na(df[[comment_col]])) & df$CHOAVLDFg_calculated >= 0, comment_col] <- paste0(comment_message)


    } else { #If NegativeValueReplacement is set to nothing, or delete (the only other valid options), the comments for the negative values don't matter. All comments are therefore the same - and negative values do not need a custom message.

      #If comment == TRUE and there is already a comment col in the df, then this appends the message to the existing comments.
      df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col], "; ", comment_message)

      #If comment == TRUE and there is already a comment col in the df, but its empty, then this becomes the first entry into the column.
      df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]), comment_col] <- paste0(comment_message)
    }
  }


  BelowZeroNumber <- length(df[df$CHOAVLDFg_calculated < 0, "CHOAVLDFg_calculated"]) #Sees how many values are less than 0.

  if(BelowZeroNumber > 0){ #Triggers a warning if they are present.

    Min_Number <- min(df$CHOAVLDFg_calculated) #Finds the lowest value.
    Number_Below_Minus5 <- length(df[df$CHOAVLDFg_calculated < -5, "CHOAVLDFg_calculated"]) #Finds the number of values less than -5.

    message("---------------------------") #Prints a warning message.
    message()
    message(BelowZeroNumber, " CHOAVLDFg_calculated values calculated to be less than 0. Minimum result: ", Min_Number, ". Number of values below -5: ", Number_Below_Minus5, ". Please rerun the function with NegativeValueDF = TRUE if you wish to inspect these values.")
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
      result_df[result_df$CHOAVLDFg_calculated < 0, "CHOAVLDFg_calculated"] <- 0
    } else if (is.na(NegativeValueReplacement)){
      result_df <- df
      result_df[result_df$CHOAVLDFg_calculated < 0, "CHOAVLDFg_calculated"] <- NA
    } else if (tolower(NegativeValueReplacement) %in% c("rm", "del", "remove", "delete")) {
      result_df <- df[df$CHOAVLDFg_calculated >= 0,] #Only outputs rows with CHOAVLDFg_calculated values over 0.
    } else { #The only valid option left is to do nothing - so nothing happens.
      result_df <- df
    }
  }

  return(result_df)

}

# ¬ Testing ----

test_df <- read.csv("~/GitHub/UoN-FAO/Output/Global_nct_imitation_v1.0.2.csv")

colnames(test_df)[colnames(test_df) == 'FAT_g_std'] <- 'FAT_g_standardised'
colnames(test_df)[colnames(test_df) == 'FIBTGg_standardised'] <- 'FIBTGg_std'

library(dplyr)
time_1 <- Sys.time()

old_method_output <- CHOAVLDFg_std_creator(test_df)

time_2 <- Sys.time()

new_method_output <- CHOAVLDFg_std_creator_New(test_df, FIBTGg_standardised_column = "FIBTGg_std", NegativeValueReplacement = "nothing")

time_3 <- Sys.time()

isitadf <- function(df){
  stopifnot("Its not a df" = is.data.frame(df))
}

time_2 - time_1

time_3 - time_2

parity_test <- signif(old_method_output$CHOAVLDFg_standardised, 10) == signif(new_method_output$CHOAVLDFg_standardised, 10)

print(table(parity_test))

results_comparison <- old_method_output[, c(
  "fdc_id.x",
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

calc_lessthanminusfive <- results_comparison[results_comparison$CHOAVLDFg_standardised_New < -5,]

problematic_Carb_foods <- new_method_output[new_method_output$fdc_id.x %in% calc_lessthanminusfive$fdc_id.x, ]

#write.csv(problematic_Carb_foods, "problematic_Carb_foods.csv")

different_results <- results_comparison[!(signif(results_comparison$CHOAVLDFg_standardised, 10) == signif(results_comparison$CHOAVLDFg_standardised_New, 10)),]

print(paste0("Largest difference is ", max(results_comparison$difference)))

#Problematic Carbs testing ----



Average_F0875_CPC <- test_df[test_df$Grouping_CPC %in% "Average_F0875",]
Average_F0875_CPC_stdCHO <- CHOAVLDFg_std_creator_New(Average_F0875_CPC, FAT_g_standardised_column = "FAT_g_std", NegativeToZero = F)
Average_F0875_CPC_stdCHO[Average_F0875_CPC_stdCHO$Grouping_FAO_Code %in% "SUMMARY ROW - 0875", "CHOAVLDFg_standardised"] <- mean(Average_F0875_CPC_stdCHO[!(Average_F0875_CPC_stdCHO$Grouping_FAO_Code %in% "SUMMARY ROW - 0875"), "CHOAVLDFg_standardised"])
Average_F0875_CPC_stdCHO_set0 <- CHOAVLDFg_std_creator_New(Average_F0875_CPC, FAT_g_standardised_column = "FAT_g_std")
Average_F0875_CPC_stdCHO_set0[Average_F0875_CPC_stdCHO_set0$Grouping_FAO_Code %in% "SUMMARY ROW - 0875", "CHOAVLDFg_standardised"] <- mean(Average_F0875_CPC_stdCHO_set0[!(Average_F0875_CPC_stdCHO_set0$Grouping_FAO_Code %in% "SUMMARY ROW - 0875"), "CHOAVLDFg_standardised"])
Average_F0875_CPC_stdCHO_selectiveset0 <- CHOAVLDFg_std_creator_New(Average_F0875_CPC, FAT_g_standardised_column = "FAT_g_std", NegativeToZero = F)
Average_F0875_CPC_stdCHO_selectiveset0[Average_F0875_CPC_stdCHO_selectiveset0$CHOAVLDFg_standardised < 0 & Average_F0875_CPC_stdCHO_selectiveset0$CHOAVLDFg_standardised > -5, "CHOAVLDFg_standardised"] <- 0
Average_F0875_CPC_stdCHO_selectiveset0[Average_F0875_CPC_stdCHO_selectiveset0$Grouping_FAO_Code %in% "SUMMARY ROW - 0875", "CHOAVLDFg_standardised"] <- mean(Average_F0875_CPC_stdCHO_selectiveset0[!(Average_F0875_CPC_stdCHO_selectiveset0$Grouping_FAO_Code %in% "SUMMARY ROW - 0875"), "CHOAVLDFg_standardised"])
Average_F0875_CPC_stdCHO_Nobelowminus5set0 <- Average_F0875_CPC_stdCHO_selectiveset0[Average_F0875_CPC_stdCHO_selectiveset0$CHOAVLDFg_standardised >= 0,]
Average_F0875_CPC_stdCHO_Nobelowminus5set0[Average_F0875_CPC_stdCHO_Nobelowminus5set0$Grouping_FAO_Code %in% "SUMMARY ROW - 0875", "CHOAVLDFg_standardised"] <- mean(Average_F0875_CPC_stdCHO_Nobelowminus5set0[!(Average_F0875_CPC_stdCHO_Nobelowminus5set0$Grouping_FAO_Code %in% "SUMMARY ROW - 0875"), "CHOAVLDFg_standardised"])


print(paste0("Average with 0 to -5 set to 0: ", Average_F0875_CPC_stdCHO_selectiveset0[Average_F0875_CPC_stdCHO_selectiveset0$Grouping_FAO_Code %in% "SUMMARY ROW - 0875", "CHOAVLDFg_standardised"], ", Average values with < -5 removed: ", Average_F0875_CPC_stdCHO_Nobelowminus5set0[Average_F0875_CPC_stdCHO_Nobelowminus5set0$Grouping_FAO_Code %in% "SUMMARY ROW - 0875", "CHOAVLDFg_standardised"], ". Average value with values < -5 set to 0: ", Average_F0875_CPC_stdCHO_set0[Average_F0875_CPC_stdCHO_set0$Grouping_FAO_Code %in% "SUMMARY ROW - 0875", "CHOAVLDFg_standardised"]))


unique_problematic_entries <- unique(problematic_Carb_foods$Grouping_CPC)

for (i in 1:length(unique_problematic_entries)){
  problematic_CPC_Code <- unique_problematic_entries[i]
  problematic_item <- test_df[test_df$Grouping_CPC %in% problematic_CPC_Code,]
  problematic_item_stdCHO <- CHOAVLDFg_std_creator_New(problematic_item, NegativeToZero = F)
  sumrow_text <- tail(problematic_item$Grouping_FAO_Code, 1)
  FAO_Code <- head(problematic_item$Grouping_FAO_Code, 1)
  problematic_item_stdCHO[problematic_item_stdCHO$Grouping_FAO_Code %in% sumrow_text, "CHOAVLDFg_standardised"] <- mean(problematic_item_stdCHO[!(problematic_item_stdCHO$Grouping_FAO_Code %in% sumrow_text), "CHOAVLDFg_standardised"])

  problematic_item_stdCHO_set0 <- CHOAVLDFg_std_creator_New(problematic_item)
  problematic_item_stdCHO_set0[problematic_item_stdCHO_set0$Grouping_FAO_Code %in% sumrow_text, "CHOAVLDFg_standardised"] <- mean(problematic_item_stdCHO_set0[!(problematic_item_stdCHO_set0$Grouping_FAO_Code %in% sumrow_text), "CHOAVLDFg_standardised"])
  problematic_item_stdCHO_selectiveset0 <- CHOAVLDFg_std_creator_New(problematic_item, NegativeToZero = F)
  problematic_item_stdCHO_selectiveset0[problematic_item_stdCHO_selectiveset0$CHOAVLDFg_standardised < 0 & problematic_item_stdCHO_selectiveset0$CHOAVLDFg_standardised > -5, "CHOAVLDFg_standardised"] <- 0
  problematic_item_stdCHO_selectiveset0[problematic_item_stdCHO_selectiveset0$Grouping_FAO_Code %in% sumrow_text, "CHOAVLDFg_standardised"] <- mean(problematic_item_stdCHO_selectiveset0[!(problematic_item_stdCHO_selectiveset0$Grouping_FAO_Code %in% sumrow_text), "CHOAVLDFg_standardised"])
  problematic_item_stdCHO_Nobelowminus5set0 <- problematic_item_stdCHO_selectiveset0[problematic_item_stdCHO_selectiveset0$CHOAVLDFg_standardised >= 0,]
  problematic_item_stdCHO_Nobelowminus5set0[problematic_item_stdCHO_Nobelowminus5set0$Grouping_FAO_Code %in% sumrow_text, "CHOAVLDFg_standardised"] <- mean(problematic_item_stdCHO_Nobelowminus5set0[!(problematic_item_stdCHO_Nobelowminus5set0$Grouping_FAO_Code %in% sumrow_text), "CHOAVLDFg_standardised"])

  print(paste0("FAO Code: ", FAO_Code, ". Average with 0 to -5 set to 0: ", problematic_item_stdCHO_selectiveset0[problematic_item_stdCHO_selectiveset0$Grouping_FAO_Code %in% sumrow_text, "CHOAVLDFg_standardised"], ", Average values with < -5 removed: ", problematic_item_stdCHO_Nobelowminus5set0[problematic_item_stdCHO_Nobelowminus5set0$Grouping_FAO_Code %in% sumrow_text, "CHOAVLDFg_standardised"], ". Average value with values < 0 set to 0: ", problematic_item_stdCHO_set0[problematic_item_stdCHO_set0$Grouping_FAO_Code %in% sumrow_text, "CHOAVLDFg_standardised"]))


}


# ¬¬ Results ----
# Differences are in the order of 10^-14 out. So insignificant.
# Speed tests: Old method: 0.120, 0.128, 0.124, 0.171
# Speed tests: New method: 0.00520, 0.00485, 0.00520, 0.00498

# Speed improvement of ~23x

#With new options (added choices of what to do kikwith negative values) speed improvement is now ~10x for set to 0, remove, or NA. still ~23x if no replacement.



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


# ¬ New Version ----



VITAmcg_calculator <- function(df,
                                 RETOLmcg_column = "RETOLmcg",
                                 CARTBEQmcg_std_column = "CARTBEQmcg_std",
                                 comment = TRUE,
                                 comment_col = "comments") {

  #' @title Vitamin A Calculator
  #' @description Calculates VITAmcg_calculated = (RETOLmcg + (CARTBEQmcg_std/6)).
  #' Column names are case sensitive and an error is returned if not found.
  #' @param df Required - the data.frame the data is currently stored in.
  #' @param RETOLmcg_column Required - default: \code{'RETOLmcg'} - The name of the
  #'   column containing Retinol in mcg per 100g of Edible Portion (EP).
  #' @param CARTBEQmcg_std_column Required - default: \code{'CARTBEQmcg_std'} -
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
  #' @return Original FCT dataset with a new \code{VITAmcg_calculated}
  #'   column.
  #' @examples
  #' @export


  # Check presence of required columns

  # Input checks - goes through each input column name, and checks if its a column in the df. If it isn't, it prints an error message and stops.

  # This check makes sure the entered df is a data frame.
  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df))

  #This block of checks throws an error if the entry for the columns is not present in the df.
  stopifnot("The RETOLmcg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column one'." = RETOLmcg_column %in% colnames(df))
  stopifnot("The CARTBEQmcg_std_column is not a column name in df - please input a string that is a column name in df, e.g. 'column two'." = CARTBEQmcg_std_column %in% colnames(df))

  #This block of checks makes sure the columns that are meant to be numeric are numeric.
  stopifnot("The RETOLmcg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[RETOLmcg_column]]))
  stopifnot("The CARTBEQmcg_std_column is not numeric. Please ensure it is numeric." = is.numeric(df[[CARTBEQmcg_std_column]]))

  #This block checks to make sure logical entries are True or False.
  stopifnot("The comment parameter is not set to TRUE or FALSE - please use TRUE or FALSE." = is.logical(comment))



  df$VITAmcg_calculated <- NA #This row creates the VITAmcg_calculated column, and fills it with NA values
  df$TEMPsixthCARTBEQ <- df[[CARTBEQmcg_std_column]]/6 #This creates a column for CARTBEQ_std divided by 6

  #This adds the CARTBEQ/6 and RETOL columns together, ignoring NA results
  df$VITAmcg_calculated <- rowSums(df[, c(
    RETOLmcg_column,
    "TEMPsixthCARTBEQ"
  )], na.rm = T)

  # This checks if any rows were entirely NA values, and sets the VITAmcg_calculated to NA if so.
  df[is.na(df[[RETOLmcg_column]]) &
       is.na(df[[CARTBEQmcg_std_column]]), "VITAmcg_calculated"] <- NA

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


# ¬ Testing ----

test_df <- read.csv("~/GitHub/UoN-FAO/Output/Global_nct_imitation_v1.0.2.csv")

test_df$CARTBEQmcg_std
test_df$RETOLmcg

library(dplyr)
time_1 <- Sys.time()

old_method_output <- VITAmcg_std_creator(test_df)

time_2 <- Sys.time()

new_method_output <- VITAmcg_calculator(test_df)

time_3 <- Sys.time()

time_2 - time_1

time_3 - time_2

# 15-20x faster

parity_test <- signif(old_method_output$VITAmcg_std, 10) == signif(new_method_output$VITAmcg_calculated, 10)

print(table(parity_test)) #Parity reached



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


# ¬ New Version ----



VITA_RAEmcg_calculator <- function(df,
                               RETOLmcg_column = "RETOLmcg",
                               CARTBEQmcg_std_column = "CARTBEQmcg_std",
                               comment = TRUE,
                               comment_col = "comments") {

  #' @title Vitamin A (Retinol Activity Equivalent) Calculator
  #' @description Calculates VITA_RAEmcg_calculated = (RETOLmcg + (CARTBEQmcg_std/12)).
  #' Column names are case sensitive and an error is returned if not found.
  #' @param df Required - the data.frame the data is currently stored in.
  #' @param RETOLmcg_column Required - default: \code{'RETOLmcg'} - The name of the
  #'   column containing Retinol in mcg per 100g of Edible Portion (EP).
  #' @param CARTBEQmcg_std_column Required - default: \code{'CARTBEQmcg_std'} -
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
  #' @return Original FCT dataset with a new \code{VITA_RAEmcg_calculated}
  #'   column.
  #' @examples
  #' @export


  # Check presence of required columns

  # Input checks - goes through each input column name, and checks if its a column in the df. If it isn't, it prints an error message and stops.

  # This check makes sure the entered df is a data frame.
  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df))

  #This block of checks throws an error if the entry for the columns is not present in the df.
  stopifnot("The RETOLmcg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column one'." = RETOLmcg_column %in% colnames(df))
  stopifnot("The CARTBEQmcg_std_column is not a column name in df - please input a string that is a column name in df, e.g. 'column two'." = CARTBEQmcg_std_column %in% colnames(df))

  #This block of checks makes sure the columns that are meant to be numeric are numeric.
  stopifnot("The RETOLmcg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[RETOLmcg_column]]))
  stopifnot("The CARTBEQmcg_std_column is not numeric. Please ensure it is numeric." = is.numeric(df[[CARTBEQmcg_std_column]]))

  #This block checks to make sure logical entries are True or False.
  stopifnot("The comment parameter is not set to TRUE or FALSE - please use TRUE or FALSE." = is.logical(comment))



  df$VITA_RAEmcg_calculated <- NA #This row creates the VITA_RAEmcg_calculated column, and fills it with NA values
  df$TEMPtwelthCARTBEQ <- df[[CARTBEQmcg_std_column]]/12 #This creates a column for CARTBEQ_std divided by 6

  #This adds the CARTBEQ/6 and RETOL columns together, ignoring NA results
  df$VITA_RAEmcg_calculated <- rowSums(df[, c(
    RETOLmcg_column,
    "TEMPtwelthCARTBEQ"
  )], na.rm = T)

  # This checks if any rows were entirely NA values, and sets the VITA_RAEmcg_calculated to NA if so.
  df[is.na(df[[RETOLmcg_column]]) &
       is.na(df[[CARTBEQmcg_std_column]]), "VITA_RAEmcg_calculated"] <- NA

  # This deletes the temporary TEMPsixthCARTBEQ column

  df$TEMPtwelthCARTBEQ <- NULL

  # Comments process
  if (comment == TRUE) {
    #Creates comments_message if comments are true
    comment_message <- "VITA_RAEmcg_calculated value calculated from Retinol + 1/12 Beta-Carotene Equivalents"

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

# Testing ----




test_df <- read.csv("~/GitHub/UoN-FAO/Output/Global_nct_imitation_v1.0.2.csv")

test_df$CARTBEQmcg_std
test_df$RETOLmcg

library(dplyr)
time_1 <- Sys.time()

old_method_output <- VITA_RAEmcg_std_creator(test_df)

time_2 <- Sys.time()

new_method_output <- VITA_RAEmcg_calculator(test_df)

time_3 <- Sys.time()

time_2 - time_1

time_3 - time_2

# 15-20x faster

parity_test <- signif(old_method_output$VITA_RAEmcg_std, 10) == signif(new_method_output$VITA_RAEmcg_calculated, 10)

print(table(parity_test)) #Parity reached



# nia_calculator ----
# ¬ Problem - not found, not referenced ----

nia_conversion_creator <- function(dataset) {
  #' @title nia_conversion_creator
  #' @description Calculates NIAmg_std if based on presence of NIAmg. i.e. NIAmg_std = case_when(!is.na(NIAmg) ~ NIAmg,is.na(NIAmg) ~ (NIAEQmg - (1 / 60 * TRPmg)))
  #' @param NIAmg_std Niacin was combined into the Niacin standardised variable (`NIAmg_std`)
  #' @param NIAEQmg Niacin equivalents, total. Preformed niacin plus niacin equivalents from tryptophan (TRP) in mg per 100g EP
  #' @param TRPmg Tryptophan in mg per 100g of EP (includes only L-tryptophan)
  #' @param NIAmg Niacin, prefrormed in mg per 100g EP
  #' @return Original FCT dataset with additional column

  columns <- c("NIAEQmg", "TRPmg", "NIAmg")
  check_columns(dataset = dataset, columns = columns)
  tryCatch(
    dataset %>%
      as_tibble() %>%
      mutate_at(.vars = columns, .funs = as.numeric) %>%
      mutate(NIAmg_std = case_when(
        !is.na(NIAmg) ~ NIAmg,
        is.na(NIAmg) ~ (NIAEQmg - (1 / 60 * TRPmg))
      )),
    error = function(e) {
      print("Error : Required columns not found i.e :")
      print(columns)
    }
  )
}


# ¬¬ Needed Updates ----
# NIAmg can be calculated by (NIAEQmg-NIATRPmg) and that the function needs to
# add the information about how it was calculated in the comments variable!


# ¬ New Version ----



NIAmg_std_calculator <- function(df,
                             NIAmg_column = "NIAmg",
                             TRPmg_column = "TRPmg",
                             NIAEQmg_column = "NIAEQmg",
                             NIATRPmg_column = "NIATRPmg",
                             comment = TRUE,
                             comment_col = "comments") {

  #' @title Niacin Standardiser and Calculator
  #' @description This function calculates potential values for Niacin, and then
  #'   uses the most appropriate one to standardise the value with.
  #' @param df Required - the data.frame the data is currently stored in.
  #' @param RETOLmcg_column Required - default: \code{'RETOLmcg'} - The name of the
  #'   column containing Retinol in mcg per 100g of Edible Portion (EP).
  #' @param CARTBEQmcg_std_column Required - default: \code{'CARTBEQmcg_std'} -
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
  #' @return Original FCT dataset with a new \code{VITA_RAEmcg_calculated}
  #'   column.
  #' @examples
  #' @export


  # Check presence of required columns

  # Input checks - goes through each input column name, and checks if its a column in the df. If it isn't, it prints an error message and stops.

  # This check makes sure the entered df is a data frame.
  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df))

  #This block of checks throws an error if the entry for the columns is not present in the df.
  stopifnot("The RETOLmcg_column is not a column name in df - please input a string that is a column name in df, e.g. 'column one'." = RETOLmcg_column %in% colnames(df))
  stopifnot("The CARTBEQmcg_std_column is not a column name in df - please input a string that is a column name in df, e.g. 'column two'." = CARTBEQmcg_std_column %in% colnames(df))

  #This block of checks makes sure the columns that are meant to be numeric are numeric.
  stopifnot("The RETOLmcg_column is not numeric. Please ensure it is numeric." = is.numeric(df[[RETOLmcg_column]]))
  stopifnot("The CARTBEQmcg_std_column is not numeric. Please ensure it is numeric." = is.numeric(df[[CARTBEQmcg_std_column]]))

  #This block checks to make sure logical entries are True or False.
  stopifnot("The comment parameter is not set to TRUE or FALSE - please use TRUE or FALSE." = is.logical(comment))



  df$VITA_RAEmcg_calculated <- NA #This row creates the VITA_RAEmcg_calculated column, and fills it with NA values
  df$TEMPtwelthCARTBEQ <- df[[CARTBEQmcg_std_column]]/12 #This creates a column for CARTBEQ_std divided by 6

  #This adds the CARTBEQ/6 and RETOL columns together, ignoring NA results
  df$VITA_RAEmcg_calculated <- rowSums(df[, c(
    RETOLmcg_column,
    "TEMPtwelthCARTBEQ"
  )], na.rm = T)

  # This checks if any rows were entirely NA values, and sets the VITA_RAEmcg_calculated to NA if so.
  df[is.na(df[[RETOLmcg_column]]) &
       is.na(df[[CARTBEQmcg_std_column]]), "VITA_RAEmcg_calculated"] <- NA

  # This deletes the temporary TEMPsixthCARTBEQ column

  df$TEMPtwelthCARTBEQ <- NULL

  # Comments process
  if (comment == TRUE) {
    #Creates comments_message if comments are true
    comment_message <- "VITA_RAEmcg_calculated value calculated from Retinol + 1/12 Beta-Carotene Equivalents"

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


# ¬ New Version ----

# Looks like a job for the nutri combiner. But alongside that...


Thiamine_standardiser <-  function(df, THIAmg_column = "THIAmg", THIAHCLmg_column = "THIAHCLmg", comment = T, comment_col = "comments"){

  #' @title Thiamine Nutrient Combiner/Standardiser
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
  #' @param comment Required - default: \code{T} - \code{TRUE} or \code{FALSE}.
  #'   If comment is set to \code{TRUE} (as it is by default), when the function
  #'   is run a comment describing the source of the
  #'   \code{THIAmg_standardised} column is added to the comment_col. If no
  #'   comment_col is selected, and \code{comment = T}, one is created.
  #' @param comment_col Optional - default: \code{'comments'} - A potential
  #'   input variable; the column which contains the metadata comments for the
  #'   food item in question. Not required if the comment parameter is set to
  #'   \code{FALSE}. If set to true, and the comment_col entry is not found in
  #'   the df, it will create a column with the name of the entry.
  #' @return Original FCT dataset with a new THIAmg_standardised column.


  # This check makes sure the entered df is a data frame.
  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df))

  #This block of checks throws an error if the entry for the columns is not present in the df.
  stopifnot("The THIAmg_column is not a column name in df - please input a string that is a column name in df, e.g. 'Thiamine_mg'" = THIAmg_column %in% colnames(df))
  stopifnot("The THIAHCLmg_column is not a column name in df - please input a string that is a column name in df, e.g. 'Thiamine_HCL_mg'" = THIAHCLmg_column %in% colnames(df))

  #This checks to make sure logical entries are True or False.
  stopifnot("The comment parameter is not set to TRUE or FALSE - please use TRUE or FALSE, or T or F" = is.logical(comment))


  df$THIAmg_standardised <- NA #Creates the new column, and sets the value to equal to NA

  if (comment == TRUE){
    df$THIAmg_standardised_comment_col_temp <- "No suitable value for THIAmg_standardised found"
  }


  #Fills values in from THIAHCLmg_column if there are legitimate values there (not NA or blank)

  df[!(df[[THIAHCLmg_column]] %in% "" | is.na(df[[THIAHCLmg_column]])), "THIAmg_standardised"] <- df[!(df[[THIAHCLmg_column]] %in% "" | is.na(df[[THIAHCLmg_column]])), THIAHCLmg_column]

  if(comment == TRUE){ #If comments are true, sets the relevant rows to mention they come from var 2
    df[!(df[[THIAHCLmg_column]] %in% "" | is.na(df[[THIAHCLmg_column]])), "THIAmg_standardised_comment_col_temp"] <- paste0("THIAmg_standardised equal to THIAHCLmg")
  }


  #Then overrides those values if better ones (THIAmg values) are available.

  df[!(df[[THIAmg_column]] %in% "" | is.na(df[[THIAmg_column]])), "THIAmg_standardised"] <- df[!(df[[THIAmg_column]] %in% "" | is.na(df[[THIAmg_column]])), THIAmg_column]

  if(comment == TRUE){ #If comments are true, sets the relevant rows to mention they come from var 1
    df[!(df[[THIAmg_column]] %in% "" | is.na(df[[THIAmg_column]])), "THIAmg_standardised_comment_col_temp"] <- paste0("THIAmg_standardised equal to THIAmg")
  }



  #Then sorts out the comments - depending on whether there is already an existing column or not.

  if (comment == TRUE) {
    if(!(comment_col %in% colnames(df))){
      df[[comment_col]] <- comment_message #If the comment column isn't present yet in the data frame, but comments are set to True, then it creates the comment column
    }

    df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col] <- paste0(df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), comment_col], "; ", df[!(df[[comment_col]] %in% "" | is.na(df[[comment_col]])), "THIAmg_standardised_comment_col_temp"])

    #If comment == T and there is already a comment col in the df, but its empty, then this becomes the first entry into the column.
    df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]), comment_col] <- df[df[[comment_col]] %in% "" | is.na(df[[comment_col]]), "THIAmg_standardised_comment_col_temp"]

    df$THIAmg_standardised_comment_col_temp <- NULL # Remove the temp column
  }

  return(df)

}






# ¬ Testing ----

test_df <- read.csv("~/GitHub/UoN-FAO/Output/Global_nct_imitation_v1.0.2.csv")

library(dplyr)
time_1 <- Sys.time()

old_method_output <- THIAmg_std_creator(test_df)

time_2 <- Sys.time()

new_method_output <- Thiamine_standardiser(test_df)
time_3 <- Sys.time()

time_2 - time_1

time_3 - time_2

# 1.5x to 3x the time of the old version, but old version has annoying
# dependencies, gives less clear error messages, has less checks, and less
# potential comments.

parity_test <- signif(old_method_output$THIAmg_std, 10) == signif(new_method_output$THIAmg_standardised, 10)

print(table(parity_test)) #Parity reached



# New function testing

test_df <- read.csv("~/GitHub/UoN-FAO/Output/Global_nct_imitation_v1.0.2.csv")

library(dplyr)
time_1 <- Sys.time()

old_method_output <- THIAmg_std_creator(test_df)

time_2 <- Sys.time()

new_method_output <- nutri_combiner_new(test_df, var1_column = "THIAmg", var2_column = "THIAHCLmg", new_var = "THIAmg_std")

time_3 <- Sys.time()

time_2 - time_1

time_3 - time_2

# 2-3x slower with nutri_combiner. But still consistently less than 0.1 second.

parity_test <- signif(old_method_output$THIAmg_std, 10) == signif(new_method_output$THIAmg_std, 10)

print(table(parity_test)) #Parity reached



