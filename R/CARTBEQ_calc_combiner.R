#---
#Title: CARTBEQ_calc_combiner
#Author: Thomas Codd - https://github.com/TomCodd
#Contributor: Lucia Segovia de la Revilla  - https://github.com/LuciaSegovia
#Version: V1.1.0
#Changelog:
#V1.0.0 -> V1.1.0: Changed the name, changed output name, fixed error where NA
# results could cause problems.
#Github: https://github.com/TomCodd/NutritionTools
#---

#' Calculate a combined CARTBEQ value from various inputs
#'
#' @description This function includes various ways of calculating or combining
#'   a beta-carotene equivalents (CARTBEQ) value. The most appropriate
#'   calculation is selected based on the availability of input data.
#' @param df Required - the data.frame the data is currently stored in
#' @param item_ID Required - default: \code{'fdc_id'} - The column which
#'   contains the data for the ID column name
#' @param CARTAmcg Optional - default: \code{'CARTAmcg'} - A potential input
#'   variable; the column which contains the data for alpha-carotene in
#'   micrograms
#' @param CARTBmcg Optional - default: \code{'CARTBmcg'} - A potential input
#'   variable; the column which contains the data for beta-carotene in
#'   micrograms
#' @param CARTBEQmcg Optional - default: \code{'CARTBEQmcg'} - A potential input
#'   variable; the column which contains the data for beta-carotene equivalent
#'   in micrograms
#' @param CARTBEQmcg_std Optional - default: \code{'CARTBEQmcg_std'} - A
#'   potential input variable; the column which contains the data for
#'   standardised beta-carotene equivalent in micrograms
#' @param CRYPXBmcg OPtional - default: \code{'CRYPXBmcg'} - A potential input
#'   variable; the column which contains the data for beta-crpytoxanthin in
#'   micrograms
#' @param VITAmcg Optional - default: \code{'VITAmcg'} - A potential input
#'   variable; the column which contains the data for Vitamin-A in micrograms
#' @param VITA_RAEmcg Optional - default: \code{'VITA_RAEmcg'} - A potential
#'   input variable; the column which contains the data for Vitamin-A in Retinol
#'   Activity Equivalents, in micrograms
#' @param RETOLmcg Optional - default: \code{'RETOLmcg'} - A potential input
#'   variable; the column which contains the data for Retinol in micrograms
#' @param comment_col Optional - default: \code{'comments'} - A potential input
#'   variable; the column which contains the metadata comments for the food item
#'   in question
#' @param compile Optional - default: \code{TRUE} - \code{TRUE} or \code{FALSE}. If
#'   compile is set to \code{TRUE} (as it is by default), then when Beta-Carotene
#'   (CARTBmcg) is not available then Beta-Carotene Eq. (CARTBEQ) is used as the
#'   value for Beta-Carotene equivalent (standardised).
#' @param carotene Optional - default: \code{TRUE} - \code{TRUE} or \code{FALSE}.
#'   If carotene is set to \code{TRUE} (as it is by default), then when
#'   Beta-Carotene (CARTBmcg) is available then it is used as the value for
#'   Beta-Carotene equivalent (standardised)
#' @param comment Optional - default: \code{TRUE} - \code{TRUE} or \code{FALSE}. If
#'   compile is set to \code{TRUE} (as it is by default), then when the function is
#'   run a description of the calculation used to find CARTBEQ_standardised is
#'   added to the comment_col. If no comment_col is selected, and \code{comment
#'   = T}, one is created, called \code{comments}.
#' @param trackingcomment Optional - default: \code{FALSE} - \code{TRUE} or
#'   \code{FALSE}. If set to \code{TRUE}, the function will output the CARTBEQ
#'   calue for each row as it calculates it, to allow tracking of progress.
#' @details The Standardiser can use 6 different ways of calculating
#'   Beta-Carotene, on a case-by-case (or row-by-row) basis to ensure that each
#'   item has the most appropriate calculation used on it. If the most
#'   appropriate calculation cannot be completed (due to incomplete input data)
#'   then the next most appropriate calculation is used, and so on. The
#'   calculations and their position in the hierarchy is shown below:
#'   \itemize{
#'   \item 1. \code{recalculated_CARTBEQmcg_std = CARTBmcg_value + 0.5*CARTAmcg_value
#'       + 0.5*CRYPXBmcg_value}. Beta-Carotene Equivalents (standardised) value
#'       (CARTBEQ_std) is calculated from its base elements, dependent on valid
#'       Alpha-Carotene (CARTA), Beta-Carotene (CARTB), and Beta-Cryptoxanthin
#'       CRYPXB) values.
#'   \item 2. \code{recalculated_CARTBEQmcg_std = CARTBEQmcg_std_value}. If the
#'       base elements for the calculation are unavailable, but a standardised
#'       CARTBEQ value already exists, it is used.
#'   \item 3. \code{recalculated_CARTBEQmcg_std = CARTBEQmcg _value}. If neither
#'       the base elements for the calculation or a standardised CARTBEQ value
#'       are unavailable then the non-standardised CARTBEQ value is used
#'       instead, if it exists.
#'   \item 4. \code{recalculated_CARTBEQmcg_std = CARTBmcg _value}. If neither the
#'       base elements for the calculation, or a CARTBEQ value (standardised or
#'       otherwise) exist, then the Beta-Carotene (CARTB) value is used if
#'       available.
#'   \item 5. \code{recalculated_CARTBEQmcg_std = (VITA_RAEmcg_value -
#'       RETOLmcg_value)*12}. If none of the above are available, then
#'       CATBEQ_std is calculated from Vitamin A Retinol Activity Equivalent
#'       (VITA_RAE) and Retinol (RETOL).
#'   \item 6. \code{recalculated_CARTBEQmcg_std = (VITAmcg_value -
#'       RETOLmcg_value)*6}. If none of the above are available, then CATBEQ_std
#'       is calculated from Vitamin A (VITA) and Retinol (RETOL).
#'   }
#'
#'   This hierarchy remains unchanged – however, the equations in the hierarchy
#'   can be altered depending on the compile and carotene input options. If both
#'   are set to True, then the full list of 6 equations are made available. If
#'   compile == F, then options 2 and 3 (to use pre-existing CARTBEQ_std or
#'   CARTBEQ values) are removed, meaning that the only options 1, 4, 5, 6 are
#'   available in the Hierarchy. Similarly, if carotene == F, then option 4 (to
#'   use a pre-existing CARTB value) is removed, leaving only options 1, 5, 6 if
#'   both are set to false.
#'   If none of those options are available (i.e. the data is incomplete, and no
#'   value can be calculated and no fill-in value used) then the CARTBEQ_std
#'   value is set to NA.

#' @return A data.frame that mirrors \code{df}, but with a new column -
#'   \code{recalculated_CARTBEQmcg_std} - added. New comments may also be added.
#'
#' @examples
#' #The most common usage scenario will be covered. First we will create a test
#' #data.frame with dummy data to use the function on. This function is designed
#' #to be able to be used with patchy data - if certain columns are incomplete
#' #then then the best calculation will be used on a row-by-row basis.
#'
#'test_df <- data.frame( ID = c("test_01", "test_02", "test_03", "test_04",
#'"test_05", "test_06", "test_07", "test_08", "test_09", "test_10" ), food_name
#'= c("Bread (wholemeal)", "Pepper, ground, black", "Milk, cow, whole, fresh,
#'raw", "Orange Juice", "Butter (cow milk), salted", "Salt, Iodized", "Egg,
#'chicken, whole, raw", "Tomato, red, ripe, raw", "Mushroom, fresh, raw",
#'"Parsley, fresh, raw"), "CART B (mcg)" = c(NA, 105, "", 130, NA, "", 111, NA,
#'112, 101), "CART A (mcg)" = c(0, 35, 23, 27, 6, 34, NA, 18, "", 40), "CRYPXB
#'(mcg)" = c(110, 67, 72, NA, 160, 102, 98, 37, 28, 60), "CART B eq (std) (mcg)"
#'= c("", 107, 102, NA, "", NA, 72, "", "", 143), "CART B eq (mcg)" = c(159,
#'103, 132, NA, "", "", "", 78, NA, 92), "Vit A RAE (mcg)" = c(13, 8, NA, 15,
#'13, NA, NA, NA, 7, 10), "Vit A (mcg)" = c(12, 11, 8, 13, 3, 1, 10, 15, 3, 6),
#'"Retinol (mcg)" = c(0, 7, 12, NA, 5, 2, 10, 6, "", 1), "comments" = c(
#'"Imaginary values", "Completely fictional values", "Fictional values #2",
#'"More fictional values", "Fictional #4", "Fictional no. 5", "fictional 6",
#'"more fiction", "again, fiction", "Fictional number 9" ), check.names = FALSE)
#'
#'#In this case the data.frame we want to run through is called test_df.
#'#However the standard INFOODS names haven't been used, so the assumed nutrient
#'#column names won't work, and they will have to be manually assigned. However,
#'#the comment column is named the default name (comments) and so even if left
#'#out will still be valid.
#'
#'output_df <- CARTBEQ_calc_combiner(df = test_df, item_ID = 'ID', CARTAmcg =
#''CART A (mcg)', CARTBmcg = 'CART B (mcg)', CARTBEQmcg = 'CART B eq (mcg)',
#'CARTBEQmcg_std = 'CART B eq (std) (mcg)',  CRYPXBmcg = 'CRYPXB (mcg)',
#'VITAmcg = 'Vit A (mcg)',  VITA_RAEmcg = 'Vit A RAE (mcg)',  RETOLmcg =
#''Retinol (mcg)')
#'
#'#The resulting output will have a modified comments column, and a new column -
#'#CARTBEQmcg_combined
#'
#' @export

CARTBEQ_calc_combiner <- function(df,
                                 item_ID = 'fdc_id',
                                 CARTAmcg = 'CARTAmcg',
                                 CARTBmcg = 'CARTBmcg',
                                 CARTBEQmcg = 'CARTBEQmcg',
                                 CARTBEQmcg_std = 'CARTBEQmcg_std',
                                 CRYPXBmcg = 'CRYPXBmcg',
                                 VITAmcg = 'VITAmcg',
                                 VITA_RAEmcg = 'VITA_RAEmcg',
                                 RETOLmcg = 'RETOLmcg',
                                 comment_col = 'comments',
                                 compile = TRUE,
                                 carotene = TRUE,
                                 comment = TRUE,
                                 trackingcomment = FALSE) {
  #This block attributes the column names to the actual columns

  stopifnot(
    "item_ID column not found. Please ensure that the item_ID input is the name of a valid column in df, which contains the ID codes or numbers for the items" = item_ID %in% colnames(df)
  ) #Checks to see if the ID column is an actual column.
  item_ID <-
    df[item_ID] #If not stopped, then the item_ID variable becomes a copy of the item_ID column.

  comment_col_name <-
    comment_col #saves the name, allows the comment_col variable to be overwritten

  # This next block does a similar thing for all variables. It checks to see if they're actually column names, and if they aren't then the variable is set to blank/missing
  if (CARTAmcg %in% colnames(df) == TRUE) {
    CARTAmcg <- df[CARTAmcg]
  } else {
    CARTAmcg <- ""
  }

  if (CARTBmcg %in% colnames(df) == TRUE) {
    CARTBmcg <- df[CARTBmcg]
  } else {
    CARTBmcg <- ""
  }

  if (CRYPXBmcg %in% colnames(df) == TRUE) {
    CRYPXBmcg <- df[CRYPXBmcg]
  } else {
    CRYPXBmcg <- ""
  }

  if (CARTBEQmcg %in% colnames(df) == TRUE) {
    CARTBEQmcg <- df[CARTBEQmcg]
  } else {
    CARTBEQmcg <- ""
  }

  if (CARTBEQmcg_std %in% colnames(df) == TRUE) {
    CARTBEQmcg_std <- df[CARTBEQmcg_std]
  } else {
    CARTBEQmcg_std <- ""
  }

  if (VITAmcg %in% colnames(df) == TRUE) {
    VITAmcg <- df[VITAmcg]
  } else {
    VITAmcg <- ""
  }

  if (VITA_RAEmcg %in% colnames(df) == TRUE) {
    VITA_RAEmcg <- df[VITA_RAEmcg]
  } else {
    VITA_RAEmcg <- ""
  }


  if (RETOLmcg %in% colnames(df) == TRUE) {
    RETOLmcg <- df[RETOLmcg]
  } else {
    RETOLmcg <- ""
  }


  if (comment_col %in% colnames(df) == TRUE) {
    comment_col <- df[comment_col]
  } else {
    comment_col <- ""
  }

  input_length <- nrow(item_ID) #finds the row number

  stopifnot("No input values assigned (input max length <= 1). Please assign input values." = input_length > 1) #if the lowest length isn't at least 1, the function stops.

  recalculated_CARTBEQmcg_std_list <-
    c() #creates an empty list of the recalculated/standardised cartbeq's
  comment_list <- c() #creates an empty list of comments

  for (i in 1:input_length) {
    #loops through the input length

    #Need to check values for presence. If done in other ways missing columns cause errors.


    CARTAmcg_value <- "" #sets the value to nothing
    if (class(CARTAmcg) == 'data.frame') {
      #Then if the column is present and has values
      CARTAmcg_value <-
        CARTAmcg[i, ] #The blank is overwritten with the actual value
    }

    CARTBmcg_value <- "" #sets the value to nothing
    if (class(CARTBmcg) == 'data.frame') {
      #Then if the column is present and has values
      CARTBmcg_value <-
        CARTBmcg[i, ] #The blank is overwritten with the actual value
    }

    CRYPXBmcg_value <- "" #sets the value to nothing
    if (class(CRYPXBmcg) == 'data.frame') {
      #Then if the column is present and has values
      CRYPXBmcg_value <-
        CRYPXBmcg[i, ] #The blank is overwritten with the actual value
    }

    CARTBEQmcg_value <- "" #sets the value to nothing
    if (class(CARTBEQmcg) == 'data.frame') {
      #Then if the column is present and has values
      CARTBEQmcg_value <-
        CARTBEQmcg[i, ] #The blank is overwritten with the actual value
    }

    CARTBEQmcg_std_value <- "" #sets the value to nothing
    if (class(CARTBEQmcg_std) == 'data.frame') {
      #Then if the column is present and has values
      CARTBEQmcg_std_value <-
        CARTBEQmcg_std[i, ] #The blank is overwritten with the actual value
    }

    VITAmcg_value <- "" #sets the value to nothing
    if (class(VITAmcg) == 'data.frame') {
      #Then if the column is present and has values
      VITAmcg_value <-
        VITAmcg[i, ] #The blank is overwritten with the actual value
    }

    VITA_RAEmcg_value <- "" #sets the value to nothing
    if (class(VITA_RAEmcg) == 'data.frame') {
      #Then if the column is present and has values
      VITA_RAEmcg_value <-
        VITA_RAEmcg[i, ] #The blank is overwritten with the actual value
    }

    RETOLmcg_value <- "" #sets the value to nothing
    if (class(RETOLmcg) == 'data.frame') {
      #Then if the column is present and has values
      RETOLmcg_value <-
        RETOLmcg[i, ] #The blank is overwritten with the actual value
    }

    comment_col_value <- "" #sets the value to nothing
    if (class(comment_col) == 'data.frame') {
      #Then if the column is present and has values
      comment_col_value <-
        comment_col[i, ] #The blank is overwritten with the actual value
    }


    if (CARTBmcg_value != "" &
        CARTAmcg_value != "" &
        CRYPXBmcg_value != "" &
        !is.na(CARTBmcg_value) &
        !is.na(CARTAmcg_value) &
        !is.na(CRYPXBmcg_value)) {
      #checks to make sure values aren't blank or NA
      recalculated_CARTBEQmcg_std <-
        (
          as.numeric(CARTBmcg_value) + 0.5 * as.numeric(CARTAmcg_value) + 0.5 * as.numeric(CRYPXBmcg_value)
        ) #f they aren't, the following sum is performed to find a recalculated cartbeq standardised value
      if (comment == TRUE) {
        #if a comment is requested, a predefined comment is created, stating the equation used to find the recalculated_CARTBEQmcg_std value
        recorded_comment <-
          paste0("CARTBEQ_standardised calculated using standard equation")
      }
      #The following code is essentially repeats of the last section of code, with different criteria and different calculations used due to different input criteria
    } else if (compile == TRUE &
               CARTBEQmcg_std_value != "" &
               !is.na(CARTBEQmcg_std_value)) {
      recalculated_CARTBEQmcg_std <- as.numeric(CARTBEQmcg_std_value)
      if (comment == TRUE) {
        recorded_comment <-
          paste0("CARTBEQ_standardised compiled from previous CARTBEQmcg_std value")
      }
    } else if (compile == TRUE &
               CARTBEQmcg_value != "" & !is.na(CARTBEQmcg_value)) {
      recalculated_CARTBEQmcg_std <- as.numeric(CARTBEQmcg_value)
      if (comment == TRUE) {
        recorded_comment <-
          paste0("CARTBEQ_standardised compiled from previous CARTBEQmcg value")
      }
    } else if (carotene == TRUE &
               CARTBmcg_value != "" & !is.na(CARTBmcg_value)) {
      recalculated_CARTBEQmcg_std <- as.numeric(CARTBmcg_value)
      if (comment == TRUE) {
        recorded_comment <-
          paste0("CARTBEQ_standardised compiled from previous CARTBmcg value")
      }
    } else if (VITA_RAEmcg_value != "" &
               !is.na(VITA_RAEmcg_value) &
               RETOLmcg_value != "" & !is.na(RETOLmcg_value)) {
      recalculated_CARTBEQmcg_std <-
        (as.numeric(VITA_RAEmcg_value) - as.numeric(RETOLmcg_value)) * 12
      if (comment == TRUE) {
        recorded_comment <-
          paste0("CARTBEQ_standardised calculated from VITA_RAE and RETOL")
      }
    } else if (VITAmcg_value != "" &
               !is.na(VITAmcg_value) &
               RETOLmcg_value != "" & !is.na(RETOLmcg_value)) {
      recalculated_CARTBEQmcg_std <-
        (as.numeric(VITAmcg_value) - as.numeric(RETOLmcg_value)) * 6
      if (comment == TRUE) {
        recorded_comment <-
          paste0("CARTBEQ_standardised calculated from VITA and RETOL")
      }
    } else {
      recalculated_CARTBEQmcg_std <- NA
      if (comment == TRUE) {
        recorded_comment <-
          paste0("CARTBEQ_standardised could not be calculated")
      }
    }
    #Once the appropriate calculation is found and used, then checks are performed
    if (!is.na(recalculated_CARTBEQmcg_std) && recalculated_CARTBEQmcg_std < 0) {
      #this checks to see if the value is less than zero, and resets it to zero if so, with a recorded comment if comments are on
      recalculated_CARTBEQmcg_std <- 0
      if (comment == TRUE) {
        recorded_comment <-
          paste0(
            recorded_comment,
            " - recalculated_CARTBEQmcg_std calculated to be less than 0. Value reset to 0"
          )
      }
    }

    if (!missing(comment_col)) {
      comment_list <-
        c(comment_list,
          paste0(comment_col_value, " - ", recorded_comment)) #this adds the recorded comments to the comment column with the existing comments from input
    } else {
      comment_list <-
        c(comment_list, recorded_comment) #this adds the recorded comments to the comment column
    }

    recalculated_CARTBEQmcg_std_list <-
      c(recalculated_CARTBEQmcg_std_list,
        recalculated_CARTBEQmcg_std) #results are added to the list

    if(trackingcomment == TRUE){
      message(
        #the recalculated values are then outputted as a message
        paste0(
          "Item ",
          item_ID[i, ],
          " CARTBEQ_standardised calculated to be ",
          recalculated_CARTBEQmcg_std,
          "mcg. ",
          recorded_comment,
          "."
        )
      )
    }
  }

  return_df <-
    df #the return data frame is created, a direct copy of the input df

  if (comment == TRUE) {
    #if comments are accepted, then comments are overwritten to add the new comments too
    prev_comment_col_name <-
      "comments" #the new comment column name is set
    if (!missing(comment_col_name) & comment_col_name != "" &
        !is.na(comment_col_name)) {
      prev_comment_col_name <- as.character(comment_col_name)
    }
    eval(parse(
      text = paste0("return_df$", prev_comment_col_name, " <- comment_list")
    )) #this writes the comments in the comments column
  }

  return_df$CARTBEQmcg_combined <-
    recalculated_CARTBEQmcg_std_list #A new column is created and its values are assigned as the calculated values this function has created.
  return(return_df)
}
