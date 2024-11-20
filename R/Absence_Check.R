#---
#Title: Absence_Check
#Author: Thomas Codd - https://github.com/TomCodd
#Contributor: Lucia Segovia de la Revilla  - https://github.com/LuciaSegovia
#Version: V1.0.0
#Changelog:

#Github: https://github.com/TomCodd/NutritionTools
#---

#' @title Absence Checker
#' @description The Absence Checker runs over a data.frame containing food items
#'   with CPC codes, and checks those food items and the nutrients they contain
#'   against a list. If a nutrient is showing as present when that food item
#'   shouldn't contain that nutrient, the Absence_Check function will sort those
#'   values, depending on the user input.
#' @param df Required - The data.frame which contains the food items you are
#'   looking to check.
#' @param assumed_zero_table Optional - default: \code{Absence_Table} - Choose
#'   whether to use the supplied preset table which contains information on
#'   which CPC code shouldn't contain certain nutrients.
#' @param assume_zero_absence_inputs Optional - default: \code{c("a")}
#'   - If using the default \code{assumed_zero_table}, this allows you to pick
#'   which options for selection you want. Choosing \code{c("a")} only applies
#'   corrections that are certain, \code{c("a", "b")} results in corrections
#'   being applied to a broader selection of checks and food groups, with high
#'   certainty, but less certain than just using \code{c("a")}.
#' @param method Required - default: \code{"check"} - Either \code{"check"},
#'   \code{"fill_all"},  or \code{"fill_blank"}. If set to \code{"check"}, the
#'   function will print out warnings where it encounters values which should be
#'   zero. If set to \code{"fill_all"}, the function will correct any incorrect
#'   values to 0. If set to \code{"fill_blank"}, the function will correct
#'   incorrect values to 0 only if they're missing (e.g. NA or "").
#' @param CPC_Code_Column Required - default: \code{"CPC_Code"} - the name of
#'   the column in \code{df} which contains the CPC codes for the food items.
#' @param comments Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. Choose whether to record the changes made. It is strongly
#'   recommended to leave this as \code{TRUE} to maintain records of any changes
#'   in the data.
#' @param comment_col Optional - default: \code{"comments"} - Required if
#'   \code{comments} is set to \code{TRUE}. The name of the column in \code{df}
#'   where comments should be stored. If the entry for \code{comment_col} is not
#'   the name of a column in \code{df}, then a new column will be created with
#'   the name of the entry.
#' @param Water_check Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. Decide whether you would like to check the Water values in
#'   \code{df}.
#' @param water_column Optional - default: \code{"WATERg"} - Required if
#'   \code{comments} is set to \code{TRUE}. The name of the column in \code{df}
#'   which contains water values.
#' @param CHOAVL_check Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. Decide whether you would like to check the Available
#'   Carbohydrate values in \code{df}.
#' @param CHOAVL_column Optional - default: \code{"CHOAVLg"} - Required if
#'   \code{CHOAVL_check} is set to \code{TRUE}. The name of the column in
#'   \code{df} which contains Available Carbohydrate values.
#' @param CHOAVLDF_check Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. Decide whether you would like to check the Available
#'   Carbohydrate (calculated by difference) values in \code{df}.
#' @param CHOAVLDFg_std_column Optional - default:
#'   \code{"CHOAVLDFg_standardised"} - Required if \code{CHOAVLDF_check} is set
#'   to \code{TRUE}. The name of the column in \code{df} which contains
#'   Available Carbohydrate (calculated by difference) values.
#' @param ALC_check Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. Decide whether you would like to check the Alcohol values in
#'   \code{df}.
#' @param ALC_column Optional - default: \code{"ALCg"} - Required if
#'   \code{ALC_check} is set to \code{TRUE}. The name of the column in \code{df}
#'   which contains Alcohol values.
#' @param FIBG_check Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. Decide whether you would like to check the Fibre values in
#'   \code{df}.
#' @param FIBG_column Optional - default: \code{"FIBGg"} - Required if
#'   \code{FIBG_check} is set to \code{TRUE}. The name of the column in
#'   \code{df} which contains Fibre values.
#' @param NIA_check Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. Decide whether you would like to check the Niacin values in
#'   \code{df}.
#' @param NIA_column Optional - default: \code{"NIAmg"} - Required if
#'   \code{NIA_check} is set to \code{TRUE}. The name of the column in \code{df}
#'   which contains Niacin values.
#' @param VITB12_check Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. Decide whether you would like to check the Vitamin B12 values
#'   in \code{df}.
#' @param VITB6_check Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. Decide whether you would like to check the Vitamin B6 values
#'   in \code{df}.
#' @param VITB12_column Optional - default: \code{"VITB12mcg"} - Required if
#'   \code{VITB12_check} is set to \code{TRUE}. The name of the column in
#'   \code{df} which contains Vitamin B12 values.
#' @param VITB6_column Optional - default: \code{"VITB6_mg_standardised"} -
#'   Required if \code{VITB6_check} is set to \code{TRUE}. The name of the
#'   column in \code{df} which contains Vitamin B6 values.
#' @param CU_check Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. Decide whether you would like to check the Copper values in
#'   \code{df}.
#' @param CU_column Optional - default: \code{"CUmg"} - Required if
#'   \code{CU_check} is set to \code{TRUE}. The name of the column in \code{df}
#'   which contains Copper values.
#' @param F22D6N3_check Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. Decide whether you would like to check the F22D6N3 values in
#'   \code{df}.
#' @param F22D6N3_column Optional - default: \code{"F22D6N3g"} - Required if
#'   \code{F22D6N3_check} is set to \code{TRUE}. The name of the column in
#'   \code{df} which contains F22D6N3 values.
#' @param F20D5N3_check Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. Decide whether you would like to check the F20D5N3 values in
#'   \code{df}.
#' @param F20D5N3_column Optional - default: \code{"F20D5N3g"} - Required if
#'   \code{F20D5N3_check} is set to \code{TRUE}. The name of the column in
#'   \code{df} which contains F20D5N3 values.
#' @param assumed_zero_water_col Optional - default: \code{"WATERg"} - The
#'   column in the \code{assumed_zero_table} that contains the absence status of
#'   water for different food groups. If using \code{assumed_zero_table}, and if
#'   using the default table, there is no need to change this. If using a
#'   modified table, please change this to match the column that lists water in
#'   that modified table.
#' @param assumed_zero_CHOAVL_col Optional - default: \code{"CHOAVLg"} - The
#'   column in the \code{assumed_zero_table} that contains the absence status of
#'   Carbohydrates for different food groups. If using
#'   \code{assumed_zero_table}, and if using the default table, there is no need
#'   to change this. If using a modified table, please change this to match the
#'   column that lists Carbohydrates in that modified table.
#' @param assumed_zero_CHOAVLDF_col Optional - default:
#'   \code{"CHOAVLDFg_standardised"} - The column in the
#'   \code{assumed_zero_table} that contains the absence status of Carbohydrates
#'   calculated by difference for different food groups. If using
#'   \code{assumed_zero_table}, and if using the default table, there is no need
#'   to change this. If using a modified table, please change this to match the
#'   column that lists Carbohydrates calculated by difference in that modified
#'   table.
#' @param assumed_zero_ALC_col Optional - default: \code{"ALCg"} - The column in
#'   the \code{assumed_zero_table} that contains the absence status of alcohol
#'   for different food groups. If using \code{assumed_zero_table}, and if using
#'   the default table, there is no need to change this. If using a modified
#'   table, please change this to match the column that lists Alcohol in that
#'   modified table.
#' @param assumed_zero_FIBG_col Optional - default: \code{"FIBGg"} - The column
#'   in the \code{assumed_zero_table} that contains the absence status of Fibre
#'   for different food groups. If using \code{assumed_zero_table}, and if using
#'   the default table, there is no need to change this. If using a modified
#'   table, please change this to match the column that lists Fibre in that
#'   modified table.
#' @param assumed_zero_NIA_col Optional - default: \code{"NIAmg"} - The column
#'   in the \code{assumed_zero_table} that contains the absence status of Niacin
#'   for different food groups. If using \code{assumed_zero_table}, and if using
#'   the default table, there is no need to change this. If using a modified
#'   table, please change this to match the column that lists Niacin in that
#'   modified table.
#' @param assumed_zero_VITB6_col Optional - default:
#'   \code{"VITB6_mg_standardised"} - The column in the
#'   \code{assumed_zero_table} that contains the absence status of Vitamin B6
#'   for different food groups. If using \code{assumed_zero_table}, and if using
#'   the default table, there is no need to change this. If using a modified
#'   table, please change this to match the column that lists Vitamin B6 in that
#'   modified table.
#' @param assumed_zero_VITB12_col Optional - default: \code{"VITB12mg"} - The
#'   column in the \code{assumed_zero_table} that contains the absence status of
#'   Vitamin B12 for different food groups. If using \code{assumed_zero_table},
#'   and if using the default table, there is no need to change this. If using a
#'   modified table, please change this to match the column that lists Vitamin
#'   B12 in that modified table.
#' @param assumed_zero_F22D6N3_col Optional - default: \code{"F22D6N3g"} - The
#'   column in the \code{assumed_zero_table} that contains the absence status of
#'   F22D6N3 for different food groups. If using \code{assumed_zero_table}, and
#'   if using the default table, there is no need to change this. If using a
#'   modified table, please change this to match the column that lists F22D6N3
#'   in that modified table.
#' @param assumed_zero_F20D5N3_col Optional - default: \code{"F20D5N3g"} - The
#'   column in the \code{assumed_zero_table} that contains the absence status of
#'   F20D5N3 for different food groups. If using \code{assumed_zero_table}, and
#'   if using the default table, there is no need to change this. If using a
#'   modified table, please change this to match the column that lists F20D5N3
#'   in that modified table.
#' @param assumed_zero_CU_col Optional - default: \code{"CUmg"} - The column in
#'   the \code{assumed_zero_table} that contains the absence status of Copper
#'   for different food groups. If using \code{assumed_zero_table}, and if using
#'   the default table, there is no need to change this. If using a modified
#'   table, please change this to match the column that lists Copper in that
#'   modified table.
#' @param assumed_zero_CPC_column Optional - default: \code{"CPC_Code"} - The
#'   column in the \code{assumed_zero_table} that contains the CPC Codes for
#'   different food groups. If using \code{assumed_zero_table}, and if using the
#'   default table, there is no need to change this. If using a modified table,
#'   please change this to match the column that lists the CPC Codes in that
#'   modified table.
#' @param No_Water_CPC_Codes Optional - default: \code{c("")} - Required if not
#'   using the \code{assumed_zero_table} input, if \code{water_check} is
#'   \code{TRUE}. Please input the CPC codes where you wouldn't expect to find
#'   Water.
#' @param No_CHOAVL_CPC_Codes Optional - default: \code{c("")} - Required if not
#'   using the \code{assumed_zero_table} input, if \code{CHOAVL_check} is
#'   \code{TRUE}. Please input the CPC codes where you wouldn't expect to find
#'   Carbohydrates.
#' @param No_CHOAVLDFg_standardised_CPC_Codes Optional - default: \code{c("")} -
#'   Required if not using the \code{assumed_zero_table} input, if
#'   \code{CHOAVLDF_check} is \code{TRUE}. Please input the CPC codes where you
#'   wouldn't expect to find Carbohydrates calculated by difference.
#' @param No_ALC_CPC_Codes Optional - default: \code{c("")} - Required if not
#'   using the \code{assumed_zero_table} input, if \code{ALC_check} is
#'   \code{TRUE}. Please input the CPC codes where you wouldn't expect to find
#'   Alcohol.
#' @param No_FIBG_CPC_Codes Optional - default: \code{c("")} - Required if not
#'   using the \code{assumed_zero_table} input, if \code{FIBG_check} is
#'   \code{TRUE}. Please input the CPC codes where you wouldn't expect to find
#'   Fibre.
#' @param No_NIA_CPC_Codes Optional - default: \code{c("")} - Required if not
#'   using the \code{assumed_zero_table} input, if \code{NIA_check} is
#'   \code{TRUE}. Please input the CPC codes where you wouldn't expect to find
#'   Niacin.
#' @param No_VITB6_CPC_Codes Optional - default: \code{c("")} - Required if not
#'   using the \code{assumed_zero_table} input, if \code{VITB6_check} is
#'   \code{TRUE}. Please input the CPC codes where you wouldn't expect to find
#'   Vitamin B6.
#' @param No_VITB12_CPC_Codes Optional - default: \code{c("")} - Required if not
#'   using the \code{assumed_zero_table} input, if \code{VITB12_check} is
#'   \code{TRUE}. Please input the CPC codes where you wouldn't expect to find
#'   Vitamin B12.
#' @param No_F22D6N3_CPC_Codes Optional - default: \code{c("")} - Required if not
#'   using the \code{assumed_zero_table} input, if \code{F22D6N3_check} is
#'   \code{TRUE}. Please input the CPC codes where you wouldn't expect to find
#'   F22D6N3.
#' @param No_F20D5N3_CPC_Codes Optional - default: \code{c("")} - Required if not
#'   using the \code{assumed_zero_table} input, if \code{F20D5N3_check} is
#'   \code{TRUE}. Please input the CPC codes where you wouldn't expect to find
#'   F20D5N3.
#' @param No_CU_CPC_Codes Optional - default: \code{c("")} - Required if not
#'   using the \code{assumed_zero_table} input, if \code{CU_check} is
#'   \code{TRUE}. Please input the CPC codes where you wouldn't expect to find
#'   Copper.
#' @return If \code{method = "check"}, the function will output a list of
#'   problematic entries in the data.frame to the console for manual
#'   investigation. If \code{method = "fill_all"}, the output will be a
#'   data.frame that mimics the input \code{df}, but with nutrient values
#'   changed where an error is detected - e.g. if a food item is in a CPC group
#'   that shouldn't contain water, and it does, then the water value for that
#'   item will be changed to 0. If \code{method = "fill_blank"}, the output will
#'   be a data.frame that mimics the input \code{df}, but with nutrient values
#'   changed where blank value is detect where we can assume the value to be 0 -
#'   e.g. if a food item is in a CPC group that shouldn't contain water, and the
#'   water value for that item is NA, or "", then the water value for that item
#'   will be changed to 0.
#' @export


# Main Function ----
#Essentially a wrapper function.

Absence_Check <- function(df,
                          assumed_zero_table = Absence_Table,
                          assume_zero_absence_inputs = c("a"),
                          method = "check",
                          CPC_Code_Column = "CPC_Code" ,
                          comments = TRUE,
                          comment_col = "comments" ,
                          Water_check = TRUE,
                          water_column = "WATERg",
                          CHOAVL_check = TRUE,
                          CHOAVL_column = "CHOAVLg",
                          CHOAVLDF_check = TRUE,
                          CHOAVLDFg_std_column = "CHOAVLDFg_standardised",
                          ALC_check = TRUE,
                          ALC_column = "ALCg",
                          FIBG_check = TRUE,
                          FIBG_column = "FIBGg",
                          NIA_check = TRUE,
                          NIA_column = "NIAmg",
                          VITB6_check = TRUE,
                          VITB6_column = "VITB6_mg_standardised",
                          VITB12_check = TRUE,
                          VITB12_column = "VITB12mcg",
                          CU_check = TRUE,
                          CU_column = "CUmg",
                          F22D6N3_check = TRUE,
                          F22D6N3_column = "F22D6N3g",
                          F20D5N3_check = TRUE,
                          F20D5N3_column = "F20D5N3g",
                          assumed_zero_water_col = "WATERg",
                          assumed_zero_CHOAVL_col = "CHOAVLg",
                          assumed_zero_CHOAVLDF_col = "CHOAVLDFg_standardised",
                          assumed_zero_ALC_col = "ALCg",
                          assumed_zero_FIBG_col = "FIBGg",
                          assumed_zero_NIA_col = "NIAmg",
                          assumed_zero_VITB6_col = "VITB6_mg_standardised",
                          assumed_zero_VITB12_col = "VITB12mg",
                          assumed_zero_F22D6N3_col = "F22D6N3g",
                          assumed_zero_F20D5N3_col = "F20D5N3g",
                          assumed_zero_CU_col = "CUmg",
                          assumed_zero_CPC_column = "CPC_Code",
                          No_Water_CPC_Codes = c(""),
                          No_CHOAVL_CPC_Codes = c(""),
                          No_CHOAVLDFg_standardised_CPC_Codes = c(""),
                          No_ALC_CPC_Codes = c(""),
                          No_FIBG_CPC_Codes = c(""),
                          No_NIA_CPC_Codes = c(""),
                          No_VITB6_CPC_Codes = c(""),
                          No_VITB12_CPC_Codes = c(""),
                          No_F22D6N3_CPC_Codes = c(""),
                          No_F20D5N3_CPC_Codes = c(""),
                          No_CU_CPC_Codes = c("")
                          ) {

  if(!is.data.frame(df)){ #Checks if the df input is correct or not
    message("df is not a data.frame. Please enter a data.frame.")
    return()
  }

  # The next block is all checking if the logical inputs are true or false, and stopping if they aren't.

  if(!is.logical(comments)){
    message("comments input is not logical. Please enter TRUE or FALSE.")
    return()
  }

  if(!is.logical(Water_check)){
    message("Water_check input is not logical. Please enter TRUE or FALSE.")
    return()
  }

  if(!is.logical(CHOAVL_check)){
    message("CHOAVL_check input is not logical. Please enter TRUE or FALSE.")
    return()
  }

  if(!is.logical(CHOAVLDF_check)){
    message("CHOAVLDF_check input is not logical. Please enter TRUE or FALSE.")
    return()
  }

  if(!is.logical(ALC_check)){
    message("ALC_check input is not logical. Please enter TRUE or FALSE.")
    return()
  }

  if(!is.logical(FIBG_check)){
    message("FIBG_check input is not logical. Please enter TRUE or FALSE.")
    return()
  }

  if(!is.logical(NIA_check)){
    message("NIA_check input is not logical. Please enter TRUE or FALSE.")
    return()
  }

  if(!is.logical(VITB12_check)){
    message("VITB12_check input is not logical. Please enter TRUE or FALSE.")
    return()
  }

  if(!is.logical(VITB6_check)){
    message("VITB6_check input is not logical. Please enter TRUE or FALSE.")
    return()
  }

  if(!is.logical(CU_check)){
    message("CU_check input is not logical. Please enter TRUE or FALSE.")
    return()
  }

  if(!is.logical(F22D6N3_check)){
    message("F22D6N3_check input is not logical. Please enter TRUE or FALSE.")
    return()
  }

  if(!is.logical(F20D5N3_check)){
    message("F20D5N3_check input is not logical. Please enter TRUE or FALSE.")
    return()
  }

  # Then the specific assumed_zero_table checks

  if(!is.data.frame(assumed_zero_table) & !isFALSE(assumed_zero_table)){ #Checks if the assume zero table input is correct or not
    message(
      "assumed_zero_table is not either a data.frame or FALSE. If using the assume_zero_table, please set the input to the name of the dataframe containing the relevant information (e.g. Absence_Table). If not using this ability, please set to FALSE."
    )
    return()
  }

  if(is.data.frame(assumed_zero_table)){ #Checks if the input is a df

    #Noticed some CPC codes are formatted weirdly on import - this finds the ones with more than 2 decimal places, converts them to a number, rounds them back to 2, then converts back to a character, and overwrites the original.
    #I think there was a weird floating point number problem on import, that this fixes.

    assumed_zero_table[[assumed_zero_CPC_column]][grepl("\\.[0-9][0-9][0-9]", assumed_zero_table[[assumed_zero_CPC_column]])] <- as.character(round(as.numeric(assumed_zero_table[[assumed_zero_CPC_column]][grepl("\\.[0-9][0-9][0-9]", assumed_zero_table[[assumed_zero_CPC_column]])]), 2))


    #Proceeds with a set of checks to make sure that the inputs are correct if using the assume zero df

    if(isTRUE(Water_check) && !(assumed_zero_water_col %in% colnames(assumed_zero_table))){
      message("assumed_zero_water_col is not a column name in assumed_zero_table. Please input a valid column name from assumed_zero_table.")
      return()
    }

    if(isTRUE(CHOAVL_check) && !(assumed_zero_CHOAVL_col %in% colnames(assumed_zero_table))){
      message("assumed_zero_CHOAVL_col is not a column name in assumed_zero_table. Please input a valid column name from assumed_zero_table.")
      return()
    }

    if(isTRUE(CHOAVLDF_check) && !(assumed_zero_CHOAVLDF_col %in% colnames(assumed_zero_table))){
      message("assumed_zero_CHOAVLDF_col is not a column name in assumed_zero_table. Please input a valid column name from assumed_zero_table.")
      return()
    }

    if(isTRUE(ALC_check) && !(assumed_zero_ALC_col %in% colnames(assumed_zero_table))){
      message("assumed_zero_ALC_col is not a column name in assumed_zero_table. Please input a valid column name from assumed_zero_table.")
      return()
    }

    if(isTRUE(FIBG_check) && !(assumed_zero_FIBG_col %in% colnames(assumed_zero_table))){
      message("assumed_zero_FIBG_col is not a column name in assumed_zero_table. Please input a valid column name from assumed_zero_table.")
      return()
    }

    if(isTRUE(NIA_check) && !(assumed_zero_NIA_col %in% colnames(assumed_zero_table))){
      message("assumed_zero_NIA_col is not a column name in assumed_zero_table. Please input a valid column name from assumed_zero_table.")
      return()
    }

    if(isTRUE(VITB12_check) && !(assumed_zero_VITB12_col %in% colnames(assumed_zero_table))){
      message("assumed_zero_VITB12_col is not a column name in assumed_zero_table. Please input a valid column name from assumed_zero_table.")
      return()
    }

    if(isTRUE(VITB6_check) && !(assumed_zero_VITB6_col %in% colnames(assumed_zero_table))){
      message("assumed_zero_VITB6_col is not a column name in assumed_zero_table. Please input a valid column name from assumed_zero_table.")
      return()
    }

    if(isTRUE(CU_check) && !(assumed_zero_CU_col %in% colnames(assumed_zero_table))){
      message("assumed_zero_CU_col is not a column name in assumed_zero_table. Please input a valid column name from assumed_zero_table.")
      return()
    }

    if(isTRUE(F22D6N3_check) && !(assumed_zero_F22D6N3_col %in% colnames(assumed_zero_table))){
      message("assumed_zero_F22D6N3_col is not a column name in assumed_zero_table. Please input a valid column name from assumed_zero_table.")
      return()
    }

    if(isTRUE(F20D5N3_check) && !(assumed_zero_F20D5N3_col %in% colnames(assumed_zero_table))){
      message("assumed_zero_F20D5N3_col is not a column name in assumed_zero_table. Please input a valid column name from assumed_zero_table.")
      return()
    }
  } else {

    #If the inputs are correct, and assume zero df isn't present, then the user must be inputting the codes manually.
    # This block makes sure the manual inputs are correct.

    if(isTRUE(Water_check) && !length(No_Water_CPC_Codes[!No_Water_CPC_Codes %in% ""]) > 0){
      message("Water_check is selected, but no items are present in No_Water_CPC_Codes. Please input the CPC codes you would like to mark as those that aren't supposed to contain water.")
      return()
    }

    if(isTRUE(CHOAVL_check) && !length(No_CHOAVL_CPC_Codes[!No_CHOAVL_CPC_Codes %in% ""]) > 0){
      message("CHOAVL_check is selected, but no items are present in No_CHOAVL_CPC_Codes Please input the CPC codes you would like to mark as those that aren't supposed to contain CHOAVL.")
      return()
    }

    if(isTRUE(CHOAVLDF_check) && !length(No_CHOAVLDFg_standardised_CPC_Codes[!No_CHOAVLDFg_standardised_CPC_Codes %in% ""]) > 0){
      message("CHOAVLDF_check is selected, but no items are present in No_CHOAVLDFg_standardised_CPC_Codes Please input the CPC codes you would like to mark as those that aren't supposed to contain CHOAVLDF.")
      return()
    }

    if(isTRUE(ALC_check) && !length(No_ALC_CPC_Codes[!No_ALC_CPC_Codes %in% ""]) > 0){
      message("ALC_check is selected, but no items are present in No_ALC_CPC_Codes Please input the CPC codes you would like to mark as those that aren't supposed to contain ALC.")
      return()
    }

    if(isTRUE(FIBG_check) && !length(No_FIBG_CPC_Codes[!No_FIBG_CPC_Codes %in% ""]) > 0){
      message("FIBG_check is selected, but no items are present in No_FIBG_CPC_Codes Please input the CPC codes you would like to mark as those that aren't supposed to contain FIBG.")
      return()
    }

    if(isTRUE(NIA_check) && !length(No_NIA_CPC_Codes[!No_NIA_CPC_Codes %in% ""]) > 0){
      message("NIA_check is selected, but no items are present in No_NIA_CPC_Codes Please input the CPC codes you would like to mark as those that aren't supposed to contain NIA.")
      return()
    }

    if(isTRUE(VITB12_check) && !length(No_VITB12_CPC_Codes[!No_VITB12_CPC_Codes %in% ""]) > 0){
      message("VITB12_check is selected, but no items are present in No_VITB12_CPC_Codes Please input the CPC codes you would like to mark as those that aren't supposed to contain VITB12.")
      return()
    }

    if(isTRUE(VITB6_check) && !length(No_VITB6_CPC_Codes[!No_VITB6_CPC_Codes %in% ""]) > 0){
      message("VITB6_check is selected, but no items are present in No_VITB6_CPC_Codes Please input the CPC codes you would like to mark as those that aren't supposed to contain VITB6.")
      return()
    }

    if(isTRUE(CU_check) && !length(No_CU_CPC_Codes[!No_CU_CPC_Codes %in% ""]) > 0){
      message("CU_check is selected, but no items are present in No_CU_CPC_Codes Please input the CPC codes you would like to mark as those that aren't supposed to contain CU.")
      return()
    }

    if(isTRUE(F22D6N3_check) && !length(No_F22D6N3_CPC_Codes[!No_F22D6N3_CPC_Codes %in% ""]) > 0){
      message("F22D6N3_check is selected, but no items are present in No_F22D6N3_CPC_Codes Please input the CPC codes you would like to mark as those that aren't supposed to contain F22D6N3.")
      return()
    }

    if(isTRUE(F20D5N3_check) && !length(No_F20D5N3_CPC_Codes[!No_F20D5N3_CPC_Codes %in% ""]) > 0){
      message("F20D5N3_check is selected, but no items are present in No_F20D5N3_CPC_Codes Please input the CPC codes you would like to mark as those that aren't supposed to contain F20D5N3.")
      return()
    }
  }

  if(is.data.frame(assumed_zero_table)){

    # Converts the table into the inputs. Between this and the above block, should ensure all the selected tests have an input.

    No_Water_CPC_Codes <- assumed_zero_table[assumed_zero_table[[assume_zero_water_col]] %in% assume_zero_absence_inputs, assume_zero_CPC_column][[assume_zero_CPC_column]] #Extracts the relevant CPC codes
    No_CHOAVL_CPC_Codes <- assumed_zero_table[assumed_zero_table[[assume_zero_CHOAVL_col]] %in% assume_zero_absence_inputs, assume_zero_CPC_column][[assume_zero_CPC_column]] #Extracts the relevant CPC codes
    No_CHOAVLDFg_standardised_CPC_Codes <- assumed_zero_table[assumed_zero_table[[assume_zero_CHOAVLDF_col]] %in% assume_zero_absence_inputs, assume_zero_CPC_column][[assume_zero_CPC_column]] #Extracts the relevant CPC codes
    No_ALC_CPC_Codes <- assumed_zero_table[assumed_zero_table[[assume_zero_ALC_col]] %in% assume_zero_absence_inputs, assume_zero_CPC_column][[assume_zero_CPC_column]] #Extracts the relevant CPC codes
    No_FIBG_CPC_Codes <- assumed_zero_table[assumed_zero_table[[assume_zero_FIBG_col]] %in% assume_zero_absence_inputs, assume_zero_CPC_column][[assume_zero_CPC_column]] #Extracts the relevant CPC codes
    No_NIA_CPC_Codes <- assumed_zero_table[assumed_zero_table[[assume_zero_NIA_col]] %in% assume_zero_absence_inputs, assume_zero_CPC_column][[assume_zero_CPC_column]] #Extracts the relevant CPC codes
    No_VITB12_CPC_Codes <- assumed_zero_table[assumed_zero_table[[assume_zero_VITB12_col]] %in% assume_zero_absence_inputs, assume_zero_CPC_column][[assume_zero_CPC_column]] #Extracts the relevant CPC codes
    No_F22D6N3_CPC_Codes <- assumed_zero_table[assumed_zero_table[[assume_zero_F22D6N3_col]] %in% assume_zero_absence_inputs, assume_zero_CPC_column][[assume_zero_CPC_column]] #Extracts the relevant CPC codes
    No_F20D5N3_CPC_Codes <- assumed_zero_table[assumed_zero_table[[assume_zero_F20D5N3_col]] %in% assume_zero_absence_inputs, assume_zero_CPC_column][[assume_zero_CPC_column]] #Extracts the relevant CPC codes
    No_CU_CPC_Codes <- assumed_zero_table[assumed_zero_table[[assume_zero_CU_col]] %in% assume_zero_absence_inputs, assume_zero_CPC_column][[assume_zero_CPC_column]] #Extracts the relevant CPC codes
    No_VITB6_CPC_Codes <- assumed_zero_table[assumed_zero_table[[assume_zero_VITB6_col]] %in% assume_zero_absence_inputs, assume_zero_CPC_column][[assume_zero_CPC_column]] #Extracts the relevant CPC codes
  }


  #Need to round the CPC codes! -----------------------
  #Only if they have a decimal point though


  # Calling functions ----

  # Check functions mean we can't have an output

  if (tolower(method) %in% "check") {
    if(Water_check == TRUE){
      Absence_Check_Water(df = df, water_column = water_column, method = method, CPC_Code_Column = CPC_Code_Column, No_Water_CPC_Codes = No_Water_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(CHOAVL_check == TRUE){
      Absence_Check_CHOAVLg(df = df, CHOAVL_column = CHOAVL_column, No_CHOAVL_CPC_Codes = No_CHOAVL_CPC_Codes, method = method, CPC_Code_Column = CPC_Code_Column, comments = comments, comment_col = comment_col)
    }

    if(CHOAVLDF_check == TRUE){
      Absence_Check_CHOAVLDFg_std(df = df, CHOAVLDFg_std_column = CHOAVLDFg_std_column, No_CHOAVLDFg_standardised_CPC_Codes = No_CHOAVLDFg_standardised_CPC_Codes, method = method, CPC_Code_Column = CPC_Code_Column, comments = comments, comment_col = comment_col)
    }

    if(ALC_check == TRUE){
      Absence_Check_ALC(df = df, ALC_column = ALC_column, method = method, CPC_Code_Column = CPC_Code_Column, No_ALC_CPC_Codes = No_ALC_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(FIBG_check == TRUE){
      Absence_Check_FIBG(df = df, FIBG_column = FIBG_column, method = method, CPC_Code_Column = CPC_Code_Column, No_FIBG_CPC_Codes = No_FIBG_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(NIA_check == TRUE){
      Absence_Check_NIA(df = df, NIA_column = NIA_column, method = method, CPC_Code_Column = CPC_Code_Column, No_NIA_CPC_Codes = No_NIA_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(VITB12_check == TRUE){
      Absence_Check_VITB12(df = df, VITB12_column = VITB12_column, method = method, CPC_Code_Column = CPC_Code_Column, No_VITB12_CPC_Codes = No_VITB12_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(VITB6_check == TRUE){
      Absence_Check_VITB6(df = df, VITB6_column = VITB6_column, method = method, CPC_Code_Column = CPC_Code_Column, No_VITB6_CPC_Codes = No_VITB6_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(CU_check == TRUE){
      Absence_Check_CU(df = df, CU_column = CU_column, method = method, CPC_Code_Column = CPC_Code_Column, No_CU_CPC_Codes = No_CU_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(F20D5N3_check == TRUE){
      Absence_Check_F20D5N3(df = df, F20D5N3_column = F20D5N3_column, method = method, CPC_Code_Column = CPC_Code_Column, No_F20D5N3_CPC_Codes = No_F20D5N3_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(F22D6N3_check == TRUE){
      Absence_Check_F22D6N3(df = df, F22D6N3_column = F22D6N3_column, method = method, CPC_Code_Column = CPC_Code_Column, No_F22D6N3_CPC_Codes = No_F22D6N3_CPC_Codes, comments = comments, comment_col = comment_col)
    }
  } else {
    output_df <- df #rename to output - then run output, adding to itself each time.

    if(Water_check == TRUE){
      output_df <- Absence_Check_Water(df = output_df, water_column = water_column, method = method, CPC_Code_Column = CPC_Code_Column, No_Water_CPC_Codes = No_Water_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(CHOAVL_check == TRUE){
      output_df <- Absence_Check_CHOAVLg(df = output_df, CHOAVL_column = CHOAVL_column, No_CHOAVL_CPC_Codes = No_CHOAVL_CPC_Codes, method = method, CPC_Code_Column = CPC_Code_Column, comments = comments, comment_col = comment_col)
    }

    if(CHOAVLDF_check == TRUE){
      output_df <- Absence_Check_CHOAVLDFg_std(df = output_df, CHOAVLDFg_std_column = CHOAVLDFg_std_column, No_CHOAVLDFg_standardised_CPC_Codes = No_CHOAVLDFg_standardised_CPC_Codes, method = method, CPC_Code_Column = CPC_Code_Column, comments = comments, comment_col = comment_col)
    }

    if(ALC_check == TRUE){
      output_df <- Absence_Check_ALC(df = output_df, ALC_column = ALC_column, method = method, CPC_Code_Column = CPC_Code_Column, No_ALC_CPC_Codes = No_ALC_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(FIBG_check == TRUE){
      output_df <- Absence_Check_FIBG(df = output_df, FIBG_column = FIBG_column, method = method, CPC_Code_Column = CPC_Code_Column, No_FIBG_CPC_Codes = No_FIBG_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(NIA_check == TRUE){
      output_df <- Absence_Check_NIA(df = output_df, NIA_column = NIA_column, method = method, CPC_Code_Column = CPC_Code_Column, No_NIA_CPC_Codes = No_NIA_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(VITB12_check == TRUE){
      output_df <- Absence_Check_VITB12(df = output_df, VITB12_column = VITB12_column, method = method, CPC_Code_Column = CPC_Code_Column, No_VITB12_CPC_Codes = No_VITB12_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(VITB6_check == TRUE){
      output_df <- Absence_Check_VITB6(df = output_df, VITB6_column = VITB6_column, method = method, CPC_Code_Column = CPC_Code_Column, No_VITB6_CPC_Codes = No_VITB6_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(CU_check == TRUE){
      output_df <- Absence_Check_CU(df = output_df, CU_column = CU_column, method = method, CPC_Code_Column = CPC_Code_Column, No_CU_CPC_Codes = No_CU_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(F20D5N3_check == TRUE){
      output_df <- Absence_Check_F20D5N3(df = output_df, F20D5N3_column = F20D5N3_column, method = method, CPC_Code_Column = CPC_Code_Column, No_F20D5N3_CPC_Codes = No_F20D5N3_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    if(F22D6N3_check == TRUE){
      output_df <- Absence_Check_F22D6N3(df = output_df, F22D6N3_column = F22D6N3_column, method = method, CPC_Code_Column = CPC_Code_Column, No_F22D6N3_CPC_Codes = No_F22D6N3_CPC_Codes, comments = comments, comment_col = comment_col)
    }

    return(output_df)
  }
}

# Individual Absence Check Functions ----

Absence_Check_Water <- function(df,
                                water_column = "WATERg",
                                method = "Check",
                                CPC_Code_Column = "CPC_Code",
                                No_Water_CPC_Codes = Standard_No_Water_CPC_Codes,
                                comments = TRUE,
                                comment_col = "comments") {
  #Check, Fill_all, or Fill_blank.

  # Input checks ----

  stopifnot(
    "df is not a data frame - please input a data frame" = is.data.frame(df),
    #Checks to see if the df item is a data frame
    "The water_column is not a character or string - please input a character or string that is a column name in df, e.g. 'WATERg'" = is.character(water_column),
    #checks to see if the water_column is a character string
    "The CPC_Code_Column is not a character or string - please input a character or string that is a column name in df, e.g. 'CPC_Codes'" = is.character(CPC_Code_Column),
    #checks to see if the CPC_Code_Column is a character string
    "The water_column is not a column name in df - please input a string that is a column name in df." = water_column %in% colnames(df),
    #Checks if the column names are actually columns in the df
    "The CPC_Code_Column is not a column name in df - please input a string that is a column name in df." = CPC_Code_Column %in% colnames(df),
    "method option is not valid - please choose between 'Check', 'Fill_all', 'Fill_blank'." = tolower(method) %in% c(
      "check",
      "fill_all",
      "fill_blank"
    ),
    "No flagged CPC Codes present in the CPC Code column. Function stopped." = TRUE %in% (unique(df[[CPC_Code_Column]]) %in% No_Water_CPC_Codes)
  )



  if (tolower(method) %in% "check") {
    method_type <- 1
  }

  if (tolower(method) %in% "fill_all") {
    method_type <- 2
  }

  if (tolower(method) %in% "fill_blank") {
    method_type <- 3
  }

  # Column conversion checks ----

  df[[CPC_Code_Column]] <- as.character(df[[CPC_Code_Column]]) #convert fbs id col to character
  df[[water_column]] <- as.character(df[[water_column]]) #convert fbs id col to character


  # Comments column creation check ----

  #Sees if a comments column already exists, and creates one if not. Only applies on a Fill method, as this is where the fill is recorded.

  if (method_type %in% c(2, 3)) {
    if (comments == TRUE) {
      message("comments included")
      if (!(comment_col %in% colnames(df))) {
        message("No comments column detected, new one being created")
        df[[comment_col]] <- NA
      }
    }
  }


  # Non-0 rows ----
  Water_not_0 <- df[df[[CPC_Code_Column]] %in% No_Water_CPC_Codes &
                      !(df[[water_column]] %in% "0"), ]

  # method - Checks ----

  if(nrow(Water_not_0) == 0){ #Does nothing if there are no relevant rows
    if (method_type == 2){
      return(df)
    }
  } else {
    if (method_type == 1) {

      lapply(1:nrow(Water_not_0), function(i) {
        message(
          paste(
            "FLAG: Water value for row number",
            rownames(Water_not_0[i, ]),
            " is not 0; value is",
            Water_not_0[[water_column]][i],
            "."
          )
        )
      })
      #Option to produce code to create a df subset for inspection:
      #message("You Can run the message below to create a subset of the input df for closer inspection. Please replace 'df' with the variable name of the dataframe you inputted to this function.")
      #message(paste0("Non_zero_water <- df[rownames(df) %in% c(",  paste(rownames(Water_not_0), collapse = ", "), "), ]"))

      #Option to automatically create and view subset df for inspection:
      assign("Non_zero_water", Water_not_0, envir = globalenv())
      View(Non_zero_water)
    }


    # Method -  Fill All ----

    if (method_type == 2) {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(Water_not_0)) {
          #If so, loops through all items where Water isn't 0, but should be
          new_comment <- paste0(water_column,
                                " value changed from ",
                                Water_not_0[[water_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(Water_not_0[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[water_column]][df[[CPC_Code_Column]] %in% No_Water_CPC_Codes &
                           !(df[[water_column]] %in% "0")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
      return(df)
    }
  }

  #If the method type is 1, so just checking, this loops through rows where water isno 0 and prints the row numbers out.




  if (method_type == 3) {
    Water_blank <- df[df[[CPC_Code_Column]] %in% No_Water_CPC_Codes &
                        df[[water_column]] %in% c(NA, ""), ] #Creates a df of Water values which are either blank or NA
    if(nrow(Water_blank) == 0){
    } else {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(Water_blank)) {
          #If so, loops through all items where Water isn't 0, but should be
          new_comment <- paste0(water_column,
                                " value changed from ",
                                Water_blank[[water_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(Water_blank[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[water_column]][df[[CPC_Code_Column]] %in% No_Water_CPC_Codes &
                           df[[water_column]] %in% c(NA, "")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
    }
    return(df)
  }
}







Absence_Check_CHOAVLg <- function(df,
                                  CHOAVL_column = "CHOAVLg",
                                  method = "Check",
                                  CPC_Code_Column = "CPC_Code",
                                  No_CHOAVL_CPC_Codes = Standard_No_CHOAVL_CPC_Codes,
                                  comments = TRUE,
                                  comment_col = "comments") {
  #Check, Fill_all, or Fill_blank.

  # Input checks ----

  stopifnot(
    "df is not a data frame - please input a data frame" = is.data.frame(df),
    #Checks to see if the df item is a data frame
    "The CHOAVL_column is not a character or string - please input a character or string that is a column name in df, e.g. 'CHOAVLg'" = is.character(CHOAVL_column),
    #checks to see if the CHOAVL_column is a character string
    "The CPC_Code_Column is not a character or string - please input a character or string that is a column name in df, e.g. 'CPC_Codes'" = is.character(CPC_Code_Column),
    #checks to see if the CPC_Code_Column is a character string
    "The CHOAVL_column is not a column name in df - please input a string that is a column name in df." = CHOAVL_column %in% colnames(df),
    #Checks if the column names are actually columns in the df
    "The CPC_Code_Column is not a column name in df - please input a string that is a column name in df." = CPC_Code_Column %in% colnames(df),
    "method option is not valid - please choose between 'Check', 'Fill_all', 'Fill_blank'." = tolower(method) %in% c(
      "check",
      "fill_all",
      "fill_blank"
    ),
    "No flagged CPC Codes present in the CPC Code column. Function stopped." = TRUE %in% (unique(df[[CPC_Code_Column]]) %in% No_CHOAVL_CPC_Codes)
  )



  if (tolower(method) %in% "check") {
    method_type <- 1
  }

  if (tolower(method) %in% "fill_all") {
    method_type <- 2
  }

  if (tolower(method) %in% "fill_blank") {
    method_type <- 3
  }

  # Column conversion checks ----

  df[[CPC_Code_Column]] <- as.character(df[[CPC_Code_Column]]) #convert fbs id col to character
  df[[CHOAVL_column]] <- as.character(df[[CHOAVL_column]]) #convert fbs id col to character


  # Comments column creation check ----

  #Sees if a comments column already exists, and creates one if not. Only applies on a Fill method, as this is where the fill is recorded.

  if (method_type %in% c(2, 3)) {
    if (comments == TRUE) {
      message("comments included")
      if (!(comment_col %in% colnames(df))) {
        message("No comments column detected, new one being created")
        df[[comment_col]] <- NA
      }
    }
  }


  # Non-0 rows ----

  CHOAVL_not_0 <- df[df[[CPC_Code_Column]] %in% No_CHOAVL_CPC_Codes &
                       !(df[[CHOAVL_column]] %in% "0"), ]

  # method - Checks ----

  if(nrow(CHOAVL_not_0) == 0){
    #Does nothing if no relevant rows
    if(method_type == 2){
      return(df)
    }
  } else {
    if (method_type == 1) {

      lapply(1:nrow(CHOAVL_not_0), function(i) {
        message(
          paste(
            "FLAG: CHOAVL value for row number",
            rownames(CHOAVL_not_0[i, ]),
            " is not 0; value is",
            CHOAVL_not_0[[CHOAVL_column]][i],
            "."
          )
        )
      })
      #Option to produce code to create a df subset for inspection:
      #message("You Can run the message below to create a subset of the input df for closer inspection. Please replace 'df' with the variable name of the dataframe you inputted to this function.")
      #message(paste0("Non_zero_CHOAVL <- df[rownames(df) %in% c(",  paste(rownames(CHOAVL_not_0), collapse = ", "), "), ]"))

      #Option to automatically create and view subset df for inspection:
      assign("Non_zero_CHOAVL", CHOAVL_not_0, envir = globalenv())
      View(Non_zero_CHOAVL)
    }


    # Method -  Fill All ----

    if (method_type == 2) {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(CHOAVL_not_0)) {
          #If so, loops through all items where CHOAVL isn't 0, but should be
          new_comment <- paste0(CHOAVL_column,
                                " value changed from ",
                                CHOAVL_not_0[[CHOAVL_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(CHOAVL_not_0[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[CHOAVL_column]][df[[CPC_Code_Column]] %in% No_CHOAVL_CPC_Codes &
                            !(df[[CHOAVL_column]] %in% "0")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
      return(df)
    }
  }

  if (method_type == 3) {
    CHOAVL_blank <- df[df[[CPC_Code_Column]] %in% No_CHOAVL_CPC_Codes &
                         df[[CHOAVL_column]] %in% c(NA, ""), ] #Creates a df of CHOAVL values which are either blank or NA
    if(nrow(CHOAVL_blank) == 0){
      #Does nothing if no relevant rows
    } else {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(CHOAVL_blank)) {
          #If so, loops through all items where CHOAVL isn't 0, but should be
          new_comment <- paste0(CHOAVL_column,
                                " value changed from ",
                                CHOAVL_blank[[CHOAVL_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(CHOAVL_blank[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[CHOAVL_column]][df[[CPC_Code_Column]] %in% No_CHOAVL_CPC_Codes &
                            df[[CHOAVL_column]] %in% c(NA, "")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.

    }
    return(df)
  }
}




Absence_Check_CHOAVLDFg_std <- function(df,
                                        CHOAVLDFg_std_column = "CHOAVLDFg_standardised",
                                        method = "Check",
                                        CPC_Code_Column = "CPC_Code",
                                        No_CHOAVLDFg_standardised_CPC_Codes = Standard_No_CHOAVLDFg_standardised_CPC_Codes,
                                        comments = TRUE,
                                        comment_col = "comments") {
  #Check, Fill_all, or Fill_blank.

  # Input checks ----

  stopifnot(
    "df is not a data frame - please input a data frame" = is.data.frame(df),
    #Checks to see if the df item is a data frame
    "The CHOAVLDFg_std_column is not a character or string - please input a character or string that is a column name in df, e.g. 'CHOAVLDFg_standardised'" = is.character(CHOAVLDFg_std_column),
    #checks to see if the CHOAVLDFg_std_column is a character string
    "The CPC_Code_Column is not a character or string - please input a character or string that is a column name in df, e.g. 'CPC_Codes'" = is.character(CPC_Code_Column),
    #checks to see if the CPC_Code_Column is a character string
    "The CHOAVLDFg_std_column is not a column name in df - please input a string that is a column name in df." = CHOAVLDFg_std_column %in% colnames(df),
    #Checks if the column names are actually columns in the df
    "The CPC_Code_Column is not a column name in df - please input a string that is a column name in df." = CPC_Code_Column %in% colnames(df),
    "method option is not valid - please choose between 'Check', 'Fill_all', 'Fill_blank'." = tolower(method) %in% c(
      "check",
      "fill_all",
      "fill_blank"
    ),
    "No flagged CPC Codes present in the CPC Code column. Function stopped." = TRUE %in% (unique(df[[CPC_Code_Column]]) %in% No_CHOAVLDFg_standardised_CPC_Codes)
  )



  if (tolower(method) %in% "check") {
    method_type <- 1
  }

  if (tolower(method) %in% "fill_all") {
    method_type <- 2
  }

  if (tolower(method) %in% "fill_blank") {
    method_type <- 3
  }

  # Column conversion checks ----

  df[[CPC_Code_Column]] <- as.character(df[[CPC_Code_Column]]) #convert fbs id col to character
  df[[CHOAVLDFg_std_column]] <- as.character(df[[CHOAVLDFg_std_column]]) #convert fbs id col to character


  # Comments column creation check ----

  #Sees if a comments column already exists, and creates one if not. Only applies on a Fill method, as this is where the fill is recorded.

  if (method_type %in% c(2, 3)) {
    if (comments == TRUE) {
      message("comments included")
      if (!(comment_col %in% colnames(df))) {
        message("No comments column detected, new one being created")
        df[[comment_col]] <- NA
      }
    }
  }


  # Non-0 rows ----

  CHOAVLDFg_standardised_not_0 <- df[df[[CPC_Code_Column]] %in% No_CHOAVLDFg_standardised_CPC_Codes &
                                       !(df[[CHOAVLDFg_std_column]] %in% "0"), ]



  # method - Checks ----

  if(nrow(CHOAVLDFg_standardised_not_0) == 0){
    # Does nothing if no relevant rows
    if(method_type == 2){
      return(df)
    }
  } else {
    if (method_type == 1) {

      lapply(1:nrow(CHOAVLDFg_standardised_not_0), function(i) {
        message(
          paste(
            "FLAG: CHOAVLDFg_standardised value for row number",
            rownames(CHOAVLDFg_standardised_not_0[i, ]),
            " is not 0; value is",
            CHOAVLDFg_standardised_not_0[[CHOAVLDFg_std_column]][i],
            "."
          )
        )
      })
      #Option to produce code to create a df subset for inspection:
      #message("You Can run the message below to create a subset of the input df for closer inspection. Please replace 'df' with the variable name of the dataframe you inputted to this function.")
      #message(paste0("Non_zero_CHOAVLDFg_standardised <- df[rownames(df) %in% c(",  paste(rownames(CHOAVLDFg_standardised_not_0), collapse = ", "), "), ]"))

      #Option to automatically create and view subset df for inspection:
      assign("Non_zero_CHOAVLDFg_standardised",
             CHOAVLDFg_standardised_not_0,
             envir = globalenv())
      View(Non_zero_CHOAVLDFg_standardised)
    }


    # Method -  Fill All ----

    if (method_type == 2) {
      if(nrow(CHOAVLDFg_standardised_not_0) == 0){
        stop()
      }
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(CHOAVLDFg_standardised_not_0)) {
          #If so, loops through all items where CHOAVLDFg_standardised isn't 0, but should be
          new_comment <- paste0(
            CHOAVLDFg_std_column,
            " value changed from ",
            CHOAVLDFg_standardised_not_0[[CHOAVLDFg_std_column]][i],
            " to 0"
          ) #Creates the new comment
          row_number <- rownames(CHOAVLDFg_standardised_not_0[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[CHOAVLDFg_std_column]][df[[CPC_Code_Column]] %in% No_CHOAVLDFg_standardised_CPC_Codes &
                                   !(df[[CHOAVLDFg_std_column]] %in% "0")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
      return(df)
    }
  }

  #If the method type is 1, so just checking, this loops through rows where CHOAVLDFg_standardised isno 0 and prints the row numbers out.



  if (method_type == 3) {
    CHOAVLDFg_standardised_blank <- df[df[[CPC_Code_Column]] %in% No_CHOAVLDFg_standardised_CPC_Codes &
                                         df[[CHOAVLDFg_std_column]] %in% c(NA, ""), ] #Creates a df of CHOAVLDFg_standardised values which are either blank or NA
    if(nrow(CHOAVLDFg_standardised_blank) == 0){
      #Does nothing
    } else {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(CHOAVLDFg_standardised_blank)) {
          #If so, loops through all items where CHOAVLDFg_standardised isn't 0, but should be
          new_comment <- paste0(
            CHOAVLDFg_std_column,
            " value changed from ",
            CHOAVLDFg_standardised_blank[[CHOAVLDFg_std_column]][i],
            " to 0"
          ) #Creates the new comment
          row_number <- rownames(CHOAVLDFg_standardised_blank[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[CHOAVLDFg_std_column]][df[[CPC_Code_Column]] %in% No_CHOAVLDFg_standardised_CPC_Codes &
                                   df[[CHOAVLDFg_std_column]] %in% c(NA, "")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
    }

    return(df)
  }
}




Absence_Check_ALC <- function(df,
                              ALC_column = "ALCg",
                              method = "Check",
                              CPC_Code_Column = "CPC_Code",
                              No_ALC_CPC_Codes = Standard_No_ALC_CPC_Codes,
                              comments = TRUE,
                              comment_col = "comments") {
  #Check, Fill_all, or Fill_blank.

  # Input checks ----

  stopifnot(
    "df is not a data frame - please input a data frame" = is.data.frame(df),
    #Checks to see if the df item is a data frame
    "The ALC_column is not a character or string - please input a character or string that is a column name in df, e.g. 'ALCg'" = is.character(ALC_column),
    #checks to see if the ALC_column is a character string
    "The CPC_Code_Column is not a character or string - please input a character or string that is a column name in df, e.g. 'CPC_Codes'" = is.character(CPC_Code_Column),
    #checks to see if the CPC_Code_Column is a character string
    "The ALC_column is not a column name in df - please input a string that is a column name in df." = ALC_column %in% colnames(df),
    #Checks if the column names are actually columns in the df
    "The CPC_Code_Column is not a column name in df - please input a string that is a column name in df." = CPC_Code_Column %in% colnames(df),
    "method option is not valid - please choose between 'Check', 'Fill_all', 'Fill_blank'." = tolower(method) %in% c(
      "check",
      "fill_all",
      "fill_blank"
    ),
    "No flagged CPC Codes present in the CPC Code column. Function stopped." = TRUE %in% (unique(df[[CPC_Code_Column]]) %in% No_ALC_CPC_Codes)
  )



  if (tolower(method) %in% "check") {
    method_type <- 1
  }

  if (tolower(method) %in% "fill_all") {
    method_type <- 2
  }

  if (tolower(method) %in% "fill_blank") {
    method_type <- 3
  }

  # Column conversion checks ----

  df[[CPC_Code_Column]] <- as.character(df[[CPC_Code_Column]]) #convert fbs id col to character
  df[[ALC_column]] <- as.character(df[[ALC_column]]) #convert fbs id col to character


  # Comments column creation check ----

  #Sees if a comments column already exists, and creates one if not. Only applies on a Fill method, as this is where the fill is recorded.

  if (method_type %in% c(2, 3)) {
    if (comments == TRUE) {
      message("comments included")
      if (!(comment_col %in% colnames(df))) {
        message("No comments column detected, new one being created")
        df[[comment_col]] <- NA
      }
    }
  }


  # Non-0 rows ----
  ALC_not_0 <- df[df[[CPC_Code_Column]] %in% No_ALC_CPC_Codes &
                    !(df[[ALC_column]] %in% "0"), ]

  # method - Checks ----

  if(nrow(ALC_not_0) == 0){ #Does nothing if there are no relevant rows
    if (method_type == 2){
      return(df)
    }
  } else {
    if (method_type == 1) {

      lapply(1:nrow(ALC_not_0), function(i) {
        message(
          paste(
            "FLAG: ALC value for row number",
            rownames(ALC_not_0[i, ]),
            " is not 0; value is",
            ALC_not_0[[ALC_column]][i],
            "."
          )
        )
      })
      #Option to produce code to create a df subset for inspection:
      #message("You Can run the message below to create a subset of the input df for closer inspection. Please replace 'df' with the variable name of the dataframe you inputted to this function.")
      #message(paste0("Non_zero_ALC <- df[rownames(df) %in% c(",  paste(rownames(ALC_not_0), collapse = ", "), "), ]"))

      #Option to automatically create and view subset df for inspection:
      assign("Non_zero_ALC", ALC_not_0, envir = globalenv())
      View(Non_zero_ALC)
    }


    # Method -  Fill All ----

    if (method_type == 2) {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(ALC_not_0)) {
          #If so, loops through all items where ALC isn't 0, but should be
          new_comment <- paste0(ALC_column,
                                " value changed from ",
                                ALC_not_0[[ALC_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(ALC_not_0[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[ALC_column]][df[[CPC_Code_Column]] %in% No_ALC_CPC_Codes &
                         !(df[[ALC_column]] %in% "0")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
      return(df)
    }
  }

  #If the method type is 1, so just checking, this loops through rows where ALC isno 0 and prints the row numbers out.




  if (method_type == 3) {
    ALC_blank <- df[df[[CPC_Code_Column]] %in% No_ALC_CPC_Codes &
                      df[[ALC_column]] %in% c(NA, ""), ] #Creates a df of ALC values which are either blank or NA
    if(nrow(ALC_blank) == 0){
    } else {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(ALC_blank)) {
          #If so, loops through all items where ALC isn't 0, but should be
          new_comment <- paste0(ALC_column,
                                " value changed from ",
                                ALC_blank[[ALC_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(ALC_blank[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[ALC_column]][df[[CPC_Code_Column]] %in% No_ALC_CPC_Codes &
                         df[[ALC_column]] %in% c(NA, "")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
    }
    return(df)
  }
}



Absence_Check_FIBG <- function(df,
                               FIBG_column = "FIBGg",
                               method = "Check",
                               CPC_Code_Column = "CPC_Code",
                               No_FIBG_CPC_Codes = Standard_No_FIBG_CPC_Codes,
                               comments = TRUE,
                               comment_col = "comments") {
  #Check, Fill_all, or Fill_blank.

  # Input checks ----

  stopifnot(
    "df is not a data frame - please input a data frame" = is.data.frame(df),
    #Checks to see if the df item is a data frame
    "The FIBG_column is not a character or string - please input a character or string that is a column name in df, e.g. 'FIBGg'" = is.character(FIBG_column),
    #checks to see if the FIBG_column is a character string
    "The CPC_Code_Column is not a character or string - please input a character or string that is a column name in df, e.g. 'CPC_Codes'" = is.character(CPC_Code_Column),
    #checks to see if the CPC_Code_Column is a character string
    "The FIBG_column is not a column name in df - please input a string that is a column name in df." = FIBG_column %in% colnames(df),
    #Checks if the column names are actually columns in the df
    "The CPC_Code_Column is not a column name in df - please input a string that is a column name in df." = CPC_Code_Column %in% colnames(df),
    "method option is not valid - please choose between 'Check', 'Fill_all', 'Fill_blank'." = tolower(method) %in% c(
      "check",
      "fill_all",
      "fill_blank"
    ),
    "No flagged CPC Codes present in the CPC Code column. Function stopped." = TRUE %in% (unique(df[[CPC_Code_Column]]) %in% No_FIBG_CPC_Codes)
  )



  if (tolower(method) %in% "check") {
    method_type <- 1
  }

  if (tolower(method) %in% "fill_all") {
    method_type <- 2
  }

  if (tolower(method) %in% "fill_blank") {
    method_type <- 3
  }

  # Column conversion checks ----

  df[[CPC_Code_Column]] <- as.character(df[[CPC_Code_Column]]) #convert fbs id col to character
  df[[FIBG_column]] <- as.character(df[[FIBG_column]]) #convert fbs id col to character


  # Comments column creation check ----

  #Sees if a comments column already exists, and creates one if not. Only applies on a Fill method, as this is where the fill is recorded.

  if (method_type %in% c(2, 3)) {
    if (comments == TRUE) {
      message("comments included")
      if (!(comment_col %in% colnames(df))) {
        message("No comments column detected, new one being created")
        df[[comment_col]] <- NA
      }
    }
  }


  # Non-0 rows ----
  FIBG_not_0 <- df[df[[CPC_Code_Column]] %in% No_FIBG_CPC_Codes &
                     !(df[[FIBG_column]] %in% "0"), ]

  # method - Checks ----

  if(nrow(FIBG_not_0) == 0){ #Does nothing if there are no relevant rows
    if (method_type == 2){
      return(df)
    }
  } else {
    if (method_type == 1) {

      lapply(1:nrow(FIBG_not_0), function(i) {
        message(
          paste(
            "FLAG: FIBG value for row number",
            rownames(FIBG_not_0[i, ]),
            " is not 0; value is",
            FIBG_not_0[[FIBG_column]][i],
            "."
          )
        )
      })
      #Option to produce code to create a df subset for inspection:
      #message("You Can run the message below to create a subset of the input df for closer inspection. Please replace 'df' with the variable name of the dataframe you inputted to this function.")
      #message(paste0("Non_zero_FIBG <- df[rownames(df) %in% c(",  paste(rownames(FIBG_not_0), collapse = ", "), "), ]"))

      #Option to automatically create and view subset df for inspection:
      assign("Non_zero_FIBG", FIBG_not_0, envir = globalenv())
      View(Non_zero_FIBG)
    }


    # Method -  Fill All ----

    if (method_type == 2) {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(FIBG_not_0)) {
          #If so, loops through all items where FIBG isn't 0, but should be
          new_comment <- paste0(FIBG_column,
                                " value changed from ",
                                FIBG_not_0[[FIBG_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(FIBG_not_0[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[FIBG_column]][df[[CPC_Code_Column]] %in% No_FIBG_CPC_Codes &
                          !(df[[FIBG_column]] %in% "0")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
      return(df)
    }
  }

  #If the method type is 1, so just checking, this loops through rows where FIBG isno 0 and prints the row numbers out.




  if (method_type == 3) {
    FIBG_blank <- df[df[[CPC_Code_Column]] %in% No_FIBG_CPC_Codes &
                       df[[FIBG_column]] %in% c(NA, ""), ] #Creates a df of FIBG values which are either blank or NA
    if(nrow(FIBG_blank) == 0){
    } else {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(FIBG_blank)) {
          #If so, loops through all items where FIBG isn't 0, but should be
          new_comment <- paste0(FIBG_column,
                                " value changed from ",
                                FIBG_blank[[FIBG_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(FIBG_blank[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[FIBG_column]][df[[CPC_Code_Column]] %in% No_FIBG_CPC_Codes &
                          df[[FIBG_column]] %in% c(NA, "")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
    }
    return(df)
  }
}


Absence_Check_NIA <- function(df,
                              NIA_column = "NIAmg",
                              method = "Check",
                              CPC_Code_Column = "CPC_Code",
                              No_NIA_CPC_Codes = Standard_No_NIA_CPC_Codes,
                              comments = TRUE,
                              comment_col = "comments") {
  #Check, Fill_all, or Fill_blank.

  # Input checks ----

  stopifnot(
    "df is not a data frame - please input a data frame" = is.data.frame(df),
    #Checks to see if the df item is a data frame
    "The NIA_column is not a character or string - please input a character or string that is a column name in df, e.g. 'NIAmg'" = is.character(NIA_column),
    #checks to see if the NIA_column is a character string
    "The CPC_Code_Column is not a character or string - please input a character or string that is a column name in df, e.g. 'CPC_Codes'" = is.character(CPC_Code_Column),
    #checks to see if the CPC_Code_Column is a character string
    "The NIA_column is not a column name in df - please input a string that is a column name in df." = NIA_column %in% colnames(df),
    #Checks if the column names are actually columns in the df
    "The CPC_Code_Column is not a column name in df - please input a string that is a column name in df." = CPC_Code_Column %in% colnames(df),
    "method option is not valid - please choose between 'Check', 'Fill_all', 'Fill_blank'." = tolower(method) %in% c(
      "check",
      "fill_all",
      "fill_blank"
    ),
    "No flagged CPC Codes present in the CPC Code column. Function stopped." = TRUE %in% (unique(df[[CPC_Code_Column]]) %in% No_NIA_CPC_Codes)
  )



  if (tolower(method) %in% "check") {
    method_type <- 1
  }

  if (tolower(method) %in% "fill_all") {
    method_type <- 2
  }

  if (tolower(method) %in% "fill_blank") {
    method_type <- 3
  }

  # Column conversion checks ----

  df[[CPC_Code_Column]] <- as.character(df[[CPC_Code_Column]]) #convert fbs id col to character
  df[[NIA_column]] <- as.character(df[[NIA_column]]) #convert fbs id col to character


  # Comments column creation check ----

  #Sees if a comments column already exists, and creates one if not. Only applies on a Fill method, as this is where the fill is recorded.

  if (method_type %in% c(2, 3)) {
    if (comments == TRUE) {
      message("comments included")
      if (!(comment_col %in% colnames(df))) {
        message("No comments column detected, new one being created")
        df[[comment_col]] <- NA
      }
    }
  }


  # Non-0 rows ----
  NIA_not_0 <- df[df[[CPC_Code_Column]] %in% No_NIA_CPC_Codes &
                    !(df[[NIA_column]] %in% "0"), ]

  # method - Checks ----

  if(nrow(NIA_not_0) == 0){ #Does nothing if there are no relevant rows
    if (method_type == 2){
      return(df)
    }
  } else {
    if (method_type == 1) {

      lapply(1:nrow(NIA_not_0), function(i) {
        message(
          paste(
            "FLAG: NIA value for row number",
            rownames(NIA_not_0[i, ]),
            " is not 0; value is",
            NIA_not_0[[NIA_column]][i],
            "."
          )
        )
      })

      #Option to produce code to create a df subset for inspection:
      #message("You Can run the message below to create a subset of the input df for closer inspection. Please replace 'df' with the variable name of the dataframe you inputted to this function.")
      #message(paste0("Non_zero_NIA <- df[rownames(df) %in% c(",  paste(rownames(NIA_not_0), collapse = ", "), "), ]"))

      #Option to automatically create and view subset df for inspection:
      assign("Non_zero_NIA", NIA_not_0, envir = globalenv())
      View(Non_zero_NIA)
    }


    # Method -  Fill All ----

    if (method_type == 2) {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(NIA_not_0)) {
          #If so, loops through all items where NIA isn't 0, but should be
          new_comment <- paste0(NIA_column,
                                " value changed from ",
                                NIA_not_0[[NIA_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(NIA_not_0[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[NIA_column]][df[[CPC_Code_Column]] %in% No_NIA_CPC_Codes &
                         !(df[[NIA_column]] %in% "0")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
      return(df)
    }
  }

  #If the method type is 1, so just checking, this loops through rows where NIA isno 0 and prints the row numbers out.




  if (method_type == 3) {
    NIA_blank <- df[df[[CPC_Code_Column]] %in% No_NIA_CPC_Codes &
                      df[[NIA_column]] %in% c(NA, ""), ] #Creates a df of NIA values which are either blank or NA
    if(nrow(NIA_blank) == 0){
    } else {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(NIA_blank)) {
          #If so, loops through all items where NIA isn't 0, but should be
          new_comment <- paste0(NIA_column,
                                " value changed from ",
                                NIA_blank[[NIA_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(NIA_blank[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[NIA_column]][df[[CPC_Code_Column]] %in% No_NIA_CPC_Codes &
                         df[[NIA_column]] %in% c(NA, "")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
    }
    return(df)
  }
}





Absence_Check_RETOL <- function(df,
                                RETOL_column = "RETOLmcg",
                                method = "Check",
                                CPC_Code_Column = "CPC_Code",
                                No_RETOL_CPC_Codes = Standard_No_RETOL_CPC_Codes,
                                comments = TRUE,
                                comment_col = "comments") {
  #Check, Fill_all, or Fill_blank.

  # Input checks ----

  stopifnot(
    "df is not a data frame - please input a data frame" = is.data.frame(df),
    #Checks to see if the df item is a data frame
    "The RETOL_column is not a character or string - please input a character or string that is a column name in df, e.g. 'RETOLmcg'" = is.character(RETOL_column),
    #checks to see if the RETOL_column is a character string
    "The CPC_Code_Column is not a character or string - please input a character or string that is a column name in df, e.g. 'CPC_Codes'" = is.character(CPC_Code_Column),
    #checks to see if the CPC_Code_Column is a character string
    "The RETOL_column is not a column name in df - please input a string that is a column name in df." = RETOL_column %in% colnames(df),
    #Checks if the column names are actually columns in the df
    "The CPC_Code_Column is not a column name in df - please input a string that is a column name in df." = CPC_Code_Column %in% colnames(df),
    "method option is not valid - please choose between 'Check', 'Fill_all', 'Fill_blank'." = tolower(method) %in% c(
      "check",
      "fill_all",
      "fill_blank"
    ),
    "No flagged CPC Codes present in the CPC Code column. Function stopped." = TRUE %in% (unique(df[[CPC_Code_Column]]) %in% No_RETOL_CPC_Codes)
  )



  if (tolower(method) %in% "check") {
    method_type <- 1
  }

  if (tolower(method) %in% "fill_all") {
    method_type <- 2
  }

  if (tolower(method) %in% "fill_blank") {
    method_type <- 3
  }

  # Column conversion checks ----

  df[[CPC_Code_Column]] <- as.character(df[[CPC_Code_Column]]) #convert fbs id col to character
  df[[RETOL_column]] <- as.character(df[[RETOL_column]]) #convert fbs id col to character


  # Comments column creation check ----

  #Sees if a comments column already exists, and creates one if not. Only applies on a Fill method, as this is where the fill is recorded.

  if (method_type %in% c(2, 3)) {
    if (comments == TRUE) {
      message("comments included")
      if (!(comment_col %in% colnames(df))) {
        message("No comments column detected, new one being created")
        df[[comment_col]] <- NA
      }
    }
  }




  # Non-0 rows ----
  RETOL_not_0 <- df[df[[CPC_Code_Column]] %in% No_RETOL_CPC_Codes &
                      !(df[[RETOL_column]] %in% "0"), ]

  # method - Checks ----

  if(nrow(RETOL_not_0) == 0){ #Does nothing if there are no relevant rows
    if (method_type == 2){
      return(df)
    }
  } else {
    if (method_type == 1) {

      lapply(1:nrow(RETOL_not_0), function(i) {
        message(
          paste(
            "FLAG: RETOL value for row number",
            rownames(RETOL_not_0[i, ]),
            " is not 0; value is",
            RETOL_not_0[[RETOL_column]][i],
            "."
          )
        )
      })
      #Option to produce code to create a df subset for inspection:
      #message("You Can run the message below to create a subset of the input df for closer inspection. Please replace 'df' with the variable name of the dataframe you inputted to this function.")
      #message(paste0("Non_zero_RETOL <- df[rownames(df) %in% c(",  paste(rownames(RETOL_not_0), collapse = ", "), "), ]"))

      #Option to automatically create and view subset df for inspection:
      assign("Non_zero_RETOL", RETOL_not_0, envir = globalenv())
      View(Non_zero_RETOL)
    }


    # Method -  Fill All ----

    if (method_type == 2) {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(RETOL_not_0)) {
          #If so, loops through all items where RETOL isn't 0, but should be
          new_comment <- paste0(RETOL_column,
                                " value changed from ",
                                RETOL_not_0[[RETOL_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(RETOL_not_0[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[RETOL_column]][df[[CPC_Code_Column]] %in% No_RETOL_CPC_Codes &
                           !(df[[RETOL_column]] %in% "0")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
      return(df)
    }
  }

  #If the method type is 1, so just checking, this loops through rows where RETOL isno 0 and prints the row numbers out.




  if (method_type == 3) {
    RETOL_blank <- df[df[[CPC_Code_Column]] %in% No_RETOL_CPC_Codes &
                        df[[RETOL_column]] %in% c(NA, ""), ] #Creates a df of RETOL values which are either blank or NA
    if(nrow(RETOL_blank) == 0){
    } else {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(RETOL_blank)) {
          #If so, loops through all items where RETOL isn't 0, but should be
          new_comment <- paste0(RETOL_column,
                                " value changed from ",
                                RETOL_blank[[RETOL_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(RETOL_blank[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[RETOL_column]][df[[CPC_Code_Column]] %in% No_RETOL_CPC_Codes &
                           df[[RETOL_column]] %in% c(NA, "")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
    }
    return(df)
  }
}



Absence_Check_VITB12 <- function(df,
                                 VITB12_column = "VITB12mcg",
                                 method = "Check",
                                 CPC_Code_Column = "CPC_Code",
                                 No_VITB12_CPC_Codes = Standard_No_VITB12_CPC_Codes,
                                 comments = TRUE,
                                 comment_col = "comments") {
  #Check, Fill_all, or Fill_blank.

  # Input checks ----

  stopifnot(
    "df is not a data frame - please input a data frame" = is.data.frame(df),
    #Checks to see if the df item is a data frame
    "The VITB12_column is not a character or string - please input a character or string that is a column name in df, e.g. 'VITB12mcg'" = is.character(VITB12_column),
    #checks to see if the VITB12_column is a character string
    "The CPC_Code_Column is not a character or string - please input a character or string that is a column name in df, e.g. 'CPC_Codes'" = is.character(CPC_Code_Column),
    #checks to see if the CPC_Code_Column is a character string
    "The VITB12_column is not a column name in df - please input a string that is a column name in df." = VITB12_column %in% colnames(df),
    #Checks if the column names are actually columns in the df
    "The CPC_Code_Column is not a column name in df - please input a string that is a column name in df." = CPC_Code_Column %in% colnames(df),
    "method option is not valid - please choose between 'Check', 'Fill_all', 'Fill_blank'." = tolower(method) %in% c(
      "check",
      "fill_all",
      "fill_blank"
    ),
    "No flagged CPC Codes present in the CPC Code column. Function stopped." = TRUE %in% (unique(df[[CPC_Code_Column]]) %in% No_VITB12_CPC_Codes)
  )



  if (tolower(method) %in% "check") {
    method_type <- 1
  }

  if (tolower(method) %in% "fill_all") {
    method_type <- 2
  }

  if (tolower(method) %in% "fill_blank") {
    method_type <- 3
  }

  # Column conversion checks ----

  df[[CPC_Code_Column]] <- as.character(df[[CPC_Code_Column]]) #convert fbs id col to character
  df[[VITB12_column]] <- as.character(df[[VITB12_column]]) #convert fbs id col to character


  # Comments column creation check ----

  #Sees if a comments column already exists, and creates one if not. Only applies on a Fill method, as this is where the fill is recorded.

  if (method_type %in% c(2, 3)) {
    if (comments == TRUE) {
      message("comments included")
      if (!(comment_col %in% colnames(df))) {
        message("No comments column detected, new one being created")
        df[[comment_col]] <- NA
      }
    }
  }


  # Non-0 rows ----
  VITB12_not_0 <- df[df[[CPC_Code_Column]] %in% No_VITB12_CPC_Codes &
                       !(df[[VITB12_column]] %in% "0"), ]

  # method - Checks ----

  if(nrow(VITB12_not_0) == 0){ #Does nothing if there are no relevant rows
    if (method_type == 2){
      return(df)
    }
  } else {
    if (method_type == 1) {

      lapply(1:nrow(VITB12_not_0), function(i) {
        message(
          paste(
            "FLAG: VITB12 value for row number",
            rownames(VITB12_not_0[i, ]),
            " is not 0; value is",
            VITB12_not_0[[VITB12_column]][i],
            "."
          )
        )
      })
      #Option to produce code to create a df subset for inspection:
      #message("You Can run the message below to create a subset of the input df for closer inspection. Please replace 'df' with the variable name of the dataframe you inputted to this function.")
      #message(paste0("Non_zero_VITB12 <- df[rownames(df) %in% c(",  paste(rownames(VITB12_not_0), collapse = ", "), "), ]"))

      #Option to automatically create and view subset df for inspection:
      assign("Non_zero_VITB12", VITB12_not_0, envir = globalenv())
      View(Non_zero_VITB12)
    }


    # Method -  Fill All ----

    if (method_type == 2) {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(VITB12_not_0)) {
          #If so, loops through all items where VITB12 isn't 0, but should be
          new_comment <- paste0(VITB12_column,
                                " value changed from ",
                                VITB12_not_0[[VITB12_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(VITB12_not_0[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[VITB12_column]][df[[CPC_Code_Column]] %in% No_VITB12_CPC_Codes &
                            !(df[[VITB12_column]] %in% "0")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
      return(df)
    }
  }

  #If the method type is 1, so just checking, this loops through rows where VITB12 isno 0 and prints the row numbers out.




  if (method_type == 3) {
    VITB12_blank <- df[df[[CPC_Code_Column]] %in% No_VITB12_CPC_Codes &
                         df[[VITB12_column]] %in% c(NA, ""), ] #Creates a df of VITB12 values which are either blank or NA
    if(nrow(VITB12_blank) == 0){
    } else {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(VITB12_blank)) {
          #If so, loops through all items where VITB12 isn't 0, but should be
          new_comment <- paste0(VITB12_column,
                                " value changed from ",
                                VITB12_blank[[VITB12_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(VITB12_blank[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[VITB12_column]][df[[CPC_Code_Column]] %in% No_VITB12_CPC_Codes &
                            df[[VITB12_column]] %in% c(NA, "")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
    }
    return(df)
  }
}
#
# df <- data.df_test_2
# VITB12_column <- "VITB12mcg"
# method<- "fill_all"
# CPC_Code_Column <- "CPC_Code"
# No_VITB12_CPC_Codes <- Standard_No_VITB12_CPC_Codes
# comments <- T
# comment_col <- "comments"


Absence_Check_VITB12_2 <- function(df,
                                   VITB12_column = "VITB12mcg",
                                   method = "Check",
                                   CPC_Code_Column = "CPC_Code",
                                   No_VITB12_CPC_Codes = Standard_No_VITB12_CPC_Codes,
                                   comments = TRUE,
                                   comment_col = "comments") {
  #Check, Fill_all, or Fill_blank.

  # Input checks ----

  stopifnot(
    "df is not a data frame - please input a data frame" = is.data.frame(df),
    #Checks to see if the df item is a data frame
    "The VITB12_column is not a character or string - please input a character or string that is a column name in df, e.g. 'VITB12mcg'" = is.character(VITB12_column),
    #checks to see if the VITB12_column is a character string
    "The CPC_Code_Column is not a character or string - please input a character or string that is a column name in df, e.g. 'CPC_Codes'" = is.character(CPC_Code_Column),
    #checks to see if the CPC_Code_Column is a character string
    "The VITB12_column is not a column name in df - please input a string that is a column name in df." = VITB12_column %in% colnames(df),
    #Checks if the column names are actually columns in the df
    "The CPC_Code_Column is not a column name in df - please input a string that is a column name in df." = CPC_Code_Column %in% colnames(df),
    "method option is not valid - please choose between 'Check', 'Fill_all', 'Fill_blank'." = tolower(method) %in% c(
      "check",
      "fill_all",
      "fill_blank"
    ),
    "No flagged CPC Codes present in the CPC Code column. Function stopped." = TRUE %in% (unique(df[[CPC_Code_Column]]) %in% No_VITB12_CPC_Codes)
  )



  if (tolower(method) %in% "check") {
    method_type <- 1
  }

  if (tolower(method) %in% "fill_all") {
    method_type <- 2
  }

  if (tolower(method) %in% "fill_blank") {
    method_type <- 3
  }

  # Column conversion checks ----

  df[[CPC_Code_Column]] <- as.character(df[[CPC_Code_Column]]) #convert fbs id col to character
  df[[VITB12_column]] <- as.character(df[[VITB12_column]]) #convert VitB12 col to character


  # Comments column creation check ----

  #Sees if a comments column already exists, and creates one if not. Only applies on a Fill method, as this is where the fill is recorded.

  if (method_type %in% c(2, 3)) {
    if (comments == TRUE) {
      message("comments included")
      if (!(comment_col %in% colnames(df))) {
        message("No comments column detected, new one being created")
        df[[comment_col]] <- NA
      }
    }
  }


  # Non-0 rows ----
  VITB12_not_0 <- df[df[[CPC_Code_Column]] %in% No_VITB12_CPC_Codes &
                       !(df[[VITB12_column]] %in% "0"), ]

  # method - Checks ----

  if(nrow(VITB12_not_0) == 0){ #Does nothing if there are no relevant rows
    if (method_type == 2){
      return(df)
    }
  } else {
    if (method_type == 1) {

      lapply(1:nrow(VITB12_not_0), function(i) {
        message(
          paste(
            "FLAG: VITB12 value for row number",
            rownames(VITB12_not_0[i, ]),
            " is not 0; value is",
            VITB12_not_0[[VITB12_column]][i],
            "."
          )
        )
      })
      #Option to produce code to create a df subset for inspection:
      #message("You Can run the message below to create a subset of the input df for closer inspection. Please replace 'df' with the variable name of the dataframe you inputted to this function.")
      #message(paste0("Non_zero_VITB12 <- df[rownames(df) %in% c(",  paste(rownames(VITB12_not_0), collapse = ", "), "), ]"))

      #Option to automatically create and view subset df for inspection:
      assign("Non_zero_VITB12", VITB12_not_0, envir = globalenv())
      View(Non_zero_VITB12)
    }


    # Method -  Fill All ----

    if (method_type == 2) {

      df$TEMP_VITB12_comment <- NA

      if(comments == TRUE){

        df[df[[CPC_Code_Column]] %in% No_VITB12_CPC_Codes &
             !(df[[VITB12_column]] %in% "0"), "TEMP_VITB12_comment"]  <- paste0(VITB12_column,
                                                                                " value changed from ",
                                                                                df[[VITB12_column]][df[[CPC_Code_Column]] %in% No_VITB12_CPC_Codes &
                                                                                                      !(df[[VITB12_column]] %in% "0")],
                                                                                " to 0") #Creates the new comment

        df[[comment_col]][!is.na(df$TEMP_VITB12_comment) &
                            is.na(df[[comment_col]]) |
                            !is.na(df$TEMP_VITB12_comment) &
                            df[[comment_col]] %in% ""] <- df$TEMP_VITB12_comment[!is.na(df$TEMP_VITB12_comment) &
                                                                                   is.na(df[[comment_col]]) |
                                                                                   !is.na(df$TEMP_VITB12_comment) &
                                                                                   df[[comment_col]] %in% ""]

        df[[comment_col]][!is.na(df$TEMP_VITB12_comment) &
                            !is.na(df[[comment_col]]) |
                            !is.na(df$TEMP_VITB12_comment) &
                            !(df[[comment_col]] %in% "")] <- paste0(df[[comment_col]][!is.na(df$TEMP_VITB12_comment) &
                                                                                        !is.na(df[[comment_col]]) |
                                                                                        !is.na(df$TEMP_VITB12_comment) &
                                                                                        !df[[comment_col]] %in% ""],
                                                                    "; ",
                                                                    df$TEMP_VITB12_comment[!is.na(df$TEMP_VITB12_comment) &
                                                                                             !is.na(df[[comment_col]]) |
                                                                                             !is.na(df$TEMP_VITB12_comment) &
                                                                                             !df[[comment_col]] %in% ""])

      }

      df[[VITB12_column]][df[[CPC_Code_Column]] %in% No_VITB12_CPC_Codes &
                            !(df[[VITB12_column]] %in% "0")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.

      df$TEMP_VITB12_comment <- NULL

      return(df)

      # if (comments == TRUE) {
      #   #Checks if comments are enabled
      #   for (i in 1:nrow(VITB12_not_0)) {
      #     #If so, loops through all items where VITB12 isn't 0, but should be
      #     new_comment <- paste0(VITB12_column,
      #                           " value changed from ",
      #                           VITB12_not_0[[VITB12_column]][i],
      #                           " to 0") #Creates the new comment
      #     row_number <- rownames(VITB12_not_0[i, ]) #Finds the rownumber
      #     existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
      #     if (!is.na(existing_comment)) {
      #       #adds new comment to existing comment if present
      #       new_comment <- paste0(existing_comment, "; ", new_comment)
      #     }
      #     df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
      #   }
      # }
      # df[[VITB12_column]][df[[CPC_Code_Column]] %in% No_VITB12_CPC_Codes &
      #                       !(df[[VITB12_column]] %in% "0")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
      # return(df)
    }
  }

  #If the method type is 1, so just checking, this loops through rows where VITB12 isno 0 and prints the row numbers out.




  if (method_type == 3) {
    VITB12_blank <- df[df[[CPC_Code_Column]] %in% No_VITB12_CPC_Codes &
                         df[[VITB12_column]] %in% c(NA, ""), ] #Creates a df of VITB12 values which are either blank or NA
    if(nrow(VITB12_blank) == 0){
    } else {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(VITB12_blank)) {
          #If so, loops through all items where VITB12 isn't 0, but should be
          new_comment <- paste0(VITB12_column,
                                " value changed from ",
                                VITB12_blank[[VITB12_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(VITB12_blank[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[VITB12_column]][df[[CPC_Code_Column]] %in% No_VITB12_CPC_Codes &
                            df[[VITB12_column]] %in% c(NA, "")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
    }
    return(df)
  }
}




Absence_Check_VITB6 <- function(df,
                                VITB6_column = "VITB6_mg_standardised",
                                method = "Check",
                                CPC_Code_Column = "CPC_Code",
                                No_VITB6_CPC_Codes = Standard_No_VITB6_CPC_Codes,
                                comments = TRUE,
                                comment_col = "comments") {
  #Check, Fill_all, or Fill_blank.

  # Input checks ----

  stopifnot(
    "df is not a data frame - please input a data frame" = is.data.frame(df),
    #Checks to see if the df item is a data frame
    "The VITB6_column is not a character or string - please input a character or string that is a column name in df, e.g. 'VITB6mcg'" = is.character(VITB6_column),
    #checks to see if the VITB6_column is a character string
    "The CPC_Code_Column is not a character or string - please input a character or string that is a column name in df, e.g. 'CPC_Codes'" = is.character(CPC_Code_Column),
    #checks to see if the CPC_Code_Column is a character string
    "The VITB6_column is not a column name in df - please input a string that is a column name in df." = VITB6_column %in% colnames(df),
    #Checks if the column names are actually columns in the df
    "The CPC_Code_Column is not a column name in df - please input a string that is a column name in df." = CPC_Code_Column %in% colnames(df),
    "method option is not valid - please choose between 'Check', 'Fill_all', 'Fill_blank'." = tolower(method) %in% c(
      "check",
      "fill_all",
      "fill_blank"
    ),
    "No flagged CPC Codes present in the CPC Code column. Function stopped." = TRUE %in% (unique(df[[CPC_Code_Column]]) %in% No_VITB6_CPC_Codes)
  )



  if (tolower(method) %in% "check") {
    method_type <- 1
  }

  if (tolower(method) %in% "fill_all") {
    method_type <- 2
  }

  if (tolower(method) %in% "fill_blank") {
    method_type <- 3
  }

  # Column conversion checks ----

  df[[CPC_Code_Column]] <- as.character(df[[CPC_Code_Column]]) #convert fbs id col to character
  df[[VITB6_column]] <- as.character(df[[VITB6_column]]) #convert fbs id col to character


  # Comments column creation check ----

  #Sees if a comments column already exists, and creates one if not. Only applies on a Fill method, as this is where the fill is recorded.

  if (method_type %in% c(2, 3)) {
    if (comments == TRUE) {
      message("comments included")
      if (!(comment_col %in% colnames(df))) {
        message("No comments column detected, new one being created")
        df[[comment_col]] <- NA
      }
    }
  }


  # Non-0 rows ----
  VITB6_not_0 <- df[df[[CPC_Code_Column]] %in% No_VITB6_CPC_Codes &
                      !(df[[VITB6_column]] %in% "0"), ]

  # method - Checks ----

  if(nrow(VITB6_not_0) == 0){ #Does nothing if there are no relevant rows
    if (method_type == 2){
      return(df)
    }
  } else {
    if (method_type == 1) {

      lapply(1:nrow(VITB6_not_0), function(i) {
        message(
          paste(
            "FLAG: VITB6 value for row number",
            rownames(VITB6_not_0[i, ]),
            " is not 0; value is",
            VITB6_not_0[[VITB6_column]][i],
            "."
          )
        )
      })
      #Option to produce code to create a df subset for inspection:
      #message("You Can run the message below to create a subset of the input df for closer inspection. Please replace 'df' with the variable name of the dataframe you inputted to this function.")
      #message(paste0("Non_zero_VITB6 <- df[rownames(df) %in% c(",  paste(rownames(VITB6_not_0), collapse = ", "), "), ]"))

      #Option to automatically create and view subset df for inspection:
      assign("Non_zero_VITB6", VITB6_not_0, envir = globalenv())
      View(Non_zero_VITB6)
    }


    # Method -  Fill All ----

    if (method_type == 2) {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(VITB6_not_0)) {
          #If so, loops through all items where VITB6 isn't 0, but should be
          new_comment <- paste0(VITB6_column,
                                " value changed from ",
                                VITB6_not_0[[VITB6_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(VITB6_not_0[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[VITB6_column]][df[[CPC_Code_Column]] %in% No_VITB6_CPC_Codes &
                           !(df[[VITB6_column]] %in% "0")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
      return(df)
    }
  }

  #If the method type is 1, so just checking, this loops through rows where VITB6 isno 0 and prints the row numbers out.




  if (method_type == 3) {
    VITB6_blank <- df[df[[CPC_Code_Column]] %in% No_VITB6_CPC_Codes &
                        df[[VITB6_column]] %in% c(NA, ""), ] #Creates a df of VITB6 values which are either blank or NA
    if(nrow(VITB6_blank) == 0){
    } else {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(VITB6_blank)) {
          #If so, loops through all items where VITB6 isn't 0, but should be
          new_comment <- paste0(VITB6_column,
                                " value changed from ",
                                VITB6_blank[[VITB6_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(VITB6_blank[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[VITB6_column]][df[[CPC_Code_Column]] %in% No_VITB6_CPC_Codes &
                           df[[VITB6_column]] %in% c(NA, "")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
    }
    return(df)
  }
}




Absence_Check_CU <- function(df,
                             CU_column = "CUmg",
                             method = "Check",
                             CPC_Code_Column = "CPC_Code",
                             No_CU_CPC_Codes = Standard_No_CU_CPC_Codes,
                             comments = TRUE,
                             comment_col = "comments") {
  #Check, Fill_all, or Fill_blank.

  # Input checks ----

  stopifnot(
    "df is not a data frame - please input a data frame" = is.data.frame(df),
    #Checks to see if the df item is a data frame
    "The CU_column is not a character or string - please input a character or string that is a column name in df, e.g. 'CUg'" = is.character(CU_column),
    #checks to see if the CU_column is a character string
    "The CPC_Code_Column is not a character or string - please input a character or string that is a column name in df, e.g. 'CPC_Codes'" = is.character(CPC_Code_Column),
    #checks to see if the CPC_Code_Column is a character string
    "The CU_column is not a column name in df - please input a string that is a column name in df." = CU_column %in% colnames(df),
    #Checks if the column names are actually columns in the df
    "The CPC_Code_Column is not a column name in df - please input a string that is a column name in df." = CPC_Code_Column %in% colnames(df),
    "method option is not valid - please choose between 'Check', 'Fill_all', 'Fill_blank'." = tolower(method) %in% c(
      "check",
      "fill_all",
      "fill_blank"
    ),
    "No flagged CPC Codes present in the CPC Code column. Function stopped." = TRUE %in% (unique(df[[CPC_Code_Column]]) %in% No_CU_CPC_Codes)
  )



  if (tolower(method) %in% "check") {
    method_type <- 1
  }

  if (tolower(method) %in% "fill_all") {
    method_type <- 2
  }

  if (tolower(method) %in% "fill_blank") {
    method_type <- 3
  }

  # Column conversion checks ----

  df[[CPC_Code_Column]] <- as.character(df[[CPC_Code_Column]]) #convert fbs id col to character
  df[[CU_column]] <- as.character(df[[CU_column]]) #convert fbs id col to character


  # Comments column creation check ----

  #Sees if a comments column already exists, and creates one if not. Only applies on a Fill method, as this is where the fill is recorded.

  if (method_type %in% c(2, 3)) {
    if (comments == TRUE) {
      message("comments included")
      if (!(comment_col %in% colnames(df))) {
        message("No comments column detected, new one being created")
        df[[comment_col]] <- NA
      }
    }
  }


  # Non-0 rows ----
  CU_not_0 <- df[df[[CPC_Code_Column]] %in% No_CU_CPC_Codes &
                   !(df[[CU_column]] %in% "0"), ]

  # method - Checks ----

  if(nrow(CU_not_0) == 0){ #Does nothing if there are no relevant rows
    if (method_type == 2){
      return(df)
    }
  } else {
    if (method_type == 1) {

      lapply(1:nrow(CU_not_0), function(i) {
        message(
          paste(
            "FLAG: CU value for row number",
            rownames(CU_not_0[i, ]),
            " is not 0; value is",
            CU_not_0[[CU_column]][i],
            "."
          )
        )
      })
      #Option to produce code to create a df subset for inspection:
      #message("You Can run the message below to create a subset of the input df for closer inspection. Please replace 'df' with the variable name of the dataframe you inputted to this function.")
      #message(paste0("Non_zero_CU <- df[rownames(df) %in% c(",  paste(rownames(CU_not_0), collapse = ", "), "), ]"))

      #Option to automatically create and view subset df for inspection:
      assign("Non_zero_CU", CU_not_0, envir = globalenv())
      View(Non_zero_CU)
    }


    # Method -  Fill All ----

    if (method_type == 2) {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(CU_not_0)) {
          #If so, loops through all items where CU isn't 0, but should be
          new_comment <- paste0(CU_column,
                                " value changed from ",
                                CU_not_0[[CU_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(CU_not_0[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[CU_column]][df[[CPC_Code_Column]] %in% No_CU_CPC_Codes &
                        !(df[[CU_column]] %in% "0")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
      return(df)
    }
  }

  #If the method type is 1, so just checking, this loops through rows where CU isno 0 and prints the row numbers out.




  if (method_type == 3) {
    CU_blank <- df[df[[CPC_Code_Column]] %in% No_CU_CPC_Codes &
                     df[[CU_column]] %in% c(NA, ""), ] #Creates a df of CU values which are either blank or NA
    if(nrow(CU_blank) == 0){
    } else {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(CU_blank)) {
          #If so, loops through all items where CU isn't 0, but should be
          new_comment <- paste0(CU_column,
                                " value changed from ",
                                CU_blank[[CU_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(CU_blank[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[CU_column]][df[[CPC_Code_Column]] %in% No_CU_CPC_Codes &
                        df[[CU_column]] %in% c(NA, "")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
    }
    return(df)
  }
}




Absence_Check_F22D6N3 <- function(df,
                                  F22D6N3_column = "F22D6N3g",
                                  method = "Check",
                                  CPC_Code_Column = "CPC_Code",
                                  No_F22D6N3_CPC_Codes = Standard_No_F22D6N3_CPC_Codes,
                                  comments = TRUE,
                                  comment_col = "comments") {
  #Check, Fill_all, or Fill_blank.

  # Input checks ----

  stopifnot(
    "df is not a data frame - please input a data frame" = is.data.frame(df),
    #Checks to see if the df item is a data frame
    "The F22D6N3_column is not a character or string - please input a character or string that is a column name in df, e.g. 'F22D6N3g'" = is.character(F22D6N3_column),
    #checks to see if the F22D6N3_column is a character string
    "The CPC_Code_Column is not a character or string - please input a character or string that is a column name in df, e.g. 'CPC_Codes'" = is.character(CPC_Code_Column),
    #checks to see if the CPC_Code_Column is a character string
    "The F22D6N3_column is not a column name in df - please input a string that is a column name in df." = F22D6N3_column %in% colnames(df),
    #Checks if the column names are actually columns in the df
    "The CPC_Code_Column is not a column name in df - please input a string that is a column name in df." = CPC_Code_Column %in% colnames(df),
    "method option is not valid - please choose between 'Check', 'Fill_all', 'Fill_blank'." = tolower(method) %in% c(
      "check",
      "fill_all",
      "fill_blank"
    ),
    "No flagged CPC Codes present in the CPC Code column. Function stopped." = TRUE %in% (unique(df[[CPC_Code_Column]]) %in% No_F22D6N3_CPC_Codes)
  )



  if (tolower(method) %in% "check") {
    method_type <- 1
  }

  if (tolower(method) %in% "fill_all") {
    method_type <- 2
  }

  if (tolower(method) %in% "fill_blank") {
    method_type <- 3
  }

  # Column conversion checks ----

  df[[CPC_Code_Column]] <- as.character(df[[CPC_Code_Column]]) #convert fbs id col to character
  df[[F22D6N3_column]] <- as.character(df[[F22D6N3_column]]) #convert fbs id col to character


  # Comments column creation check ----

  #Sees if a comments column already exists, and creates one if not. Only applies on a Fill method, as this is where the fill is recorded.

  if (method_type %in% c(2, 3)) {
    if (comments == TRUE) {
      message("comments included")
      if (!(comment_col %in% colnames(df))) {
        message("No comments column detected, new one being created")
        df[[comment_col]] <- NA
      }
    }
  }


  # Non-0 rows ----
  F22D6N3_not_0 <- df[df[[CPC_Code_Column]] %in% No_F22D6N3_CPC_Codes &
                        !(df[[F22D6N3_column]] %in% "0"), ]

  # method - Checks ----

  if(nrow(F22D6N3_not_0) == 0){ #Does nothing if there are no relevant rows
    if (method_type == 2){
      return(df)
    }
  } else {
    if (method_type == 1) {

      lapply(1:nrow(F22D6N3_not_0), function(i) {
        message(
          paste(
            "FLAG: F22D6N3 value for row number",
            rownames(F22D6N3_not_0[i, ]),
            " is not 0; value is",
            F22D6N3_not_0[[F22D6N3_column]][i],
            "."
          )
        )
      })
      #Option to produce code to create a df subset for inspection:
      #message("You Can run the message below to create a subset of the input df for closer inspection. Please replace 'df' with the variable name of the dataframe you inputted to this function.")
      #message(paste0("Non_zero_F22D6N3 <- df[rownames(df) %in% c(",  paste(rownames(F22D6N3_not_0), collapse = ", "), "), ]"))

      #Option to automatically create and view subset df for inspection:
      assign("Non_zero_F22D6N3", F22D6N3_not_0, envir = globalenv())
      View(Non_zero_F22D6N3)
    }


    # Method -  Fill All ----

    if (method_type == 2) {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(F22D6N3_not_0)) {
          #If so, loops through all items where F22D6N3 isn't 0, but should be
          new_comment <- paste0(F22D6N3_column,
                                " value changed from ",
                                F22D6N3_not_0[[F22D6N3_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(F22D6N3_not_0[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[F22D6N3_column]][df[[CPC_Code_Column]] %in% No_F22D6N3_CPC_Codes &
                             !(df[[F22D6N3_column]] %in% "0")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
      return(df)
    }
  }

  #If the method type is 1, so just checking, this loops through rows where F22D6N3 isno 0 and prints the row numbers out.




  if (method_type == 3) {
    F22D6N3_blank <- df[df[[CPC_Code_Column]] %in% No_F22D6N3_CPC_Codes &
                          df[[F22D6N3_column]] %in% c(NA, ""), ] #Creates a df of F22D6N3 values which are either blank or NA
    if(nrow(F22D6N3_blank) == 0){
    } else {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(F22D6N3_blank)) {
          #If so, loops through all items where F22D6N3 isn't 0, but should be
          new_comment <- paste0(F22D6N3_column,
                                " value changed from ",
                                F22D6N3_blank[[F22D6N3_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(F22D6N3_blank[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[F22D6N3_column]][df[[CPC_Code_Column]] %in% No_F22D6N3_CPC_Codes &
                             df[[F22D6N3_column]] %in% c(NA, "")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
    }
    return(df)
  }
}


Absence_Check_F20D5N3 <- function(df,
                                  F20D5N3_column = "F20D5N3g",
                                  method = "Check",
                                  CPC_Code_Column = "CPC_Code",
                                  No_F20D5N3_CPC_Codes = Standard_No_F20D5N3_CPC_Codes,
                                  comments = TRUE,
                                  comment_col = "comments") {
  #Check, Fill_all, or Fill_blank.

  # Input checks ----

  stopifnot(
    "df is not a data frame - please input a data frame" = is.data.frame(df),
    #Checks to see if the df item is a data frame
    "The F20D5N3_column is not a character or string - please input a character or string that is a column name in df, e.g. 'F20D5N3g'" = is.character(F20D5N3_column),
    #checks to see if the F20D5N3_column is a character string
    "The CPC_Code_Column is not a character or string - please input a character or string that is a column name in df, e.g. 'CPC_Codes'" = is.character(CPC_Code_Column),
    #checks to see if the CPC_Code_Column is a character string
    "The F20D5N3_column is not a column name in df - please input a string that is a column name in df." = F20D5N3_column %in% colnames(df),
    #Checks if the column names are actually columns in the df
    "The CPC_Code_Column is not a column name in df - please input a string that is a column name in df." = CPC_Code_Column %in% colnames(df),
    "method option is not valid - please choose between 'Check', 'Fill_all', 'Fill_blank'." = tolower(method) %in% c(
      "check",
      "fill_all",
      "fill_blank"
    ),
    "No flagged CPC Codes present in the CPC Code column. Function stopped." = TRUE %in% (unique(df[[CPC_Code_Column]]) %in% No_F20D5N3_CPC_Codes)
  )



  if (tolower(method) %in% "check") {
    method_type <- 1
  }

  if (tolower(method) %in% "fill_all") {
    method_type <- 2
  }

  if (tolower(method) %in% "fill_blank") {
    method_type <- 3
  }

  # Column conversion checks ----

  df[[CPC_Code_Column]] <- as.character(df[[CPC_Code_Column]]) #convert fbs id col to character
  df[[F20D5N3_column]] <- as.character(df[[F20D5N3_column]]) #convert fbs id col to character


  # Comments column creation check ----

  #Sees if a comments column already exists, and creates one if not. Only applies on a Fill method, as this is where the fill is recorded.

  if (method_type %in% c(2, 3)) {
    if (comments == TRUE) {
      message("comments included")
      if (!(comment_col %in% colnames(df))) {
        message("No comments column detected, new one being created")
        df[[comment_col]] <- NA
      }
    }
  }



  # Non-0 rows ----
  F20D5N3_not_0 <- df[df[[CPC_Code_Column]] %in% No_F20D5N3_CPC_Codes &
                        !(df[[F20D5N3_column]] %in% "0"), ]

  # method - Checks ----

  if(nrow(F20D5N3_not_0) == 0){ #Does nothing if there are no relevant rows
    if (method_type == 2){
      return(df)
    }
  } else {
    if (method_type == 1) {

      lapply(1:nrow(F20D5N3_not_0), function(i) {
        message(
          paste(
            "FLAG: F20D5N3 value for row number",
            rownames(F20D5N3_not_0[i, ]),
            " is not 0; value is",
            F20D5N3_not_0[[F20D5N3_column]][i],
            "."
          )
        )
      })
      #Option to produce code to create a df subset for inspection:
      #message("You Can run the message below to create a subset of the input df for closer inspection. Please replace 'df' with the variable name of the dataframe you inputted to this function.")
      #message(paste0("Non_zero_F20D5N3 <- df[rownames(df) %in% c(",  paste(rownames(F20D5N3_not_0), collapse = ", "), "), ]"))

      #Option to automatically create and view subset df for inspection:
      assign("Non_zero_F20D5N3", F20D5N3_not_0, envir = globalenv())
      View(Non_zero_F20D5N3)
    }


    # Method -  Fill All ----

    if (method_type == 2) {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(F20D5N3_not_0)) {
          #If so, loops through all items where F20D5N3 isn't 0, but should be
          new_comment <- paste0(F20D5N3_column,
                                " value changed from ",
                                F20D5N3_not_0[[F20D5N3_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(F20D5N3_not_0[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[F20D5N3_column]][df[[CPC_Code_Column]] %in% No_F20D5N3_CPC_Codes &
                             !(df[[F20D5N3_column]] %in% "0")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
      return(df)
    }
  }

  #If the method type is 1, so just checking, this loops through rows where F20D5N3 isno 0 and prints the row numbers out.




  if (method_type == 3) {
    F20D5N3_blank <- df[df[[CPC_Code_Column]] %in% No_F20D5N3_CPC_Codes &
                          df[[F20D5N3_column]] %in% c(NA, ""), ] #Creates a df of F20D5N3 values which are either blank or NA
    if(nrow(F20D5N3_blank) == 0){
    } else {
      if (comments == TRUE) {
        #Checks if comments are enabled
        for (i in 1:nrow(F20D5N3_blank)) {
          #If so, loops through all items where F20D5N3 isn't 0, but should be
          new_comment <- paste0(F20D5N3_column,
                                " value changed from ",
                                F20D5N3_blank[[F20D5N3_column]][i],
                                " to 0") #Creates the new comment
          row_number <- rownames(F20D5N3_blank[i, ]) #Finds the rownumber
          existing_comment <- df[rownames(df) %in% row_number, ][[comment_col]] #Checks if there's an existing comment
          if (!is.na(existing_comment)) {
            #adds new comment to existing comment if present
            new_comment <- paste0(existing_comment, "; ", new_comment)
          }
          df[rownames(df) %in% row_number, ][[comment_col]] <- new_comment #Applies the new comment
        }
      }
      df[[F20D5N3_column]][df[[CPC_Code_Column]] %in% No_F20D5N3_CPC_Codes &
                             df[[F20D5N3_column]] %in% c(NA, "")] <- "0" #Replaces non-0 values for the relevant fbs groups with 0.
    }
    return(df)
  }
}
