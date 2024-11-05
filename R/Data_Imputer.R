#---
#Title: Data_Imputer
#Author: Thomas Codd - https://github.com/TomCodd
#Contributor: Lucia Segovia de la Revilla  - https://github.com/LuciaSegovia
#Version: V1.0.0
#Changelog:

#Github: https://github.com/TomCodd/NutritionTools
#---

#' @title Data Imputer
#' @description The Data Imputer is an interactive function used to find
#'   suitable imputations for food items, either from the same dataset (by
#'   default) or from a different dataset. The output by default is a console
#'   output and a .txt file containing the lines of code needed to implement the
#'   imputations that the user has picked, however this can be changed to a
#'   data.frame with the changes implemented.
#' @param df Required - the data.frame which contains the food items with
#'   missing values.
#' @param receiver_title_column Required - The name of the column in \code{df}
#'   which contains food groups or titles.
#' @param receiver_search_terms Required - Search words to identify a specific
#'   food group or title from \code{receiver_title_column}. All items missing
#'   values from the \code{missing_nutrient_column} within this food group will
#'   be shortlisted for imputation.
#' @param receiver_desc_column Required - The name of the column within
#'   \code{df} that contains detailed food names.
#' @param receiver_exclude_terms Optional - = c() Optional -
#' @param receiver_id_column Required - The name of the column within \code{df}
#'   that contains the ID numbers of the food items.
#' @param missing_nutrient_column Required - The name of the column within
#'   \code{df} AND \code{donor_df} that contains the nutrient you are trying to
#'   impute values for.
#' @param water_column Required - default: \code{'WATERg'} - The name of the
#'   column within \code{df} AND \code{donor_df} that contains water values, in
#'   grams per 100g.
#' @param comment_col Required - default: \code{'comments'} - The name of the
#'   column within \code{df} that contains comments.
#' @param donor_fct_column Required - default: \code{'Source'} - The name of the
#'   column within \code{df} that contains the source Food Composition Table the
#'   food items are from.
#' @param donor_df Required - default: \code{df} - The name of the data.frame
#'   that you are looking to get fill-in values from. The default is the same df
#'   as the donor df, as this function was developed to run over large multi-FCT
#'   composite tables.
#' @param donor_id_column Required - default: \code{receiver_id_column} - The
#'   name of the column within \code{donor_df} that contains the ID numbers of
#'   the food items. The default is suitable when using \code{df} as the donor
#'   and receiver.
#' @param donor_search_column Required - default: \code{receiver_desc_column} -
#'   The name of the column within \code{donor_df} that contains detailed food
#'   names. The default is suitable when using \code{df} as the donor and
#'   receiver.
#' @param donor_search_terms Required - The search terms you would like to use
#'   to find suitable imputation values to use. Added to the food descriptions
#'   of the receiver items.
#' @param extra_info_columns Optional - The name of columns present in both
#'   data.frames (\code{df} AND \code{donor_df}) you would like to see the
#'   contents of when making decisions about which items should be used for
#'   imputation.
#' @param exclude_receiver_terms Required - default: \code{TRUE} - Either
#'   \code{TRUE} or \code{FALSE}. The donor search terms by default are
#'   generated from the food descriptions found in \code{receiver_desc_column}.
#'   If \code{TRUE} then key words present in \code{receiver_search_terms} will
#'   be excluded from these items. For example, if a food item is in a Food
#'   group 'Goat, Offal', and the food description is 'Goat, liver, raw'; if
#'   this option is set to \code{TRUE} then the search terms used will be
#'   'liver, raw'; if set to \code{FALSE} then the full 'Goat, lever, raw' will
#'   be used.
#' @param donor_search_collapse Optional - default: \code{c(",")} - The string
#'   used to separate the search terms. if the search terms are 'Goat, liver,
#'   raw', then using the default ',' will mean that the function will return
#'   items that match 'goat' and 'liver' and 'raw'. If none are used, only items
#'   that match the entire string; 'Goat, liver, raw'; will be returned.
#' @param Assume_continue Required - default: \code{FALSE} - Either \code{TRUE}
#'   or \code{FALSE}. There are several checks throughout the process to
#'   double-check inputs. If set to \code{TRUE}, this setting skips them,
#'   assuming the inputs are correct.
#' @param term_search Required - default: \code{"AND"} - Either \code{"AND"} or
#'   \code{"OR"}. Decide whether the imputation value search should results
#'   should find items which match all the search terms at once (\code{"AND"})
#'   or only one of them \code{"OR"}.
#' @param water_balance Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. If \code{TRUE} then the function will water-balance the
#'   values.
#' @param code_output Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. Decides whether the output should be pre-written code to be
#'   inserted just above where this function was called (by default), or a
#'   data.frame with the changes made (if set to \code{FALSE}).
#' @param txt_output Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. If using \code{code_output}, then this option attaches the
#'   generated code to a .txt file and saves it in your working directory.
#' @param round_imputed_figure Required - default: \code{TRUE} - Either
#'   \code{TRUE} or \code{FALSE}. Decide whether the imputed values should be
#'   rounded to 2 decimal places.
#' @return Either code that applies the imputations (if \code{code_output} is
#'   set to \code{TRUE}, as it is by default), or an altered data.frame with the
#'   imputations applied.
#'
#' @examples
#' #
#' # First we'll run through a demonstration of the Data_Imputer imputing from
#' within the same dataset. Because this is the default setting, fewer inputs
#' are needed.
#' #
#' # The dataset can be viewed using View(KE18_subset_modified)
#' #
#' # The dataset in question is missing some VITB12mcg values for 'Lamb liver,
#' # raw' and 'Lamb, liver, boiled (without salt)'. However, within the same
#' # dataset are some goat values which could be a good imputation value.
#' #
#' Data_Imputer(
#'   df = KE18_subset_modified,
#'   receiver_title_column = "food_group",
#'   receiver_search_terms = c("MEAT", "POULTRY"), #Identifies the food group using unique terms
#'   receiver_desc_column = "food_desc",
#'   receiver_exclude_terms = c("lean", "blood"), #We don't need to see any of the 'lean' or 'blood' results
#'   receiver_id_column = "fdc_id",
#'   term_search = "OR",
#'   missing_nutrient_column = "VITB12mcg",
#'   donor_search_terms = c("goat"),
#'   water_column = "WATERg",
#'   comment_col = "comments",
#'   donor_fct_column = "source_fct"
#'  )
#'
#'
#' # In this example we'll impute values from a different data.frame - the West
#' #Africa FCT subset, WA19_subset. This can be viewed using View(WA19_subset).
#' #We also want to look at some extra columns when we want to choose an item, so
#' #we've added two columns to the extra_info_columns option.
#'
#'  Data_Imputer(
#'    df = KE18_subset_modified,
#'    receiver_title_column = "food_group",
#'    receiver_search_terms = c("MEAT", "POULTRY"), #Identifies the food group using unique terms
#'    receiver_desc_column = "food_desc",
#'    receiver_exclude_terms = c("lean", "blood"), #We don't need to see any of the 'lean' or 'blood' results
#'    receiver_id_column = "fdc_id",
#'    missing_nutrient_column = "VITB12mcg",
#'    donor_search_terms = c("goat"),
#'    water_column = "WATERg",
#'    comment_col = "comments",
#'    donor_fct_column = "source_fct",
#'    donor_df = WA19_subset,
#'    donor_id_column = "fdc_id",
#'    term_search = "OR",
#'    donor_search_column = "food_desc",
#'    extra_info_columns = c("PROCNTg", "CHOAVLDFg")
#'  )

# Side function - User input check y/n ----

Check_to_continue_YesNo <- function( #Function to check if user wants to continue - must end with 'y/n: '
    Check_message
){
  # Takes user input, continues if they're happy with the items that are to be recipients of imputation
  Continue_Check <- readline(Check_message)

  # Takes the user input, and changes it to lower case, then converts it to logical if its understandable, and stops with a warning message if not.
  Continue_Check <- tolower(Continue_Check)

  if(Continue_Check %in% c("y", "yes")){ #converts yes or no into TRUE or FALSE.
    Continue_Check <- TRUE
  } else {
    if(Continue_Check %in% c("n", "no")){
      Continue_Check <- FALSE
    }

    if(!is.logical(Continue_Check)){ #input check
      stop("Incorrect input detected. Please use 'y', or 'n', to represent 'yes' or 'no'.")
    }
    if(isFALSE(Continue_Check)){ #if stop detected, passes a message on that is picked up outside the function.
      message("Stop input detected. Stopping.")
      return("Stop")
    }
  }
}

# Main function ----

Data_Imputer <- function(df,
                         receiver_title_column,
                         receiver_search_terms = c(),
                         receiver_desc_column,
                         receiver_exclude_terms = c(),
                         receiver_id_column,
                         missing_nutrient_column,
                         water_column = "WATERg",
                         comment_col = "comments",
                         donor_fct_column = "Source",
                         donor_df = df,
                         donor_id_column = receiver_id_column,
                         donor_search_column = receiver_desc_column,
                         donor_search_terms = c(),
                         extra_info_columns = c(),
                         exclude_receiver_terms = TRUE,
                         donor_search_collapse = c(","),
                         Assume_continue = FALSE,
                         term_search = "AND",
                         water_balance = TRUE,
                         code_output = TRUE,
                         txt_output = TRUE,
                         round_imputed_figure = TRUE
                         ) {

  # Standard input checks and column creation checks ----

  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df)) #Checks to see if the df item is a data frame
  stopifnot("donor_df is not a data frame - please input a data frame" = is.data.frame(donor_df)) #Checks to see if the df item is a data frame

  stopifnot("The receiver_title_column is not a character or string - please input a character or string that is a column name in df, e.g. 'FAO.Title'" = is.character(receiver_title_column)) #checks to see if the group_ID_col is a character string
  stopifnot("The receiver_title_column is not a column name in df - please input a string that is a column name in df, e.g. 'FAO.Title'" = receiver_title_column %in% colnames(df)) #Checks to see if the group_ID_col is in the list of column names for the df

  stopifnot("The receiver_search_terms is not a character or string - please input a character list or string that you would like to search for, e.g. 'goat', or c('goat', 'offal')" = is.character(receiver_search_terms)) #checks to see if the group_ID_col is a character string

  stopifnot("The receiver_desc_column is not a character or string - please input a character or string that is a column name in df, e.g. 'food_description'" = is.character(receiver_desc_column)) #checks to see if the group_ID_col is a character string
  stopifnot("The receiver_desc_column is not a column name in df - please input a string that is a column name in df, e.g. 'food_description'" = receiver_desc_column %in% colnames(df)) #Checks to see if the group_ID_col is in the list of column names for the df

  stopifnot("The receiver_exclude_terms is not a character or string - please input a character list or string that you would like to search for, e.g. 'liver', or c('liver', 'kidneys')" = is.character(receiver_exclude_terms)) #checks to see if the group_ID_col is a character string

  stopifnot("The receiver_id_column is not a character or string - please input a character or string that is a column name in df, e.g. 'food_id'" = is.character(receiver_id_column)) #checks to see if the group_ID_col is a character string
  stopifnot("The receiver_id_column is not a column name in df - please input a string that is a column name in df, e.g. 'food_id'" = receiver_id_column %in% colnames(df)) #Checks to see if the group_ID_col is in the list of column names for the df

  stopifnot("The missing_nutrient_column is not a character or string - please input a character or string that is a column name in df, e.g. 'VITB12mcg'" = is.character(missing_nutrient_column)) #checks to see if the group_ID_col is a character string
  stopifnot("The missing_nutrient_column is not a column name in the df - please input a string that is a column name in the df AND the donor_df, e.g. 'VITB12mcg'" = missing_nutrient_column %in% colnames(df)) #Checks to see if the group_ID_col is in the list of column names for the df
  stopifnot("The missing_nutrient_column is not a column name in the donor_df - please input a string that is a column name in the df AND the donor_df, e.g. 'VITB12mcg'" = missing_nutrient_column %in% colnames(donor_df)) #Checks to see if the group_ID_col is in the list of column names for the df

  stopifnot("The water_column is not a character or string - please input a character or string that is a column name in df, e.g. 'WATERg'" = is.character(water_column)) #checks to see if the group_ID_col is a character string
  stopifnot("The water_column is not a column name in the df - please input a string that is a column name in the df AND the donor_df, e.g. 'WATERg'" = water_column %in% colnames(df)) #Checks to see if the group_ID_col is in the list of column names for the df
  stopifnot("The water_column is not a column name in the donor_df - please input a string that is a column name in the df AND the donor_df, e.g. 'WATERg'" = water_column %in% colnames(donor_df)) #Checks to see if the group_ID_col is in the list of column names for the df

  stopifnot("The comment_col is not a character or string - please input a character or string that is a column name in df, e.g. 'comments'" = is.character(comment_col)) #checks to see if the group_ID_col is a character string

  stopifnot("The donor_fct_column is not a character or string - please input a character or string that is a column name in donor_df, e.g. 'Source'" = is.character(donor_fct_column)) #checks to see if the group_ID_col is a character string
  stopifnot("The donor_fct_column is not a column name in the donor_df - please input a string that is a column name in the donor_df, e.g. 'Source'" = donor_fct_column %in% colnames(donor_df)) #Checks to see if the group_ID_col is in the list of column names for the df

  stopifnot("The donor_id_column is not a character or string - please input a character or string that is a column name in donor_df, e.g. 'food_id'" = is.character(donor_id_column)) #checks to see if the group_ID_col is a character string
  stopifnot("The donor_id_column is not a column name in the donor_df - please input a string that is a column name in the donor_df, e.g. 'food_id'" = donor_id_column %in% colnames(donor_df)) #Checks to see if the group_ID_col is in the list of column names for the df

  stopifnot("The donor_search_column is not a character or string - please input a character or string that is a column name in donor_df, e.g. 'food_description'" = is.character(donor_search_column)) #checks to see if the group_ID_col is a character string
  stopifnot("The donor_search_column is not a column name in the donor_df - please input a string that is a column name in the donor_df, e.g. 'food_description'" = donor_search_column %in% colnames(donor_df)) #Checks to see if the group_ID_col is in the list of column names for the df

  stopifnot("The donor_search_terms is not a character or string - please input a character list or string that you would like to search for, e.g. 'goat', or c('goat', 'offal')" = is.character(donor_search_terms)) #checks to see if the group_ID_col is a character string

  if(length(extra_info_columns[!extra_info_columns %in% ""])>0){
    stopifnot("The extra_info_columns is not a character or string - please input a character list or string that you would like to search for, e.g. 'PROCNTg', or c('PROCNTg', 'CHOAVLDFg')" = is.character(extra_info_columns)) #checks to see if the group_ID_col is a character string
    stopifnot("The extra_info_columns contains items which are not column names in the df - please input a set of strings that are all column names in the df AND the donor_df, e.g. 'PROCNTg'" = !(FALSE %in% (extra_info_columns %in% colnames(df)))) #Checks to see if the group_ID_col is in the list of column names for the df
    stopifnot("The extra_info_columns contains items which are not column names in the donor_df - please input a set of strings that are all column names in the df AND the donor_df, e.g. 'PROCNTg'" = !(FALSE %in% (extra_info_columns %in% colnames(donor_df)))) #Checks to see if the group_ID_col is in the list of column names for the df
  }

  stopifnot("The exclude_receiver_terms input is not logical - please set it to TRUE or FALSE" = is.logical(exclude_receiver_terms))

  stopifnot("The donor_search_collapse is not a character or string - please input a character list or string that you would like to use to collapse the searches. For example, using ',' will mean that donor_search_terms of 'lamb, raw' will look for 'lamb' and 'raw'. Leaving this blank (e.g. '') will mean that the search will be for 'lamb, raw' as a string, not the seperate words" = is.character(donor_search_collapse)) #checks to see if the group_ID_col is a character string

  stopifnot("The Assume_continue input is not logical - please set it to TRUE or FALSE" = is.logical(Assume_continue))

  term_search <- toupper(term_search) #Makes sure its all upper case

  stopifnot("The term_search is not a character or string - please input either 'AND' or 'OR'" = is.character(donor_id_column)) #checks to see if the group_ID_col is a character string
  stopifnot("The term_search is not 'AND' or 'OR' - please input either 'AND' or 'OR'" = term_search %in% c('AND', 'OR')) #Checks to see if the group_ID_col is in the list of column names for the df

  stopifnot("The water_balance input is not logical - please set it to TRUE or FALSE" = is.logical(water_balance))

  stopifnot("The code_output input is not logical - please set it to TRUE or FALSE" = is.logical(code_output))

  stopifnot("The txt_output input is not logical - please set it to TRUE or FALSE" = is.logical(txt_output))

  stopifnot("The round_imputed_figure input is not logical - please set it to TRUE or FALSE" = is.logical(round_imputed_figure))

  deparsed_df_name <- deparse(substitute(df))

  if(!(comment_col %in% colnames(df))){
    df[[comment_col]] <- NA #If the comment column isn't present yet in the data frame then this creates the comment column
  }


  # Identifying the imputation recipient items ----

  # Checks if the search df is different from the input df
  seperate_search_df <- identical(donor_df, df)
  if(isTRUE(seperate_search_df)){
    message()
    message("donor dataframe identified")
    message()
    message()
  }


  # First, taking the inputs and forming them into search queries within the dataset.
  formatting_search_terms <- paste0('grepl("', receiver_search_terms, '", df[[receiver_title_column]], ignore.case = TRUE)')

  # Creates a series of grepl queries, combining both search and exclusion queries if relevant.
  if(length(receiver_exclude_terms) > 0){
    formatting_exclude_terms <- paste0('!grepl("', receiver_exclude_terms, '", df[[receiver_desc_column]], ignore.case = TRUE)')
    combined_grepl_search <- paste(paste0(formatting_search_terms, collapse = " & "), paste(formatting_exclude_terms, collapse = " & "), sep = " & ")
  } else {
    combined_grepl_search <- paste(paste(formatting_search_terms, collapse = " & "))
  }

  # Evaluates the combined search, creating missing_data from it
  eval(parse(text = paste("missing_data <- unique(df[!is.na(df[[receiver_title_column]]) & is.na(df[[missing_nutrient_column]]) & ", combined_grepl_search, ", c(receiver_id_column, receiver_desc_column, missing_nutrient_column, water_column, extra_info_columns)])")))

  # Stops if no missing_data present, or describes the next step.
  if(nrow(missing_data) > 0){
    message("Recipient items found - please see below. If you would like to exclude any of these items, please identify a unique key word in that items description, and re-run the function with that key word in the 'receiver_exclude_terms' input.")
    print(missing_data)
  } else {

    return(message("No receiver items found. Please modify your inputs and try again."))
  }

  if(Assume_continue == FALSE){
    if(Check_to_continue_YesNo("Are you happy to continue to impute values for the items above? (y/n): ") == "Stop"){
      return(message(""))
    }
  }

  # Creating Donor Search Terms ----

  # First, formatting the original items to extract useful terms, not specific to the item family being searched (e.g. "goat, liver" would extract "liver" if "goat" was in the main title)
  receiver_search_terms_2 <- paste0(receiver_search_terms, collapse = "|")

  missing_data$extracted_search_terms <- missing_data[[receiver_desc_column]]

  if(exclude_receiver_terms == TRUE){
    missing_data$extracted_search_terms <- gsub(receiver_search_terms_2, "", missing_data$extracted_search_terms, ignore.case = TRUE) #removes search terms used to find the original match
    missing_data$extracted_search_terms <- gsub("^.*?(?=[[:alnum:]])", "", missing_data$extracted_search_terms, perl = TRUE) #Removes everything before an alphanumeric character. I hate regex.
  }

  if(length(donor_search_collapse[!donor_search_collapse %in% ""]) > 0){ #Sees if the donor search collapse is in use, as it is by default
    donor_search_collapse <- paste0(donor_search_collapse, collapse = "|") #collapses search terms
    missing_data$extracted_search_terms <- gsub(donor_search_collapse, "|", missing_data$extracted_search_terms, ignore.case = TRUE) #replaces search terms with | (OR)
  }

  if(length(donor_search_terms)>0){ #Sees if the donor search terms are in use
    donor_search_terms <- paste0(donor_search_terms, collapse = "|") #collapses search terms
    missing_data$extracted_search_terms <- paste0(missing_data$extracted_search_terms, "|", donor_search_terms) #adds them to the search query
  }


  missing_data$extracted_search_terms <- strsplit(missing_data$extracted_search_terms, "\\|") #Splits out the search terms

  for(i in 1:nrow(missing_data)){
    missing_data$extracted_search_terms[i] <- paste0(unique(gsub("^.*?(?=[[:alnum:]])|(?<=[[:alnum:]])[[:space:]]*?$", "", missing_data$extracted_search_terms[i][[1]], perl = TRUE)), collapse = "|") #removes spaces from before or after it
  }
  print(missing_data)

  #Looping through missing items ----

  if(code_output == TRUE){
    Code_output_text <- ""
    }


  for(i in 1:nrow(missing_data)){

    if(term_search == "AND"){
      #The next splits out the search terms, and makes them into "And" not "Or"
      grepl_search <- paste0("grepl('", paste0(strsplit(paste(missing_data$extracted_search_terms[i]), "\\|")[[1]], collapse = "', df[[donor_search_column]], ignore.case = TRUE) & grepl('"), "', df[[donor_search_column]], ignore.case = TRUE)")
      eval(parse(text = paste("potential_matches <- donor_df[", grepl_search, " & !is.na(donor_df[[missing_nutrient_column]]), c(donor_id_column, donor_search_column, missing_nutrient_column, water_column, extra_info_columns)]")))
    } else {
      #If Or is selected, keeps it as-is instead
      potential_matches <- donor_df[grepl(missing_data$extracted_search_terms[i], donor_df[[donor_search_column]], ignore.case = TRUE)  & !is.na(donor_df[[missing_nutrient_column]]), c(donor_id_column, donor_search_column, missing_nutrient_column, water_column, extra_info_columns)]
    }

    # Loops through each item in the list
    if(nrow(potential_matches) > 0){

      # Finds the closest stringdist match, and orders them that way round

      suppressWarnings({

      potential_matches$match_level <- stringdist::stringdist(missing_data$extracted_search_terms[i], potential_matches[[donor_search_column]], method = 'jw')
      potential_matches <- potential_matches[order(potential_matches$match_level),]
      potential_matches$match_level <- NULL

      })

      # Resets row numbers
      rownames(potential_matches) <- NULL

      #Checks if the user wants to continue if a large number of matches is found

      if(Assume_continue == FALSE){
        if(nrow(potential_matches)>10){
          message("")
        if(Check_to_continue_YesNo(paste0(nrow(potential_matches), " items found that match ", missing_data$extracted_search_terms[i], ". Are you sure you wish to continue? (y/n): ")) == "Stop"){
          next
        }
        }
      }

      # Creates a subset of the missing data table, to show the user what they're looking to find a value for

      missing_data_subset <- missing_data[i, c(receiver_id_column, receiver_desc_column, missing_nutrient_column, water_column, "extracted_search_terms", extra_info_columns)]

      #User selects donors ----

      repeat{ # repeats until the user says they're happy with their selection
        #prints the subsetted table, and the results of the search
        message("")
        message("--------")
        message("")
        message(paste0("Matches for: "))
        print(missing_data_subset)
        message("")
        message(paste0("Shown here: "))
        print.data.frame(potential_matches, max = length(potential_matches)*nrow(potential_matches))
        message("")
        message("Please select the row numbers of the items you would like to impute values from, seperated by commas.")
        message("Multimatches will create an averaged result.")
        message("An input of 'All' will create a multimatch result from all items shown.")
        message("An input of 'None', or selecting nothing, will result in no values being selected, and no imputation taking place.")

        Selection_chosen <- readline("Which items would you like to impute from? Please type the row numbers, separated by commas: ")

        if(TRUE %in% (tolower(Selection_chosen) %in% c("", "none"))){ #Checks for the exit items
          message("No items selected. Skipping.")
          Selection_chosen <- "ESCAPE"
          break
        } else {
          Selection_chosen <- strsplit(Selection_chosen, ",")[[1]] #Splits out the results
          Selection_chosen <- gsub("[^[:alnum:]]", "", Selection_chosen) #removes everything but letters and numbers

          if(TRUE %in% (tolower(Selection_chosen) %in% "all")){
            message("All items selected.")
            Selection_chosen <- 1:nrow(potential_matches)
          }

          Selection_chosen <- as.numeric(Selection_chosen) #converts to numeric

          Selection_chosen[Selection_chosen > nrow(potential_matches)] <- NA

          if(TRUE %in% (is.na(Selection_chosen))){ #All valid text inputs have been processed - anything else is an invalid input, which will cause a repeat
            message("Invalid input detected. Please only input 'all', 'none', nothing, or a selection of available row numbers seperated by commas.")
          } else {
            message("")
            message(paste0("Items selected - ")) #Shows user the selected items
            print(potential_matches[Selection_chosen, donor_search_column])
            message("")

            if(Check_to_continue_YesNo("Would you like to continue with these items? (y/n): ") == "Stop"){ #Checks if the user wants to continue - if they do, then the break cause is given
            } else {
              break #Breaks the loop, and then continues
            }
          }
        }


      } #Loop ends here

      if(TRUE %in% (Selection_chosen == "ESCAPE")){
        next
      }

      Chosen_IDs <- potential_matches[Selection_chosen, donor_id_column]

      #print(Chosen_IDs)

      Selected_Imputations <- donor_df[donor_df[[donor_id_column]] %in% Chosen_IDs, c(donor_id_column, donor_search_column, missing_nutrient_column, water_column, donor_fct_column)]

      Selected_Imputations$full_id <- paste0(Selected_Imputations[[donor_fct_column]],"(", Selected_Imputations[[donor_id_column]], ")")


      # Water balancing ----

      if(water_balance == TRUE){

        Recipient <- missing_data[i,]

        Selected_Imputations$WB_nutrient <- Selected_Imputations[[missing_nutrient_column]]*(100-as.numeric(Recipient[[water_column]]))/(100-Selected_Imputations[[water_column]])
        message("")
        message("Imputated values water balanced:")

        print(Selected_Imputations)
        message("")
      }

      # Averaging multimatches ----

      if(nrow(Selected_Imputations) > 0){ #This includes single items, but it doesn't matter, as mean(x) = x
        if(isTRUE(water_balance)){
          imputed_value <- mean(Selected_Imputations$WB_nutrient)
        } else {
          imputed_value <- mean(Selected_Imputations[[missing_nutrient_column]])
        }
      }

      if(isTRUE(round_imputed_figure)){
        imputed_value <- round(imputed_value, 2)
      }

      # Creating comment ----

      if(nrow(Selected_Imputations) > 1 & isTRUE(water_balance)){
        comment <- paste0(missing_nutrient_column, " value imputed using the averaged, water-balanced ", missing_nutrient_column, " values from (", paste0(Selected_Imputations$full_id, collapse = ", "), ")")
      } else if(nrow(Selected_Imputations) > 1 & isFALSE(water_balance)){
        comment <- paste0(missing_nutrient_column, " value imputed using the averaged ", missing_nutrient_column, " values from (", paste0(Selected_Imputations$full_id, collapse = ", "), ")")
      } else if(nrow(Selected_Imputations) == 1 & isTRUE(water_balance)){
        comment <- paste0(missing_nutrient_column, " value imputed using the water-balanced ", missing_nutrient_column, " value from ", paste0(Selected_Imputations$full_id, collapse = ", "))
      } else {
        comment <- paste0(missing_nutrient_column, " value imputed using the ", missing_nutrient_column, " value from ", paste0(Selected_Imputations$full_id, collapse = ", "))
      }

      if(df[df[[receiver_id_column]] %in% Recipient[[receiver_id_column]][1], comment_col] %in% c("", NA)){
      } else {
        comment <- paste0(df[df[[receiver_id_column]] %in% Recipient[[receiver_id_column]], comment_col], "; ", comment)
      }

      # Code output ----

      if(isTRUE(code_output)){

        Code_output_text <- paste0(Code_output_text, " # ", missing_nutrient_column, " value for ", Recipient[[receiver_id_column]], " imputed - changed from '", Recipient[[missing_nutrient_column]], "' to ", imputed_value, " \n") #Creates comment in the code output

        Code_output_text <- paste0(Code_output_text, deparsed_df_name, "[", deparsed_df_name, "$", receiver_id_column, " %in% '", Recipient[[receiver_id_column]], "', c('", missing_nutrient_column, "', '", comment_col, "')] <- c(", imputed_value, ", '", comment, "') \n \n ") #Creates the line of code that will do the change

      } else {


        # df output ----

        # Tested as faster than filtering - admittedly on a small df

        df[df[[receiver_id_column]] %in% Recipient[[receiver_id_column]], comment_col] <- comment #Writes the comment

        df[df[[receiver_id_column]] %in% Recipient[[receiver_id_column]], missing_nutrient_column] <- imputed_value #writes the value

      }


    } else { #if no match found, skips to the next one, with a message
      message("")
      message("--------")
      message("")
      message(paste0("No matches found for ", missing_data$extracted_search_terms[i], ". Please try again with different terms - terms can be added using the 'donor_search_terms' input."))
    }
  } #This ends the loop

  if(isFALSE(code_output)){
    return(df)
  } else {

    if(exists("Code_output_text")){

      message("Please copy the code below into the script just above where the Data_Imputer was run: ")
      message("")

      Code_output_text <- paste0("# Imputations generated on ", Sys.Date(), " at ", format(Sys.time(), "%X"), ", Using the Data_Imputer (V1.0.0) function from the NutritionTools Package (https://tomcodd.github.io/NutritionTools/). \n \n", Code_output_text) #Creates a blank code output item


      cat(Code_output_text)
      if(isTRUE(txt_output)){

        message(paste0(".txt file created at ", getwd(), " with the code to add the imputation to the script"))
        filename <- paste0("Imputation_R_Script_", gsub("[^[:alnum:]\\-\\_]", "", Sys.time()), ".txt")
        writeLines(Code_output_text, filename)
      }
    }
  }
}


# Imputations generated on 2024-11-05 at 15:52:53, Using the Data_Imputer (V1.0.0) function from the NutritionTools Package (https://tomcodd.github.io/NutritionTools/).

# VITB12mcg value for 7018 imputed - changed from 'NA' to 82.29
KE18_subset_modified[KE18_subset_modified$fdc_id %in% '7018', c('VITB12mcg', 'comments')] <- c(82.29, 'VITB12mcg value imputed using the water-balanced VITB12mcg value from KE18(7015)')

# VITB12mcg value for 7048 imputed - changed from 'NA' to 0
KE18_subset_modified[KE18_subset_modified$fdc_id %in% '7048', c('VITB12mcg', 'comments')] <- c(0, 'VITB12mcg value imputed using the water-balanced VITB12mcg value from KE18(7045)')

