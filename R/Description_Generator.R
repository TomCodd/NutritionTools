#---
#Title: Description_Generator
#Author: Thomas Codd - https://github.com/TomCodd
#Version: V1.0.0
#Changelog:

#Github: https://github.com/TomCodd/NutritionTools
#---

#' @title Description Generator
#' @description This is a function designed to speed up the writing of function
#'   documentation, and make a regular 'style' of function easier to repeat.
#' @param R_function_file Required - the location of the R file which contains
#'   the function you wish to generate the Description and Documentation for.
#'   Only the first function in the file will have its description generated.
#' @param boilerplate Optional - default: \code{c("#---\n", paste0("#Title: ",
#'   Function_Name, "\n"), "#Author: Thomas Codd -
#'   https://github.com/TomCodd\n", "#Contributor: Lucia Segovia de la Revilla
#'   - https://github.com/LuciaSegovia\n", "#Version: V1.0.0\n",
#'   "#Changelog:\n", "\n", "#Github:
#'   https://github.com/TomCodd/NutritionTools\n", "#---\n", "\n")} - Please
#'   enter any message or further information you would like to include at the
#'   top of the function.
#' @return A pre-formatted set of lines of code, ready to be copied and pasted
#'   above the function in the function file specified in
#'   \code{R_function_file}.
#' @export

Description_Generator <- function(R_function_file,
                                  boilerplate = c(
                                    "#---\n",
                                    paste0("#Title: ", Function_Name, "\n"),
                                    "#Author: Thomas Codd - https://github.com/TomCodd\n",
                                    "#Contributor: Lucia Segovia de la Revilla  - https://github.com/LuciaSegovia\n",
                                    "#Version: V1.0.0\n",
                                    "#Changelog:\n",
                                    "\n",
                                    "#Github: https://github.com/TomCodd/NutritionTools\n",
                                    "#---\n",
                                    "\n")) {

  R_function_file_read <- read.delim2(R_function_file, quote = "", header = FALSE)

  Function_Name <- R_function_file_read[grepl(" <- function\\(", R_function_file_read[[1]]),][1]
  Function_Name <- trimws(gsub("\\<.*", "", Function_Name, perl = TRUE)) #Removes everything before the first <
  Function_Name_spaces <- gsub("_", " ", Function_Name, perl = TRUE) #Replaces Underscores with a space

  # Looking for " <- function(" as this is the start

  first_row <- which(R_function_file_read == R_function_file_read[grepl(" <- function\\(", R_function_file_read[[1]]),][1])


  # Now looking for first {

  rows_with_open_curly <- which(grepl("\\{", R_function_file_read[[1]]))

  last_row <- rows_with_open_curly[1]

  # So, we have the first and last rows
  # Now we can remove the rest, and sort out formatting

  R_function_file_cleaned <- paste0(R_function_file_read[c(first_row:last_row),], collapse = "")

  R_function_file_cleaned <- gsub("\\)[^\\)]+$", "", R_function_file_cleaned, perl = TRUE) #Removes everything after the last )
  R_function_file_cleaned <- gsub("^[^\\(]+\\(", "", R_function_file_cleaned, perl = TRUE) #Removes everything before the first (

  split_inputs <- trimws(unlist(strsplit(R_function_file_cleaned, ","))) #separates inputs out

  #identifies wrapped inputs, where comma's should be ignored
  open_bracket_split_inputs <- which(grepl("\\(", split_inputs)) #identifies open brackets
  closed_bracket_split_inputs <- which(grepl("\\)", split_inputs)) #identifies closed brackets

  if(length(open_bracket_split_inputs) != length(closed_bracket_split_inputs)){ #Error message if brackets are uneven
    message("ERROR: function inputs contain unmatched opening and closing brackets. Description_Generator cannot proceed. Please correct this issue and try again.")
    return()
  }

  if(length(open_bracket_split_inputs) > 0){

    for(i in 1:length(closed_bracket_split_inputs)){ #loops through closed brackets
      closed_bracket_of_interest <- closed_bracket_split_inputs[i]
      matched_opening_bracket <- max(open_bracket_split_inputs[open_bracket_split_inputs < closed_bracket_of_interest])

      #Need to remove the opening bracket now its been accounted for
      open_bracket_split_inputs <- open_bracket_split_inputs[open_bracket_split_inputs != matched_opening_bracket]

      change_in_length <- closed_bracket_of_interest - matched_opening_bracket

      if(closed_bracket_of_interest == length(split_inputs)){
        split_inputs <- c(split_inputs[c(1:matched_opening_bracket-1)], paste0(split_inputs[c(matched_opening_bracket:closed_bracket_of_interest)], collapse = ", "))
      } else {
        split_inputs <- c(split_inputs[c(1:matched_opening_bracket-1)], paste0(split_inputs[c(matched_opening_bracket:closed_bracket_of_interest)], collapse = ", "), split_inputs[c((closed_bracket_of_interest+1):length(split_inputs))])
      }


      #Now to adjust for the change in length
      open_bracket_split_inputs[open_bracket_split_inputs > closed_bracket_of_interest] <- open_bracket_split_inputs[open_bracket_split_inputs > closed_bracket_of_interest] - change_in_length
      closed_bracket_split_inputs[closed_bracket_split_inputs > closed_bracket_of_interest] <- closed_bracket_split_inputs[closed_bracket_split_inputs > closed_bracket_of_interest] - change_in_length
    }

  }


  output_item <- c( #Puts in the boilerplate of the function
    boilerplate,
    paste0("#' @title ", Function_Name_spaces, "\n"),
    "#' @description [Function_Description]\n"
  )

  for(i in 1:length(split_inputs)){ #Goes through each input

    parameter_name <- split_inputs[i] #identifies the individual parameters

    if(grepl("=", parameter_name)){ #Sees if theres a preset, reacts accordingly
      split_parameter <- trimws(unlist(strsplit(parameter_name, "=")))
      split_parameter_name <- split_parameter[1]
      split_parameter_preset <- split_parameter[2]
      if(split_parameter_preset %in% c("TRUE", "FALSE")){ #If true or false goes through special process
        output_item <- c(
          output_item,
          paste0(
            "#' @param ",
            split_parameter_name,
            " Required - default: \\code{", # four backslashes produces two backslashes in result, we want 1
            split_parameter_preset,
            "} - Either \\code{TRUE} or \\code{FALSE}. param_details\n"
          )
        )
      } else { #if not true or false doesn't need the true or false bits
        output_item <- c(
          output_item,
          paste0(
            "#' @param ",
            split_parameter_name,
            " Required/Optional - default: \\code{",
            split_parameter_preset,
            "} - param_details\n"
          )
        )
      }
    } else { #if no preset, its simple
      output_item <- c(output_item, paste0("#' @param ", parameter_name, " Required/Optional - param_details\n"))
    }
  }

  # Once done, need to do the final detail bit
  output_item <- c(output_item, "#' @return return_info\n", "#' @export\n", "#' @importFrom import_details_here\n", "#' \n", "#' @examples\n")

  # Prints it out for copying and pasting above the function
  cat(output_item)
}
