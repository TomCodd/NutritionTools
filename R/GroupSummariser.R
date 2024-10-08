#---
#Title: Group_Summariser
#Author: Thomas Codd - https://github.com/TomCodd
#Contributor: Lucia Segovia de la Revilla  - https://github.com/LuciaSegovia
#Version: V1.3.5
#Changelog:

#v1.3.4 -> v1.3.5; Feature added - can round summarised values to 2 significant figures
#v1.3.3 -> v1.3.4; Bug Fix - Fixed issue where a single NA in a column would result in NA in the summary row, by adding na_rm option
#v1.3.2 -> v1.3.3; Bug Fix - error where all rows had 'SUMMARY ROW - NA'; fixed
#v1.3.1 -> v1.3.2; Bug Fix - error where, after the first item, the Summary rows couldn't find the correct group ID and so outputted 'SUMMARY ROW - NA' fixed
#v1.3.0 -> v1.3.1; Bug Fix - error messages were only outputting the first group ID as the group with the issue, even if that group was fine. Fixed.
#v1.2.4 -> v1.3.0; New Feature - Added the ability to introduce leniency to the 'weightings must equal 1' rule when pre-existing weightings are used.
#v1.2.3 -> v1.2.4; Bug Fix - issue where Summary Row label appeared as "SUMMARY ROW - NA" fixed
#V1.2.2 -> V1.2.3; Bug Fix - Issue where weighting column for summary rows !=1 when pre-existing weighting columns aren't used fixed
#V1.2.1 -> V1.2.2; Bug Fix - issue where averages row didn't appear fixed. Issue where weighting column for summary rows !=1 fixed
#V1.2.0 -> V1.2.1; Bug Fix - tidying up unnecessary lines
#V1.1.1 -> V1.2.0; New Feature - Added the ability to set columns that should be empty in the average rows (for metadata etc)
#V1.1.0 -> V1.1.1; Bug Fix - Fixed NaN appearing in character columns, replaced with ""
#V1.0.0 -> V1.1.0; New Feature - Added the ability to take preset weights into account
#Github: https://github.com/TomCodd/NutritionTools
#---


#' Insert summary rows between groups of rows in a data frame
#'
#' @description This function analyses a data frame, sorting it based on the groups detailed in the group_ID_col,
#' and inserts summary/mean rows in between each group.
#'
#' For this to work the data frame must be structured such that it has a group ID column of some sort, where the group
#' of each item is listed. All the data columns that need to be averaged need to be numeric also.
#'
#' A secondary option is for the sorting of items within their groups, using the secondary_sort_col parameter.
#'
#' @param df Required - The data.frame that summary rows need to be inserted into.
#' @param group_ID_col Required - The column name specifying the groups that summary rows are created for.
#' @param secondary_sort_col Optional - Specify the column that the results should be sorted by after they're sorted into groups.
#' @param input_weighting_column Optional - Specify a column which contains set weightings. If selected, these weightings will be used in the summariser instead of a set average. Where partial weightings are given for an item, the remaining matches will have their weightings split evenly between them.
#' @param weighting_leniency Optional - default: \code{0} - Introduces some forgiveness in the 'group weightings must equal 1' rule. In some cases using existing weightings can lead to a total weighting value that is not equal to 1 (particularly if the weightings have been rounded in some way before using the Group Summariser). The inputted value is sets the range around 1 which the tool will accept - e.g. an input value of 0.03 will mean that the weighting total can be from 0.97 to 1.03.
#' @param blank_cols Optional -  Specify a lits of column names that you wish to leave blank on the average rows (e.g. metadata). Recommended to run the function once, see the results, and then check which columns you want to list here.
#' @param sep_row Optional - default: \code{'FALSE'} - if set to \code{TRUE}, The Summariser will insert an empty row after each summary row, to help reading and separation. The column names listed here must exactly match the columns you want excluded, in a character string; e.g. \code{c("FCT Food Item Code", "FCT Food Name")} for the columns \code{FCT Food Item Code} and \code{FCT Food Name}.
#' @param seq_col Optional - default: \code{'FALSE'} - if set to \code{TRUE}, The Summariser will insert a sequence column, numbering each item that goes into a summary row.
#' @param weighting_col Optional - default: \code{'FALSE'} - if set to \code{TRUE}, The Summariser will insert a weighting factor for each item that goes into a summary row.
#' @param round_weighting Optional - default: \code{'TRUE'} - If set to \code{TRUE}, The Summariser will round each weighted value to 2 decimal places.
#' @param round_averages Optional - default: \code{'FALSE'} - If set to \code{TRUE}, The Summariser will round each summarised average to 2 decimal places.
#' @param na_rm Optional - default: \code{'TRUE'} - If set to \code{TRUE}, The Summariser will round values even if an NA value is present. If set to \code{'FALSE'}, an NA value in the column will result in the Summary row for that column being NA as well.
#' @return A data.frame that mirrors \code{df}, but after each group a summary row is inserted, containing the mean of the data columns.
#'
#' @importFrom dplyr select_if
#' @importFrom stats weighted.mean
#'
#' @export


### TESTING BLOCK ----

# Useful for running datasets through block by block for error checking
#
# df = FAO_Table_recombined
# group_ID_col = "Grouping_FAO_Code"
# input_weighting_column = "Weighting factor"
# weighting_leniency = 0.001
# blank_cols = c("FAO Code", "CPC_Code")
# sep_row = TRUE
# round_averages = TRUE
# seq_col = FALSE
# weighting_col = FALSE
# round_weighting = TRUE
# na_rm = TRUE

### ----



Group_Summariser <- function(df,
                             group_ID_col,
                             secondary_sort_col,
                             input_weighting_column,
                             weighting_leniency = 0,
                             blank_cols = c(),
                             sep_row = FALSE,
                             seq_col = FALSE,
                             weighting_col = FALSE,
                             round_weighting = TRUE,
                             round_averages = FALSE,
                             na_rm = TRUE) {

  # Data input checking ----

  #These checks are run on the inputs to make sure the data frames are data frames, and that the string input is just a string, and the string inputs are a legitimate column name

  numeric_cols <- dplyr::select_if(df, is.numeric) #identifies the numeric columns

  stopifnot("df is not a data frame - please input a data frame" = is.data.frame(df)) #Checks to see if the df item is a data frame
  stopifnot("The group_ID_col is not a character or string - please input a character or string that is a column name in df, e.g. 'column one'" = is.character(group_ID_col)) #checks to see if the group_ID_col is a character string
  stopifnot("The group_ID_col is not a column name in df - please input a string that is a column name in df, e.g. 'column one'" = group_ID_col %in% colnames(df)) #Checks to see if the group_ID_col is in the list of column names for the df
  stopifnot("weighting_leniency is not a number. Please input a number between 0 and 1 for the weighting_leniency - e.g. weighting_leniency = 0.03" = is.numeric(weighting_leniency)) #checks that the weighting_leniency is numeric.
  stopifnot("weighting_leniency is not between 0 and 1. Please input a number between 0 and 1 for the weighting_leniency - e.g. weighting_leniency = 0.03" = 0 <= weighting_leniency) #checks that the weighting_leniency is greater or equal to 0.
  stopifnot("weighting_leniency is not between 0 and 1. Please input a number between 0 and 1 for the weighting_leniency - e.g. weighting_leniency = 0.03" = 1 > weighting_leniency) #checks that the weighting_leniency is less than 1.

  if(!missing(secondary_sort_col)){
    stopifnot("The secondary_sort_col is not a character or string - please input a character or string that is a column name in df, e.g. 'column one'" = is.character(secondary_sort_col)) #checks to see if the secondary_sort_col is a character string
    stopifnot("The secondary_sort_col is not a column name in df - please input a string that is a column name in df, e.g. 'column one'" = secondary_sort_col %in% colnames(df)) #Checks to see if the secondary_sort_col is in the list of column names for the df
  }

  if(!missing(input_weighting_column)){
    stopifnot("The input_weighting_column is not a character or string - please input a character or string that is a column name in df, e.g. 'column two - weightings'" = is.character(input_weighting_column)) #checks to see if the input_weighting_column is a character string
    stopifnot( "The input_weighting_column is not a column name in df - please input a string that is a column name in df, e.g. 'column two - weightings'" = input_weighting_column %in% colnames(df)) #Checks to see if the input_weighting_column is in the list of column names for the df
  }

  stopifnot("The sep_row input is not logical - please set it to TRUE or FALSE" = is.logical(sep_row)) #Checks to see if the sep_row input is logical
  stopifnot("The seq_col input is not logical - please set it to TRUE or FALSE" = is.logical(seq_col)) #Checks to see if the seq_col input is logical
  stopifnot("The weighting_col input is not logical - please set it to TRUE or FALSE" = is.logical(weighting_col)) #Checks to see if the weighting_col input is logical
  stopifnot("The round_weighting input is not logical - please set it to TRUE or FALSE" = is.logical(round_weighting)) #Checks to see if the round_weighting input is logical
  stopifnot("The round_averages input is not logical - please set it to TRUE or FALSE" = is.logical(round_weighting)) #Checks to see if the round_weighting input is logical
  stopifnot("The na_rm input is not logical - please set it to TRUE or FALSE" = is.logical(na_rm)) #Checks to see if the na_rm input is logical



  # Table sorting ----

  #This makes sure the group ID column is a character string, finds the unique ID's and sorts by them

  df[[group_ID_col]] <- as.character(df[[group_ID_col]]) #Ensures the group_ID_col column is a character column
  group_IDs <- df[[group_ID_col]] #Creates a character string list of group ID's
  group_ID_list <- sort(unique(group_IDs)) #Sorts the list, and pulls out the unique values (making sure there aren't duplicates)

  df <- as.data.frame(df)
  sorted_table <- df[df[[group_ID_col]] == group_ID_list[1],]   #This creates a new table, which will be the basis for the results table, using a subset of the main table only including the first group ID

  weighting_col_present <- F #Sets the weighting_col_present value to F

  if(!missing(input_weighting_column)){ #Checks if the input_weighting_column value is missing or not - if it is present then carries out the following actions
    weighting_col_present <- T #sets the weighting_col_present value to T
    weighting_col <- T #sets the weighting_col value to T
    weightings_column <- sorted_table[[input_weighting_column]] #identifies the weightings_column and creates a character list item from it
    weightings_column[weightings_column == ""] <- NA #replaces blank items in that list with NA
    number_of_NA <- sum(is.na(weightings_column)) #finds the number of NA values in the weightings-column
    sorted_weights <- sum(as.numeric(weightings_column), na.rm = TRUE) #adds up all the weights in that weightings column for this first group
    remaining_total <- 1-sorted_weights #finds the total share of the weightings left over - i.e. 1 minus the weightings it can see and are already set
    if(remaining_total < 0-weighting_leniency){ #checks to see if the remaining total is less than zero- weighting_leniency - i.e. the set weightings are greater than 1, even accounting for the weighting_leniency. Stops if this is the case.
      message(paste0("Error - weighting values for item ID ", group_ID_list[1], " are greater than 1 + weighting_leniency. Weighting cannot be completed."))
      stop()
    }
    weightings_column[is.na(weightings_column)] <- remaining_total/number_of_NA #sets the NA columns to be an equal proportion of the unaccounted for weighting remainder
    sorted_table[[input_weighting_column]] <- as.numeric(weightings_column) #reassigns the input_weighting_column in the sorted_table (the start of the results table) to be the same as the weightings_column numeric values, with the filled-in missing weightings.
    weighting_total <- sum(sorted_table[[input_weighting_column]]) #finds the weightings total
    if(weighting_total > 1+weighting_leniency | weighting_total < 1-weighting_leniency){ #checks if the weightings_total is close enough to 1 to count.
      message(paste0("Error - weighting values for item ID ", group_ID_list[1], " do not total 1. Weighting cannot be completed."))
      stop()
    }
  }

  if(!missing(secondary_sort_col)){ #if the secondary_sort_col is present, sorts the result by that (primary sort is the grouping ID)
    sorted_table <- sorted_table[order(sorted_table[, secondary_sort_col]),]
  }


  # Starting table setup ----

  #This creates a list for a new row, then iterates over the first subtable, generated averaged values where appropriate for the data columns.

  new_row <- c() #creation of the new row

  for (i in 1:ncol(sorted_table)){ #loops through all the columns in the starter table

    if(colnames(sorted_table)[i] %in% blank_cols){ #checks to see if the colname for the column the loop is on is among the blank_cols names
      new_row_entry <- "" #if it is, sets the new_row_entry value to blank
    } else { #if it isn't, then the following process occurs

      new_row_entry <- NA #set to NA (this is to cover a stopgap in case for whatever reason no value is assigned)
      unique_entries <- unique(sorted_table[[i]]) #checks to see if theres only 1 unique entry; first by finding all unique entries
      if(length(unique_entries) == 1){ #then by seeing if theres only 1. If there is, then
        if(!is.na(unique_entries)){ #If its not NA, then
          if(!missing(input_weighting_column)){
            if(colnames(sorted_table)[i] == input_weighting_column){ #and this column in the loop is the input weighting column
              new_row_entry <- 1 #then the total value is set to 1, which will be the total value of the weights, after the checks in the previous section
            } else {
              new_row_entry <- paste(unique_entries) #the unique entry is applied. If it is NA, then the stopgap NA value is used by virtue of not being replaced as its the default.
            }
          } else {
            new_row_entry <- paste(unique_entries) #the unique entry is applied. If it is NA, then the stopgap NA value is used by virtue of not being replaced as its the default.
          }
        }
      } else { #If there is more than one unique value the 'else' statement gets used, and the following happens
        if(!missing(input_weighting_column)){ #if there is an input weighting column
          if(colnames(sorted_table)[i] == input_weighting_column){ #and this column in the loop is the input weighting column
            new_row_entry <- 1 #then the total value is set to 1, which will be the total value of the weights, after the checks in the previous section
          } else {
            new_row_entry <- stats::weighted.mean(as.numeric(sorted_table[[i]]), sorted_table[[input_weighting_column]], na.rm = na_rm) #If the column isn't the input weighting column, then the weighted average value of all items in this column is returned.
          }
        } else {
          new_row_entry <- mean(as.numeric(sorted_table[[i]]), na.rm = TRUE) #If there isn't a weighting column set, then the mean value for all items in that column is used instead.
        }
      }
    }

    if(is.na(new_row_entry) | is.nan(new_row_entry)){ #If the new_row_entry is still NA after all these steps, it gets reset to be "", or blank.
      new_row_entry <- ""
    } else {
      if(is.numeric(new_row_entry)){
        if(round_averages == TRUE){
          new_row_entry <- round(new_row_entry, 2)
        }
      }
    }
    new_row <- append(new_row, new_row_entry) #Adds the new_row_entry to the list of new_row_entry's called new_row
  }

  group_col_num <- which(colnames(sorted_table) == group_ID_col) #Finds the group column number

  group_ID_value <- new_row[[group_col_num]] #assigns the group ID value based on the location of the group ID column through the group_col_num
  new_row[group_col_num] <- paste0("SUMMARY ROW - ", group_ID_value) #Edits the group_col_num to include that this is a summary row for that group
  sorted_table <- rbind(sorted_table, new_row) #attaches this modified averages row to the data rows in the sorted_table

  if(weighting_col == TRUE & weighting_col_present == FALSE){ #Special case for if a weighting column is asked for, but a pre-existing one isn't present
    if(round_weighting == TRUE){ #if rounded weighting values are used, then
      sorted_table$Weighting_Factor <- c(replicate((nrow(sorted_table) - 1), round((1/(nrow(sorted_table) - 1)), 2)), 1) #the weighting is 1 divided by the number of rows, rounded to 2 decimal places
    } else { #if rounded weighting values aren't used, then
      sorted_table$Weighting_Factor <- c(replicate((nrow(sorted_table) - 1), (1/(nrow(sorted_table) - 1))), 1) #the weighting values is 1/the number of rows, without any rounding
    }
  }

  if(seq_col == T){ #if the sequence column is requested, then
    sorted_table$Sequence <- c(1:(nrow(sorted_table) - 1), "") #the "Sequence" column is populated with the sequence number for the item in question (i.e. the subtable row number)
  }
  if(sep_row == T){ #If the seperator row column is requested, then
    sorted_table[nrow(sorted_table) + 1,] <- "" #an empty row is created at the bottom of the sub table
  }

  # Secondary tables setup ----

  #This repeats the process above, appending the results to the starter table.

  for (i in 2:length(group_ID_list)){ #runs through all the other group ID's (the first was used to create the starter table)

    secondary_table <- df[df[[group_ID_col]] == group_ID_list[i],] #subsets the main dataframe to a subtable for only that group

    if(!missing(input_weighting_column)){ #Checks if the input_weighting_column value is missing or not - if it is present then carries out the following actions
      weightings_column <- secondary_table[[input_weighting_column]] #identifies the weightings_column and creates a character list item from it
      weightings_column[weightings_column == ""] <- NA #replaces blank items in that list with NA
      sorted_weights <- sum(as.numeric(weightings_column), na.rm = TRUE) #adds up all the weights in that weightings column for this first group
      number_of_NA <- sum(is.na(weightings_column)) #finds the number of NA values in the weightings-column
      remaining_total <- 1-sorted_weights #finds the total share of the weightings left over - i.e. 1 minus the weightings it can see and are already set
      if(remaining_total < 0-weighting_leniency){ #checks to see if the remaining total is less than zero- weighting_leniency - i.e. the set weightings are greater than 1, even accounting for the weighting_leniency. Stops if this is the case.
        message(paste0("Error - weighting values for item ID ", group_ID_list[i], " are greater than 1 + weighting_leniency. Weighting cannot be completed."))
        stop()
      }
      weightings_column[is.na(weightings_column)] <- remaining_total/number_of_NA #sets the NA columns to be an equal proportion of the unaccounted for weighting remainder
      secondary_table[[input_weighting_column]] <- as.numeric(weightings_column) #reassigns the input_weighting_column in the secondary_table (the start of the results table) to be the same as the weightings_column numeric values, with the filled-in missing weightings.
      weighting_total <- sum(secondary_table[[input_weighting_column]]) #finds the weightings total
      if(weighting_total > 1+weighting_leniency | weighting_total < 1-weighting_leniency){ #checks if the weightings_total is close enough to 1 to count.
        message(paste0("Error - weighting values for item ID ", group_ID_list[i], " do not total 1. Weighting cannot be completed."))
        stop()
      }
    }

    if(!missing(secondary_sort_col)){ #if the secondary_sort_col is present, sorts the result by that (primary sort is the grouping ID)
      secondary_table <- secondary_table[order(secondary_table[secondary_sort_col]),]
    }


    new_row <- c()

    for (j in 1:ncol(secondary_table)){ #loops through all the columns in the secondary table

      if(colnames(secondary_table)[j] %in% blank_cols){ #checks to see if the colname for the column the loop is on is among the blank_cols names
        new_row_entry <- "" #if it is, sets the new_row_entry value to blank
      } else { #if it isn't, then the following process occurs

        new_row_entry <- NA #set to NA (this is to cover a stopgap in case for whatever reason no value is assigned)
        unique_entries <- unique(secondary_table[[j]]) #checks to see if theres only 1 unique entry; first by finding all unique entries
        if(length(unique_entries) == 1){ #then by seeing if theres only 1. If there is, then
          if(!is.na(unique_entries)){ #If its not NA, then
            if(!missing(input_weighting_column)){
              if(colnames(secondary_table)[j] == input_weighting_column){ #and this column in the loop is the input weighting column
                new_row_entry <- 1 #then the total value is set to 1, which will be the total value of the weights, after the checks in the previous section
              } else {
                new_row_entry <- paste(unique_entries) #the unique entry is applied. If it is NA, then the stopgap NA value is used by virtue of not being replaced as its the default.
              }
            } else {
              new_row_entry <- paste(unique_entries) #the unique entry is applied. If it is NA, then the stopgap NA value is used by virtue of not being replaced as its the default.
            }
          }
        } else { #If there is more than one unique value the 'else' statement gets used, and the following happens
          if(!missing(input_weighting_column)){ #if there is an input weighting column
            if(colnames(secondary_table)[j] == input_weighting_column){ #and this column in the loop is the input weighting column
              new_row_entry <- 1 #then the total value is set to 1, which will be the total value of the weights, after the checks in the previous section
            } else {
              new_row_entry <- stats::weighted.mean(as.numeric(secondary_table[[j]]), secondary_table[[input_weighting_column]], na.rm = na_rm) #If the column isn't the input weighting column, then the weighted average value of all items in this column is returned.
            }
          } else {
            new_row_entry <- mean(as.numeric(secondary_table[[j]]), na.rm = TRUE) #If there isn't a weighting column set, then the mean value for all items in that column is used instead.
          }
        }
      }

      if(is.na(new_row_entry) | is.nan(new_row_entry)){ #If the new_row_entry is still NA after all these steps, it gets reset to be "", or blank.
        new_row_entry <- ""
      } else {
        if(is.numeric(new_row_entry)){
          if(round_averages == TRUE){
            new_row_entry <- round(new_row_entry, 2)
          }
        }
      }
      new_row <- append(new_row, new_row_entry) #Adds the new_row_entry to the list of new_row_entry's called new_row
    }
    group_col_num <- which(colnames(secondary_table) == group_ID_col) #Finds the group column number
    group_ID_value <- new_row[group_col_num] #assigns the group ID value based on the location of the group ID column through the group_col_num
    new_row[group_col_num] <- paste0("SUMMARY ROW - ", group_ID_value) #Edits the group_col_num to include that this is a summary row for that group

    secondary_table <- rbind(secondary_table, new_row) #attaches this modified averages row to the data rows in the secondary_table


    if(weighting_col == T & weighting_col_present == F){ #Special case for if a weighting column is asked for, but a pre-existing one isn't present
      if(round_weighting == T){ #if rounded weighting values are used, then
        secondary_table$Weighting_Factor <- c(replicate((nrow(secondary_table) - 1), round((1/(nrow(secondary_table) - 1)), 2)), 1) #the weighting is 1 divided by the number of rows, rounded to 2 decimal places
      } else if(round_weighting == F){ #if rounded weighting values aren't used, then
        secondary_table$Weighting_Factor <- c(replicate((nrow(secondary_table) - 1), (1/(nrow(secondary_table) - 1))), 1) #the weighting values is 1/the number of rows, without any rounding
      }
    }

    if(seq_col == T){ #if the sequence column is requested, then
      secondary_table$Sequence <- c(1:(nrow(secondary_table) - 1), "") #the "Sequence" column is populated with the sequence number for the item in question (i.e. the subtable row number)
    }

    sorted_table <- rbind(sorted_table, secondary_table) #The sorted table and secondary table are then merged

    if(sep_row == T){ #If the seperator row column is requested, then
      sorted_table[nrow(sorted_table) + 1,] <- "" #an empty row is created at the bottom of the sorted table
    }

  }

  return(sorted_table) #Once the loop is finished, the sorted table contains all the subtables and average rows for every group ID, and is returned as the dataframe output

}
