#---
#Title: Table_Creator
#Author: Thomas Codd - https://github.com/TomCodd
#Version: V1.0.0
#Changelog:

#Github: https://github.com/TomCodd/NutritionTools
#---

#' @title Table Creator
#' @description This function generates the code needed to create a pre-existing
#'   data.frame from scratch.
#' @param df Required - The data.frame you are looking to recreate.
#' @param for_R_package Required - default: \code{TRUE} - Either \code{TRUE} or
#'   \code{FALSE}. If set to \code{TRUE}, the output will include the
#'   description framework for the data.frame, to be inserted into the data.R
#'   file of the package.
#' @return The code required to recreate a pre-existing table, and (optionally)
#'   the description framework of the data.frame to be inserted into a packages
#'   data.R file.
#' @export


Table_Creator <- function(df, for_R_package = TRUE){

  # Checks if input is a data.frame
  if(!is.data.frame(df)){
    message("Error: df is not a data.frame. Please input a data.frame.")
    return()
  }

  # Finds the name of the data.frame inputted
  df_name <- deparse(substitute(df))

  # First, the table code ----

  # Finds column names
  column_names <- colnames(df)

  #Creates empty list
  contents_part <- c()

  #Loops through each column, reading and printing them, with the correct formatting inserted throughout
  for(i in 1:ncol(df)){
    if(isTRUE(is.numeric(df[[i]]))){
      contents_part <- c(contents_part, paste0(column_names[i], " = c(", paste0(df[[i]], collapse = ", "), ")"))
    } else {
      contents_part <- c(contents_part, paste0(column_names[i], ' = c("', paste0(df[[i]], collapse = '", "'), '")'))
    }
    contents_part <- paste0(contents_part, collapse = ",\n")
    contents_part <- gsub("'NA'", "NA", contents_part)
  }
  output <- paste0(df_name, " <- data.frame(", contents_part, ")" #Inserts contents in the correct place
  )

  # Then, the code to add to data.R ----
  # if the code is for a package.
  if(isTRUE(for_R_package)){

    output <- paste0(output, #This gets placed in the data.R file within a package.
                     "\n\n\n\n Please copy the section below into the data.R file of the package.\n\n\n\n",
                     "#' ", df_name," \n#'\n",
                     "#' [df description]\n#'\n",
                     "#' @format A data frame with ", nrow(df), " rows and ", ncol(df), " columns:\n",
                     "#' \\describe{\n",
                     "#'   \\item{",
                     paste0(colnames(df), collapse = "}{Column_Description}\n#'   \\item{"),
                     "}{Column_Description}\n#' }\n#' @source [Description_Of_Source]\n",
                     "'", df_name, "'")
  }
  cat(output)
}
