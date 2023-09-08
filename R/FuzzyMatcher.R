#---
#Title: Fuzzy_Matcher
#Author: Thomas Codd - https://github.com/TomCodd
#Contributor: Lucia Segovia de la Revilla  - https://github.com/LuciaSegovia
#Version: 1.0.2
#Changelog: V1.0.1 -> V1.0.2; Applied extra conversions (encoding to latin1) on input to prevent function crashes, and fixed csv save name (removed invalid characters)
#Changelog: V1.0.0 -> V1.0.1; Sped up the process considerably by removing some conditional formatting on output table
#Github: https://github.com/TomCodd/NutritionTools
#---


#' A GUI interface to match rows in two dataframes to each other via a fuzzy
#' string search.
#'
#' @description This function reads in two dataframes, both comprised of an ID
#' row and a name row. The name rows are matched based on fuzzy search
#' suggestions and human confirmation using the GUI interface.
#' @param df1 Required - The primary dataframe, with items that need matches.
#'   The dataframe must be two columns in size; the first column must be the ID
#'   column, the second must be the item names.
#' @param df2 Required - The secondary dataframe, with a list of potential items
#'   to match the contents of df1 against. The dataframe must be two columns in
#'   size; the first column must be the ID column, the second must be the item
#'   names.
#' @param focus_term Optional - Specify a string. If the string is contained
#' in the item name, then the fuzzy matcher opens a wider potential list of
#' matches to that item.
#' @return An R object of csv that contains items from \code{df1} and their
#' counterparts from \code{df2} in the same row.
#'
#' @import rhandsontable
#' @import shiny
#' @importFrom fuzzyjoin stringdist_join
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom stringr str_c
#' @importFrom tibble rowid_to_column
#'
#' @export
#'
#' @details Because of the GUI nature of the Fuzzy Matcher, an R-based
#'   description or example is not able to help guide using this function as
#'   it might with a more standard R function. Because of this a quick guide has
#'   been created which I encourage you to examine, which can be found at
#'   https://tomcodd.github.io/NutritionTools/articles/Fuzzy-Matcher-Guide.html






Fuzzy_Matcher <- function(df1, df2, focus_term){ #Focus term is a string that
  #makes the filtering more lenient - use to catch more items with this term in
  #them; for example "raw" when looking at food items would prioritise raw foods.

  # Data input checking ----

  #These checks are run on the inputs to make sure the data frames are data frames and the correct length, and that the string input is just a string

  stopifnot("df1 is not a data frame - please input a data frame consisting of an id/code column and an item name column." = is.data.frame(df1))
  stopifnot("df2 is not a data frame - please input a data frame consisting of an id/code column and an item name column." = is.data.frame(df2))
  stopifnot("df1 is too long - please make sure the input dataframes are two columns in length." = (length(df1) == 2))
  stopifnot("df2 is too long - please make sure the input dataframes are two columns in length." = (length(df2) == 2))

  if(!missing(focus_term)){
    stopifnot("The focus term is not a character or string - please input a character or string, e.g. 'raw'" = is.character(focus_term))
  }



  # Data pre-processing ----

  #Starting checks - the timer is started, dataframe metadata is gathered, and columns are renamed
  #Also column name creation, as some quirk means it doesn't work when its wanted later on

  start_time <- Sys.time() #Start time for the timer is set

  new_column_names <- c("ID", "Pseudo ID", #Creates a list of new column names to be applied to the table
                        paste0(deparse(substitute(df1)), "_code"),
                        paste0(deparse(substitute(df1)), "_name"),
                        paste0(deparse(substitute(df2)), "_code"),
                        paste0(deparse(substitute(df2)), "_name"),
                        "Correct Match", "Confidence")

  df1_item_number <- nrow(df1) #number of rows from the primary df is found

  df1_names <- colnames(df1) #original column names are taken for preservation
  df2_names <- colnames(df2)

  colnames(df1)[1] <- "item_code" #column names are reset - only columns with the same name can be matched with stringdist_join
  colnames(df2)[1] <- "item_code"
  colnames(df1)[2] <- "item_name"
  colnames(df2)[2] <- "item_name"

  #Encoding is reqired - if not in latin1 then stringdist_join either runs endlessly or causes errors enough to abort the R session
  Encoding(df1$item_name) <- "latin1"
  Encoding(df2$item_name) <- "latin1"


  # Fuzzy Matching ----

  #This is the actual fuzzy matching, where the closest entries are found for each entry

  fuzzy_output <- fuzzyjoin::stringdist_join(df1, df2, #This selects the two lists to check matches against
                                  by = "item_name", #This allows you to select the field by which the search will be done
                                  mode = "left",
                                  method = "jw", #the fuzzy search method - more info here, need to do some research - https://www.rdocumentation.org/packages/stringdist/versions/0.9.8/topics/stringdist-metrics
                                  ignore_case = TRUE,
                                  max_dist = 0.35, #The maximum distance between the two strings - I believe this varies dependent on the method
                                  distance_col = "dist") #This lists the distances and sets the column name they should be listed under - a perfect match should be 0


  # Fuzzy Results processing ----

  #Results are grouped and sorted

  fuzzy_output_selection <- fuzzy_output |>
    dplyr::group_by(item_name.x) |> #output formatting - this makes it so that the output is organised by the item_name.x, (x being the first list item at the start of the tool)
    dplyr::slice_min(dist, n = 5) #This means only the closest 5 matches are listed per item on the second dataframe

  if(!missing(focus_term)){
    fuzzy_output_selection <- fuzzy_output_selection |>
      dplyr::filter(grepl(focus_term, item_name.x) | dist<=0.225) #This introduces a filter. By combining this with the max_dist in the fuzzy search,  the end
  }# result is that any items with the focus term in their name are listed if their distance is under 0.3,
  # along with anything with a distance of 0.225 or lower. This makes the distance more forgiving for items with that focus term in them.


  # Prep work for match confirmations ----

  #Tables are set up and sorted for the Shiny interface and functionality

  fuzzy_output_selection$cor_match <- FALSE #Correct Match column is created and populated with False

  fuzzy_output_selection$Item_min_dist <- "" #Item minimum distance column is created
  unique_entries <- unique(fuzzy_output_selection$item_code.x) #unique entries are listed for items from df1

  for (i in 1:length(unique_entries)){ #This for loop finds all potential matches for a particular item, finds the closest one, and populates the item_min_dist column for all of those items with that minimum for sorting
    i_subsection <- fuzzy_output_selection |>
      dplyr::filter(item_code.x == unique_entries[i])
    i_min <- min(i_subsection$dist)
    fuzzy_output_selection <- fuzzy_output_selection |> dplyr::mutate(Item_min_dist = replace(Item_min_dist, item_code.x == unique_entries[i], i_min))
  }

  fuzzy_output_selection <- fuzzy_output_selection[order(fuzzy_output_selection$Item_min_dist),] #This sorts items based on the item min dist

  fuzzy_output_selection <- tibble::rowid_to_column(fuzzy_output_selection, "ID") #This creates a column with the current row number as the value
  fuzzy_output_selection$Pseudo_ID <- fuzzy_output_selection$ID #This duplicates this value into a Pseudo_ID column
  fuzzy_output_selection$Confidence <- "" #This creates the Confidence column
  fuzzy_output_selection <- fuzzy_output_selection |> #This moves columns around for ease of reading in the output table
    dplyr::relocate(Pseudo_ID, .after = ID) |>
    dplyr::relocate(Confidence, .after = cor_match)
  fuzzy_output_selection <- fuzzy_output_selection[,-c(7,10)] #This removes certain rows no longer needed - dist and item_min_dist

  colnames(fuzzy_output_selection) <- new_column_names




  # RShiny - Match confirmation ----

  DF <- fuzzy_output_selection

  ui <- (shiny::fluidPage( #this outlines the main page design
    shiny::fluidRow(
      shiny::column(12,
                    shiny::h1("food item potential matches", align = "center"))),
    shiny::fluidRow(
      shiny::column(12,
             shiny::actionButton("saveBtn", "All matches identified"))),
    shiny::fluidRow(
      shiny::column(12,
             shiny::br())),
    shiny::fluidRow(
      shiny::column(12,
             rhandsontable::rHandsontableOutput("table", height = "500px"))),
  )
  )

  server <- (function(input, output, session){

    values <- shiny::reactiveValues(data = DF) #Imports the data as reactiveValues

    shiny::observeEvent(input$table,{
      input_table <- as.data.frame(rhandsontable::hot_to_r(input$table)) #Makes the table "hot" - i.e. interact-able with rhandsontable

      matched_df2_codes <- input_table[,3][input_table[,7] == TRUE] #Creates a list of list A codes that have been successfully matched
      matched_df1_codes <- input_table[,5][input_table[,7] == TRUE] #creates a list of matched df1 codes
      incorrect_matched_codes <- input_table[,1][input_table[,3] %in% matched_df2_codes & input_table[,7] == FALSE | input_table[,5] %in% matched_df1_codes & input_table[,7] == FALSE] #creates the list of codes that are incorrect matches
      input_table[,2] <- input_table[,1] #resets pseudo_ID to ID
      input_table[,2][which(input_table[,1] %in% incorrect_matched_codes)]<-NA #Sets PseudoID to NA if the row contains an incorrect match
      input_table<-input_table[order(input_table[,2], na.last=TRUE),] #resorts the table based on pseudotable, putting NA matches at the bottom
      values$data<-input_table #Resets the data values to match the edited table

    })

    output$table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(values$data)|> #outputs the data table
        rhandsontable::hot_col(1:6, readOnly = TRUE) |> #Outputs the table, and makes it so that only the True/False column is editable
        rhandsontable::hot_col(1:2, width = 0.5) |> #sets the ID and PseudoID columns to be very narrow, so they don't appear visible
        rhandsontable::hot_col(col="Confidence", type = "dropdown", source = c("","high", "medium", "low")) |> #Creates the confidence dropdown for that column

        #These renderers colour the incorrect matches pink, and make them uneditable - different renderers for the different type of columns
        rhandsontable::hot_col(7, renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
             var ID = instance.getData()[row][0]
             var pseudoID = instance.getData()[row][1]
             if (ID !== pseudoID) {
              td.style.background = 'pink';
              cellProperties.readOnly = true;
             }
           }")
    })

    shiny::observeEvent(input$saveBtn, { #Controls what happens when the save button is pressed
      input_table<-as.data.frame(rhandsontable::hot_to_r(input$table)) #Makes the table "hot" - i.e. interact-able with rhandsontable

      # This next bit of code is an attempt to stop the skipping of confidence values when forgotten occasionally - essentially refreshes the input table before saving.
      matched_df2_codes <- input_table[,3][input_table[,7] == TRUE] #Creates a list of list A codes that have been successfully matched
      matched_df1_codes <- input_table[,5][input_table[,7] == TRUE] #creates a list of matched df1 codes
      incorrect_matched_codes <- input_table[,1][input_table[,3] %in% matched_df2_codes & input_table[,7] == FALSE | input_table[,5] %in% matched_df1_codes & input_table[,7] == FALSE] #creates the list of codes that are incorrect matches
      input_table[,2] <- input_table[,1] #resets pseudo_ID to ID
      input_table[,2][which(input_table[,1] %in% incorrect_matched_codes)]<-NA #Sets PseudoID to NA if the row contains an incorrect match
      input_table<-input_table[order(input_table[,2], na.last=TRUE),] #re-sorts the table based on pseudotable, putting NA matches at the bottom
      values$data<-input_table
      # End of refreshing input table.

      output_table <- as.data.frame(rhandsontable::hot_to_r(input$table)) #Creates an output table from the current data table
      matches <- output_table[,1][output_table[,7] == TRUE] #Creates a list of the match row ID's
      true_matches <- output_table|>
        dplyr::filter (ID %in% matches) #Creates a subset for those row ID's
      percent_completed <- round((nrow(true_matches)/df1_item_number), digits = 2)*100 #matching metadata is added - how many rows from df1 were matched
      true_matches_without_confidence <- true_matches |> #Checks all matches have a confidence value
        dplyr::filter (Confidence == "")
      match_IDs_without_confidence <- true_matches_without_confidence$ID
      output_matches <- true_matches[-c(1,2,7)]
      if (nrow(true_matches_without_confidence)>0){ #If true matches don't have confidence values, this Modal flags this for attention and fixing
        shiny::showModal(shiny::modalDialog(
          title = "Please select confidence values for these rows:",
          stringr::str_c(match_IDs_without_confidence, collapse = ", "),
          easyClose = TRUE
        ))
      } else { #Otherwise, the save options Modal appears
        shiny::showModal(shiny::modalDialog(
          title = "Please select save options",
          checkboxInput("orig_col_names", "Would you like to prepare the output for merging with original datasets (strip to ID columns and use original column names)?", value = T,  width = NULL),
          shiny::radioButtons("outputoption", shiny::h4("Output options"),
                       choices = list("R dataframe" = 1, "CSV file" = 2)),
          shiny::textInput("FileName", "Choose file/dataframe name",
                    value = paste0("fuzzy_match_outputs_", Sys.time())),
          footer = shiny::actionButton("outputcontinue", "Continue")
        ))
      }

      shiny::observeEvent(input$outputcontinue, { #Once the save buttons are selected and confirmed, the summary screen appears
        end_time <- Sys.time()
        time_taken <- round((end_time-start_time), digits = 2)
        if(input$orig_col_names == T){
          output_matches <- output_matches[,c(1,3,5)]
          colnames(output_matches) <- c(df1_names[1], df2_names[1], "Confidence")
        }
        if (input$outputoption == 1){
          assign(paste0(input$FileName), output_matches, envir = .GlobalEnv)
          shiny::showModal(shiny::modalDialog(
            title = stringr::str_c("You have matched ", nrow(true_matches), " items!"),
            stringr::str_c("Thats ", percent_completed, "% of the dataframe (", df1_item_number, " items), and took ",  time_taken, " ", units(time_taken)),
            footer = shiny::actionButton("closeButton", "Close tool"),
            easyClose = TRUE
          ))
        } else { #With different outcomes if the R object or CSV outputs have been selected.
          savefilename <-
          utils::write.csv(output_matches, file = paste0(gsub("[^[:alnum:]\\-\\_]", "", input$FileName, perl = T), ".csv"), row.names = FALSE)
          shiny::showModal(shiny::modalDialog(
            title = stringr::str_c("You have matched ", nrow(true_matches), " items!"),
            stringr::str_c("Thats ", percent_completed, "% of the dataframe (", df1_item_number, " items), and took ",  time_taken, " ", units(time_taken)),
            footer = shiny::actionButton("closeButton", "Close tool"),
            easyClose = TRUE
          ))
        }
      })
    })
    shiny::observeEvent(input$closeButton, { #Controls the closing of the app
      shiny::stopApp()
    })
  })

  shiny::shinyApp(ui, server)
}
