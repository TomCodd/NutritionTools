% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DecimalSystemCheck.R
\name{Decimal_System_Checker}
\alias{Decimal_System_Checker}
\title{A tool to test the integrity of a decimal system in a dataframe}
\usage{
Decimal_System_Checker(df, first, second, third, fourth)
}
\arguments{
\item{df}{Required - The data frame containing the decimal system.}

\item{first}{Required - The first column of the decimal system - the most
basic item ID; e.g. 01 .}

\item{second}{Required - The second column of the decimal system - the first
subdivision from the base ID; e.g. 01.005 .}

\item{third}{Optional - The third column of the decimal system - the second
subdivision from the base ID and the first from the second ID; e.g.
01.005.03 .}

\item{fourth}{Optional - The fourth column of the decimal system - the third
subdivision from the base ID, second subdivision from the second ID, and
first subdivision from the third ID; e.g. 01.005.03.01 .}
}
\value{
An R dataframe detailing the errors found for each item in the
  decimal system.
}
\description{
This function reads in a dataframe, as well as the names of 2-4
  columns which comprise the decimal system within that dataframe. It then
  checks the integrity of each series of decimal identities in each row
  against the rest of the decimal identities within that row, picking up any
  inconsistencies. Any inconsistencies are reported, eith in console messages
  or in a error report dataframe.
}
\examples{
#Two examples will be covered - one that results in the output error table,
#another that produces the output messages only (not recommended for large
#dataframes).

#First, we must create a test dataframe:
test_df <- data.frame( c("Merlot", "pinot grigio", "Chateauneuf-du-Pape",
"Tokaji", "Champagne", "Sauvignon Blanc", "Chardonnay", "Malbec"), c("01",
"01", "01", "01", "02", "02", "02", "02"), c("02.01", "01.01", "01.02",
"01.02", "02.01", "02.01", "02.02", "02.02"), c("02.01.0111", "01.01.0131",
"01.02.0001", "01.02.2031", "02.01.1001", "02.01.1001", "02.02.3443",
"02.03.4341"), c("02.01.0111.01", "01.01.0131.04", "01.02.0001.01",
"01.02.2031.03", "02.01.1001.06", "02.01.1001.06", "02.01.3443.02",
"02.02.4341.03") )

  #Then we should rename the columns of the dataframe:

  colnames(test_df) <-
    c("Wine names",
      "ID1",
      "ID2",
      "ID3",
      "ID4"
    )

 #This first line runs the dataframe, and has an output variable listed. This
 #means that as well as putting a message in the console when an error is
 #found, all the error reports will be saved to a dataframe too.

 output_test <- Decimal_System_Checker(test_df, first = "ID1", second =
 "ID2", third = "ID3", fourth = "ID4")

 #However, if we only want to get the readouts and not have an error
 #dataframe to refer back to, then the code can be run like so:

 Decimal_System_Checker(test_df, first = "ID1", second = "ID2", third =
 "ID3", fourth = "ID4")

 #This will do the same thing as the previous run, producing error printouts,
 #but it will not create an error report dataframe.

}
