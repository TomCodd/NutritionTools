## code to prepare `SOP_example_df` dataset goes here

# Completely fictitious dataset

SOP_example_df <- data.frame(food_code = c("F0001", "F0002", "F0003", "F0004", "F0005", "F0006", "F0007", "F0008", "F0009", "F0010"),
                             WATERg = c(10, 15, 20, 25, 30, 35, 40, NA, NA, NA),
                             PROCNTg = c(35, 20, 20, 20, 31, 50, 10, 27, NA, NA),
                             FAT_g_standardised = c(1, 2, NA, 7, 1, 3, 2, 6, NA, NA),
                             CHOAVLDFg_standardised = c(10, 1, 24, 50, 20, 10, 25, 32, NA, NA),
                             FIBTGg_standardised = c(12, 3, 8, 15, 6, 2, 9, 13, NA, NA),
                             ALCg = c(12, 43, 8, 15, 6, 2, 9, 13, NA, NA),
                             ASHg = c(12, 3, 28, 15, 6, 2, 9, 13, NA, NA),
                             comments = c("", "These are imaginary food items", NA, "With imaginary nutrient values", "", "And blanks", NA, "To test different outputs", "", "And scenarios"))


usethis::use_data(SOP_example_df, overwrite = TRUE)
