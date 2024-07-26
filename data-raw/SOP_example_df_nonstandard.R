## code to prepare `SOP_example_df_nonstandard` dataset goes here

# Completely fictitious dataset

SOP_example_df_nonstandard <- data.frame(food_code = c("F0001", "F0002", "F0003", "F0004", "F0005", "F0006", "F0007", "F0008", "F0009", "F0010"),
                                         Water_values_g = c(10, 15, 20, 25, 30, 35, 40, NA, NA, NA),
                                         PROCNT_values_g = c(35, 20, 20, 20, 31, 50, 10, 27, NA, NA),
                                         FAT_values_g_standardised = c(1, 2, NA, 7, 1, 3, 2, 6, NA, NA),
                                         CHOAVLDF_values_g_standardised = c(10, 1, 24, 50, 20, 10, 25, 32, NA, NA),
                                         FIBTG_values_g_standardised = c(12, 3, 8, 15, 6, 2, 9, 13, NA, NA),
                                         ALC_values_g = c(12, 43, 8, 15, 6, 2, 9, 13, NA, NA),
                                         ASH_values_g = c(12, 3, 28, 15, 6, 2, 9, 13, NA, NA),
                                         comments_column = c("", "These are imaginary food items", NA, "With imaginary nutrient values", "", "And blanks", NA, "To test different outputs", "", "And scenarios"))


usethis::use_data(SOP_example_df_nonstandard, overwrite = TRUE)
