## code to prepare `breakfast_df_nonstandard` dataset goes here

# Completely fictitious dataset

breakfast_df_nonstandard <- data.frame(food_code = c("F0001", "F0002", "F0003", "F0004", "F0005", "F0006", "F0007", "F0008", "F0009", "F0010"),
                                       food_name = c("Bacon", "Beans", "Toast", "Mushroom", "Eggs", "Tomato", "Sausage", "Butter", "Brown Sauce", "Tomato Ketchup"),
                                       Water_values_g = c(10, 15, 20, 25, 30, 35, 40, NA, NA, NA),
                                       PROCNT_values_g = c(15, 10, 20, 15, 21, 28, 10, 27, NA, NA),
                                       FAT_values_g_combined = c(21, 12, NA, 16, 11, 33, 13, 16, NA, NA),
                                       CHOAVL_values_g = c(10, 1, 24, 46, 20, 2, 24, 22, NA, NA),
                                       FIBTG_values_g_combined = c(12, 3, 8, 15, 6, 2, 9, 13, NA, NA),
                                       ALC_values_g = c(12, 43, 8, 15, 6, 2, 9, 13, NA, NA),
                                       ASH_values_g = c(12, 3, 28, 15, 6, 2, 9, 13, NA, NA),
                                       Thiamine_milligrams = c(32, 90, 130, 21, NA, 61, 21, "", 59, NA),
                                       Thiamine_from_HCL_milligrams = c(21, 45, 20, 69, 42, 150, 23, 30, 52, NA),
                                       Retinol_micrograms = c(53, 12, 20, NA, 62, 40, 140, 210, 41, NA),
                                       Beta_Carotene_Equivalents_micrograms = c(NA, 51, 91, 22, 62, 102, 32, 72, 112, NA),
                                       Niacin_milligrams = c(172, NA, 24.4, NA, 8.1, 134.6, 10.2, 187.9, 92, NA),
                                       Tryptophan_milligrams = c(126, 142.5, 142.7, 167, NA, 76.3, 98.6, 41.4, 172, NA),
                                       Niacin_eq_milligrams = c(85.3, 49.2, 86.4, 23.2, 30.5, 83.3, 16.6, 84.5, 17.7, NA),
                                       Niacine_from_TRP_mg = c(2.10, NA, 2.38,2.80, NA, 1.27, 1.64, 0.69, 2.87, NA),
                                       FAT_in_g = c(21, NA, NA, NA, 11, NA, 13, 16, NA, NA),
                                       FAT_unknown_calc_g = c(20.9, 12.0, NA, NA, 10.9, 33, 12.1, 16.1, NA, NA),
                                       FAT_continuous_extraction_g = c(NA, NA, NA, 16, NA, 33, NA, 15.9, NA, NA),
                                       comments_column = c("", "These are imaginary food items", NA, "With imaginary nutrient values", "", "And blanks", NA, "To test different outputs", "", "And scenarios"))



usethis::use_data(breakfast_df_nonstandard, overwrite = TRUE)
