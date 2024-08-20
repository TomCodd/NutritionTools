## code to prepare `breakfast_df` dataset goes here

# Completely fictitious dataset

breakfast_df <- data.frame(food_code = c("F0001", "F0002", "F0003", "F0004", "F0005", "F0006", "F0007", "F0008", "F0009", "F0010"),
                           food_name = c("Bacon", "Beans", "Toast", "Mushroom", "Eggs", "Tomato", "Sausage", "Butter", "Brown Sauce", "Tomato Ketchup"),
                           WATERg = c(10, 15, 20, 25, 30, 35, 40, NA, NA, NA),
                           PROCNTg = c(15, 10, 20, 15, 21, 28, 10, 27, NA, NA),
                           FAT_g_combined = c(21, 12, NA, 16, 11, 33, 13, 16, NA, NA),
                           CHOAVLg = c(10, 1, 24, 46, 20, 2, 24, 22, NA, NA),
                           FIBTGg_combined = c(12, 3, 8, 15, 6, 2, 9, 13, NA, NA),
                           ALCg = c(12, 43, 8, 15, 6, 2, 9, 13, NA, NA),
                           ASHg = c(12, 3, 28, 15, 6, 2, 9, 13, NA, NA),
                           THIAmg = c(32, 90, 130, 21, NA, 61, 21, "", 59, NA),
                           THIAHCLmg = c(21, 45, 20, 69, 42, 150, 23, 30, 52, NA),
                           RETOLmcg = c(53, 12, 20, NA, 62, 40, 140, 210, 41, NA),
                           CARTBEQmcg_combined = c(NA, 51, 91, 22, 62, 102, 32, 72, 112, NA),
                           NIAmg = c(172, NA, 24.4, NA, 8.1, 134.6, 10.2, 187.9, 92, NA),
                           TRPmg = c(126, 142.5, 142.7, 167, NA, 76.3, 98.6, 41.4, 172, NA),
                           NIAEQmg = c(85.3, 49.2, 86.4, 23.2, 30.5, 83.3, 16.6, 84.5, 17.7, NA),
                           NIATRPmg = c(2.10, NA, 2.38,2.80, NA, 1.27, 1.64, 0.69, 2.87, NA),
                           FATg = c(21, NA, NA, NA, 11, NA, 13, 16, NA, NA),
                           FAT_g = c(20.9, 12.0, NA, NA, 10.9, 33, 12.1, 16.1, NA, NA),
                           FATCEg = c(NA, NA, NA, 16, NA, 33, NA, 15.9, NA, NA),
                           comments = c("", "These are imaginary food items", NA, "With imaginary nutrient values", "", "And blanks", NA, "To test different outputs", "", "And scenarios"))


usethis::use_data(breakfast_df, overwrite = TRUE)
