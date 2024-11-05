## code to prepare `KE18_subset_modified` dataset goes here


# Generated using the script at https://github.com/LuciaSegovia/FAO-fisheries Repo at KE18/KE18_FCT_FAO_Tags.R
#Dataset based on the Kenya FCT, here - https://nutritionhealth.or.ke/programmes/healthy-diets-physical/food-composition-tables/

# KE18 <- read.csv("KE18_FCT_FAO_Tags.csv")
# KE18_subset <- KE18[grepl("goat|Goat|lamb|Lamb|Sheep|sheep|Groundnut", KE18$food_desc), c("source_fct", "fdc_id", "food_desc", "food_group", "ENERCkcal", "WATERg", "PROCNTg", "CHOAVLDFg", "VITB12mcg")]
# KE18_subset[c("375", "376"), "VITB12mcg"] <- NA

# A copy of this dataset is generated using this:

KE18_subset_modified <- data.frame(source_fct = c('KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18', 'KE18'),
                          fdc_id = c('6023', '6030', '6025', '6031', '7013', '7014', '7043', '7044', '7015', '7045', '7016', '7046', '7047', '7018', '7048', '7019', '7049', '7050', '15063', '15099', '15108'),
                          food_desc = c('Milk, goat, fluid, whole, raw', 'Milk, goat, fluid, whole, boiled', 'Milk, sheep, fluid, whole, raw', 'Milk, sheep, fluid, whole, boiled', 'Goat blood, raw', 'Goat, lean, raw', 'Goat, lean, boiled (without salt)', 'Goat, lean, grilled (without salt and fat)', 'Goat liver, raw', 'Goat, liver, boiled (without salt)', 'Goat, medium fat, raw', 'Goat, medium fat, boiled (without salt)', 'Goat, medium fat, grilled (without salt and fat)', 'Lamb liver, raw', 'Lamb, liver, boiled (without salt)', 'Lamb, raw (unspecified part)', 'Lamb, unspecified part, boiled (without salt)', 'Lamb, unspecified part, grilled (without salt and fat)', 'Groundnut Sauce', 'Stir Fried Goat Meat', 'Stewed Goat Meat'),
                          food_group = c('MILK AND DAIRY PRODUCTS', 'MILK AND DAIRY PRODUCTS', 'MILK AND DAIRY PRODUCTS', 'MILK AND DAIRY PRODUCTS', 'MEAT, POULTRY AND EGGS', 'MEAT, POULTRY AND EGGS', 'MEAT, POULTRY AND EGGS', 'MEAT, POULTRY AND EGGS', 'MEAT, POULTRY AND EGGS', 'MEAT, POULTRY AND EGGS', 'MEAT, POULTRY AND EGGS', 'MEAT, POULTRY AND EGGS', 'MEAT, POULTRY AND EGGS', 'MEAT, POULTRY AND EGGS', 'MEAT, POULTRY AND EGGS', 'MEAT, POULTRY AND EGGS', 'MEAT, POULTRY AND EGGS', 'MEAT, POULTRY AND EGGS', 'Mixed dishes', 'Mixed dishes', 'Mixed dishes'),
                          ENERCkcal = c(78, 92, 94, 110, 69, 119, 174, 169, 115, 176, 166, 196, 192, 161, 193, 269, 344, 338, 298, 383, 222),
                          WATERg = c(85.3, 82.7, 82.8, 79.8, 86.4, 74.6, 62, 63.1, 74.8, 61.9, 68.2, 56, 57.1, 67.6, 54.5, 60, 47.2, 48.3, 50.2, 30.2, 59.4),
                          PROCNTg = c(3.5, 4.1, 5.1, 6, 3.8, 19.5, 29.1, 28.3, 20.2, 30.5, 18.6, 27.8, 26.9, 20.4, 30.8, 17, 25.4, 24.6, 10.6, 32.1, 17.1),
                          CHOAVLDFg = c(5.8, 6.8, 5.2, 6.2, 4.9, 0.6, 0, 0, 0, 0, 3.5, 0, 0, 3.1, 0, 0, 0, 0, 9.6, 10.4, 5.2),
                          VITB12mcg = c(0.35, 0, 0.7, 1, NA, 3.09, 3, 3, 64, 0, 2.34, 2, 3, NA, NA, 2.39, 2, 3, 0.17, 2.84, 1.52))


usethis::use_data(KE18_subset_modified, overwrite = TRUE)
