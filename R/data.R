#' Imaginary Food Items
#'
#' An imaginary dataset of Food Items used to demonstrate various functions
#'
#' @format A data frame with 10 rows and 21 columns:
#' \describe{
#'   \item{food_code}{Imaginary Food ID}
#'   \item{food_name}{Food names to attach the imaginary nutritional data to}
#'   \item{WATERg}{Imaginary Water values}
#'   \item{PROCNTg}{Imaginary Protein values}
#'   \item{FAT_g_combined}{Imaginary Fat values}
#'   \item{CHOAVLg}{Imaginary Carbohyrate values}
#'   \item{FIBTGg_combined}{Imaginary Fibre values}
#'   \item{ALCg}{Imaginary Alcohol values}
#'   \item{ASHg}{Imaginary Ash values}
#'   \item{THIAmg}{Imaginary Thiamine values}
#'   \item{THIAHCLmg}{Imaginary Thiamine from HCL values}
#'   \item{RETOLmcg}{Imaginary retinol values}
#'   \item{CARTBEQmcg_combined}{Imaginary Beta-Carotene equiv. values}
#'   \item{NIAmg}{Imaginary Niacin values}
#'   \item{TRPmg}{Imaginary Tryptophan values}
#'   \item{NIAEQmg}{Imaginary Niacin equivalent values}
#'   \item{NIATRPmg}{Imaginary Niacin from Tryptophan values}
#'   \item{FATg}{Imaginary Fat values}
#'   \item{FAT_g}{Imaginary Fat from unknown source values}
#'   \item{FATCEg}{Imaginary Fat from CE method values}
#'   \item{comments}{Imaginary comments}
#' }
#' @source An imaginary dataset; no formal source.
"breakfast_df"

#' Imaginary Food Items
#'
#' An imaginary dataset of Food Items with non-standard column names used to
#' demonstrate various functions
#'
#' @format A data frame with 10 rows and 21 columns:
#' \describe{
#'   \item{food_code}{Imaginary Food ID}
#'   \item{food_name}{Food names to attach the imaginary nutritional data to}
#'   \item{Water_values_g}{Imaginary Water values}
#'   \item{PROCNT_values_g}{Imaginary Protein values}
#'   \item{FAT_values_g_combined}{Imaginary Fat values}
#'   \item{CHOAVL_values_g}{Imaginary Carbohyrate values}
#'   \item{FIBTG_values_g_combined}{Imaginary Fibre values}
#'   \item{ALC_values_g}{Imaginary Alcohol values}
#'   \item{ASH_values_g}{Imaginary Ash values}
#'   \item{Thiamine_milligrams}{Imaginary Thiamine values}
#'   \item{Thiamine_from_HCL_milligrams}{Imaginary Thiamine from HCL values}
#'   \item{Retinol_micrograms}{Imaginary retinol values}
#'   \item{Beta_Carotene_Equivalents_micrograms}{Imaginary Beta-Carotene equiv. values}
#'   \item{Niacin_milligrams}{Imaginary Niacin values}
#'   \item{Tryptophan_milligrams}{Imaginary Tryptophan values}
#'   \item{Niacin_eq_milligrams}{Imaginary Niacin equivalent values}
#'   \item{Niacine_from_TRP_mg}{Imaginary Niacin from Tryptophan values}
#'   \item{FAT_in_g}{Imaginary Fat values}
#'   \item{FAT_unknown_calc_g}{Imaginary Fat from unknown source values}
#'   \item{FAT_continuous_extraction_g}{Imaginary Fat from CE method values}
#'   \item{comments_column}{Imaginary comments}
#' }
#' @source An imaginary dataset; no formal source.
"breakfast_df_nonstandard"


#' WA19 subset
#'
#' A subset of the West Africa 2019 FCT dataset.
#'
#' @format A data frame with 49 rows and 9 columns:
#' \describe{
#'   \item{source_fct}{The FCT that this data is sourced from}
#'   \item{fdc_id}{food item ID}
#'   \item{food_desc}{Food Description}
#'   \item{food_group}{Food Group the food item belongs to}
#'   \item{ENERCkcal}{Energy in Kcal per 100g food}
#'   \item{WATERg}{Water in grams per 100g food}
#'   \item{PROCNTg}{Protein in grams per 100g food}
#'   \item{CHOAVLDFg}{Carbohydrates calculated by difference per 100g food}
#'   \item{VITB12mcg}{Vitamin B12 in micrograms per 100g of food}
#' }
#' @source A subset of the West Africa Food Composition  Table 2018 - found here: https://www.fao.org/infoods/infoods/tables-and-databases/faoinfoods-databases/en/
'WA19_subset'



#' KE19 Subset, modified to contain missing data
#'
#' A subset of the Kenya 2018 FCT dataset, with some data removed.
#'
#' @format A data frame with 21 rows and 9 columns:
#' \describe{
#'   \item{source_fct}{The FCT that this data is sourced from}
#'   \item{fdc_id}{food item ID}
#'   \item{food_desc}{Food Description}
#'   \item{food_group}{Food Group the food item belongs to}
#'   \item{ENERCkcal}{Energy in Kcal per 100g food}
#'   \item{WATERg}{Water in grams per 100g food}
#'   \item{PROCNTg}{Protein in grams per 100g food}
#'   \item{CHOAVLDFg}{Carbohydrates calculated by difference per 100g food}
#'   \item{VITB12mcg}{Vitamin B12 in micrograms per 100g of food}
#' }
#' @source A modified subset of the Kenya 2018 FCT, available here: https://openknowledge.fao.org/items/ece9c958-74f6-4d90-b14e-6b8a16c0dd9d
'KE18_subset_modified'
