params <-
list(my_css = "css/rmdformats.css")

## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(httptest)
start_vignette("2")

## ----setup, echo=FALSE, message=FALSE, warning=FALSE--------------------------
if (!library(ctxR, logical.return = TRUE)){
  devtools::load_all()
}
old_options <- options("width")

## ----echo=FALSE, warning=FALSE------------------------------------------------
# Used to visualize data in a variety of plot designs
library(ggplot2)
library(gridExtra)

## ----setup-print, echo = FALSE------------------------------------------------
# Redefining the knit_print method to truncate character values to 25 characters
# in each column and to truncate the columns in the print call to prevent 
# wrapping tables with several columns.
#library(ctxR)
knit_print.data.table = function(x, ...) {
  y <- data.table::copy(x)
  y <- y[, lapply(.SD, function(t){
    if (is.character(t)){
      t <- strtrim(t, 25)
    }
    return(t)
  })]
  print(y, trunc.cols = TRUE)
}

registerS3method(
  "knit_print", "data.table", knit_print.data.table,
  envir = asNamespace("knitr")
)

## ----ctxR dtxsid data chemical, message=FALSE, eval=FALSE---------------------
#  chemical_details_by_dtxsid <- get_chemical_details(DTXSID = 'DTXSID7020182')

## ----ctxR dtxcid data chemical, message=FALSE, eval=FALSE---------------------
#  chemical_details_by_dtxcid <- get_chemical_details(DTXCID = 'DTXCID30182')

## ----ctxR batch data chemical, message=FALSE, eval=FALSE----------------------
#  vector_dtxsid<- c("DTXSID7020182", "DTXSID9020112", "DTXSID8021430")
#  chemical_details_by_batch_dtxsid <- get_chemical_details_batch(DTXSID = vector_dtxsid)
#  
#  vector_dtxcid <- c("DTXCID30182", "DTXCID801430", "DTXCID90112")
#  chemical_details_by_batch_dtxcid <- get_chemical_details_batch(DTXCID = vector_dtxcid)

## ----ctxr dtxsid check, message=FALSE, eval=FALSE-----------------------------
#  dtxsid_check_true <- check_existence_by_dtxsid(DTXSID = 'DTXSID7020182')
#  dtxsid_check_false <- check_existence_by_dtxsid(DTXSID = 'DTXSID7020182f')

## ----ctxr dtxsid check batch, message=FALSE, eval=FALSE-----------------------
#  vector_dtxsid_and_non_dtxsid <- c('DTXSID7020182F', 'DTXSID7020182', 'DTXSID0020232F')
#  dtxsid_checks <- check_existence_by_dtxsid_batch(DTXSID = vector_dtxsid_and_non_dtxsid)

## ----ctxR property range chemical, message=FALSE, eval=FALSE------------------
#  chemical_by_property_range <- get_chemical_by_property_range(start = 1.311,
#                                           end = 1.313,
#                                           property = 'Density')

## ----ctxR info chemical, message=FALSE, eval=FALSE----------------------------
#  chemical_info <- get_chem_info(DTXSID = 'DTXSID7020182')

## ----ctxR fate data chemical, message=FALSE, eval=FALSE-----------------------
#  fate_by_dtxsid <- get_fate_by_dtxsid(DTXSID = 'DTXSID7020182')

## ----ctxR starting value chemical, message=FALSE, eval=FALSE------------------
#  search_starts_with_dtxsid <- chemical_starts_with(word = 'DTXSID7020182')
#  search_starts_with_chem_name <- chemical_starts_with(word = 'Bisph')
#  search_starts_with_casrn <- chemical_starts_with(word = '80-05-7')
#  search_starts_with_inchikey <- chemical_starts_with(word = 'IISBACLAFKSPIT')

## ----ctxR exact value chemical, message=FALSE, eval=FALSE---------------------
#  search_exact_dtxsid <- chemical_equal(word = 'DTXSID7020182')
#  search_exact_chem_name <- chemical_equal(word = 'Bisphenol A')
#  search_exact_casrn <- chemical_equal(word = '80-05-7')
#  search_exact_inchikey <- chemical_equal(word = 'IISBACLAFKSPIT-UHFFFAOYSA-N')

## ----ctxR substring value chemical, message=FALSE, eval=FALSE-----------------
#  search_contains_dtxsid <- chemical_contains(word = 'DTXSID702018')
#  search_contains_chem_name <- chemical_contains(word = 'Bisph')
#  search_contains_casrn <- chemical_contains(word = '80-05-7')
#  search_contains_inchikey <- chemical_contains(word = 'IISBACLAF')

## ----ctxR mass range ms ready chemical, message=FALSE, eval=FALSE-------------
#  msready_by_mass <- get_msready_by_mass(start = 200.9,
#                                end = 200.95)

## ----ctxR chemical formula ms ready chemical, message=FALSE, eval=FALSE-------
#  msready_by_formula <- get_msready_by_formula(formula = 'C16H24N2O5S')

## ----ctxR dtxcid ms ready chemical, message=FALSE, eval=FALSE-----------------
#  msready_by_dtxcid <- get_msready_by_dtxcid(DTXCID = 'DTXCID30182')

## ----ctxR types of chemical lists, message=FALSE, eval=FALSE------------------
#  get_all_list_types()

## ----ctxR all list types chemical, message=FALSE, eval=FALSE------------------
#  chemical_lists_by_type <- get_chemical_lists_by_type(type =  'federal')

## ----ctxR list by name chemical, message=FALSE, eval=FALSE--------------------
#  public_chemical_list_by_name <- get_public_chemical_list_by_name(listname = 'CCL4')

## ----ctxR lists containing chemical, message=FALSE, eval=FALSE----------------
#  lists_containing_chemical <- get_lists_containing_chemical(DTXSID = 'DTXSID7020182')

## ----ctxR chemicals-in-list-start, message=FALSE, eval=FALSE------------------
#  chemicals_in_ccl4_start <- get_chemicals_in_list_start(list_name = 'CCL4', word = 'Bi')

## ----ctxR chemicals-in-list-exact, message=FALSE, eval=FALSE------------------
#  chemicals_in_ccl4_exact <- get_chemicals_in_list_exact(list_name = 'BIOSOLIDS2021', word = 'Bisphenol A')

## ----ctxR chemicals-in-list-contain, message=FALSE, eval=FALSE----------------
#  chemicals_in_ccl4_contain <- get_chemicals_in_list_contain(list_name = 'CCL4', word = 'Bis')

## ----ctxR chemical in list chemical, message=FALSE, eval=FALSE----------------
#  chemicals_in_list <- get_chemicals_in_list(list_name = 'CCL4')

## ----ctxR mrv by dtxsid dtxcid chemical, message=FALSE, eval=FALSE------------
#  chemical_mrv_by_dtxsid <- get_chemical_mrv(DTXSID = 'DTXSID7020182')
#  chemical_mrv_by_dtxcid <- get_chemical_mrv(DTXCID = 'DTXCID30182')

## ----ctxR mol by dtxsid dtxcid chemical, message=FALSE, eval=FALSE------------
#  chemical_mol_by_dtxsid <- get_chemical_mol(DTXSID = 'DTXSID7020182')
#  chemical_mol_by_dtxcid <- get_chemical_mol(DTXCID = 'DTXCID30182')

## ----ctxR image by dtxsid dtxcid chemical, message=FALSE, eval=FALSE----------
#  chemical_image_by_dtxsid <- get_chemical_image(DTXSID = 'DTXSID7020182')
#  chemical_image_by_dtxcid <- get_chemical_image(DTXCID = 'DTXCID30182')
#  chemical_image_by_smiles <- get_chemical_image(SMILES = 'CC(C)(C1=CC=C(O)C=C1)C1=CC=C(O)C=C1')
#  
#  countcolors::plotArrayAsImage(chemical_image_by_dtxsid)
#  countcolors::plotArrayAsImage(chemical_image_by_dtxcid)
#  countcolors::plotArrayAsImage(chemical_image_by_smiles)

## ----ctxR synonym by dtxsid chemical, message=FALSE, eval=FALSE---------------
#  chemical_synonym <- get_chemical_synonym(DTXSID = 'DTXSID7020182')

## ----breakdown, echo = FALSE, results = 'hide'--------------------------------
# This chunk will be hidden in the final product. It serves to undo defining the
# custom print function to prevent unexpected behavior after this module during
# the final knitting process and restores original option values.

knit_print.data.table = knitr::normal_print
  
registerS3method(
  "knit_print", "data.table", knit_print.data.table,
  envir = asNamespace("knitr")
)

options(old_options)

## ----include=FALSE------------------------------------------------------------
end_vignette()

