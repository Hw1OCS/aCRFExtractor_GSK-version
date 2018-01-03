
studyId <- "114460"

## --------------------------- ##
## Read files to compare.      ##
## --------------------------- ##
print("Import manually created Define Origin xls/xlsx file. Thanks!")
path_defOrig_manual <- file.choose()
path_defOrig_aCRFExtractor <- file.choose()

library(sqldf)
library(magrittr)

sheetName_manual <- "Variable"
defOrig_manual <- xlsx::read.xlsx(file = path_defOrig_manual, sheetName = sheetName_manual)
defOrig_manual_tbl <- dplyr::tbl_df(defOrig_manual)

sheetName_aCRFExtractor <- studyId
defOrig_aCRFExtractor <- xlsx::read.xlsx(file = path_defOrig_aCRFExtractor, sheetName = sheetName_aCRFExtractor)
defOrig_aCRFExtractor_tbl <- dplyr::tbl_df(defOrig_aCRFExtractor)

## -----------------##
## Preprocessing.   ##
## -----------------##

## -- Remove Derived and Assigned rows
defOrig_manual_tbl <- defOrig_manual_tbl %>%
  # dplyr::filter(!Origin %in% c('Assigned', 'Derived', 'Protocol'))
  dplyr::filter(Origin %in% c('CRF Page'))

defOrig_aCRFExtractor_tbl <- defOrig_aCRFExtractor_tbl %>%
  # dplyr::filter(!Origin %in% c('Assigned', 'Derived', 'Protocol'))
  dplyr::filter(Origin %in% c('CRF Page'))

## -- Remove Domains for which page numbers are not needed (e.g., RELREC, SUPP&domain) from manual file
domain_unwanted <- unique(as.character(unlist(stringr::str_extract_all(defOrig_manual_tbl$Domain, "^(SUPP\\w+)|^(RELREC)|^(T\\w+)"))))

defOrig_manual_tbl <- defOrig_manual_tbl %>%
  dplyr::filter(!Domain %in% domain_unwanted)


## -- Sort the files using Domain & Variable columns
defOrig_manual_tbl <- defOrig_manual_tbl %>%
  dplyr::arrange(Domain, Variable)

defOrig_aCRFExtractor_tbl <- defOrig_aCRFExtractor_tbl %>%
  dplyr::arrange(Domain, Variable)


## ------------------------------------------- ##
## Identify differences between the files.     ##
## ------------------------------------------- ##

## 1. Compare #rows
##    - Select Domain & Variable columns for comparing row difference
defOrig_manual_tbl2 <- defOrig_manual_tbl %>%
  dplyr::select(Domain, Variable)

defOrig_aCRFExtractor_tbl2 <- defOrig_aCRFExtractor_tbl %>%
  dplyr::select(Domain, Variable)

valid1_InManNotInPkg <- sqldf('SELECT * FROM defOrig_manual_tbl2 EXCEPT SELECT * FROM defOrig_aCRFExtractor_tbl2')
assign(x = "valid1_InManNotInPkg", value = valid1_InManNotInPkg, envir = .GlobalEnv)


## 2. Compare page numbers
valid2_descrepancyPgNbs <- sqldf::sqldf("SELECT A.Domain, A.Variable, A.CRF_Page_No, B.page_nbr_concat3, B.flg_further_check_needed3
                                         FROM defOrig_manual_tbl AS A
                                         INNER JOIN defOrig_aCRFExtractor_tbl AS B
                                         ON A.Domain = B.Domain AND A.Variable = B.Variable")

valid2_descrepancyPgNbs_Equal <- valid2_descrepancyPgNbs[valid2_descrepancyPgNbs$CRF_Page_No == valid2_descrepancyPgNbs$page_nbr_concat3, ]
valid2_descrepancyPgNbs_NotEqual <- valid2_descrepancyPgNbs[valid2_descrepancyPgNbs$CRF_Page_No != valid2_descrepancyPgNbs$page_nbr_concat3, ]

## -- calculate percent of error
nbr_Equal <- dim(valid2_descrepancyPgNbs_Equal)[1]
nbr_NotEqual <- dim(valid2_descrepancyPgNbs_NotEqual)[1]

percent_error <- round((nbr_NotEqual / (nbr_NotEqual + nbr_Equal)) * 100, 1)
assign(x = "percent_error", value = percent_error, envir = .GlobalEnv)

## -- categorize discrepany variables
## studyid/visitnum
valid2_studyid <- valid2_descrepancyPgNbs_NotEqual %>%
  dplyr::filter(Variable %in% c('STUDYID', 'VISITNUM'))

nbr_studyid <- dim(valid2_studyid)[1]

percent_error_studyid <- round((nbr_studyid / (nbr_studyid + nbr_NotEqual)) * 100, 1)
assign(x = "percent_error_studyid", value = percent_error_studyid, envir = .GlobalEnv)

## other domains
valid2_otherSDTM <- valid2_descrepancyPgNbs_NotEqual %>%
  dplyr::filter(!Variable %in% c('STUDYID', 'VISITNUM'))


