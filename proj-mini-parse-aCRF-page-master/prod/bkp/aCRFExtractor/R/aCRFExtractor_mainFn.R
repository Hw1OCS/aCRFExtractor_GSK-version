#' Extracts page numbers of SDTM variables available in manually created aCRF file. The output is used for creation of Define XML file.
#' The input files (e.g., studyid, aCRF file, and Define Origin file) are taken from a user via console
#'
#' @example run_aCRF_minner()
#' @author Hailemichael M. Worku (aka, Haile). Email: <hailemichael.x.worku@gsk.com>
#'
#'


run_aCRFExtractor <- function() {

  ## Fill studyId info
  studyid <- readline(prompt = "Enter StudyId: ")

  ## Import aCRF pdf file
  print("Import aCRF pdf file. Thanks!")
  aCRF_fileName <- file.choose()

  ## Import defineOrigin file
  print("Import Define Origin draft xls/xlsx file. Thanks!")
  defOrig_fileName <- file.choose()                             ## the current version of the package assumes VariableTab

  ## Ask user to specify output destination
  # print("Where do you want to save the output file (select a folder)?")
  # output_dir <- tk_choose.dir(default = "",
  #                             caption = "Where do you want to save the output file (select a folder)?")

  output_dir <- dirname(defOrig_fileName)

  ##
## Environmental settings
# studyid <- "115158"
# studyid <- "115649"
# location_source <- "/Users/workuhm/Desktop/aCRF_files/"
# location_source <- paste("/Users/workuhm/Desktop/aCRF_files/", studyid, "/", sep = "")

## Define the aCRF PDF file
# pdf_fileName = paste(location_source, "blankcrf_ES_20170609.pdf", sep = "")
# pdf_fileName = paste(location_source, "blankcrf.pdf", sep = "")
pdf_fileName <- aCRF_fileName

## load source files
# source("./functions/load_library.R")
#
# source("./functions/preprocess_crf_fn.R")
# source("./functions/get_sdtmVars_pgNbr_fn.R")
# source("./functions/get_pgsSpecialVars_fn.R")
# source("./functions/get_finalaCRFpages_fn.R")
# source("./functions/get_linkPages_fn.R")

# require(pdftools)
# require(stringr)
# require(tidyverse)
# require(xlsx)     ## obsolete due to its dependency on rJava
# require(readxl)     ## to import xls file
# require(readr)      ## part of pkg::tidyr, to export xls/csv file
# require(sqldf)


## -------------------------------------------------------- ##
## <!-- Get MFS specs.                                -->   ##
## -------------------------------------------------------- ##
# fileMFS_name <- paste(location_source, "SDTM_SPECIFICATION_", studyid, ".XLSX", sep = "")
# sheetMFSMapping_name <- "Mapping"
#
# MFS_mappingTab <- read.xlsx(file = fileMFS_name, sheetName = sheetMFSMapping_name)
# MFS_mappingTab2 <- MFS_mappingTab[, c("SDTM_DS", "SDTM_VAR", "SPECIFICATION", "FUNCTION")]



## -------------------------------------------------------- ##
## <!-- Get the Define Origin file.                   -->   ##
## -------------------------------------------------------- ##
# fileDefOrigin_name <- paste(location_source, studyid, "_DEFINE_ORIGIN", ".xlsx", sep = "")
# sheetVarTab_name <- "Variable"
# sheetVLMTab_name <- "VLM"

fileDefOrigin_name <- defOrig_fileName
# sheetVarTab_name <- defOrig_sheetName
sheetVarTab_name <- "Variable"

defineOrigin_variableTab <- read.xlsx(file = fileDefOrigin_name, sheetName = sheetVarTab_name)
# defineOrigin_variableTab <- read_excel(path = fileDefOrigin_name, sheet = "Variable", col_names = T)
defineOrigin_variableTab <- as.data.frame(defineOrigin_variableTab)

# defineOrigin_VLMTab <- read.xlsx(file = fileDefOrigin_name, sheetName = sheetVLMTab_name)

## keep main variables
defineOrigin_variableTab2 <- defineOrigin_variableTab[,c("Domain", "Variable", "Codelist", "Origin")]
# defineOrigin_VLMTab2 <- defineOrigin_VLMTab[,c("Domain", "Variable", "VLM_variable", "VLM_value")]

## remove codelist records and others
defineOrigin_variableTab3 <- defineOrigin_variableTab2[!(defineOrigin_variableTab2$Codelist %in% c("MedDRA", "GSKDD", "DOMAIN")), ]

## <!-- Get variables not in aCRF as mentoined in Define Origin file
otherOrigins_varTab1 <- subset(defineOrigin_variableTab3, !(Origin) %in% c("CRF Page"))
otherOrigins_varTab2 <- otherOrigins_varTab1[order(otherOrigins_varTab1[, 2]), ]         ## ORDER function works only with numeric value for column definition
otherOrigins_varTab3 <- otherOrigins_varTab2[!duplicated(otherOrigins_varTab2[, "Variable"]), ]


## select domains
# sel_domain <- "AE"
#
# dsinOrig <- defineOrigin_variableTab3[defineOrigin_variableTab3$Domain == sel_domain, ]
# dsinaCRF <- crf_out[crf_out$domain == sel_domain, ]

## ------------------------------------------------------------------- ##
## <!-- Get list of Domains as specified in Define Origin file.   -->  ##
## ------------------------------------------------------------------- ##
## VariableTab
domain_DefOrigin <- as.character(unique(defineOrigin_variableTab3$Domain))
domain_DefOrigin_supps <- as.character(domain_DefOrigin[!is.na(unlist(str_extract(string = domain_DefOrigin, pattern = "^SUPP\\w+")))])
domain_DefOrigin_Tdomains <- as.character(domain_DefOrigin[!is.na(unlist(str_extract(string = domain_DefOrigin, pattern = "^T\\w+")))])

domain_DefOrigin_VarTab_filtered <- domain_DefOrigin[!(domain_DefOrigin %in% c(domain_DefOrigin_supps, domain_DefOrigin_Tdomains, "SE", "RELREC"))]
# domain_DefOrigin_VarTab_filtered

## VLMTab
# domain_DefOrigin_VLMTab <- as.character(unique(defineOrigin_VLMTab2$Domain))
# domain_DefOrigin_VLMTab2 <- as.character(unlist(str_extract(domain_DefOrigin_VLMTab, "[^T]*")))
# domain_DefOrigin_VLMTab2 <- ifelse(str_detect(domain_DefOrigin_VLMTab, "^T"),
#                                    "NA",
#                                    domain_DefOrigin_VLMTab)
# domain_DefOrigin_VLMTab_filtered <- domain_DefOrigin_VLMTab2[!(domain_DefOrigin_VLMTab2 %in% c("IS", "NA"))]
# domain_DefOrigin_VLMTab_filtered


# query_tmp <- paste(domain_DefOrigin_supps, domain_DefOrigin_Tdomains, sep = "")
# # query_domain <- paste("c(", query_tmp, ")", sep = "")
# defineOrigin_variableTab4 <- defineOrigin_variableTab3[!(defineOrigin_variableTab3$Domain %in% c(query_tmp)), ]
# unique(defineOrigin_variableTab4$Domain)

## ---------------------------------------------- ##
## <!-- Import aCRF pdf file and parse it     --> ##
## ---------------------------------------------- ##
##pdf_info(pdf_fileName)

pages <- pdf_info(pdf_fileName)$pages
crf_raw <- pdf_text(pdf_fileName)
##crf_txt2 <- pdf_render_page(pdf_fileName)

## ---------------------------------------------- ##
## <!-- Pre-process 1: Clean the meesy data.  --> ##
## ---------------------------------------------- ##
# crf_preprocess <- preprocess_crf(crf_txt_raw = crf_raw, def_OS = "windows")
# crf_preprocess <- preprocess_crf(crf_txt_raw = crf_raw, def_OS = "mac")
crf_preprocess <- preprocess_crf(crf_txt_raw = crf_raw)

## ---------------------------------------------------------- ##
## <!-- Pre-process 2: Extract domain specific variables. --> ##
## ---------------------------------------------------------- ##
# crf_out <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess, domain_list = c("DM", "CM"), until_pgNbr = "51")
# crf_out <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess, domain_list = c("DM", "CM", "AE"))
# crf_out_batch1 <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess, domain_list = c("MH", "AE", "CM", "DM"))
crf_out_varTab <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess,
                                     domain_list = domain_DefOrigin_VarTab_filtered)


## get SDTM vars without domain name
# crf_out$Variable <- unlist(str_trim(str_extract(crf_out$sdtm_vars, "[^.]*$")))

## remove duplicates
# crf_out_unique <- crf_out[!duplicated(crf_out[, c("page_nbr", "Variable")]), c("page_nbr", "domain", "Variable")]
crf_out_VarTab_unique <- crf_out_varTab[!duplicated(crf_out_varTab[, c("page_nbr", "Variable")]), c("page_nbr", "sdtm_vars", "domain", "Variable")]

## collapse page numbers
# delim <- paste("(\"", ", ", "\")", sep = "")
# query_pgNbr <- paste(paste("SELECT A.*, group_concat(page_nbr SEPARATOR ", delim, " AS page_nbr_concat ", sep = ""),
#                      "FROM qwe AS A", sep = "")



## ---------------------------------------------------------- ##
## <!-- Pre-process 3: Extract link page numbers.         --> ##
## ---------------------------------------------------------- ##
# linkPages <- get_linkPages(dsin = crf_preprocess, debugme = 40)
linkPages <- get_linkPages(dsin = crf_preprocess, dset_defOrigin = defineOrigin_variableTab3)
linkPages_unique <- linkPages[!duplicated(linkPages[, c("Variable", "pgNbr_aCRF")]), c(4,5,1,2)]
colnames(linkPages_unique) <- c("page_nbr", "sdtm_vars", "domain", "Variable")


## ------------------------------------------------------------- ##
## <!-- Grap page numbers for all SDTM variables by Domain.  --> ##
## ------------------------------------------------------------- ##
# crf_out_DM <- get_finalaCRFpages(dsin_crf = crf_out_unique[crf_out_unique$domain == "DM", ])
crf_out_VarTab_final <- get_finalaCRFpages(dsin_crf = crf_out_VarTab_unique, domain_list = domain_DefOrigin_VarTab_filtered,
                                           dsin_linkPages = linkPages_unique)

# test <- sqldf("SELECT A.*, group_concat(page_nbr SEPARATOR) AS page_nbr_concat
#                       FROM qwe AS A")

# ddply(qwe, .(page_nbr), summarize, C = toString(C))

## save file
# fileout_name <- paste(location_source, "sdtmVars_aCRF_all_", studyid, ".xlsx", sep = "")
# sheetout_name <- paste("sdtmVars_aCRF_all", studyid, sep = "")
#
# write.xlsx(x = crf_out_VarTab_final, file = fileout_name, sheetName = sheetout_name,
#            row.names = FALSE, col.names = TRUE)


## ------------------------------------------------------------------------------- ##
## <!-- Keep page numbers of SDTM variables only found in Define Origin file.  --> ##
## ------------------------------------------------------------------------------- ##
defineOrigin_variableTab_mainSDTM <- subset(defineOrigin_variableTab3, Domain %in% c(domain_DefOrigin_VarTab_filtered))

defineOrigin_variableTab_mainSDTM2 <- sqldf("SELECT A.*, B.page_nbr_concat
                                            FROM defineOrigin_variableTab_mainSDTM AS A
                                            LEFT JOIN crf_out_VarTab_final AS B
                                            ON A.Domain = B.domain AND A.Variable = B.Variable")

## ----------------------------- ##
## <!-- Flag other Origins  -->  ##
## ----------------------------- ##
## Derived variables
defineOrigin_variableTab_mainSDTM2$flg_further_check_needed <- ifelse(defineOrigin_variableTab_mainSDTM2$Origin == "Derived" &
                                                                        !is.na(defineOrigin_variableTab_mainSDTM2$page_nbr_concat),
                                                                      "Origin is Derived in the Define Origin file, but the program generated page numbers for it. Please check. Thanks!",
                                                                      "")
## Assigned variables
defineOrigin_variableTab_mainSDTM2$flg_further_check_needed <- ifelse(defineOrigin_variableTab_mainSDTM2$Origin == "Assigned" &
                                                                        !is.na(defineOrigin_variableTab_mainSDTM2$page_nbr_concat),
                                                                      "Origin is Assigned in the Define Origin file, but the program generated page numbers for it. Please check. Thanks!",
                                                                      defineOrigin_variableTab_mainSDTM2$flg_further_check_needed)
## eDT variables
defineOrigin_variableTab_mainSDTM2$flg_further_check_needed <- ifelse(defineOrigin_variableTab_mainSDTM2$Origin == "eDT" &
                                                                        !is.na(defineOrigin_variableTab_mainSDTM2$page_nbr_concat),
                                                                      "Origin is eDT in the Define Origin file, but the program generated page numbers for it. Please check. Thanks!",
                                                                      defineOrigin_variableTab_mainSDTM2$flg_further_check_needed)
## Protocol variables
defineOrigin_variableTab_mainSDTM2$flg_further_check_needed <- ifelse(defineOrigin_variableTab_mainSDTM2$Origin == "Protocol" &
                                                                        !is.na(defineOrigin_variableTab_mainSDTM2$page_nbr_concat),
                                                                      "Origin is Protocol in the Define Origin file, but the program generated page numbers for it. Please check. Thanks!",
                                                                      defineOrigin_variableTab_mainSDTM2$flg_further_check_needed)

## --------------------------------------------------------- ##
## <!-- Get page numbers for STUDYID and VISITNUM.       --> ##
## --------------------------------------------------------- ##

domain_list <- domain_DefOrigin_VarTab_filtered

## studyid
pgs_studyid_all <- data.frame(Domain = character(), Variable = character(), Codelist = character(),
                              Origin = character(), pg_nbr_concat = character(), flg_further_check_needed = character())

## visitnum
pgs_visitnum_all <- data.frame(Domain = character(), Variable = character(), Codelist = character(),
                               Origin = character(), pg_nbr_concat = character(), flg_further_check_needed = character())

for(j in domain_list) {

  # get_pgsSpecialVars(dsin_unique = crf_out_VarTab_unique, dsin_crf = crf_preprocess,
  #                    variable = "__\\.STUDYID", domain_list = domain_DefOrigin_VarTab_filtered)
  pgs_studyid <- get_pgsSpecialVars(dsin_unique = crf_out_VarTab_unique, dsin_crf = crf_preprocess, dsin_defOrigFinal = defineOrigin_variableTab_mainSDTM2,
                                    variable = "__\\.STUDYID", domain = j)
  # print(pgs_studyid)

  ## visitnum
  pgs_visitnum <- get_pgsSpecialVars(dsin_unique = crf_out_VarTab_unique, dsin_crf = crf_preprocess, dsin_defOrigFinal = defineOrigin_variableTab_mainSDTM2,
                                     variable = "__\\.VISITNUM", domain = j)

  ## -- combine domain datasets
  pgs_studyid_all <- rbind(pgs_studyid_all, pgs_studyid)
  pgs_visitnum_all <- rbind(pgs_visitnum_all, pgs_visitnum)
}


## ---------------------------------------------------- ##
## <!-- Combine main dataset and STUDYID dataset.   --> ##
## ---------------------------------------------------- ##
# defineOrigin_variableTab_mainSDTM3 <- sqldf("SELECT A.*, B.pg_nbr_concat AS pg_nbr_concat_B, B.flg_further_check_needed
#                                              FROM defineOrigin_variableTab_mainSDTM2 AS A
#                                              LEFT JOIN pgs_studyid_all AS B
#                                              ON A.Domain = B.Domain AND A.Variable = B.Variable")

defineOrigin_variableTab_mainSDTM3 <- sqldf("SELECT A.*, B.pg_nbr_concat AS pg_nbr_concat_B, B.flg_further_check_needed AS flg_further_check_needed_B,
                                            C.pg_nbr_concat AS pg_nbr_concat_C, C.flg_further_check_needed AS flg_further_check_needed_C
                                            FROM defineOrigin_variableTab_mainSDTM2 AS A
                                            LEFT JOIN pgs_studyid_all AS B
                                            ON A.Domain = B.Domain AND A.Variable = B.Variable
                                            LEFT JOIN pgs_visitnum_all AS C
                                            ON A.Domain = C.Domain AND A.Variable = C.Variable")

## add studyid info
defineOrigin_variableTab_mainSDTM3$page_nbr_concat2 <- ifelse(!is.na(defineOrigin_variableTab_mainSDTM3$pg_nbr_concat_B),
                                                              defineOrigin_variableTab_mainSDTM3$pg_nbr_concat_B,
                                                              defineOrigin_variableTab_mainSDTM3$page_nbr_concat)
defineOrigin_variableTab_mainSDTM3$flg_further_check_needed2 <- ifelse(!is.na(defineOrigin_variableTab_mainSDTM3$flg_further_check_needed_B),
                                                                       defineOrigin_variableTab_mainSDTM3$flg_further_check_needed_B,
                                                                       defineOrigin_variableTab_mainSDTM3$flg_further_check_needed)

## add visitnum info
defineOrigin_variableTab_mainSDTM3$page_nbr_concat3 <- ifelse(!is.na(defineOrigin_variableTab_mainSDTM3$pg_nbr_concat_C),
                                                              defineOrigin_variableTab_mainSDTM3$pg_nbr_concat_C,
                                                              defineOrigin_variableTab_mainSDTM3$page_nbr_concat2)
defineOrigin_variableTab_mainSDTM3$flg_further_check_needed3 <- ifelse(!is.na(defineOrigin_variableTab_mainSDTM3$flg_further_check_needed_C),
                                                                       defineOrigin_variableTab_mainSDTM3$flg_further_check_needed_C,
                                                                       defineOrigin_variableTab_mainSDTM3$flg_further_check_needed2)

# defineOrigin_variableTab_mainSDTM4 <- defineOrigin_variableTab_mainSDTM3[, !names(defineOrigin_variableTab_mainSDTM3) %in% c("pg_nbr_concat", "pg_nbr_concat_B")]
# defineOrigin_variableTab_mainSDTM4 <- defineOrigin_variableTab_mainSDTM3[, c("Domain", "Variable", "Codelist", "page_nbr_concat2", "flg_further_check_needed")]
# defineOrigin_variableTab_mainSDTM4 <- sqldf("SELECT A.Domain, A.Variable, A.Codelist, A.page_nbr_concat2 AS page_nbr_concat, A.flg_further_check_needed
#                                              FROM defineOrigin_variableTab_mainSDTM3")

defineOrigin_variableTab_mainSDTM4 <- defineOrigin_variableTab_mainSDTM3[, c("Domain", "Variable", "Codelist", "Origin", "page_nbr_concat3", "flg_further_check_needed3")]
# defineOrigin_variableTab_mainSDTM4 <- defineOrigin_variableTab_mainSDTM3[, c("Domain", "Variable", "Codelist", "page_nbr_concat2", "flg_further_check_needed")]



## Export updated Define Origin file
# fileout_name <- paste(location_source, "defineOrigin_variableTab_pageNumbers_", studyid, ".xlsx", sep = "")
# sheetout_name <- paste("defineOrigin_variableTab_pageNumbers", studyid, sep = "")

# write.xlsx(x = defineOrigin_variableTab_mainSDTM4, file = fileout_name, sheetName = sheetout_name,
#            row.names = FALSE, col.names = TRUE)


## export result to default directory, the same place as Define Origin file
fileOut_name <- paste("defineOrigin_variableTab_pageNumbers_", studyid, "_",
                      str_replace_all(format(Sys.time(), "%b %d %Y"), " ", "_"),
                      ".xls", sep = "")

write.xlsx(x = defineOrigin_variableTab_mainSDTM4, file = file.path(output_dir, fileOut_name), sheetName = studyid,
           row.names = FALSE, col.names = TRUE)

# write_excel_csv(x = pgs_aCRF_out, path = fileOut_name, col_names = T)
# write_csv(x = pgs_aCRF_out, path = file.path(output_dir, fileOut_name), col_names = T)     ## powered by pkg::readr
# write.table(x = pgs_aCRF_out, file = file.path(output_dir, fileOut_name), row.names = F, col.names = T)

# write_csv(x = defineOrigin_variableTab_mainSDTM4,
#           path = file.path(output_dir, fileOut_name),
#           col_names = T, na = "")                                     ## powered by pkg::readr

print("")
print("<!-- *************************************************  -->")
print(paste("The output file (", fileOut_name, ") is saved in directory: ", output_dir, ". Please check!",
            sep = ""))
print("<!-- *************************************************  -->")

# return(defineOrigin_variableTab_mainSDTM4)

}







