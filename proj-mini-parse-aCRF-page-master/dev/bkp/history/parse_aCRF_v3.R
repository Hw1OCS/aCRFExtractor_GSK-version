
## Environmental settings
# studyid <- "115158"
studyid <- "115649"
# location_source <- "/Users/workuhm/Desktop/aCRF_files/"
location_source <- paste("/Users/workuhm/Desktop/aCRF_files/", studyid, "/", sep = "")

## Define the aCRF PDF file
# pdf_fileName = paste(location_source, "blankcrf_ES_20170609.pdf", sep = "")
pdf_fileName = paste(location_source, "blankcrf.pdf", sep = "")

library(pdftools)
library(stringr)
library(tidyr)    ## to unlist variable
library(xlsx)     ## for reading/writing xls files
library(sqldf)
# library(data.table)

## load source codes
source("./functions/preprocess_crf_fn.R")
source("./functions/get_sdtmVars_pgNbr_fn.R")
source("./functions/get_finalaCRFpages_fn.R")

## -------------------------------------------------------- ##
## <!-- Get the Define Origin file.                   -->   ##
## -------------------------------------------------------- ##
filein_name <- paste(location_source, studyid, "_DEFINE_ORIGIN", ".xlsx", sep = "")
sheetVarTab_name <- "Variable"
sheetVLMTab_name <- "VLM"

defineOrigin_variableTab <- read.xlsx(file = filein_name, sheetName = sheetVarTab_name)
defineOrigin_VLMTab <- read.xlsx(file = filein_name, sheetName = sheetVLMTab_name)

## keep main variables
defineOrigin_variableTab2 <- defineOrigin_variableTab[,c("Domain", "Variable", "Codelist")]
defineOrigin_VLMTab2 <- defineOrigin_VLMTab[,c("Domain", "Variable", "VLM_variable", "VLM_value")]

## remove codelist records and others
defineOrigin_variableTab3 <- defineOrigin_variableTab2[!(defineOrigin_variableTab2$Codelist %in% c("MedDRA", "GSKDD", "DOMAIN")), ]

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
domain_DefOrigin_VarTab_filtered

## VLMTab
domain_DefOrigin_VLMTab <- as.character(unique(defineOrigin_VLMTab2$Domain))
# domain_DefOrigin_VLMTab2 <- as.character(unlist(str_extract(domain_DefOrigin_VLMTab, "[^T]*")))
domain_DefOrigin_VLMTab2 <- ifelse(str_detect(domain_DefOrigin_VLMTab, "^T"),
                                   "NA",
                                   domain_DefOrigin_VLMTab)
domain_DefOrigin_VLMTab_filtered <- domain_DefOrigin_VLMTab2[!(domain_DefOrigin_VLMTab2 %in% c("IS", "NA"))]
domain_DefOrigin_VLMTab_filtered


# query_tmp <- paste(domain_DefOrigin_supps, domain_DefOrigin_Tdomains, sep = "")
# # query_domain <- paste("c(", query_tmp, ")", sep = "")
# defineOrigin_variableTab4 <- defineOrigin_variableTab3[!(defineOrigin_variableTab3$Domain %in% c(query_tmp)), ]
# unique(defineOrigin_variableTab4$Domain)

## ---------------------------------------------- ##
## <!-- Import aCRF pdf file and parse it     --> ##
## ---------------------------------------------- ##
##pdf_info(pdf_fileName)

pages = pdf_info(pdf_fileName)$pages 
crf_raw = pdf_text(pdf_fileName)
##crf_txt2 = pdf_render_page(pdf_fileName)

## ---------------------------------------------- ##
## <!-- Pre-process 1: Clean the meesy data.  --> ##
## ---------------------------------------------- ##
# crf_preprocess <- preprocess_crf(crf_txt_raw = crf_raw, def_OS = "windows")
crf_preprocess <- preprocess_crf(crf_txt_raw = crf_raw, def_OS = "mac")

## ---------------------------------------------------------- ##
## <!-- Pre-process 2: Extract domain specific variables. --> ##
## ---------------------------------------------------------- ##
# crf_out <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess, domain_list = c("DM", "CM"), until_pgNbr = "51")
# crf_out <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess, domain_list = c("DM", "CM", "AE"))
# crf_out_batch1 <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess, domain_list = c("MH", "AE", "CM", "DM"))
crf_out_varTab <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess, domain_list = domain_DefOrigin_VarTab_filtered)

## get SDTM vars without domain name
# crf_out$Variable <- unlist(str_trim(str_extract(crf_out$sdtm_vars, "[^.]*$")))

## remove duplicates
# crf_out_unique <- crf_out[!duplicated(crf_out[, c("page_nbr", "Variable")]), c("page_nbr", "domain", "Variable")]
crf_out_VarTab_unique <- crf_out_varTab[!duplicated(crf_out_varTab[, c("page_nbr", "Variable")]), c("page_nbr", "sdtm_vars", "domain", "Variable")]

## collapse page numbers
# delim <- paste("(\"", ", ", "\")", sep = "")
# query_pgNbr <- paste(paste("SELECT A.*, group_concat(page_nbr SEPARATOR ", delim, " AS page_nbr_concat ", sep = ""), 
#                      "FROM qwe AS A", sep = "")


## ------------------------------------------------------------- ##
## <!-- Grap page numbers for all SDTM variables by Domain.  --> ##
## ------------------------------------------------------------- ##
# crf_out_DM <- get_finalaCRFpages(dsin_crf = crf_out_unique[crf_out_unique$domain == "DM", ])
crf_out_VarTab_final <- get_finalaCRFpages(dsin_crf = crf_out_VarTab_unique, domain_list = domain_DefOrigin_VarTab_filtered)

# test <- sqldf("SELECT A.*, group_concat(page_nbr SEPARATOR) AS page_nbr_concat
#                       FROM qwe AS A")

# ddply(qwe, .(page_nbr), summarize, C = toString(C))

## save file
fileout_name <- paste(location_source, "sdtmVars_aCRF_all_", studyid, ".xlsx", sep = "")
sheetout_name <- paste("sdtmVars_aCRF_all", studyid, sep = "")

write.xlsx(x = crf_out_VarTab_final, file = fileout_name, sheetName = sheetout_name,
           row.names = FALSE, col.names = TRUE)


## ------------------------------------------------------------------------------- ##
## <!-- Keep page numbers of SDTM variables only found in Define Origin file.  --> ##
## ------------------------------------------------------------------------------- ##
defineOrigin_variableTab_mainSDTM <- subset(defineOrigin_variableTab3, Domain %in% c(domain_DefOrigin_VarTab_filtered))

defineOrigin_variableTab_mainSDTM2 <- sqldf("SELECT A.*, B.page_nbr_concat
                                            FROM defineOrigin_variableTab_mainSDTM AS A 
                                            LEFT JOIN crf_out_VarTab_final AS B 
                                            ON A.Domain = B.domain AND A.Variable = B.Variable")

## save file
fileout_name <- paste(location_source, "defineOrigin_variableTab_mainSDTM_", studyid, ".xlsx", sep = "")
sheetout_name <- paste("defineOrigin_variableTab_mainSDTM", studyid, sep = "")

write.xlsx(x = defineOrigin_variableTab_mainSDTM2, file = fileout_name, sheetName = sheetout_name,
           row.names = FALSE, col.names = TRUE)

## ---------------------------------------- ##
## <!-- Get page numbers for VLMTab.   -->  ##
## ---------------------------------------- ##

# test <- str_extract(qwe, "(\\w+)\\.(\\w+)\\s+(when)\\s+(\\w+)")

# qwe <- as.character(str_trim(crf_preprocess[crf_preprocess$page_nbr==54, "crf_txt"]))
qwe <- as.character(str_trim(crf_preprocess[crf_preprocess$page_nbr==58, "crf_txt"]))

# supp_domain <- unlist(str_extract(qwe, "^(SUPP)\\w+\\."))
# str_extract(qwe, "(\\w+)\\.(\\w+)\\s+(when)\\s+[^=]")
supp_domain <- str_extract(qwe, "(SUPP)\\w+")
supp_variable <- str_extract(qwe, "(QVAL)")
supp_VLM_variable <- str_extract(qwe, "(QNAM)|([^=])?")
supp_VLM_value1 <- str_extract(qwe, "\"(\\w+){2,}\"")
supp_VLM_value2 <- str_replace(supp_VLM_value1, "\"", "")
supp_VLM_value <- str_replace(supp_VLM_value2, "\"", "")

supp_domain_f <- unique(supp_domain[!is.na(supp_domain)])
supp_variable_f <- unique(supp_variable[!is.na(supp_variable)])
supp_VLM_variable_f <- unique(supp_VLM_variable[!is.na(supp_VLM_variable)])
supp_VLM_value_f <- unique(supp_VLM_value[!is.na(supp_VLM_value)])

supp_VLMTab <- paste(supp_domain_f, ".", supp_variable_f, " when ", supp_VLM_variable_f, " = ", supp_VLM_value_f, 
                     sep = "")

# supp_info1 <- data.frame(supp_domain=supp_domain, supp_variable=supp_variable, supp_VLM_variable=supp_VLM_variable, supp_VLM_value=supp_VLM_value, na.strings="NA")
# cols <- names(supp_info1)
# supp_info2 <- supp_info1[supp_info1 == "NA"] <- NA

# supp_info2 <- supp_info1[complete.cases(supp_info1[,1:4]), ]
# supp_info2 <- na.omit(supp_info1)


# supp_info2$flg_na <- apply(supp_info1[1:2,], 1, function(x) {
#   # count_na <- sum(is.na(supp_info1[x,])); print(count_na)
#   # if (count_na == 4) {
#   #   TRUE
#   # } else {
#   #   FALSE
#   # }
#   # count_na <- 0
# 
#   if(any(as.character(supp_info1[x, ]) != "NA")) {
#     FALSE
#   } else {
#     TRUE
#   }
# })

supp_info2 <- subset(supp_info1, 
                     !is.na(supp_info1[,1]) & !is.na(supp_info1[,2]) & 
                       !is.na(supp_info1[,3]) & !is.na(supp_info1[,4]))



# supp_info2 <- as.data.frame(supp_info2)
# colnames(supp_info2) <- cols
# supp_info <- supp_info1[complete.cases(supp_info1), ]
# supp_info <- supp_info1[complete.cases(supp_info1[cols]), ]
supp_info <- supp_info1[complete.cases(supp_info1[, cols]), ]
supp_info




