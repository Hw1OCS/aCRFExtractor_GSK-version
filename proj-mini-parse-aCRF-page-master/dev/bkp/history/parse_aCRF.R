
studyid <- "115158"

user = "test"
## setwd(paste("C:\\Users\\", user, "\\Desktop\\_<user>\\PortableApps\\labratory\\crf_page", sep = "")
## setwd(paste("/Users/", user, "/Desktop", sep = ""))

library(pdftools)
library(stringr)
library(tidyr)    ## to unlist variable
library(xlsx)     ## for reading/writing xls files
library(sqldf)
library(data.table)

file_name = "/Users/workuhm/Desktop/aCRF_files/blankcrf_ES_20170609.pdf"

## download.file(file_name, "test")

##pdf_toc(file_name)
##pdf_info(file_name)

pages = pdf_info(file_name)$pages 
crf_raw = pdf_text(file_name)
##crf_txt2 = pdf_render_page(file_name)

## ---------------------------------------------- ##
## <!-- Pre-process 1: Clean the meesy data.  --> ##
## ---------------------------------------------- ##
preprocess_crf = function(crf_txt_raw = NULL, def_OS = "windows") {
  
  crf_page2 = data.frame(a=numeric(), b=character())
  count = 0
  
  for (txtin in crf_txt_raw) {
    
    count = count + 1
    
    if (!(str_detect(def_OS, "windows"))) {
      # print("I am Mac")
      txtin_splt = unlist(strsplit(as.character(txtin), "\n"))
    }
    else {
      # print("I am Windows")
      txtin_splt = unlist(strsplit(as.character(txtin), "\r\n"))
    }
    
    txtin_splt = cbind(count, txtin_splt)
    
    crf_page2 = rbind(crf_page2, txtin_splt)
  }
  
  crf_page2 <- as.data.frame(crf_page2)
  colnames(crf_page2) <- c("page_nbr", "crf_txt")
  
  return(crf_page2)
  
}

crf_preprocess <- preprocess_crf(crf_txt_raw = crf_raw, def_OS = "mac")


## ---------------------------------------------------------- ##
## <!-- Pre-process 2: Extract domain specific variables. --> ##
## ---------------------------------------------------------- ##
# qwe = str_trim(as.character(crf_preprocess[crf_preprocess$page_nbr==8,]$crf_txt))
# demog <- crf_preprocess[crf_preprocess$page_nbr==8, ]
# 
# domain = "DM"

get_sdtmVars_pgNbr <- function (crf_pageIn = NULL, domain_list = NULL, 
                                until_pgNbr = NULL) {
  
  ## get total number of CRF pages
  if (!is.null(until_pgNbr)) {
    print("I am in until zone...")
    # total_crf_pages <- 8
    # total_crf_pages <- 51
    total_crf_pages <- until_pgNbr
  }
  else {
    print("I am in all pages")
    total_crf_pages <- dim(crf_pageIn)[1]
  }
  
  
  ## create an empty dataset
  crf_out <- data.frame(a = numeric(), b = character(), c = character())
  
  ## get SDTM vars and corresponding page number
  for (domain in domain_list) {
    
    query <- paste("(", domain, "\\.\\w+", ")", sep = ""); query
    
    for (i in seq(1:total_crf_pages)) {
      
      crf_page <- crf_pageIn[crf_pageIn$page_nbr == i, ]
      crf_page$crf_txt <- str_replace(string = crf_page$crf_txt, pattern = "\\s+", " ")    ## replace wide spaces
      
      #       crf_page$sdtm_vars <- ifelse(str_detect(crf_page$crf_txt, query), 
      #                                    str_extract(crf_page$crf_txt, query), 
      #                                    "NA")
      #       
      #       crf_page <- subset(crf_page[, c("page_nbr", "sdtm_vars")], crf_page$sdtm_vars != "NA")
      #       
      #       crf_page$domain <- domain
      #       
      #       ## concatenate domain result
      #       crf_out <- rbind(crf_out, crf_page)
      
      if (any(str_detect(crf_page$crf_txt, query))) {
        
        crf_page$sdtm_vars <- ifelse(str_detect(crf_page$crf_txt, query), 
                                     str_extract_all(crf_page$crf_txt, query), 
                                     "NA")
        #         crf_page$sdtm_vars <- ifelse(str_detect(crf_page$crf_txt, query), 
        #                                      unlist(str_extract_all(crf_page$crf_txt, query)), 
        #                                      "NA")
        crf_page <- unnest(data = crf_page, sdtm_vars)   ## unlist - hard coded (pkg: tidyr)
        
        crf_page <- subset(crf_page[, c("page_nbr", "sdtm_vars")], crf_page$sdtm_vars != "NA")
        
        crf_page$domain <- domain
        
        ## concatenate domain result
        crf_out <- rbind(crf_out, crf_page)
        
      }
    }
    
    #     crf_page$domain <- domain
    #     
    #     ## concatenate domain result
    #     crf_out <- rbind(crf_out, crf_page)
  }
  
  crf_out <- as.data.frame(crf_out)
  colnames(crf_out) <- c("page_nbr", "sdtm_vars", "domain")
  
  return(crf_out)
}

## run 
# crf_out <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess, domain_list = c("DM", "CM"), until_pgNbr = "51")
crf_out <- get_sdtmVars_pgNbr(crf_pageIn = crf_preprocess, domain_list = c("DM", "CM", "AE"))

## get SDTM vars without domain name
crf_out$Variable <- unlist(str_trim(str_extract(crf_out$sdtm_vars, "[^.]*$")))

## remove duplicates
# crf_out_unique <- crf_out[!duplicated(crf_out[, c("page_nbr", "Variable")]), c("page_nbr", "domain", "Variable")]
crf_out_unique <- crf_out[!duplicated(crf_out[, c("page_nbr", "Variable")]), c("page_nbr", "sdtm_vars", "domain", "Variable")]

## collapse page numbers
# delim <- paste("(\"", ", ", "\")", sep = "")
# query_pgNbr <- paste(paste("SELECT A.*, group_concat(page_nbr SEPARATOR ", delim, " AS page_nbr_concat ", sep = ""), 
#                      "FROM qwe AS A", sep = "")

get_finalaCRFpages <- function (dsin_crf = NULL) {
  
  # vars_list <- unique(crf_out_unique$Variable)
  # domain_list <- unique(crf_out_unique$domain)
  
  # domains <- dsin_crf[ , "domain"]
  # variables <- dsin_crf[ , "Variable"]
  variables <- dsin_crf[ , "sdtm_vars"]
  
  # domain_list <- unique(domains)
  vars_list <- unique(variables)
  
  # crf_out_final <- data.frame(domain = character(), Variable = character(), page_nbr_concat = character())
  crf_out_final <- data.frame(domain = character(), sdtm_vars = character(), Variable = character(), page_nbr_concat = character())
  
  # for (sel_domain in domain_list) {
  #   print(sel_domain)
  
  # query_sel <- paste("\"", sel_domain, "\"", sep = "")
  # dsin <- dsin_crf[dsin_crf$domain == query_sel, ]
  # dsin <- dsin_crf[data.table(dsin_crf)[, domain == sel_domain], ]
    
    for (var in vars_list) {
      print(var)
      # dsin <- crf_out_unique[(crf_out_unique$domain == sel_domain) & 
      #                          crf_out_unique$Variable == var, ]
      
      # dsin <- crf_out_unique[((crf_out_unique$domain == sel_domain) & (crf_out_unique$sdtm_vars == var)), ]
      # dsin <- dsin[dsin$sdtm_vars == var, ]
      dsin <- subset(dsin_crf,  sdtm_vars %in% var)
      print(dsin)
      
      query_pgNbr <- paste(paste("SELECT A.*, group_concat(page_nbr)", " AS page_nbr_concat ", sep = ""), 
                           "FROM dsin AS A", sep = "")
      
      crf_out_res <- sqldf(query_pgNbr)
      # crf_out_res <- crf_out_res[order(crf_out_res$domain), ]
      # crf_out_res <- crf_out_res[, !names(crf_out_res) %in% c("page_nbr", "domain")]
      crf_out_res <- crf_out_res[, !names(crf_out_res) %in% c("page_nbr")]
      crf_out_res$page_nbr_concat <- unlist(str_replace(crf_out_res$page_nbr_concat, ",", ", "))
      
      ## add domain
      # crf_out_res <- data.frame(domain = sel_domain, crf_out_res)
      # crf_out_res <- cbind(sel_domain, crf_out_res)
      
      ## concatenate result
      crf_out_final <- rbind(crf_out_final, crf_out_res)
    }
    
  # }
  
  crf_out_final <- as.data.frame(crf_out_final)
  
  return(crf_out_final)
}

crf_out_DM <- get_finalaCRFpages(dsin_crf = crf_out_unique[crf_out_unique$domain == "DM", ])

# test <- sqldf("SELECT A.*, group_concat(page_nbr SEPARATOR) AS page_nbr_concat
#                       FROM qwe AS A")

# ddply(qwe, .(page_nbr), summarize, C = toString(C))

## save file
fileout_name <- paste("/Users/workuhm/Desktop/aCRF_files/sdtmVars_aCRF_", studyid, ".xlsx", sep = "")
sheetout_name <- paste("sdtmVars_aCRF", studyid, sep = "")

write.xlsx(x = crf_out, file = fileout_name, sheetName = sheetout_name,
           row.names = FALSE, col.names = TRUE)


## -------------------------------------------------------- ##
## <!-- Merge aCRF variables and Define Origin file.   -->  ##
## -------------------------------------------------------- ##
filein_name <- paste("/Users/workuhm/Desktop/aCRF_files/", studyid, "_DEFINE_ORIGIN", ".xlsx", sep = "")
sheetin_name <- "Variable"

defineOrigin_variableTab <- read.xlsx(file = filein_name, sheetName = sheetin_name)

## keep main variables
defineOrigin_variableTab2 <- defineOrigin_variableTab[,c("Domain", "Variable", "Codelist")]

## remove codelist records and others
defineOrigin_variableTab3 <- defineOrigin_variableTab2[!(defineOrigin_variableTab2$Codelist %in% c("MedDRA", "GSKDD", "DOMAIN")), ]


## select domains
sel_domain <- "AE"

dsinOrig <- defineOrigin_variableTab3[defineOrigin_variableTab3$Domain == sel_domain, ]
dsinaCRF <- crf_out[crf_out$domain == sel_domain, ]

