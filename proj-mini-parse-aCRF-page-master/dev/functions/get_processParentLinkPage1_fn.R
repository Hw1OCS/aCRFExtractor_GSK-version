get_processParentLinkPage1 <- function (aPage_parent = NULL, linkInfo_child = NULL, 
                                       pgNbr_child = NULL, pgNbr_parent = NULL, 
                                       dset_defOrigin = NULL) {
  
  ## -- seperate strings with extra space
  # aPage_parent <- as.character(unlist(stringr::str_split(aPage_parent, "\\s{3,}"))) 
  # aPage_parent <- as.character(unlist(stringr::str_split(aPage_parent, "\\s{3,}|(AND)"))) 
  # aPage_parent <- as.character(unlist(stringr::str_split(aPage_parent, regex("\\s{3,}|(AND)", ignore_case = TRUE)))) 
  
  # aPage_parent <- as.character(unlist(stringr::str_split(aPage_parent, regex("\\s{3,}|(AND)|\\=", ignore_case = TRUE))))     ## 23-Sep-2017
  aPage_parent <- as.character(unlist(stringr::str_split(aPage_parent, regex("\\s{3,}|(AND)|\\(|\\)", ignore_case = TRUE))))     ## 23-Sep-2017
  aPage_parent <- stringr::str_replace_all(string = aPage_parent, "\\(|\\)", "")      ## replace brackets
  
  # aPage_parent <- stringr::str_replace_all(string = aPage_parent, "\"", "")           ## replace double quotes

  ## -- remove strings stored in double quotes
  # aPage_parent <- stringr::str_replace_all(string = aPage_parent, pattern = '(?<=").*?(?=")', "")    ## replace double quotes  (SOURCE: https://stackoverflow.com/questions/32320081/how-to-extract-text-between-quotations-in-r)
  aPage_parent <- stringr::str_replace(string = aPage_parent, pattern = '(?<=").*?(?=")', "")          ## replace double quotes  (SOURCE: https://stackoverflow.com/questions/32320081/how-to-extract-text-between-quotations-in-r)
  
  aPage_parent <- stringr::str_trim(as.character(unlist(aPage_parent)))
  
  ## remove leading/trailing spaces
  # aPage_parent <- stringr::str_trim(aPage_parent)
  
  ## -- get link information from parent page, e.g., [SER02], [PBMC02], etc.
  # linkInfo_parent <- as.character(aPage_parent[which(stringr::str_detect(aPage_parent, "\\[\\w+\\]$"))])
  # linkInfo_parent <- as.character(stringr::str_trim(aPage_parent[which(stringr::str_detect(aPage_parent, "\\?"))]))
  # linkInfo_parent <- stringr::str_trim(stringr::str_replace_all(linkInfo_parent, "\\[\\w+\\]", ""))
  # linkInfo_parent <- unlist(stringr::str_extract_all(aPage_parent, "^[A-Z]{3,}\\s+[A-Z]{3,}"))
  
  ## preprocess child page
  # aPage_parent <- as.character(unlist(stringr::str_split(aPage_parent, "\\s{3,}")))
  
  # linkInfo_parent <- as.character(unlist(stringr::str_extract_all(aPage_parent, "^[[:upper:]]{3,}|^[[:upper:]]{3,}[\\s+\\d+]|^[[:upper:]]{3,}\\s+[[:upper:]]{3,}|^[[:upper:]]{3,}\\s+\\/\\s+[[:upper:]]{3,}|^[[:upper:]]{3,}\\/\\s+[[:upper:]]{3,}")))
  # linkInfo_parent <- unique(linkInfo_parent[!linkInfo_parent %in% c("FOR ANNOTATIONS", "FOR", "LABORATORY TESTS", 
  #                                                                   "ELIGIBILITY CHECK", "NOT SUBMITTED", 
  #                                                                   "NOT ", "NOT", "SUBMITTED ", 
  #                                                                   "SUBMITTED", "CONFIDENTIAL")])
  
  ## STOPPED, 11-jul-2016
  
  # linkInfo_parent <- as.character(unlist(stringr::str_extract_all(aPage_parent, 
  #                                                                "^[[:upper:]]{3,}|
  #                                                                    ^[[:upper:]]{3,}\\s+\\d+|
  #                                                                    ^[[:upper:]]{3,}\\s+[[:upper:]]{3,}|
  #                                                                    ^([[:upper:]]{3,}\\s+\\/\\s+[[:upper:]]{3,})|
  #                                                                    ^[[:upper:]]{3,}\\/\\s+[[:upper:]]{3,}")))
  
  # linkInfo_parent <- as.character(unlist(stringr::str_extract_all(aPage_parent, "^[[:upper:]]{3,}(((\\s+\\/\\s+)|(\\/\\s+)|(\\s+))[[:alnum:]]+)?")))     ## example: "ABC / DEF", "XYZ", "QDF 2", "GHI/ QRS", "AE.AESEV", "CONFIDENTIAL", "abx"  
  # 
  # linkInfo_parent <- unique(linkInfo_parent[!linkInfo_parent %in% c("FOR ANNOTATIONS", "FOR", "LABORATORY TESTS", 
  #                                                                "ELIGIBILITY CHECK", "NOT SUBMITTED", 
  #                                                                "NOT ", "NOT", "SUBMITTED ", 
  #                                                                "SUBMITTED", "CONFIDENTIAL",
  #                                                                "VACCINATION", "DAY")])
  
  # linkInfo_parent <- as.character(unlist(stringr::str_extract_all(aPage_parent, 
  #                                                                 "^[[:upper:]]{3,}(((\\s+\\/\\s+)|(\\/\\s+)|(\\s+))[[:alnum:]]+)?")))     ## example: "ABC / DEF", "XYZ", "QDF 2", "GHI/ QRS", "AE.AESEV", "CONFIDENTIAL", "abx"  

  # linkInfo_parent <- as.character(unlist(stringr::str_extract_all(aPage_parent, 
  #                                                                 "^[[:upper:]]{3,}(((\\s+\\/\\s+)|(\\/\\s+)|(\\s+))[[:alnum:]]+)?[^(\\w+\\.\\w+)]")))     ## example: "ABC / DEF", "XYZ", "QDF 2", "GHI/ QRS", "AE.AESEV", "CONFIDENTIAL", "abx"  
  
  # linkInfo_parent <- aPage_parent[which(stringr::str_detect(aPage_parent, "^[[:upper:]]{3,}[^\\w+\\.\\w+]"))]      ## example: "ABC / DEF", "XYZ", "QDF 2", "GHI/ QRS", "AE.AESEV", "CONFIDENTIAL", "abx"  
  # linkInfo_parent <- as.character(unlist(stringr::str_trim(linkInfo_parent)))
  
  # linkInfo_parent <- aPage_parent[which(stringr::str_detect(aPage_parent, "^[[:upper:]]{3,}[^\\w+\\.\\w+]"))]
  # linkInfo_parent <- aPage_parent[which(stringr::str_detect(aPage_parent, "^[[:upper:]]{3,}[^\\w+\\.\\w+]"))]
  
  # linkInfo_parent <- aPage_parent[which(stringr::str_detect(aPage_parent, "^[[:upper:]]{3,}"))]
  # linkInfo_parent <- aPage_parent[which(stringr::str_detect(aPage_parent, "^[[:upper:]]{3,}|(Epoch)"))]
  linkInfo_parent <- aPage_parent[which(stringr::str_detect(aPage_parent, "^[[:upper:]]{3,}|(CONFIDENTIAL)"))]      ## 23-Sep-2017: This is because sometimes annotations are done above Epoch and below CONFIDENTIAL
  
  ## -- combine if NA exist, e.g., "SUBMITTED        NA      (female of non childbearing potential or male)" 
  linkInfo_parent <- ifelse(stringr::str_detect(linkInfo_parent, "\\s+(NA)\\s+"), 
                            stringr::str_replace_all(linkInfo_parent, "\\s+(NA)\\s+", " "), 
                            linkInfo_parent)
  
  linkInfo_parent <- as.character(unlist(stringr::str_split(linkInfo_parent, "\\s{3,}|\\(\\w+\\)")))    ## to remove some strange patterns, e.g., "EXCLUSION CRITERIA                  IE.IECAT = \"EXCLUSION\""
  
  ## -- apply the same logic again on the new variable remove patterns like "IE.IECAT = \"EXCLUSION\""
  linkInfo_parent <- linkInfo_parent[which(!stringr::str_detect(linkInfo_parent, "^\\w+\\.\\w+"))]
  
  ## -- split because of some strange patterns, e.g., "VACCINATION                 2                          DAY 1                                                    |__|__|__|__|__|__|"
  # linkInfo_parent <- as.character(unlist(stringr::str_split(linkInfo_parent, "\\s{3,}"))) 
  # linkInfo_parent <- as.character(unlist(stringr::str_trim(linkInfo_parent)))
  
  ## -- search upcase again on linkInfo_parent dataset
  # linkInfo_parent <- linkInfo_parent[which(stringr::str_detect(linkInfo_parent, "^[[:upper:]]{3,}"))]
  # linkInfo_parent <- linkInfo_parent[which(stringr::str_detect(linkInfo_parent, "^[[:upper:]]{3,}|(Epoch)"))]
  
  ## -- remove SUPP&domain and RELREC variables, if exist
  # lst_commonVars <- intersect(linkInfo_parent, sdtmVars_supp_relrec)
  # linkInfo_parent_lst <- list()
  
  # for (commonVar in lst_commonVars) {
  #   linkInfo_parent <- linkInfo_parent[which(is.na(str_extract(linkInfo_parent, commonVar)))]
  # }
  
  for ( i in linkInfo_parent ) {
    
    ## -- To remove some strange patterns, e.g., "QNAM = \"PRDECMAK\""   
    if ( any(stringr::str_detect(i, sdtmVars_supp_relrec)) ) {
      linkInfo_parent <- linkInfo_parent[!linkInfo_parent %in% i]
    }
    
    ## -- To remove some strange patterns, e.g., "DSDECOD for subject disposition record (DS.DSCAT = \"DISPOSITION EVENT\") is"
    defOrig_sdtmVars <- dset_defOrigin %>%
      dplyr::filter(Domain %in% domain_DefOrigin_VarTab_filtered) %>%
      dplyr::pull(Variable)
    defOrig_sdtmVars <- as.character(unique(defOrig_sdtmVars))
    
    if ( any(stringr::str_detect(i, defOrig_sdtmVars)) ) {
      linkInfo_parent <- linkInfo_parent[!linkInfo_parent %in% i]
    }
  }
  
  # linkInfo_parent <- unique(linkInfo_parent[!linkInfo_parent 
  #                                           %in% c("FOR ANNOTATIONS", "FOR", "LABORATORY TESTS", 
  #                                                  "ELIGIBILITY CHECK", "NOT SUBMITTED", 
  #                                                  "NOT ", "NOT", "SUBMITTED ", 
  #                                                  "SUBMITTED", "CONFIDENTIAL", "DAY", 
  #                                                  "SUBMITTED          Yes" )])
  
  # linkInfo_parent <- as.character(unlist(stringr::str_replace(linkInfo_parent, 
  #                                                             "(FOR\\s+\\w+)|(ANNOTATIONS)|(LABORATORY TESTS)|
  #                                                             (ELIGIBILITY CHECK)|(NOT(\\s+\\w+)?)|(SUBMITTED(\\s+\\w+)?)|(CONFIDENTIAL)|
  #                                                             (DAY)", "")))
  
  # linkInfo_parent <- linkInfo_parent[which(!stringr::str_detect(linkInfo_parent, 
  #                                                              "(FOR\\s+\\w+)|(ANNOTATIONS)|(FOR ANNOTATIONS)|(LABORATORY TESTS)|(ELIGIBILITY CHECK)|(NOT(\\s+\\w+)?)|(SUBMITTED(\\s+\\w+)?)|(CONFIDENTIAL)|(DAY)"))]
  
  # linkInfo_parent <- linkInfo_parent[which(!stringr::str_detect(linkInfo_parent, 
  #                                                               "^(FOR\\s+\\w+)|(ANNOTATIONS)|^(FOR ANNOTATIONS)|(VACCINATION)|(LABORATORY TESTS)|(ELIGIBILITY CHECK)|(NOT(\\s+\\w+)?)|(SUBMITTED(\\s+\\w+)?)|(CONFIDENTIAL)|(DAY)|^\\||^(Yes)|^(Result)|(Date)|^(Normal)|^\\("|(NA)))]
  
  # linkInfo_parent <- linkInfo_parent[which(!stringr::str_detect(linkInfo_parent, "^(FOR\\s+\\w+)|(ANNOTATIONS)|^(FOR ANNOTATIONS)|(VACCINATION)|(LABORATORY TESTS)|(ELIGIBILITY CHECK)|(NOT(\\s+\\w+)?)|(SUBMITTED(\\s+\\w+)?)|(CONFIDENTIAL)|(DAY)|^\\||^(Yes)|^(Result)|(Date)|^(Normal)"))]
  linkInfo_parent <- linkInfo_parent[which(!stringr::str_detect(linkInfo_parent, "^(FOR\\s+\\w+)|(ANNOTATIONS)|^(FOR ANNOTATIONS)|(VACCINATION)|(GSK)|(OTH)|(UNK)|(SCREENING)|(ELIGIBILITY CHECK)|(NOT(\\s+\\w+)?)|(SUBMITTED(\\s+\\w+)?)|(DAY)|^\\||^(Yes)|^(Result)|(Date)|^(Normal)|(Study)"))]
  
  
  ## -- remove mising (lower and upper), e.g., "NHANES I criteria (see Appendix A of the protocol)."
  # linkInfo_parent <- as.character(unlist(stringr::str_extract_all(linkInfo_parent, "^[^a-z]*$")))          ## Determine if string is all caps with regular expression (https://stackoverflow.com/questions/2323988/determine-if-string-is-all-caps-with-regular-expression)
  
  linkInfo_parent <- as.character(unlist(stringr::str_split(linkInfo_parent, "\\s{3,}")))    ## to remove some strange patterns, e.g., "HEMATOLOGY                                                       S"
  
  ## remove vaccination/visit section text, e.g., "VACCINATION               2", "VISIT 1" 
  # linkInfo_parent <- linkInfo_parent[!linkInfo_parent %in% c(stringr::str_extract(linkInfo_parent, "^(VACCINATION)(\\s+[[:alnum:]])?"), stringr::str_extract(linkInfo_parent, "^(VISIT)(\\s+[[:alnum:]])?"))]
  
  ## -- remove at most one character string
  linkInfo_parent <- as.character(unlist(stringr::str_trim(linkInfo_parent)))
  linkInfo_parent <- linkInfo_parent[!nchar(linkInfo_parent) %in% c(0,1)]
  
  ## -- 23-Sep-2017: remove everything after equal sign (e.g., "DSCAT=\"\"" )
  linkInfo_parent <- stringr::str_extract(string = linkInfo_parent, pattern = "[^=]*")
  linkInfo_parent <- as.character(unlist(stringr::str_trim(linkInfo_parent)))
  
  ## -- conditional filtering
  ##  23-Sep-2017: flagged of
  # if ( any(stringr::str_detect(linkInfo_parent, "^(CONCOMITANT)|^(MEDICATION)|^(END OF CHALLENGE EPOCH)")) ) {
  #   # linkInfo_parent <- toupper(linkInfo_parent)
  #   linkInfo_parent <- linkInfo_parent
  # } else {
  #   # linkInfo_parent <- unique(toupper(linkInfo_parent))
  #   linkInfo_parent <- unique(linkInfo_parent)
  # }
  
  ## split linkInfo_parent and search for each string, e.g., "HEMATOLOGY / BIOCHEMISTRY"
  # linkInfo_parent_split <- as.character(unlist(stringr::str_split(linkInfo_parent, "\\s+|\\/")))
  # linkInfo_parent_split <- linkInfo_parent_split[!is.null(linkInfo_parent_split)]
  # linkInfo_parent_split <- linkInfo_parent_split[!linkInfo_parent_split == ""]
  # linkInfo_parent <- c(linkInfo_parent, linkInfo_parent_split)
    
  ## add last row info
  # linkInfo_parent <- c(linkInfo_parent, "LAST_ROW")
  
  # query_linkInfo <- paste("(", linkInfo_parent, ")", sep = "")
  # rows_linkInfo_parent <- stringr::str_detect(aPage_parent, query_linkInfo)
  
  ## -- get section page numbers from parent page
  rows_linkInfo_parent <- character()
  
  for (sect_parent in linkInfo_parent) {
    # query_linkInfo <- paste("(", sect_parent, ")", sep = "")
    # query_linkInfo <- paste("^(", sect_parent, "(\\s+)?", ")", sep = "")
    
    # print(paste("sect_parent = ", sect_parent, sep = ""))
    
    query_linkInfo <- paste("^(", sect_parent, ")", sep = "")
    
    # row_tmp <- which(stringr::str_detect(aPage_parent, query_linkInfo))
    if ( sect_parent %in% c("CHALLENGE", "VACCINATION") ) {
      row_tmp <- which.max(stringr::str_detect(aPage_parent, query_linkInfo))     ## NB. Find the index position of the first non-NA value in an R vector? (https://stackoverflow.com/questions/6808621/find-the-index-position-of-the-first-non-na-value-in-an-r-vector)
    } 
    else {
      row_tmp <- which(stringr::str_detect(aPage_parent, query_linkInfo))
    }
    
    # row_tmp <- which(stringr::str_detect(aPage_parent, regex(query_linkInfo, ignore_case = TRUE)))
    
    # aPage_parent_mdf <- as.character(unlist(stringr::str_split(aPage_parent, "\\s{3,}")))
    # row_tmp <- which(stringr::str_detect(aPage_parent_mdf, regex(query_linkInfo, ignore_case = TRUE)))      ## case insensetive pattern matching
    
    rows_linkInfo_parent <- c(rows_linkInfo_parent, row_tmp)
  }
  
  ## preprocess the row numbers
  rows_linkInfo_parent <- as.character(sort(as.numeric(unique(rows_linkInfo_parent)), decreasing = F))
  
  ## -- add last row info 
  linkInfo_parent <- toupper(linkInfo_parent)
  linkInfo_parent <- c(linkInfo_parent, "LAST_ROW")
  
  ## add last row page number
  rows_linkInfo_parent <- c(rows_linkInfo_parent, length(aPage_parent))
  
  ## -- merge child and parent sections and include page numbers 
  # linkInfo_parent_processed <- stringr::str_replace_all(linkInfo_parent, "\\s+|\\/", "")
  # linkInfo_parent_processed <- as.character(unlist(stringr::str_extract(linkInfo_parent, "\\w+[^\\s+\\/]")))
  linkInfo_parent_processed0 <- as.character(unlist(stringr::str_split(linkInfo_parent, "\\s{3,}")))     ## split cases like "HEMATOLOGY                                                             BIOCHEMISTRY"
  # linkInfo_parent_processed <- as.character(unlist(stringr::str_extract(linkInfo_parent_processed0, "\\w+[^\\s+\\/]")))
  linkInfo_parent_processed <- as.character(unlist(stringr::str_replace_all(linkInfo_parent_processed0, "(\\s+)|(\\/)|(\\s+\\/)]", "")))
  
  linkInfo_parent_df <- data.frame(linkInfo = linkInfo_parent_processed, rowNbr = as.numeric(rows_linkInfo_parent))
  linkInfo_parent_df$linkInfo <- as.character(linkInfo_parent_df$linkInfo)
  
  ## -- derive row range for each section
  # linkInfo_parent_df <- linkInfo_parent_df %>% 
  #   dplyr::mutate(leadRowNbr = as.numeric(lead(rowNbr))) %>%
  #   dplyr::mutate(rowNbrRange = paste(rowNbr, ":", leadRowNbr, sep = "")) %>%
  #   dplyr::filter(!linkInfo %in% c("LAST_ROW")) 
  
  # ## -- 23-Sep-2017
  if ( dim(linkInfo_parent_df)[1] > 3 ) {
    linkInfo_parent_df <- linkInfo_parent_df %>%
      dplyr::mutate(leadRowNbr = as.numeric(lead(rowNbr))) %>%
      dplyr::mutate(rowNbrRange = paste(rowNbr, ":", leadRowNbr, sep = ""))
  }
  
  ## -- 23-Sep-2017: Replace leadRowNbr='NA' for linkInfo='LAST_ROW' by the same value as the rowNbr
  # linkInfo_parent_df <- linkInfo_parent_df %>%
  #   mutate(leadRowNbr = ifelse(linkInfo_parent_df$linkInfo=="LAST_ROW", rowNbr, leadRowNbr))
  
  
  ## add last row info used for linking with parent page
  linkInfo_child_processed0 <- as.character(unlist(stringr::str_split(linkInfo_child, "\\s{3,}")))     ## split cases like "HEMATOLOGY                                                             BIOCHEMISTRY"
  # linkInfo_child_processed <- as.character(unlist(stringr::str_extract(linkInfo_child_processed0, "\\w+[^\\s+\\/]")))
  linkInfo_child_processed <- as.character(unlist(stringr::str_replace_all(linkInfo_child_processed0, "(\\s+)|(\\/)|(\\s+\\/)]", "")))
  linkInfo_child_processed <- c(linkInfo_child_processed, "LAST_ROW")
  
  linkInfo_child_df <- data.frame(linkInfo = linkInfo_child_processed)
  linkInfo_child_df$linkInfo <- as.character(linkInfo_child_df$linkInfo)
  
  linkInfo_merged <- linkInfo_child_df %>% 
    dplyr::inner_join(linkInfo_parent_df, by = 'linkInfo')
  
  ## -- 23-Sep-2017
  if ( dim(linkInfo_merged)[1] <= 3 ) {
    linkInfo_merged <- linkInfo_merged %>%
      dplyr::mutate(leadRowNbr = as.numeric(lead(rowNbr))) %>%
      dplyr::mutate(rowNbrRange = paste(rowNbr, ":", leadRowNbr, sep = ""))
  }
    
  # ## -- derive row range for each section
  # linkInfo_merged <- linkInfo_merged %>% 
  #   dplyr::mutate(leadRowNbr = as.numeric(lead(rowNbr))) %>%
  #   dplyr::mutate(rowNbrRange = paste(rowNbr, ":", leadRowNbr, sep = "")) %>%
  #   dplyr::filter(!linkInfo %in% c("LAST_ROW")) 
  
  ## -- grab SDTM variables within row ranges
  # linkInfo_sdtmVars <- aPage_parent %>% 
  #   dplyr::mutate(sdtmVars = aPage_parent[rowNbrRange])
  # linkInfo_sdtmVarsAll <- data.frame(a = character(), b = character(), c = numeric(), d = numeric())
  linkInfo_sdtmVarsAll <- data.frame(page_nbr=numeric(),  sdtm_vars=character(), domain=character(), Variable=character())
  
  # for (l in 1:dim(linkInfo_merged)[1]) {
  loop_rng <- dim(linkInfo_merged)[1] - 1
  
  for (l in 1:loop_rng) {              ## 23-Sep-2017: leading is applied on last_row
    # row_rng <- linkInfo_merged[l, "rowNbrRange"]
    # linkInfo_merged$sdtmTxt <- aPage_parent[noquote(row_rng)]
    row_start <- as.numeric(linkInfo_merged[l, "rowNbr"])
    row_end <- as.numeric(linkInfo_merged[l, "leadRowNbr"])
    row_txt <- aPage_parent[row_start:row_end]
    
    ## -- get all domains available in the current section
    row_txt <- as.character(unlist(stringr::str_split(row_txt, "\\s{3,}")))
    
    ## -- 23-Sep-2017: remove everything after equal sign (e.g., "DSCAT=\"\"" )
    # row_txt <- stringr::str_extract(string = row_txt, pattern = "[^=]*")
    # row_txt <- as.character(unlist(stringr::str_trim(row_txt)))
    
    # linkInfo_extract <- unique(as.character(unlist(stringr::str_extract_all(row_txt, "^\\w{2}\\.\\w{3,}"))))
    linkInfo_extract <- unique(as.character(unlist(stringr::str_extract_all(row_txt, "\\w+\\.\\w{3,}"))))        ## 23-Sep-2017
    
    # linkInfo_domains <- unique(as.character(unlist(stringr::str_extract_all(linkInfo_extract, "^\\w+[^\\.]"))))
    linkInfo_domains <- unique(as.character(unlist(stringr::str_extract_all(linkInfo_extract, "^\\w+[^\\.]"))))    ## 23-Sep-2017
    
    ## 23-Sep-2017: remove strange patterns for domain (e.g. "__" "VS" "CE")
    linkInfo_domains <- linkInfo_domains[!stringr::str_detect(linkInfo_domains, "__")]
    
    ## remove supplementary domains
    # linkInfo_domains_supps <- as.character(linkInfo_domains[!is.na(unlist(stringr::str_extract(linkInfo_domains, pattern = "^SUPP\\w+")))])
    linkInfo_domains_supps <- as.character(linkInfo_domains[!is.na(unlist(stringr::str_extract(linkInfo_domains, pattern = "^(SUPP\\w+)|^(RELREC)")))])   ## 23-Sep-2017
    # linkInfo_domains <- linkInfo_domains[!linkInfo_domains %in% c(linkInfo_domains_supps)]
    
    ## 23-Sep-2017
    if ( !identical(linkInfo_domains_supps, character(0)) ) {
      linkInfo_domains <- linkInfo_domains[!linkInfo_domains %in% c(linkInfo_domains_supps)]
    }
    
    ## -- check if STUDYID/VISITNUM exists inside the parent page
    chk_STUDYID <- any(stringr::str_detect(aPage_parent, "(STUDYID)"))
    chk_VISITNUM <- any(stringr::str_detect(aPage_parent, "(VISITNUM)"))
    
    ## -- grab domain specific SDTM variables, if there is at least one domain
    count_domains <- 0
    
    if ( !identical(linkInfo_domains, character(0)) ) {
      
      count_domains <- count_domains + 1
      
      for (domain in linkInfo_domains) {
        # query_domain <- paste("(", domain, "\\.", "\\w{3,}", ")", sep = "")
        query_domain <- paste("(", domain, "\\.", "\\w{3,}", ")", sep = "")
        # query_domain <- paste("^(", domain, "\\.", domain, "(?!\\_)\\w*", ")", sep = "")      ## 23-Sep-2017
        
        linkInfo_extract_domain <- unique(as.character(unlist(stringr::str_extract_all(row_txt, query_domain))))
        linkInfo_getVars_parent_fltr <- unique(as.character(unlist(stringr::str_extract_all(linkInfo_extract_domain, "[^.]\\w+$"))))
        
        # ## -- add STUYID/VISITNUM if exist
        # if (chk_STUDYID) {
        #   linkInfo_getVars_parent_fltr <- c("STUDYID", linkInfo_getVars_parent_fltr)
        #   linkInfo_extract_domain <- c("STUDYID", linkInfo_extract_domain)
        # }
        # if (chk_VISITNUM) {
        #   linkInfo_getVars_parent_fltr <- c("VISITNUM", linkInfo_getVars_parent_fltr)
        #   linkInfo_extract_domain <- c("VISITNUM", linkInfo_extract_domain)
        # }
        
        ## -- remove SDTM variables related to SUPP&domain and RELREC variables
        lst_commonVars <- intersect(linkInfo_getVars_parent_fltr, sdtmVars_supp_relrec)
        
        for (commonVar in lst_commonVars) {
          linkInfo_getVars_parent_fltr <- linkInfo_getVars_parent_fltr[which(is.na(str_extract(linkInfo_getVars_parent_fltr, commonVar)))]
          
          ## with domain, i.e., DS.QVAL
          query_domain <- paste(domain, ".", commonVar, sep = "")
          linkInfo_extract_domain <- linkInfo_extract_domain[which(is.na(str_extract(linkInfo_extract_domain, query_domain)))]
        }
        
        ## -- add STUYID/VISITNUM if exist
        if (chk_STUDYID) {
          linkInfo_getVars_parent_fltr <- c("STUDYID", linkInfo_getVars_parent_fltr)
          linkInfo_extract_domain <- c("STUDYID", linkInfo_extract_domain)
        }
        if (chk_VISITNUM) {
          linkInfo_getVars_parent_fltr <- c("VISITNUM", linkInfo_getVars_parent_fltr)
          linkInfo_extract_domain <- c("VISITNUM", linkInfo_extract_domain)
        }
        
        ## -- combine 
        # linkInfo_sdtmVars <- data.frame(domain = domain, Variable = linkInfo_getVars_parent_fltr,
        #                                 pgNbr_parent = pgNbr_parent, pgNbr_aCRF = pgNbr_child, 
        #                                 sdtm_vars = linkInfo_extract_domain)
        
        # linkInfo_sdtmVars <- data.frame(page_nbr=pgNbr_child, sdtm_vars = linkInfo_extract_domain, 
        #                                 domain = domain, Variable = linkInfo_getVars_parent_fltr)           ## 23-Sep-2017. The columns are re-ordered to be in-line with the other datasets.
        
        linkInfo_sdtmVars <- data.frame(page_nbr=as.numeric(pgNbr_child), sdtm_vars = linkInfo_extract_domain, 
                                        domain = domain, Variable = linkInfo_getVars_parent_fltr)     ## 30-Sep-2017
        
        ## -- remove duplicate values
        linkInfo_sdtmVars <- linkInfo_sdtmVars %>% dplyr::arrange(page_nbr, domain, Variable)     ## sort dataset to use dupplicated function
        linkInfo_sdtmVars <- linkInfo_sdtmVars[!duplicated(linkInfo_sdtmVars[, c("page_nbr", "sdtm_vars")]), ]
        
        ## -- combine 
        linkInfo_sdtmVarsAll <- rbind(linkInfo_sdtmVarsAll, linkInfo_sdtmVars)
        
      }
    }
  }
  
  ## 23-Sep-2017: Flagged because pgNbr_parent column is removed above
  # if ( count_domains != 0 ) {
  #   ## -- remove duplicate rows
  #   linkInfo_sdtmVarsAll <- linkInfo_sdtmVarsAll %>%
  #     dplyr::group_by(domain, Variable, pgNbr_parent, sdtm_vars) %>%
  #     dplyr::distinct(domain, Variable, pgNbr_parent, sdtm_vars, .keep_all = TRUE)
  #   
  #   ## -- sanity check for SDTM variables (e.g., LBORRE -> LBORRES)
  #   # linkInfo_sdtmVarsAll_fltr <- chk_sdtmVars_against_defOrigin(dsin_sdtmVars = linkInfo_sdtmVarsAll, 
  #   #                                                             dsin_defOrigin = dset_defOrigin,
  #   #                                                             pgNbr_child = pgNbr_child, 
  #   #                                                             pgNbr_parent = pgNbr_parent)
  # }
  
  
  # ## get rows for all linkVars in parent page
  # query_linkVars_parent <- paste("^(", linkInfo_parent, ")", sep = "")
  # rows_linkVars_parent <- which(stringr::str_detect(aPage_parent, query_linkVars_parent))
  # 
  # ## get rows for child linkVars
  # query_linkVars_child <- paste("^(", linkInfo_child, ")", sep = "")
  # rows_linkVars_parent <- which(stringr::str_detect(aPage_parent, query_linkVars_child))
  # 
  # for (linkInfo in linkInfo_parent) {
  #   query_sdtmVars <- paste("(", "\\w+\\.\\w+", ")", "|", linkInfo_child,  sep = "")
  #   
  #   if ( any(stringr::str_detect(aPage_parent, query_parent)) ) {
  #     sdtmVars_parent <- unlist(stringr::str_extract_all(aPage_parent, query_parent))
  #     
  #   }
  # }
  
  # return(linkInfo_sdtmVarsAll_fltr)
  
  # linkInfo_sdtmVarsAll <- as.data.frame(linkInfo_sdtmVarsAll)
  linkInfo_sdtmVarsAll <- as.data.frame(cbind(linkInfo_sdtmVarsAll, pgNbr_parent=pgNbr_parent))
  
  return(linkInfo_sdtmVarsAll)
  
}