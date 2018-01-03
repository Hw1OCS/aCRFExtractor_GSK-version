get_linkPages <- function(dsin = NULL, dset_defOrigin = NULL) {

  message("aCRFExtractor is handling child/parent link pages in aCRF (e.g., FOR ANNOTATIONS SEE CRF PAGE XXX).")
  
  nbrPgs_unq <- unique(dsin$page_nbr)
  # if (!is.null(debugme)) {
  #   nbrPgs_unq <- seq(1:debugme)
  # } else {
  #   nbrPgs_unq <- unique(dsin$page_nbr)
  # }

  # aCRFAll_link <- data.frame(a = character(), b = numeric(), c = numeric())
  # dsetAll_parent <- data.frame(a = character(), b = character(), c = numeric(), d = numeric(), e = character())
  dset_parent_lst <- list()

  for (k in nbrPgs_unq) {
    
    # print(paste("k = ", k, sep = ""))

    aPage <- dsin[dsin$page_nbr == k, "crf_txt"]
    # aPage <- stringr::str_trim(aPage)
    
    ## -- get child page (needed for later processing tables in CRF page)
    aPage_child <- aPage
    aPage_child <- stringr::str_trim(aPage_child)
    
    # aPage <- as.character(unlist(stringr::str_split(aPage, "\\s{3,}|(AND)"))) 
    # aPage <- stringr::str_trim(aPage)
    
    # aPage_prss <- data.frame(crf_txt = unlist(str_replace_all(aPage, "\\s+", " ")))
    # aPage_prss$row_nbr <- row(aPage_prss)

    # domain_desc <- str_trim(as.character(aPage_prss[aPage_prss$row_nbr == 6, "crf_txt"]))
    # if ( any(stringr::str_detect(aPage, "(SEE CRF PAGE)\\s+\\d+|(SEE CRF PAGES)\\s+\\d+\\s+(AND)\\s+\\d+")) ) {
    if ( any(stringr::str_detect(aPage, "(SEE CRF PAGE)")) ) {       ## 30-Sep-2017
      
      # isParent_exist <- str_extract(aPage, "(SEE CRF PAGE)\\s+\\d+")
      # isParent_exist <- stringr::str_extract(aPage, "(SEE CRF PAGE)\\s+\\d+|(SEE CRF PAGES)\\s+\\d+\\s+(AND)\\s+\\d+")
      isParent_exist_fltr <- aPage[which(stringr::str_detect(aPage, "(SEE CRF PAGE)"))]     ## 30-Sep-2017
      # isParent_exist_fltr <- as.character(isParent_exist[!is.na(isParent_exist)])    ## 30-Sep-2017
      # txt_parent <- as.character(aPage[which(!is.na(str_extract(aPage, "(SEE CRF PAGE)\\s+\\d+")))])
      pgNbr_parent_all <- as.numeric(unlist(stringr::str_extract_all(isParent_exist_fltr, "\\d+")))
      
      ## -- as the parent page numbers are determined, now proceed with the spliting
      # aPage <- as.character(unlist(stringr::str_split(aPage, "\\s{3,}|(AND)")))
      aPage <- as.character(unlist(stringr::str_split(aPage, regex("\\s{3,}|(AND)", ignore_case = TRUE))))
      aPage <- stringr::str_trim(aPage)
      
      ## -- get link information from child page, e.g., [SER02], [PBMC02], etc.
      # linkInfo_child <- as.character(stringr::str_trim(aPage[which(stringr::str_detect(aPage, "\\[\\w+\\]$"))]))
      # linkInfo_child <- as.character(stringr::str_trim(aPage[which(stringr::str_detect(aPage, "\\?"))]))
      # linkInfo_child <- stringr::str_trim(stringr::str_replace_all(linkInfo_child, "\\[\\w+\\]", ""))
      # linkInfo_child <- unlist(str_extract_all(aPage, "^[A-Z]{3,}[\\s+\\/|\\/][[A-Z]{3,}]"))
      
      # linkInfo_child <- as.character(unlist(stringr::str_extract_all(aPage, "^[[:upper:]]{3,}\\s+[[:upper:]]{3,}|^[[:upper:]]{3,}\\s+\\/\\s+[[:upper:]]{3,}|^[[:upper:]]{3,}\\/\\s+[[:upper:]]{3,}")))
      # linkInfo_child <- unique(linkInfo_child[!linkInfo_child %in% c("FOR ANNOTATIONS", "LABORATORY TESTS", 
      #                                                                "ELIGIBILITY CHECK", "NOT SUBMITTED")])
      
      ## preprocess child page
      # aPage <- as.character(unlist(stringr::str_split(aPage, "\\s{3,}")))
      
      # linkInfo_child <- as.character(unlist(stringr::str_extract_all(aPage, 
      #                                                                "^[[:upper:]]{3,}|
      #                                                                ^[[:upper:]]{3,}\\s+\\d+|
      #                                                                ^[[:upper:]]{3,}\\s+[[:upper:]]{3,}|
      #                                                                ^([[:upper:]]{3,}\\s+\\/\\s+[[:upper:]]{3,})|
      #                                                                ^[[:upper:]]{3,}\\/\\s+[[:upper:]]{3,}")))
      
      # linkInfo_child <- as.character(unlist(
      #   stringr::str_extract_all(aPage, "^[[:upper:]]{3,}(((\\s+\\/\\s+)|(\\/\\s+)|(\\s+))[[:alnum:]]+)?")))     ## example: "ABC / DEF", "XYZ", "QDF 2", "GHI/ QRS", "AE.AESEV", "CONFIDENTIAL", "abx"  
      
      # linkInfo_child <- as.character(unlist(stringr::str_extract_all(aPage,
      #                                                                "^[[:upper:]]{3,}(((\\s+\\/\\s+)|(\\/\\s+)|(\\s+))[[:alnum:]]+)?[^\\w+\\.\\w+]")))     ## example: "ABC / DEF", "XYZ", "QDF 2", "GHI/ QRS", "AE.AESEV", "CONFIDENTIAL", "abx"
      
      # linkInfo_child_flg <- stringr::str_detect(aPage, "^[[:upper:]]{3,}[^\\w+\\.\\w+]")     ## example: "ABC / DEF", "XYZ", "QDF 2", "GHI/ QRS", "AE.AESEV", "CONFIDENTIAL", "abx"
      # linkInfo_child <- aPage[which(stringr::str_detect(aPage, "^[[:upper:]]{3,}[^\\w+\\.\\w+]"))]
      
      # linkInfo_child <- aPage[which(stringr::str_detect(aPage, "^[[:upper:]]{3,}"))]
      # linkInfo_child <- aPage[which(stringr::str_detect(aPage, "^[[:upper:]]{3,}|(Epoch)"))]
      linkInfo_child <- aPage[which(stringr::str_detect(aPage, "^[[:upper:]]{3,}|(CONFIDENTIAL)"))]      ## 23-Sep-2017: This is because sometimes annotations are done above Epoch and below CONFIDENTIAL
      
      ## -- combine if NA exist, e.g., "SUBMITTED        NA      (female of non childbearing potential or male)" 
      linkInfo_child <- ifelse(stringr::str_detect(linkInfo_child, "\\s+(NA)\\s+"), 
                               stringr::str_replace_all(linkInfo_child, "\\s+(NA)\\s+", " "), 
                               linkInfo_child)
      
      ## -- split because of some strange patterns, e.g., "VACCINATION                 2                          DAY 1                                                    |__|__|__|__|__|__|"
      linkInfo_child <- as.character(unlist(stringr::str_split(linkInfo_child, "\\s{3,}|\\(\\w+\\)"))) 
      
      ## -- apply the same logic again on the new variable remove patterns like "IE.IECAT = \"EXCLUSION\""
      linkInfo_child <- linkInfo_child[which(!stringr::str_detect(linkInfo_child, "^\\w+\\.\\w+"))]
      
      linkInfo_child <- as.character(unlist(stringr::str_trim(linkInfo_child)))
      
      ## -- search upcase again on linkInfo_parent dataset
      # linkInfo_child <- linkInfo_child[which(stringr::str_detect(linkInfo_child, "^[[:upper:]]{3,}"))]
      # linkInfo_child <- linkInfo_child[which(stringr::str_detect(linkInfo_child, "^[[:upper:]]{3,}|(Epoch)"))]
      linkInfo_child <- linkInfo_child[which(stringr::str_detect(linkInfo_child, "^[[:upper:]]{3,}|(CONFIDENTIAL)"))]      ## 23-Sep-2017: This is because sometimes annotations are done above Epoch and below CONFIDENTIAL
      
      # linkInfo_child <- unique(linkInfo_child[!linkInfo_child 
      #                                         %in% c("FOR ANNOTATIONS", "FOR", "LABORATORY TESTS", 
      #                                                "ELIGIBILITY CHECK", "NOT SUBMITTED", 
      #                                                "NOT ", "NOT", "SUBMITTED ", 
      #                                                "SUBMITTED", "CONFIDENTIAL", "DAY")])
      
      # linkInfo_child <- as.character(unlist(stringr::str_replace(linkInfo_child, "(FOR\\s+\\w+)|(ANNOTATIONS)|(LABORATORY TESTS)|(ELIGIBILITY CHECK)|(NOT(\\s+\\w+)?)|(SUBMITTED(\\s+\\w+)?)|(CONFIDENTIAL)|(DAY)", "")))
      # linkInfo_child <- linkInfo_child[which(!stringr::str_detect(linkInfo_child, 
      #                                                             "^(FOR\\s+\\w+)|(ANNOTATIONS)|^(FOR ANNOTATIONS)|(VACCINATION)|(LABORATORY TESTS)|(ELIGIBILITY CHECK)|(NOT(\\s+\\w+)?)|(SUBMITTED(\\s+\\w+)?)|(CONFIDENTIAL)|(DAY)|^\\|"))]
      
      # linkInfo_child <- linkInfo_child[which(!stringr::str_detect(linkInfo_child, "^(FOR\\s+\\w+)|(ANNOTATIONS)|^(FOR ANNOTATIONS)|(VACCINATION)|(LABORATORY TESTS)|(ELIGIBILITY CHECK)|(NOT(\\s+\\w+)?)|(SUBMITTED(\\s+\\w+)?)|(CONFIDENTIAL)|(DAY)|^\\||^(Yes)|^(Result)|(Date)|^(Normal)"))]
      # linkInfo_child <- linkInfo_child[which(!stringr::str_detect(linkInfo_child, "^(FOR\\s+\\w+)|(ANNOTATIONS)|^(FOR ANNOTATIONS)|(GSK)|(OTH)|(UNK)|(SCREENING)|(ELIGIBILITY CHECK)|(NOT(\\s+\\w+)?)|(SUBMITTED(\\s+\\w+)?)|(CONFIDENTIAL)|(DAY)|^\\||^(Yes)|^(Result)|(Date)|^(Normal)|(Study)"))]
      linkInfo_child <- linkInfo_child[which(!stringr::str_detect(linkInfo_child, "^(FOR\\s+\\w+)|(ANNOTATIONS)|^(FOR ANNOTATIONS)|(VACCINATION)|(GSK)|(OTH)|(UNK)|(SCREENING)|(ELIGIBILITY CHECK)|(NOT(\\s+\\w+)?)|(SUBMITTED(\\s+\\w+)?)|(DAY)|^\\||^(Yes)|^(Result)|(Date)|^(Normal)|(Study)"))]
      
      ## -- remove mising (lower and upper), e.g., "NHANES I criteria (see Appendix A of the protocol)."
      # linkInfo_child <- as.character(unlist(stringr::str_extract_all(linkInfo_child, "^[^a-z]*$")))          ## Determine if string is all caps with regular expression (https://stackoverflow.com/questions/2323988/determine-if-string-is-all-caps-with-regular-expression)
      
      linkInfo_child <- as.character(unlist(stringr::str_split(linkInfo_child, "\\s{3,}")))    ## to remove some strange patterns, e.g., "VACCINATION               2"  
      
      ## remove vaccination/visit section text, e.g., "VACCINATION", "VISIT 1" 
      # linkInfo_child <- linkInfo_child[!linkInfo_child %in% c(stringr::str_extract(linkInfo_child, "^(VACCINATION)(\\s+[[:alnum:]])?"), stringr::str_extract(linkInfo_child, "^(VISIT)(\\s+[[:alnum:]])?"))]
      
      ## remove at most one character string
      linkInfo_child <- as.character(unlist(stringr::str_trim(linkInfo_child)))
      linkInfo_child <- linkInfo_child[!nchar(linkInfo_child) %in% c(0,1)]
      
      linkInfo_child <- unique(toupper(linkInfo_child))
      
      # # isParent_exist <- str_extract(aPage, "(SEE CRF PAGE)\\s+\\d+")
      # isParent_exist <- stringr::str_extract(aPage, "(SEE CRF PAGE)\\s+\\d+|(SEE CRF PAGES)\\s+\\d+\\s+(AND)\\s+\\d+")
      # isParent_exist_fltr <- as.character(isParent_exist[!is.na(isParent_exist)])
      # # txt_parent <- as.character(aPage[which(!is.na(str_extract(aPage, "(SEE CRF PAGE)\\s+\\d+")))])
      # pgNbr_parent_all <- as.numeric(unlist(str_extract_all(isParent_exist_fltr, "\\d+")))

      ## -- make a call to a parent page to grab all SDTM related variables
      # dset_parent_lst <- list()
      
      for (pgNbr in pgNbr_parent_all) {

        ## -- get parent page
        # aPage_parent <- dsin[dsin$page_nbr == pgNbr_parent, "crf_txt"]
        aPage_parent <- dsin[dsin$page_nbr == pgNbr, "crf_txt"]
        
        ## -- get link information from parent page, e.g., [SER02], [PBMC02], etc.
        # linkInfo_parent <- as.character(stringr::str_trim(aPage_parent[which(stringr::str_detect(aPage_parent, "\\[\\w+\\]$"))]))
        
        # dset_parent <- get_processParentLinkPage(aPage_parent = aPage_parent, linkInfo_child = linkInfo_child, 
        #                                          pgNbr_child = k, pgNbr_parent = pgNbr,
        #                                          dset_defOrigin = dset_defOrigin)
        
        ## -- concatenate k (child page number) & pgNbr (parent page number)
        k_pgNbr <- paste(k, "_", pgNbr, sep = "")
        
        ## -- add to the main list
        # dset_parent_lst[[k_pgNbr]] <- get_processParentLinkPage(aPage_parent = aPage_parent, 
        #                                                         linkInfo_child = linkInfo_child, 
        #                                                         pgNbr_child = k, pgNbr_parent = pgNbr, 
        #                                                         dset_defOrigin = dset_defOrigin)
        
        ## -- determine first whether child page has a table (e.g., Please complete the following table)
        if( any(stringr::str_detect(string = aPage_child, pattern = "(following table)")) ) {
          ## call second link function for handling tables
          dset_parent_lst[[k_pgNbr]] <- get_processParentLinkPage2(aPage_child  = aPage_child, pgNbr_child = k,
                                                                   aPage_parent = aPage_parent, pgNbr_parent = pgNbr,
                                                                   domain_list = domain_DefOrigin_VarTab_filtered)
          
          out_csv <- as.data.frame(dset_parent_lst[[k_pgNbr]])
          # filename_csv <- paste()
          readr::write_csv(x = out_csv, path = paste("./output/", "func2_", k_pgNbr, ".csv", sep = ""), col_names = TRUE)
        }
        else {
          dset_parent_lst[[k_pgNbr]] <- get_processParentLinkPage1(aPage_parent = aPage_parent,
                                                                   linkInfo_child = linkInfo_child,
                                                                   pgNbr_child = k, pgNbr_parent = pgNbr,
                                                                   dset_defOrigin = dset_defOrigin)
          
          chk_dset_parent <- as.data.frame(dset_parent_lst[[k_pgNbr]])
          
          ## -------------------------------------------------------------------------------------------- ##
          ## NEW TASK: Sometimes a parent page could be linked to another parent (i.e., grandparent).     ##
          ##           For example, see studyId=114460 (child=130; parent=59; grandparent=16).            ##
          ## -------------------------------------------------------------------------------------------- ##
          if ( plyr::empty(chk_dset_parent ) ) {             ## check if data frame is empty (SOURCE: https://rdrr.io/cran/plyr/man/empty.html)
            
            ## get page numbers
            isGrandParent_exist <- stringr::str_extract(aPage_parent, "(SEE CRF PAGE)\\s+\\d+|(SEE CRF PAGES)\\s+\\d+\\s+(AND)\\s+\\d+")
            isGrandParent_exist_fltr <- as.character(isGrandParent_exist[!is.na(isGrandParent_exist)])
            # txt_parent <- as.character(aPage[which(!is.na(str_extract(aPage, "(SEE CRF PAGE)\\s+\\d+")))])
            pgNbr_GrandParent_all <- as.numeric(unlist(stringr::str_extract_all(isGrandParent_exist_fltr, "\\d+")))
            
            ## -- grab page numbers of SDTM variables from grandparent pages
            if ( length(pgNbr_GrandParent_all) > 1 ) {
              print("Warning message: There are multiple grandparent pages which are linked. Please grab the page numbers for that page manually. Thanks!")
              print(paste("Info: ", "child page: ", k, "; parent page: ",  pgNbr, "; grand parent pages: ", pgNbr_GrandParent_all, sep = ""))
            }
            else if ( length(pgNbr_GrandParent_all) == 1 ) {
              ## copy/paste previous pages, if condition is met
              pgNbrPrev <- pgNbr_parent_all[1]
              
              if ( pgNbr_GrandParent_all ==  pgNbrPrev) {
                k_pgNbrPrev <- paste(k, "_", pgNbrPrev, sep = "")
                dset_parent_lst[[k_pgNbr]] <- dset_parent_lst[[k_pgNbrPrev]]
                
                ## save result
                out_csv <- as.data.frame(dset_parent_lst[[k_pgNbr]])
                readr::write_csv(x = out_csv, path = paste("./output/", "func1_", k_pgNbr, ".csv", sep = ""), col_names = TRUE)
              }
              else {
                print("Warning message: The grand parent page is not the same as parent page. Please grab the page numbers for that page manually. Thanks!")
                print(paste("Info: ", "child page: ", k, "; parent page: ",  pgNbr, "; grand parent pages: ", pgNbr_GrandParent_all, sep = ""))
              }
            }
            else {
              print("Warning message: The aCRFExtractor was not able to grab page numbers from grand parent page. Please grab the page numbers for that page manually. Thanks!")
              print(paste("Info: ", "child page: ", k, "; parent page: ",  pgNbr, "; grand parent pages: ", pgNbr_GrandParent_all, sep = ""))
            }
          }
          
          ## save result
          out_csv <- as.data.frame(dset_parent_lst[[k_pgNbr]])
          readr::write_csv(x = out_csv, path = paste("./output/", "func1_", k_pgNbr, ".csv", sep = ""), col_names = TRUE)
        }
        
        ## check if the parent page is also linked to another parent page
        # if (any(stringr::str_detect(aPage_parent, "(SEE CRF PAGE)\\s+\\d+|(SEE CRF PAGES)\\s+\\d+\\s+(AND)\\s+\\d+"))) {
        # 
        #   isParent_exist2 <- stringr::str_extract(aPage_parent, "(SEE CRF PAGE)\\s+\\d+|(SEE CRF PAGES)\\s+\\d+\\s+(AND)\\s+\\d+")
        #   isParent_exist2_fltr <- as.character(isParent_exist2[!is.na(isParent_exist2)])
        #   pgNbr_parent2 <- as.character(unlist(stringr::str_extract_all(isParent_exist2_fltr, "\\d+")))
        # 
        #   aPage_parent <- dsin[dsin$page_nbr == pgNbr_parent2, "crf_txt"]
        # }
        
        # else {
        #   # aPage_parent <- dsin[dsin$page_nbr == pgNbr_parent, "crf_txt"]
        #   aPage_parent <- dsin[dsin$page_nbr == pgNbr, "crf_txt"]
        # }

        ## start extracting SDTM variables from parent page
        # getVars_parent_tmp <- unlist(stringr::str_extract_all(aPage_parent, "(\\.\\w{3,})"))
        # getVars_parent_tmp <- str_trim(stringr::str_replace(getVars_parent_tmp, ".", ""))
        # getVars_parent_tmp <- getVars_parent_tmp[!is.na(getVars_parent_tmp)]
        # getVars_parent_tmp <- stringr::str_extract(getVars_parent_tmp, "\\D+")
        # getVars_parent <- getVars_parent_tmp[!is.na(getVars_parent_tmp) & !getVars_parent_tmp %in% c("USUBJID", "IDVARVAL")]
        # getVars_parent <- sort(getVars_parent)
        
        # ## -- check if STUDYID/VISITNUM exists inside the parent page
        # chk_STUDYID <- any(stringr::str_detect(getVars_parent, "(STUDYID)"))
        # chk_VISITNUM <- any(stringr::str_detect(getVars_parent, "(VISITNUM)"))
        # 
        # if (chk_STUDYID) {
        #   getVars_parent <- getVars_parent[!getVars_parent %in% c("STUDYID")]
        # }
        # if (chk_VISITNUM) {
        #   getVars_parent <- getVars_parent[!getVars_parent %in% c("VISITNUM")]
        # }
        
        ## -- get all domains avaible inside parent page
        # domainAll_parent <- unique(substr(x = getVars_parent, start = 1, stop = 2))
        
        # for (domain in domain_DefOrigin_VarTab_filtered) {
        #   query <- paste("^", domain, sep = "")
        # 
        #   # if (any(str_detect(getVars_parent, domain))) {
        #   if (any(stringr::str_detect(getVars_parent, query))) {
        #     domain_parent <- domain
        #   }
        # }
        
        ## -- grab SDTM variables by domain
        # for (domain_parent in domainAll_parent) {
        #   
        #   ## filter SDTM variables for current domain
        #   getVars_parent_fltr <- sort(unique(getVars_parent[which(str_detect(getVars_parent, domain_parent))]))
        #   
        #   ## -- get SDTM variables which are annotated in aCRF differently
        #   # getVars_defOrigin <- as.character(dset_defOrigin[dset_defOrigin$Domain == domain_parent & dset_defOrigin$Origin == "CRF Page", ]$Variable)
        #   getVars_defOrigin <- as.character(dset_defOrigin[dset_defOrigin$Domain == domain_parent & dset_defOrigin$Origin %in% c("CRF Page", "Assigned", "Derived"), ]$Variable)
        #   getVars_defOrigin <- sort(getVars_defOrigin[!getVars_defOrigin %in% c("STUDYID", "VISITNUM", "USUBJID")])
        #   lst_addVars <- sort(getVars_defOrigin)[which(!(sort(getVars_defOrigin) %in% sort(getVars_parent)))]
        #   
        #   if (!identical(lst_addVars, character(0))) {     
        #     
        #     for (addVars in lst_addVars) {
        #       
        #       addVars2 <- paste("(", addVars, ")", "|",
        #                         "(", substr(addVars, 3, nchar(addVars)), ")", sep = "")
        #       # print(addVars2)
        #       if (any(stringr::str_detect(aPage_parent, addVars2))) {
        #         # addVars <- unlist(str_extract_all(aPage_parent, addVars2))
        #         addVars3 <- as.character(unlist(stringr::str_extract_all(aPage_parent, addVars2)))
        #         
        #         # if (!any(str_detect(addVars3, domain_parent))) {
        #         if (any(stringr::str_detect(addVars3, domain_parent))) {
        #           addVars_mdf <- paste(domain_parent, addVars3, sep = "")
        #           
        #           ## add it to parent dataset
        #           getVars_parent_fltr <- sort(c(getVars_parent_fltr, addVars_mdf))
        #         }
        #       }
        #     }
        #   }
        #   
        #   ## -- add STUYID/VISITNUM if exist
        #   if (chk_STUDYID) {
        #     getVars_parent_fltr <- c("STUDYID", getVars_parent_fltr)
        #   }
        #   if (chk_VISITNUM) {
        #     getVars_parent_fltr <- c("VISITNUM", getVars_parent_fltr)
        #   }
        #   
        #   # dset_parent <- data.frame(domain = domain_parent, Variable = getVars_parent_fltr,
        #   #                           pgNbr_parent = pgNbr_parent, pgNbr_aCRF = k,
        #   #                           sdtm_vars = paste(domain_parent, getVars_parent_fltr, sep = "."))
        #   dset_parent <- data.frame(domain = domain_parent, Variable = getVars_parent_fltr,
        #                             pgNbr_parent = pgNbr, pgNbr_aCRF = k,
        #                             sdtm_vars = paste(domain_parent, getVars_parent_fltr, sep = "."))
        #   
        
          # dsetAll_parent <- rbind(dsetAll_parent, dset_parent)
        #   # aCRFAll_link_tmp <- cbind(isParent_exist_fltr, link_pgNbr, k)
        #   #
        #   # aCRFAll_link <- rbind(aCRFAll_link, aCRFAll_link_tmp)
      #   }
      }
      
      ## -- combine all parent pages info
      # dset_parent <- do.call(rbind, dset_parent_lst)
      
      # print(dsetAll_parent)
    }
  }

  # aCRFAll_link <- as.data.frame(aCRFAll_link)
  # colnames(aCRFAll_link) <- c("link_txt", "pgNbr_link", "pgNbr_aCRF")

  ## concatenate (link) page numbers
  # query_pgNbr <- paste(paste("SELECT A.*, group_concat(pgNbr_aCRF)", " AS pgNbr_aCRF_concat ", sep = ""),
  #                      "FROM dsetAll_parent AS A GROUP BY domain, Variable",
  #                      sep = "")

  # dsetAll_parent_concat <- sqldf(query_pgNbr)
  
  dset_parent <- do.call(rbind, dset_parent_lst)

  return(dset_parent)
}
