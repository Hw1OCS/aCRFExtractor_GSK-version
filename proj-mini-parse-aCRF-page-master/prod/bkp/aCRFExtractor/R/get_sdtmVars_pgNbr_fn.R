# get_sdtmVars_pgNbr <- function (crf_pageIn = NULL, domain_list = NULL, dset_defOrigin = NULL, 
#                                 until_pgNbr = NULL) {
get_sdtmVars_pgNbr <- function (crf_pageIn = NULL, domain_list = NULL, until_pgNbr = NULL) {
  
  message("Beginning minning aCRF: ", date())
  
  ## get total number of CRF pages
  if (!is.null(until_pgNbr)) {
    # print("Minning some pages in the aCRF file")
    total_crf_pages <- until_pgNbr
  }
  else {
    # print("Minning all pages in the aCRF file")
    total_crf_pages <- dim(crf_pageIn)[1]
  }
  
  
  ## create an empty dataset
  crf_out <- data.frame(a = numeric(), b = character(), c = character())
  crf_out_strangeAll <- data.frame(a = numeric(), b = character(), c = character(), d = character())
  
  ## get SDTM vars and corresponding page number
  for (domain in domain_list) {
    
    ## list of SDTM variables without domain name, annotated differently 
    # defOrigin_vars <- sort(as.character(dset_defOrigin[dset_defOrigin$Domain == domain & dset_defOrigin$Origin == "CRF Page", ]$Variable))
    # defOrigin_vars_flt <- defOrigin_vars[which(str_detect(defOrigin_vars, domain))]
    # defOrigin_vars_flt <- substr(defOrigin_vars_flt, 3, nchar(defOrigin_vars_flt))
    
    # query1 <- paste("(", domain, "\\.\\w{3,}", ")", sep = "")
    query1 <- paste("(", domain, "\\.\\w+", ")", sep = "")
    
    # query2 <- paste("(", defOrigin_vars_flt, ")|", sep = "")
    
    for (i in seq(1:total_crf_pages)) {
      
      # crf_page <- crf_pageIn[crf_pageIn$page_nbr == i, ]
      crf_page <- subset(crf_pageIn, page_nbr %in% i)
      
      crf_page$crf_txt <- str_replace(string = crf_page$crf_txt, pattern = "\\s+", " ")    ## replace wide spaces
      
      if ( any(str_detect(crf_page$crf_txt, query1)) ) {
        # print(i)
        crf_page$sdtm_vars <- ifelse(str_detect(crf_page$crf_txt, query1), 
                                     str_extract_all(crf_page$crf_txt, query1), 
                                     "NA")
        
        crf_page <- unnest(data = crf_page, sdtm_vars)   ## unlist - hard coded (pkg: tidyr)
        
        crf_page <- subset(crf_page[, c("page_nbr", "sdtm_vars")], crf_page$sdtm_vars != "NA")
        
        crf_page$domain <- domain
        
        ## concatenate domain result
        crf_out <- rbind(crf_out, crf_page)
      }
      # else if (any(str_detect(crf_page$crf_txt, query2))) {
      #   
      # }
    }
    
    ## get additional pages for SDTM variables which are annotated differently
    # crf_out$Variable <- as.character(unlist(str_extract(crf_out$sdtm_vars, "\\w+[^\\.]$")))
    
    
    # crf_out_vars <- sort(unique(crf_out$Variable))
    # defOrigin_vars <- sort(as.character(dset_defOrigin[dset_defOrigin$Domain == domain & dset_defOrigin$Origin == "CRF Page", ]$Variable))
    
  }
  
  ## get SDTM vars without domain name
  crf_out$Variable <- unlist(str_trim(str_extract(crf_out$sdtm_vars, "[^.]*$")))
  
  crf_out <- as.data.frame(crf_out)
  colnames(crf_out) <- c("page_nbr", "sdtm_vars", "domain", "Variable")
  
  ## get strange patterns
  crf_out_strange <- crf_out[crf_out$domain == crf_out$Variable, ]
  crf_out_strangeAll <- rbind(crf_out_strangeAll, crf_out_strange)
  write.xlsx(x = crf_out_strangeAll, file.path(output_dir, paste("defineOrigin_strange_patterns_pgNbrs", "_", studyid, ".xlsx", sep = "")), 
             col.names = T, row.names = F)
  
  # class(crf_out) <- "aCRF"
  
  # message("END minning aCRF: ", date())
  
  return(crf_out)
}