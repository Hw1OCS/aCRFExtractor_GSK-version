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
      
      # crf_page <- crf_pageIn[crf_pageIn$page_nbr == i, ]
      crf_page <- subset(crf_pageIn, page_nbr %in% i)
      
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
  
  ## get SDTM vars without domain name
  crf_out$Variable <- unlist(str_trim(str_extract(crf_out$sdtm_vars, "[^.]*$")))
  
  crf_out <- as.data.frame(crf_out)
  colnames(crf_out) <- c("page_nbr", "sdtm_vars", "domain", "Variable")
  
  return(crf_out)
}