
get_pgsSpecialVars <- function (dsin_unique = NULL, 
                                dsin_crf = NULL,
                                variable_special = NULL, 
                                domain = NULL, 
                                linkPages_all = NULL) {
    
  # ## <!-- Save page numbers found in the final Define Origin file.  -->  ##
  # # if ( stringr::str_detect(variable_special, "STUDYID$") ) { 
  # if ( stringr::str_detect(variable_special, "STUDYID") ) {          ## 23-Sep-2017
  #   # print("I am in STUDYID...")
  #   # dsin_defOrig <- dsin_defOrigFinal[dsin_defOrigFinal$Variable %in% "STUDYID", c("Domain", "Variable", "page_nbr_concat")]
  #   dsin_defOrig <- dsin_defOrigFinal[dsin_defOrigFinal$Variable %in% "STUDYID", ]
  # } 
  # # else if ( stringr::str_detect(variable_special, "VISITNUM$") ) {
  # else if ( stringr::str_detect(variable_special, "VISITNUM") ) {
  #   # print("I am in VISITNUM...")
  #   # dsin_defOrig <- dsin_defOrigFinal[dsin_defOrigFinal$Variable %in% "VISITNUM", c("Domain", "Variable", "page_nbr_concat")]
  #   dsin_defOrig <- dsin_defOrigFinal[dsin_defOrigFinal$Variable %in% "VISITNUM", ]
  # } 
  # else {
  #   # dsin_defOrig <- dsin_defOrigFinal[dsin_defOrigFinal$Variable %in% variable_special, c("Domain", "Variable", "page_nbr_concat")]
  #   dsin_defOrig <- dsin_defOrigFinal[dsin_defOrigFinal$Variable %in% variable_special, ]
  # }
  
  ## get page numbers of STUDYID variable
  pgs_specialVars <- data.frame(sdtm_vars = character(),
                                domain    = character(), 
                                Variable  = character(),
                                pgNbr_num = numeric())
  
    
  ## get min and max page numbers for specific domain
  crf_domain <- dsin_unique[dsin_unique$domain %in% domain, ]
  pgs_domain <- as.character(unique(crf_domain[, "page_nbr"]))
  
  ## grab pages for STUDYID variable from aCRF within the specified range
  if ( !identical(pgs_domain, character(0)) ) {
    
    ## check child/parent info in each page
    pgs_domain <- as.numeric(pgs_domain)
    
    for(pgNbr in pgs_domain) {
      
      ## select specific page
      aPage <- dsin_crf %>% 
        dplyr::filter(page_nbr %in% pgNbr) %>%
        dplyr::pull(crf_txt) %>%
        as.character()
      
      aPage_splt <- as.character(unlist(stringr::str_split(aPage, regex("\\s{3,}", ignore_case = TRUE))))
      
      ## check for special variables (STUDYID/VISITNUM)
      query_specialVars <- paste("(", variable_special, ")", sep = "")
      
      if ( any(stringr::str_detect(aPage_splt, query_specialVars)) ) {
        
        ## extract special variable 
        sdtm_vars <- aPage_splt[which(stringr::str_detect(aPage_splt, query_specialVars))]
        Variable <- stringr::str_extract(sdtm_vars, "[^\\.]*$")                                  ## get everything after dot symbol
        
        ## store final result
        pgs_specialVars_tmp <- data.frame(sdtm_vars=sdtm_vars,
                                          domain=domain, 
                                          Variable=Variable,
                                          pgNbr_num=pgNbr)
        
        pgs_specialVars <- rbind(pgs_specialVars, pgs_specialVars_tmp)
      }
    }
    
    ## -- get child pages that are linked to current parent pages
    pgs_specialVars_linkPages <- pgs_specialVars %>% 
      dplyr::inner_join(linkPages_all, by = 'pgNbr_num') %>%
      dplyr::arrange(pgNbr_child) %>%
      dplyr::select(sdtm_vars, domain, Variable, pgNbr_child) %>%
      plyr::rename(c('pgNbr_child' = 'pgNbr_num'))                  ## rename column name using pkg::plyr
    
    ## concatenate both datasets
    pgs_specialVars <- rbind(pgs_specialVars, pgs_specialVars_linkPages)
  }
  
  return(pgs_specialVars)
}