get_pgsSpecialVars <- function (dsin_unique = NULL, dsin_crf = NULL, dsin_defOrigFinal = NULL, 
                                variable = NULL, domain = NULL) {

    
  ## <!-- Save page numbers found in the final Define Origin file.  -->  ##
  if (str_detect(variable, "STUDYID$")) {
    # print("I am in STUDYID...")
    dsin_defOrig <- dsin_defOrigFinal[dsin_defOrigFinal$Variable %in% "STUDYID", c("Domain", "Variable", "page_nbr_concat")]
  } 
  else if (str_detect(variable, "VISITNUM$")) {
    # print("I am in VISITNUM...")
    dsin_defOrig <- dsin_defOrigFinal[dsin_defOrigFinal$Variable %in% "VISITNUM", c("Domain", "Variable", "page_nbr_concat")]
  } 
  else {
    dsin_defOrig <- dsin_defOrigFinal[dsin_defOrigFinal$Variable %in% variable, c("Domain", "Variable", "page_nbr_concat")]
  }
  
  ## get page numbers of STUDYID variable
  pgs_specialVars <- data.frame(a = character(), b = character(), pg_nbr = character(), src_pgNbr = character())
  
  # for (domain in domain_list) {      ## [Haile2Comment, 15-jun-2017]. The program handles per domain only
    # print(domain)
    
    ## get min and max page numbers for specific domain
    crf_domain <- dsin_unique[dsin_unique$domain %in% domain, ]
    pgs_domain <- as.character(unique(crf_domain[, "page_nbr"]))
    
    ## grab pages for STUDYID variable from aCRF within the specified range
    for (i in pgs_domain) {
      
      # crf_pg <- crf_preprocess[crf_preprocess$page_nbr == i, "crf_txt"]
      crf_pg <- dsin_crf[dsin_crf$page_nbr %in% i, "crf_txt"]
      
      query_specialVars <- paste("(", variable, ")", sep = "")        ## this is how studyid should be define: Variable="__\\.STUDYID"
      # if (any(str_detect(str_trim(crf_pg), "(__\\.STUDYID)"))) {
      if (any(str_detect(str_trim(crf_pg), query_specialVars))) {
        # print(domain)
        
        # crf_studyid <- str_extract(str_trim(crf_pg), "(__\\.STUDYID)")
        # pgs_specialVars_tmp <- cbind(domain, variable, i, src_pgNbr = "new")
        pgs_specialVars_tmp <- cbind(domain, variable, i)
        # pgs_studyid_tmp <- cbind(domain, variable, as.character(i), src_pgNbr = "new")
        pgs_specialVars <- rbind(pgs_specialVars, pgs_specialVars_tmp)
      } else {
        i <- ""
        # pgs_specialVars_tmp <- cbind(domain, variable, i, src_pgNbr = "new")
        pgs_specialVars_tmp <- cbind(domain, variable, i)
        pgs_specialVars <- rbind(pgs_specialVars, pgs_specialVars_tmp)
      }
    }
    
    ## -- concatenate new page numbers
    # print(pgs_specialVars)
    query_pgNbr <- paste(paste("SELECT A.*, group_concat(i)", " AS i_concat ", sep = ""),
                               "FROM pgs_specialVars AS A", sep = "")
    
    pgs_specialVars <- sqldf(query_pgNbr)
    pgs_specialVars$i_concat <- unlist(str_replace_all(pgs_specialVars$i_concat, ",", " "))
    
    ## replace comma strings
    # pgs_specialVars$i <- unlist(str_replace_all(pgs_specialVars$i_concat, ",", ", "))
    # pgs_specialVars <- pgs_specialVars[, !names(pgs_specialVars) %in% c("i_concat")]
    # pgs_specialVars <- as.data.frame(pgs_specialVars)
    
    ## -- get old page numbers and add to the new dataset
    pgs_defOrig_tmp <- dsin_defOrig[dsin_defOrig$Domain %in% domain, ]
    # pgs_defOrig_tmp$src_pgNbr <- "old"         ## [Haile2Fix, 15-jun-2017]. It does not work for VISITNUM in AE domain...
    
    ## modify the delimiter
    pgs_defOrig_tmp$page_nbr_concat <- unlist(str_replace_all(pgs_defOrig_tmp$page_nbr_concat, ", ", " "))
    pgs_defOrig_tmp <- as.data.frame(pgs_defOrig_tmp)
    
    colnames(pgs_defOrig_tmp) <- c("domain", "variable", "i")
    
    ## -- combine
    pgs_specialVars$i <- pgs_specialVars$i_concat
    pgs_specialVars <- pgs_specialVars[, !names(pgs_specialVars) %in% c("i_concat")]
    
    pgs_specialVars <- rbind(pgs_specialVars, pgs_defOrig_tmp)
  # }
  
  ## -- concatenate all page numbers (new and old)
  query_pgNbr2 <- paste(paste("SELECT A.*, group_concat(i)", " AS i_concat ", sep = ""),
                              "FROM pgs_specialVars AS A", sep = "")
  pgs_specialVars <- sqldf(query_pgNbr2)
  
  ## keep main vars
  pgs_specialVars$i <- pgs_specialVars$i_concat
  pgs_specialVars <- pgs_specialVars[, !names(pgs_specialVars) %in% c("i_concat")]
  
  ## replace comma strings
  i_new <- unlist(strsplit(as.character(pgs_specialVars$i), ",|\\s+"))
  i_new <- sort(as.numeric(i_new))
  i_new2 <- toString(as.character(i_new))
  
  pgs_specialVars$i_new <- i_new2
  
  ## -- final preparation step
  pgs_specialVars$i <- pgs_specialVars$i_new2
  pgs_specialVars <- pgs_specialVars[, !names(pgs_specialVars) %in% c("i_new2")]
  
  pgs_specialVars <- as.data.frame(pgs_specialVars)
  colnames(pgs_specialVars) <- c("Domain", "Variable", "pg_nbr_concat")
  
  ## -- add additional variables for later merging with defineOrigin_variableTab_mainSDTM2 dataset.
  ##    - [Haile2Fix, 15-jun-2017]. only handles STUDYID
  pgs_specialVars$Origin <- "CRF Page"
  
  ## -- studyid
  if (str_detect(variable, "STUDYID$") & !(nchar(pgs_specialVars$pg_nbr_concat) == 0)) {
    pgs_specialVars$Codelist <- "STUDYID"       
    pgs_specialVars$flg_further_check_needed <- "Pages for STUDYID is extracted differently since the pattern is different, i.e., __.STUDYID. Thanks!"  
  }  
  else {
    pgs_specialVars$Codelist <- ""
    pgs_specialVars$flg_further_check_needed <- ""
  }
  
  ## -- visitnum
  if (str_detect(variable, "VISITNUM$")) {
    pgs_specialVars$Codelist <- "VISITNUM"       
    pgs_specialVars$flg_further_check_needed <- "Pages for VISITNUM is extracted differently since the pattern is different, i.e., __.VISITNUM. Thanks!"  
  } 
  # else {
  #   pgs_specialVars$Codelist <- ""
  #   pgs_specialVars$flg_further_check_needed <- ""
  # }
  
  pgs_specialVars <- pgs_specialVars[, c(1, 2, 4, 5, 3, 6)]    ## reordering columns
  
  
  return(pgs_specialVars)
}