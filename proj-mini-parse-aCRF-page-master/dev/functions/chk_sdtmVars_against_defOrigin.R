
chk_sdtmVars_against_defOrigin <- function (dsin_sdtmVars = NULL, dsin_defOrigin = NULL, 
                                            pgNbr_child = NULL, 
                                            pgNbr_parent = NULL) {
  
  # ## sort main table
  # dsin_sdtmVars <- dsin_sdtmVars %>%
  #   dplyr::arrange(domain, Variable)
  
  ## get all domains from input
  domain_all <- dsin_sdtmVars %>%
    dplyr::pull(domain)
  domain_all <- as.character(unique(domain_all))
  
  ## get strangely annotated SDTM variables by comparing against Define Origin file per domain
  # dsin_sdtmVars_fltr_all <- data.frame(domain=character(), Variable=character(), pgNbr_parent=character(),
  #                                      pgNbr_aCRF=character(), sdtm_vars=character())
  # dsin_sdtmVars_fltr_all <- data.table::data.table(domain=character(), Variable=character(), 
  #                                                  pgNbr_parent=character(),pgNbr_aCRF=character(), 
  #                                                  sdtm_vars=character())
  dsin_sdtmVars_fltr_all <- list()
  
  # dsin_sdtmVars_strange_all <- data.frame(domain=character(), Variable=character(), pgNbr_parent=character(),
  #                                         pgNbr_aCRF=character(), sdtm_vars=character())
  dsin_sdtmVars_strange_all <- list()
  
  for (domain in domain_all) {

    ## filter domain specific SDTM variables
    # query_domain <- paste("domain == ", sQuote(domain), sep = "")
    # query_domain <- parse(query_domain)
    
    sdtmVars_domain_aCRF <- dsin_sdtmVars %>%
      dplyr::filter((!!domain) == domain) %>%             ## Use variable names in functions of `dplyr` (https://stackoverflow.com/questions/24569154/use-variable-names-in-functions-of-dplyr/33450557)
      # dplyr::filter_(dsin_sdtmVars, query_domain) 
      dplyr::pull(Variable)
    # sdtmVars_domain_aCRF <- sort(as.character(unique(sdtmVars_domain_aCRF)))
    sdtmVars_domain_aCRF <- as.character(unique(sdtmVars_domain_aCRF))

    ## filter Define Origin SDTM variables
    dsin_defOrigin <- dplyr::tbl_df(dsin_defOrigin)
    
    sdtmVars_domain_defOrigin <- dsin_defOrigin %>%
      dplyr::mutate(domain = Domain) %>%
      dplyr::filter((!!domain) == domain) %>%      ## Use variable names in functions of `dplyr` (https://stackoverflow.com/questions/24569154/use-variable-names-in-functions-of-dplyr/33450557)
      dplyr::pull(Variable)
    # sdtmVars_domain_defOrigin <- sort(as.character(unique(sdtmVars_domain_defOrigin)))
    sdtmVars_domain_defOrigin <- as.character(unique(sdtmVars_domain_defOrigin))
    
    ## compare 
    sdtmVars_chk_result <- data.frame(domain = character(), Variable = character(), chk_flg = character())
    
    for (Variable in sdtmVars_domain_aCRF) {
      
      query_sdtm <- paste("(", Variable, ")$", sep = "")     ## match exact string in R
      
      chk_flg <- any(stringr::str_detect(sdtmVars_domain_defOrigin, query_sdtm))
      # chk_flg <- any(stringr::str_detect(sdtmVars_domain_defOrigin, fixed(query_sdtm)))    
      sdtmVars_chk_result_tmp <- cbind(domain, Variable, chk_flg = chk_flg)
      
      sdtmVars_chk_result <- rbind(sdtmVars_chk_result, sdtmVars_chk_result_tmp)
    }
    
    ## -- merge main table to flag dataset
    
    ## change variable type before join to disable the ff warning msg from pkg::dplyr
    ## -- Warning message: Column `Variable` joining factors with different levels, coercing to character vector 
    dsin_sdtmVars$domain <- as.character(dsin_sdtmVars$domain)
    dsin_sdtmVars$Variable <- as.character(dsin_sdtmVars$Variable)
    sdtmVars_chk_result$domain <- as.character(sdtmVars_chk_result$domain)
    sdtmVars_chk_result$Variable <- as.character(sdtmVars_chk_result$Variable)
    
    dsin_sdtmVars_domain <- dsin_sdtmVars %>%
      filter((!!domain) == domain) %>%
      dplyr::left_join(select(sdtmVars_chk_result, c(Variable, chk_flg)), by = "Variable")    ## to remove default domain.x and domain.y columns
    
    ## -- split by chk_flg and to retun data frame
    # dsin_sdtmVars_domain_fltr_tmp <- dsin_sdtmVars_domain %>%
    #   filter(chk_flg == TRUE) %>%
    #   select(-c(chk_flg))                                      ## drop a single column using dyplyr 
    
    dsin_sdtmVars_fltr_all[[domain]] <- dsin_sdtmVars_domain %>%
      filter(chk_flg == TRUE) %>%
      select(-c(chk_flg))                                      ## drop a single column using dyplyr  
      
    # dsin_sdtmVars_domain_strange_tmp <- dsin_sdtmVars_domain %>%
    #   filter(chk_flg == FALSE) %>%
    #   select(-c(chk_flg))
    
    dsin_sdtmVars_strange_all[[domain]] <- dsin_sdtmVars_domain %>%
      filter(chk_flg == FALSE) %>%
      select(-c(chk_flg))
    
    ## -- combine
    # dsin_sdtmVars_domain_fltr_tmp$pgNbr_parent <- as.numeric(dsin_sdtmVars_domain_fltr_tmp$pgNbr_parent)
    # dsin_sdtmVars_domain_fltr_tmp$pgNbr_aCRF <- as.numeric(dsin_sdtmVars_domain_fltr_tmp$pgNbr_aCRF)
    # dsin_sdtmVars_domain_fltr_tmp$sdtm_vars <- as.character(dsin_sdtmVars_domain_fltr_tmp$sdtm_vars)
    
    # dsin_sdtmVars_fltr_all <- dplyr::bind_rows(dsin_sdtmVars_fltr_all, dsin_sdtmVars_domain_fltr_tmp)
    # dsin_sdtmVars_strange_all <- rbind(dsin_sdtmVars_strange_all, dsin_sdtmVars_domain_strange_tmp)
    
  }
  
  ## -- combine
  dsin_sdtmVars_fltr <- do.call(rbind, dsin_sdtmVars_fltr_all)
  dsin_sdtmVars_strange <- do.call(rbind, dsin_sdtmVars_strange_all)
  
  ## -- export strange SDTM variables
  dsin_sdtmVars_strange <- dsin_sdtmVars_strange %>%
    mutate(source = "child/parent pages")
  
  if (nrow(dsin_sdtmVars_strange) != 0) {
    xlsx::write.xlsx(x = dsin_sdtmVars_strange, file.path(output_dir, paste("strange_patterns_for_linkPages", "_", 
                                                                            "childPage = ", pgNbr_child, "_",  
                                                                            "parentPage = ", pgNbr_parent, "_",  
                                                                            studyid, "_",
                                                                            str_replace_all(format(Sys.time(), "%b %d %Y"), " ", "_"), 
                                                                            ".xlsx", sep = "")), 
                     col.names = T, row.names = F)
  }
  
  ## -- retun filtered dataset
  return(dsin_sdtmVars_fltr)
}