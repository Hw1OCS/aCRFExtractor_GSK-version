
get_pgsSpecialVars_main <- function (variable_special = NULL,
                                     domain_list = NULL,
                                     dsin_crf = NULL,
                                     linkPages_special = NULL) {
  
  pgs_specialVars <- data.frame(sdtm_vars = character(),
                                domain    = character(), 
                                Variable  = character(),
                                page_nbr = numeric())
  
  pgNbr_all <- unique(as.numeric(crf_preprocess$page_nbr))
  
  for (pgNbr in pgNbr_all) {
  # for (pgNbr in 1:15) {
    
    # print(paste("pgNbr: ", pgNbr, sep = ""))
    
    ## -- select specific page
    aPage <- dsin_crf %>% 
      dplyr::filter(page_nbr %in% pgNbr) %>%
      dplyr::pull(crf_txt) %>%
      as.character()
    
    aPage_splt <- as.character(unlist(stringr::str_split(aPage, regex("\\s{3,}", ignore_case = TRUE))))
    
    ## -- check for special variable (STUDYID/VISITNUM)
    ## first pattern (e.g., CM.STUDYID)
    query1_specialVars_tmp <- paste("(", domain_list, "\\.", variable_special, ")", sep = "")
    query1_specialVars <- paste(query1_specialVars_tmp, collapse = "|")                        ## Concatenate multiple characters/strings to single character (SOURCE: https://stackoverflow.com/questions/15249781/concatenating-characters-in-r)
    
    ## second pattern (e.g., __.STUDYID)
    query2_specialVars <- paste("(", "__\\.", variable_special, ")", sep = "")
    
    if ( any(stringr::str_detect(aPage_splt, query1_specialVars)) ) {
      # print("test 1 if")
      
      ## Extract domain which is the only one for that page
      sdtm_vars <- aPage_splt[which(stringr::str_detect(aPage_splt, query1_specialVars))]
      domain <- stringr::str_extract(sdtm_vars, "[^\\.]+")
      
      ## store final result
      pgs_specialVars_tmp <- data.frame(sdtm_vars=sdtm_vars,
                                        domain=domain, 
                                        Variable=variable_special,
                                        page_nbr=pgNbr)
      
      pgs_specialVars <- rbind(pgs_specialVars, pgs_specialVars_tmp)
      
    }
    else if ( any(stringr::str_detect(aPage_splt, query2_specialVars)) ) {
      # print("test 2 if")
      
      ## Get all possible domains available on that page
      # query2_sdtmVars_tmp <- paste("(", domain_list, "\\.", domain_list, "\\w+",")", sep = "")
      query2_sdtmVars_tmp <- paste("^(", domain_list, "\\.", "(", domain_list, ")?", "\\w+",")", sep = "")    ## optional pattern for DM (e.g., DM.AGE)
      query2_sdtmVars <- paste(query2_sdtmVars_tmp, collapse = "|")
      
      sdtm_vars <- aPage_splt[which(stringr::str_detect(aPage_splt, query2_sdtmVars))]
      sdtm_vars_fltr <- as.character(unlist(stringr::str_split(sdtm_vars, "\\s+")))
      
      ## Remove unwanted variables (e.g., SITEID/SUBJID)
      # sdtm_vars <- sdtm_vars[-c(which(stringr::str_detect(sdtm_vars, "(SITEID)|(SUBJID)|(USUBJID)")))]
      
      ## Get all possible domains
      # domain_all <- unique(as.character(unlist(stringr::str_extract_all(sdtm_vars, "^[^\\.]*"))))
      # domain_all <- unique(as.character(unlist(stringr::str_extract_all(sdtm_vars_fltr, "\\w+[^\\.]"))))
      variable_all <- sdtm_vars_fltr[which(stringr::str_detect(sdtm_vars_fltr, query2_sdtmVars))]
      domain_all <- unique(as.character(unlist(stringr::str_extract_all(variable_all, "^[^\\.]*"))))
      
      for ( domain in domain_all ) {
        # print(paste("domain: ", domain, sep = ""))
        
        ## Get specific domain info
        query3_sdtmVars <- paste("^(", domain, "\\.", "(", domain, ")?", "\\w+",")", sep = "")
        # sdtm_vars_domain <- sdtm_vars[which(stringr::str_detect(sdtm_vars, query3_sdtmVars))]
        sdtm_vars_domain <- sdtm_vars[which(stringr::str_detect(sdtm_vars_fltr, query3_sdtmVars))]
        
        ## store final result
        pgs_specialVars_tmp <- data.frame(sdtm_vars=sdtm_vars_domain,
                                          domain=domain, 
                                          Variable=variable_special,
                                          page_nbr=pgNbr)
        
        pgs_specialVars <- rbind(pgs_specialVars, pgs_specialVars_tmp)
      }
    }
  }
  
  ## -- Remove duplicate values (best approach using DISTINCT in pkg::dplyr)
  pgs_specialVars_tbl <- dplyr::tbl_df(pgs_specialVars) %>%
    # dplyr::select(domain, Variable, page_nbr) %>%
    dplyr::distinct(domain, page_nbr) %>%
    dplyr::mutate(Variable = variable_special)
  
  ## -- get child pages that are linked to current parent pages
  pgs_specialVars_final <- data.frame(domain    = character(), 
                                      page_nbr  = numeric(),
                                      Variable  = character())
  
  # pgs_specialVars_tbl2 <- pgs_specialVars_tbl %>% 
  #   dplyr::inner_join(linkPages_special, by = 'page_nbr') 
  
  # print("testing...")
  
  pgs_specialVars_tbl_srt <- dplyr::tbl_df(pgs_specialVars_tbl) %>%
    dplyr::arrange(domain, page_nbr)

  linkPages_special_srt <- dplyr::tbl_df(linkPages_special) %>%
    dplyr::arrange(domain, page_nbr) 
    
  # pgs_specialVars_tbl2 <- sqldf::sqldf('SELECT A.*, B.domain AS domain_b, B.page_nbr AS page_nbr_B 
  #                                      FROM        pgs_specialVars_tbl_srt AS A 
  #                                      INNER JOIN  linkPages_special_srt       AS B
  #                                      ON A.page_nbr = B.pgNbr_parent AND
  #                                         A.domain   = B.domain')
  
  for( domain_sel in domain_list ) {
    
    ## -- select specific domain info
    pgs_specialVars_domain <- pgs_specialVars_tbl_srt %>% 
      dplyr::filter((domain) %in% domain_sel) 
    
    linkPages_specialVars_domain <- linkPages_special_srt %>% 
      dplyr::filter((domain) %in% domain_sel) %>%
      dplyr::distinct(page_nbr, pgNbr_parent) %>%
      dplyr::mutate(domain = domain_sel, Variable = variable_special) %>%
      dplyr::select(domain, page_nbr, Variable) %>%
      dplyr::arrange(page_nbr)
    
    ## -- combine both datasets
    pgs_specialVars_tmp <- rbind(pgs_specialVars_domain, linkPages_specialVars_domain)
    pgs_specialVars_final <- rbind(pgs_specialVars_final, pgs_specialVars_tmp)
  }
  
  ## -- Get final dataset ready for export
  pgs_specialVars_final_tbl <- dplyr::tbl_df(pgs_specialVars_final) %>%
    dplyr::arrange(domain, page_nbr) %>%
    dplyr::distinct(domain, page_nbr) %>%
    dplyr::mutate(Variable = variable_special) %>%
    dplyr::arrange(domain, page_nbr)
  
  ## concatenate both datasets
  # pgs_specialVars <- rbind(pgs_specialVars, pgs_specialVars_linkPages)
  
  
  return(pgs_specialVars_final_tbl)
}