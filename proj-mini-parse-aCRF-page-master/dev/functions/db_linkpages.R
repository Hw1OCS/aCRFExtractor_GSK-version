
db_linkPages <- function(crf_preprocess = NULL,
                         domain_list = NULL) {
  
  ## Define result dataframe
  # linkPages_all <- data.frame(page_nbr  = numeric(),
  #                             linkPage_txt = character(), 
  #                             # pgNbr_txt    = numeric(),
  #                             pgNbr_nbr    = numeric())
  
  # pgs_specialVars <- data.frame(sdtm_vars    = character(),
  #                               domain       = character(), 
  #                               Variable     = character(),
  #                               page_nbr     = numeric(),
  #                               linkPage_txt = character(),
  #                               pgNbr_nbr    = character())
  
  pgs_sdtmVars <- data.frame(sdtm_vars    = character(),
                             domain       = character(), 
                             Variable     = character(),
                             page_nbr     = numeric(),
                             linkPage_txt = character(),
                             pgNbr_nbr    = character())
  
  ## Get list of unique pages
  pgNbr_crf <- unique(crf_preprocess[, "page_nbr"])
  
  ## check child/parent info in each page
  for(pgNbr in pgNbr_crf) {
    
    # print(paste("pgNbr: ", pgNbr, sep = ""))
    
    ## select specific page
    aPage <- crf_preprocess %>% 
      dplyr::filter(page_nbr %in% pgNbr) %>%
      dplyr::pull(crf_txt) %>%
      as.character()
    
    aPage_splt <- as.character(unlist(stringr::str_split(aPage, regex("\\s{3,}", ignore_case = TRUE))))
    
    ## check child/parent page number
    if ( any(stringr::str_detect(aPage_splt, "(SEE CRF PAGE)")) ) {
      
      ## extract header info 
      linkPage_txt <- aPage_splt[which(stringr::str_detect(aPage_splt, "(SEE CRF PAGE)"))]
      
      ## extract page numbers for link pages
      pgNbr_txt_all <- as.character(unlist(stringr::str_extract_all(linkPage_txt, "\\d+")))
      pgNbr_nbr_all <- as.numeric(pgNbr_txt)
      
      ## -- check if there is any SDTM annotations
      query_sdtmVars_tmp <- paste("^(", domain_list, "\\.", "(", domain_list, ")?", "\\w+",")", sep = "")    ## optional pattern for DM (e.g., DM.AGE)
      query_sdtmVars <- paste(query_sdtmVars_tmp, collapse = "|")      
      
      sdtm_vars <- aPage_splt[which(stringr::str_detect(aPage_splt, query_sdtmVars))]
      sdtm_vars_fltr <- as.character(unlist(stringr::str_split(sdtm_vars, "\\s+")))
      sdtm_vars_fltr2 <- sdtm_vars_fltr[which(stringr::str_detect(sdtm_vars_fltr, query_sdtmVars))]
      
      ## Get all possible domains
      domain_all <- unique(as.character(unlist(stringr::str_extract_all(sdtm_vars_fltr2, "^[^\\.]*"))))
      # domain_all <- sdtm_vars_fltr[which(stringr::str_detect(sdtm_vars_fltr2, "^[^\\.]*"))]
      
      if ( !identical(domain_all, character(0)) ) {
        
        for ( domain in domain_all ) {
          
          # print(paste("domain: ", domain, sep = ""))
          
          ## Get specific domain info
          query3_sdtmVars <- paste("^(", domain, "\\.", "(", domain, ")?", "\\w+",")", sep = "")
          sdtm_vars_domain <- sdtm_vars_fltr2[which(stringr::str_detect(sdtm_vars_fltr2, query3_sdtmVars))]
          
          ## Get variable
          variable <- as.character(unlist(stringr::str_extract(sdtm_vars_domain, "[^\\.]*$")))
          
          ## store final result
          # pgs_specialVars_tmp <- data.frame(sdtm_vars=sdtm_vars_domain,
          #                                   domain=domain,
          #                                   Variable=variable,
          #                                   page_nbr=pgNbr,
          #                                   linkPage_txt=linkPage_txt,
          #                                   pgNbr_parent=pgNbr_nbr)
          
          pgs_sdtmVars_tmp <- data.frame(sdtm_vars=sdtm_vars_domain,
                                         domain=domain,
                                         Variable=variable,
                                         page_nbr=as.numeric(pgNbr),
                                         linkPage_txt = linkPage_txt)
          
          ## Get distinct rows
          # pgs_sdtmVars_tmp2 <- dplyr::tbl_df(pgs_sdtmVars_tmp) %>%
          #   distinct(domain, page_nbr)
          
          # pgs_sdtmVars_tmp3 <- pgs_sdtmVars_tmp %>%
          #   dplyr::mutate(pgs_sdtmVars_tmp=linkPage_txt) %>%
          #   dplyr::mutate(pgNbr_parent=as.character(paste(pgNbr_txt, collapse = " ")))
          
          # pgNbr_txt <- data.frame(pgNbr_txt)
          # linkPage_txt_df <- data.frame(linkPage_txt, pgNbr_txt)
          # 
          # pgs_sdtmVars_tmp3 <- data.frame(cbind(pgs_sdtmVars_tmp, linkPage_txt))
          # 
          # ## -- merge the two dataframes
          # pgs_sdtmVars_tmp4 <- pgs_sdtmVars_tmp3 %>%
          #   dplyr::inner_join(linkPage_txt_df, by="linkPage_txt")
          
          # pgs_sdtmVars_tmp4 <- pgs_sdtmVars_tmp3 %>% 
          #   tidyr::unnest(pgNbr_txt)
          
          pgs_sdtmVars_tmp2 <- list()
          
          for ( pgNbr_txt in pgNbr_txt_all ) {
            pgs_sdtmVars_tmp2[[pgNbr_txt]] <- cbind(pgs_sdtmVars_tmp, pgNbr_txt)
          }
          
          pgs_sdtmVars_tmp3 <- do.call(rbind, pgs_sdtmVars_tmp2)
          
          pgs_sdtmVars <- rbind(pgs_sdtmVars, pgs_sdtmVars_tmp3)
        }
        
      } else {     ## child pages where no SDTM annotations are made (e.g., pp=11)
        
        pgs_blankVars_tmp <- data.frame(sdtm_vars="",
                                        domain="",
                                        Variable="",
                                        page_nbr=as.numeric(pgNbr),
                                        linkPage_txt = linkPage_txt)
        
        # pgs_blankVars_tmp3 <- pgs_blankVars_tmp %>%
        #   dplyr::mutate(linkPage_txt=linkPage_txt) %>%
        #   dplyr::mutate(pgNbr_parent=as.character(paste(pgNbr_txt, collapse = " ")))
        
        # pgs_blankVars_tmp3 <- as.data.frame(cbind(pgs_blankVars_tmp, linkPage_txt, pgNbr_txt))
        
        pgs_blankVars_tmp2 <- list()
        
        for ( pgNbr_txt in pgNbr_txt_all ) {
          pgs_blankVars_tmp2[[pgNbr_txt]] <- cbind(pgs_blankVars_tmp, pgNbr_txt)
        }
        
        pgs_blankVars_tmp3 <- do.call(rbind, pgs_blankVars_tmp2)
        
        pgs_sdtmVars <- rbind(pgs_sdtmVars, pgs_blankVars_tmp3)
      }
      
      ## Get link page without SDTM variable annotation (i.e., blank link page) 
      # pgs_specialVars_tmp <- data.frame(sdtm_vars="",
      #                                   domain="",
      #                                   Variable="",
      #                                   page_nbr=as.numeric(pgNbr),
      #                                   linkPage_txt=linkPage_txt,
      #                                   pgNbr_parent=pgNbr_nbr)
      # 
      # pgs_specialVars <- rbind(pgs_specialVars, pgs_specialVars_tmp)
      # 
      # ## Combine datasets
      # pgs_specialVars <- rbind(pgs_specialVars, pgs_sdtmVars)
    }
  }

  ## remove dupplicate rows
  # pgs_specialVars_fltr <- pgs_specialVars %>%
  #   dplyr::distinct(domain, page_nbr, linkPage_txt, pgNbr_parent) 
  
  # return(pgs_specialVars_fltr)
  
  return(pgs_sdtmVars)
}