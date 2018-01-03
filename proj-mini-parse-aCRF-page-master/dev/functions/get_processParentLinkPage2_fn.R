
get_processParentLinkPage2 <- function(aPage_child  = NULL, pgNbr_child = NULL,
                                       aPage_parent = NULL, pgNbr_parent = NULL,
                                       domain_list = NULL) {
  
  ## preprocessing
  # aPage_parent <- stringr::str_trim(aPage_parent)
  # aPage_parent <- stringr::str_replace(string = aPage_parent, pattern = "\\s+", " ")    ## replace wide spaces
  
  aPage_parent <- as.character(unlist(stringr::str_split(aPage_parent, regex("\\s{3,}", ignore_case = TRUE))))     ## 23-Sep-2017
  aPage_parent <- stringr::str_replace_all(string = aPage_parent, "\\(|\\)", "")
  
  aPage_parent <- stringr::str_replace(string = aPage_parent, pattern = '(?<=").*?(?=")', "")      ## replace double quotes  (SOURCE: https://stackoverflow.com/questions/32320081/how-to-extract-text-between-quotations-in-r)
  aPage_parent <- stringr::str_trim(as.character(unlist(aPage_parent)))
  
  ## create empty dataset to store page numbers
  dsout_all <- data.frame(page_nbr=numeric(),  sdtm_vars=character(), domain=character(), Variable=character())
  
  ## get SDTM vars and corresponding page number
  for (domain in domain_list) {
    
    ## list of SDTM variables without domain name, annotated differently
    # query <- paste("(", domain, "\\.\\w{3,}", ")", sep = "")
    query <- paste("(", domain, "\\.", domain, "(?!\\_)\\w+", ")", sep = "")        ## 23-Sep-2017: To avoid CDASH patterns (e.g., AE_DESC)
    # query <- paste("(", domain, ")", "\\.", domain, "(?!\\_)\\w+", sep = "")        ## 23-Sep-2017: To avoid CDASH patterns (e.g., AE_DESC)
    
    ## Start grabbing SDTM variables, if criterion is satisfied
    if ( any(stringr::str_detect(aPage_parent, query)) ) {
      
      ## -- start extract SDTM variables
      aPage_parent_fltr <- aPage_parent[stringr::str_detect(aPage_parent, query)]
      
      ## -- 23-Sep-2017: remove everything after equal sign (e.g., "DSCAT=\"\"" )
      aPage_parent_fltr <- stringr::str_extract(string = aPage_parent_fltr, pattern = "[^=]*")
      aPage_parent_fltr <- as.character(unlist(stringr::str_trim(aPage_parent_fltr)))
      
      ## remove unwanted patterns (e.g., "SUPPAE.QVAL")
      aPage_parent_fltr2 <- aPage_parent_fltr[!stringr::str_detect(aPage_parent_fltr, "^(SUPP)\\w+|^(RELREC)")]
      
      ## split every string to drop strange patterns (e.g., "AE.AESER = \"N\"")
      # aPage_parent_fltr3 <- as.character(unlist(stringr::str_split(string = aPage_parent_fltr2, pattern = "=")))
      # aPage_parent_fltr3 <- as.character(stringr::str_trim(aPage_parent_fltr3))
      
      ## final pattern
      # aPage_parent_fltr4 <- aPage_parent_fltr3[stringr::str_detect(aPage_parent_fltr3, query)]
      
      ## separator SDTM variable from its domain
      # aPage_parent_sdtmVars <- unique(as.character(unlist(stringr::str_extract_all(aPage_parent_fltr4, "[^.]\\w+$"))))
      aPage_parent_sdtmVars <- unique(as.character(unlist(stringr::str_extract_all(aPage_parent_fltr2, "[^.]\\w+$"))))
      
      ## -- add STUYID/VISITNUM if exist
      chk_STUDYID <- any(stringr::str_detect(aPage_parent, "(STUDYID)"))
      chk_VISITNUM <- any(stringr::str_detect(aPage_parent, "(VISITNUM)"))
      
      if (chk_STUDYID) {
        aPage_parent_sdtmVars <- c("STUDYID", aPage_parent_sdtmVars)
        # aPage_parent_fltr4 <- c("STUDYID", aPage_parent_fltr4)
        aPage_parent_fltr2 <- c("STUDYID", aPage_parent_fltr2)
      }
      if (chk_VISITNUM) {
        aPage_parent_sdtmVars <- c("VISITNUM", aPage_parent_sdtmVars)
        # aPage_parent_fltr4 <- c("VISITNUM", aPage_parent_fltr4)
        aPage_parent_fltr2 <- c("VISITNUM", aPage_parent_fltr2)
      }
      
      # dsout_tmp <- data.frame(page_nbr=pgNbr_child,  sdtm_vars=aPage_parent_fltr4, domain=domain, Variable=aPage_parent_sdtmVars)
      # dsout_tmp <- data.frame(page_nbr=pgNbr_child,  sdtm_vars=aPage_parent_fltr2, domain=domain, Variable=aPage_parent_sdtmVars)
      dsout_tmp <- data.frame(page_nbr=as.numeric(pgNbr_child),  sdtm_vars=aPage_parent_fltr2, domain=domain, Variable=aPage_parent_sdtmVars)    ## 30-Sep-2017
      
      ## -- remove duplicate values
      dsout_tmp <- dsout_tmp %>% dplyr::arrange(page_nbr, domain, Variable)     ## sort dataset to use dupplicated function
      dsout_tmp <- dsout_tmp[!duplicated(dsout_tmp[, c("page_nbr", "sdtm_vars")]), ]
      
      ## -- combine 
      dsout_all <- rbind(dsout_all, dsout_tmp)
    }
  }
  
  dsout_all <- as.data.frame(cbind(dsout_all, pgNbr_parent))
  
  return(dsout_all)
}