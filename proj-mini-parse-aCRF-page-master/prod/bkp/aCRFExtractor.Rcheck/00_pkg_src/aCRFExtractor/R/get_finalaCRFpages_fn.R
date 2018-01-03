get_finalaCRFpages <- function (dsin_crf = NULL, domain_list = NULL, 
                                dsin_linkPages = NULL) {

  crf_out_final <- data.frame(domain = character(), sdtm_vars = character(), Variable = character(), page_nbr_concat = character())
  
  for (sel_domain in domain_list) {
    print(sel_domain)
    
    dsin_domain <- subset(dsin_crf, domain %in% sel_domain)
    
    variables <- dsin_domain[ , "sdtm_vars"]
    vars_list <- unique(variables)
    
    for (var in vars_list) {
      
      ## main page numbers
      dsin <- subset(dsin_domain,  sdtm_vars %in% var)
      
      ## add link pages, if exist
      if (any(str_detect(unique(dsin_linkPages$domain), sel_domain))) {
        dsin_linkPages_sel <- subset(dsin_linkPages,  sdtm_vars %in% var)
        
        ## combine all page numbers 
        dsin <- as.data.frame(rbind(dsin, dsin_linkPages_sel))
        dsin <- dsin[order(dsin[, "page_nbr"]), ]
      }
      
      query_pgNbr <- paste(paste("SELECT A.*, group_concat(page_nbr)", " AS page_nbr_concat ", sep = ""), 
                                 "FROM dsin AS A", sep = "")
      
      crf_out_res <- sqldf::sqldf(query_pgNbr)
      crf_out_res <- crf_out_res[, !names(crf_out_res) %in% c("page_nbr")]
      
      ## concatenate result
      crf_out_final <- rbind(crf_out_final, crf_out_res)
    }
    
  }
  
  crf_out_final$page_nbr_concat <- unlist(str_replace_all(crf_out_final$page_nbr_concat, ",", ", "))
  crf_out_final <- as.data.frame(crf_out_final)
  
  # message("END minning aCRF: ", date())
  
  return(crf_out_final)
}
