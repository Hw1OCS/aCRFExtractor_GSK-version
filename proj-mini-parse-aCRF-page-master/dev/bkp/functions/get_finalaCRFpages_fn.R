get_finalaCRFpages <- function (dsin_crf = NULL, domain_list = NULL) {
  
  # vars_list <- unique(crf_out_unique$Variable)
  # domain_list <- unique(crf_out_unique$domain)
  
  # domains <- dsin_crf[ , "domain"]
  # variables <- dsin_crf[ , "Variable"]
  # variables <- dsin_crf[ , "sdtm_vars"]
  
  # domain_list <- unique(domains); print(domain_list)
  # vars_list <- unique(variables)
  
  # crf_out_final <- data.frame(domain = character(), Variable = character(), page_nbr_concat = character())
  crf_out_final <- data.frame(domain = character(), sdtm_vars = character(), Variable = character(), page_nbr_concat = character())
  
  for (sel_domain in domain_list) {
    print(sel_domain)
    
    # query_sel <- paste("\"", sel_domain, "\"", sep = "")
    # dsin <- dsin_crf[dsin_crf$domain == query_sel, ]
    # dsin <- dsin_crf[data.table(dsin_crf)[, domain == sel_domain], ]
    
    dsin_domain <- subset(dsin_crf, domain %in% sel_domain)
    
    variables <- dsin_domain[ , "sdtm_vars"]
    vars_list <- unique(variables)
    # print(vars_list)
    
    for (var in vars_list) {
      # print(var)
      # dsin <- crf_out_unique[(crf_out_unique$domain == sel_domain) & 
      #                          crf_out_unique$Variable == var, ]
      
      # dsin <- crf_out_unique[((crf_out_unique$domain == sel_domain) & (crf_out_unique$sdtm_vars == var)), ]
      # dsin <- dsin[dsin$sdtm_vars == var, ]
      # dsin <- subset(dsin_crf,  sdtm_vars %in% var)
      dsin <- subset(dsin_domain,  sdtm_vars %in% var)
      # print(dsin)
      
      query_pgNbr <- paste(paste("SELECT A.*, group_concat(page_nbr)", " AS page_nbr_concat ", sep = ""), 
                           "FROM dsin AS A", sep = "")
      
      crf_out_res <- sqldf(query_pgNbr)
      # crf_out_res <- crf_out_res[order(crf_out_res$domain), ]
      # crf_out_res <- crf_out_res[, !names(crf_out_res) %in% c("page_nbr", "domain")]
      crf_out_res <- crf_out_res[, !names(crf_out_res) %in% c("page_nbr")]
      # crf_out_res$page_nbr_concat <- unlist(str_replace(crf_out_res$page_nbr_concat, ",", ", "))
      
      ## add domain
      # crf_out_res <- data.frame(domain = sel_domain, crf_out_res)
      # crf_out_res <- cbind(sel_domain, crf_out_res)
      
      ## concatenate result
      crf_out_final <- rbind(crf_out_final, crf_out_res)
    }
    
  }
  
  crf_out_final$page_nbr_concat <- unlist(str_replace_all(crf_out_final$page_nbr_concat, ",", ", "))
  crf_out_final <- as.data.frame(crf_out_final)
  
  return(crf_out_final)
}
