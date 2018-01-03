
get_pgsSpecialVars_main <- function(variable_special = NULL,
                                    domain_list = NULL,
                                    crf_out_VarTab_unique = NULL,
                                    crf_preprocess = NULL,
                                    # defineOrigin_variableTab_mainSDTM2 = NULL,
                                    linkPages_all = NULL) {
  
  ## -- Step 2. Start grabbing special variables (STUDYID/VISITNUM)
  # domain_list <- domain_DefOrigin_VarTab_filtered
  
  ## studyid
  # pgs_studyid_all <- data.frame(Domain = character(), Variable = character(), Codelist = character(),
  #                               Origin = character(), pg_nbr_concat = character(), flg_further_check_needed = character())
  pgs_specVar_all <- data.frame(sdtm_vars = character(),
                                domain    = character(), 
                                Variable  = character(),
                                pgNbr_num = numeric())
  
  ## visitnum
  # pgs_visitnum_all <- data.frame(Domain = character(), Variable = character(), Codelist = character(),
  #                                Origin = character(), pg_nbr_concat = character(), flg_further_check_needed = character())
  # pgs_visitnum_all <- data.frame(sdtm_vars = character(),
  #                                domain    = character(), 
  #                                Variable  = character(),
  #                                pgNbr_num = numeric())
  
  for(j in domain_list) {
    
    # print(j)
    
    # get_pgsSpecialVars(dsin_unique = crf_out_VarTab_unique, dsin_crf = crf_preprocess,
    #                    variable = "__\\.STUDYID", domain_list = domain_DefOrigin_VarTab_filtered)
    
    ## 23-Sep-2017: To incorporate other patterns for studyid (e.g., CM.STUDYID)
    other_ptrn <- paste("(", j, "\\.", variable_special, ")", sep = "")
    variable_special_ptrn <- paste("(__\\.", variable_special, ")", "|", other_ptrn, sep = "")

    # other_ptrn_visitnum <- paste("(", j, "\\.VISITNUM", ")", sep = "")
    # variable_ptrn_visitnum <- paste("(__\\.VISITNUM)", "|", other_ptrn_visitnum, sep = "")
    
    pgs_specVar <- get_pgsSpecialVars(dsin_unique = crf_out_VarTab_unique, 
                                      dsin_crf = crf_preprocess, 
                                      # dsin_defOrigFinal = defineOrigin_variableTab_mainSDTM2,
                                      # dsin_defOrigFinal = crf_out_VarTab_final,
                                      # variable_special = "__\\.STUDYID", domain = j)
                                      # variable_special = "STUDYID", domain = j, 
                                      variable_special = variable_special_ptrn, domain = j, 
                                      linkPages_all = linkPages_all)
    # print(pgs_studyid)
    
    ## visitnum
    # pgs_visitnum <- get_pgsSpecialVars(dsin_unique = crf_out_VarTab_unique, dsin_crf = crf_preprocess, 
    #                                    dsin_defOrigFinal = defineOrigin_variableTab_mainSDTM2,
    #                                    # dsin_defOrigFinal = crf_out_VarTab_final,
    #                                    # variable_special = "__\\.VISITNUM", domain = j)
    #                                    variable_special = "VISITNUM", domain = j, 
    #                                    linkPages_all = linkPages_all)
    
    ## -- combine domain datasets
    pgs_specVar_all <- rbind(pgs_specVar_all, pgs_specVar)
    # pgs_visitnum_all <- rbind(pgs_visitnum_all, pgs_visitnum)
  }
  
  ## -- Remove duplicate values (best approach using DISTINCT in pkg::dplyr)
  pgs_specVar_all_tbl <- dplyr::tbl_df(pgs_specVar_all) %>%
    dplyr::distinct(domain, pgNbr_num) %>%
    dplyr::mutate(Variable = variable_special)
  
  return(pgs_specVar_all_tbl)
}

