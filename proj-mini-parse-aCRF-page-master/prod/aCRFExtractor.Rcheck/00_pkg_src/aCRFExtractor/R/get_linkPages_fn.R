get_linkPages <- function(dsin = NULL, dset_defOrigin = NULL) {

  nbrPgs_unq <- unique(dsin$page_nbr)
  # if (!is.null(debugme)) {
  #   nbrPgs_unq <- seq(1:debugme)
  # } else {
  #   nbrPgs_unq <- unique(dsin$page_nbr)
  # }

  # aCRFAll_link <- data.frame(a = character(), b = numeric(), c = numeric())
  dsetAll_parent <- data.frame(a = character(), b = character(), c = numeric(), d = numeric(), e = character())

  for (k in nbrPgs_unq) {

    aPage <- dsin[dsin$page_nbr == k, "crf_txt"]
    # aPage_prss <- data.frame(crf_txt = unlist(str_replace_all(aPage, "\\s+", " ")))
    # aPage_prss$row_nbr <- row(aPage_prss)

    # domain_desc <- str_trim(as.character(aPage_prss[aPage_prss$row_nbr == 6, "crf_txt"]))
    if (any(stringr::str_detect(aPage, "(SEE CRF PAGE)\\s+\\d+|(SEE CRF PAGES)\\s+\\d+\\s+(AND)\\s+\\d+"))) {
    # if (any(str_detect(aPage, "(SEE CRF PAGE|PAGES)\\s+\\d+"))) {
      # isParent_exist <- str_extract(aPage, "(SEE CRF PAGE)\\s+\\d+")
      isParent_exist <- stringr::str_extract(aPage, "(SEE CRF PAGE)\\s+\\d+|(SEE CRF PAGES)\\s+\\d+\\s+(AND)\\s+\\d+")
      isParent_exist_fltr <- as.character(isParent_exist[!is.na(isParent_exist)])
      # txt_parent <- as.character(aPage[which(!is.na(str_extract(aPage, "(SEE CRF PAGE)\\s+\\d+")))])
      pgNbr_parent <- as.character(unlist(str_extract_all(isParent_exist_fltr, "\\d+")))

      ## -- make a call to a parent page to grab all SDTM related variables
      for (pgNbr in pgNbr_parent) {

        ## -- get parent page
        # aPage_parent <- dsin[dsin$page_nbr == pgNbr_parent, "crf_txt"]
        aPage_parent <- dsin[dsin$page_nbr == pgNbr, "crf_txt"]

        ## check if the parent page is also linked to another parent page
        if (any(stringr::str_detect(aPage_parent, "(SEE CRF PAGE)\\s+\\d+|(SEE CRF PAGES)\\s+\\d+\\s+(AND)\\s+\\d+"))) {

          isParent_exist2 <- stringr::str_extract(aPage_parent, "(SEE CRF PAGE)\\s+\\d+|(SEE CRF PAGES)\\s+\\d+\\s+(AND)\\s+\\d+")
          isParent_exist2_fltr <- as.character(isParent_exist2[!is.na(isParent_exist2)])
          pgNbr_parent2 <- as.character(unlist(stringr::str_extract_all(isParent_exist2_fltr, "\\d+")))

          aPage_parent <- dsin[dsin$page_nbr == pgNbr_parent2, "crf_txt"]
        }
        # else {
        #   # aPage_parent <- dsin[dsin$page_nbr == pgNbr_parent, "crf_txt"]
        #   aPage_parent <- dsin[dsin$page_nbr == pgNbr, "crf_txt"]
        # }

        ## start extracting SDTM variables from parent page
        getVars_parent_tmp <- unlist(stringr::str_extract_all(aPage_parent, "(\\.\\w{3,})"))
        getVars_parent_tmp <- str_trim(stringr::str_replace(getVars_parent_tmp, ".", ""))
        getVars_parent_tmp <- getVars_parent_tmp[!is.na(getVars_parent_tmp)]
        getVars_parent_tmp <- stringr::str_extract(getVars_parent_tmp, "\\D+")
        getVars_parent <- getVars_parent_tmp[!is.na(getVars_parent_tmp) & !getVars_parent_tmp %in% c("USUBJID", "IDVARVAL")]

        ## -- get parent domain name
        for (domain in domain_DefOrigin_VarTab_filtered) {
          query <- paste("^", domain, sep = "")

          # if (any(str_detect(getVars_parent, domain))) {
          if (any(stringr::str_detect(getVars_parent, query))) {
            domain_parent <- domain
          }
        }

        ## get SDTM variables which are annotated in aCRF differently
        domains_defOrigin <- as.character(dset_defOrigin[dset_defOrigin$Domain == domain_parent & dset_defOrigin$Origin == "CRF Page", ]$Variable)
        lst_addVars <- sort(domains_defOrigin)[which(!(sort(domains_defOrigin) %in% sort(getVars_parent)))]

        for (addVars in lst_addVars) {

          addVars2 <- paste("(", addVars, ")", "|",
                            "(", substr(addVars, 3, nchar(addVars)), ")", sep = "")
          # print(addVars2)
          if (any(stringr::str_detect(aPage_parent, addVars2))) {
            # addVars <- unlist(str_extract_all(aPage_parent, addVars2))
            addVars3 <- as.character(unlist(stringr::str_extract_all(aPage_parent, addVars2)))

            # if (!any(str_detect(addVars3, domain_parent))) {
            if (any(stringr::str_detect(addVars3, domain_parent))) {
              addVars_mdf <- paste(domain_parent, addVars3, sep = "")

              ## add it to parent dataset
              getVars_parent <- sort(c(getVars_parent, addVars_mdf))
            }
          }
        }

        # dset_parent <- data.frame(domain = domain_parent, Variable = getVars_parent,
        #                           pgNbr_parent = pgNbr_parent, pgNbr_aCRF = k,
        #                           sdtm_vars = paste(domain_parent, getVars_parent, sep = "."))
        dset_parent <- data.frame(domain = domain_parent, Variable = getVars_parent,
                                  pgNbr_parent = pgNbr, pgNbr_aCRF = k,
                                  sdtm_vars = paste(domain_parent, getVars_parent, sep = "."))

        dsetAll_parent <- rbind(dsetAll_parent, dset_parent)
        # aCRFAll_link_tmp <- cbind(isParent_exist_fltr, link_pgNbr, k)
        #
        # aCRFAll_link <- rbind(aCRFAll_link, aCRFAll_link_tmp)

      }
    }
  }

  # aCRFAll_link <- as.data.frame(aCRFAll_link)
  # colnames(aCRFAll_link) <- c("link_txt", "pgNbr_link", "pgNbr_aCRF")

  ## concatenate (link) page numbers
  # query_pgNbr <- paste(paste("SELECT A.*, group_concat(pgNbr_aCRF)", " AS pgNbr_aCRF_concat ", sep = ""),
  #                      "FROM dsetAll_parent AS A GROUP BY domain, Variable",
  #                      sep = "")

  # dsetAll_parent_concat <- sqldf(query_pgNbr)

  return(dsetAll_parent)
}
