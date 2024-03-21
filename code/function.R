# mrcheck function --------------------------------------------------------

mrcheck <- function(data,
                    xi = 0.3,
                    rm_multi = TRUE,
                    cnm = c("occasion",
                            "date",
                            "time",
                            "tag_id",
                            "site",
                            "species",
                            "section",
                            "length",
                            "weight",
                            "recap",
                            "mortality",
                            "fin_clip",
                            "fin_recap",
                            "glued")) {
  
  pacman::p_load(tidyverse,
                 foreach)
  
  colnames(data) <- str_to_lower(colnames(data))
  
  ## check column input  
  if (any(colnames(data) == "tag_id2"))
    stop("tag_id2 exists; replace tag_id with tag_id2")
  
  if(sum(colnames(data) %in% cnm) != length(cnm))
    stop(paste("Missing column(s)",
               sQuote(paste(setdiff(cnm, colnames(data)),
                            collapse = ", "))
    )
    )
  
  ## drop rows with no tag_id
  if (any(is.na(data$tag_id))) {
    data <- drop_na(data, tag_id)
    message("Rows with NA tag_id were dropped")
  }
  
  ## check species consistency
  df_n <- data %>% 
    group_by(tag_id) %>% 
    summarize(n = n_distinct(species)) %>% 
    filter(n > 1)
  
  if (nrow(df_n) > 0) {
    message("Species ID check - ERROR: One or more tags have multiple species ID")
    return(df_n)
  } else {
    message("Species ID check - PASS: Each tag has unique species ID")
  }
  
  ## size - weight relationship
  sp_i <- with(data, unique(species) %>%
                 sort() %>%
                 na.omit())
  
  list_df <- foreach(i = seq_len(length(sp_i))) %do% {
    
    df_s <- filter(data, species == sp_i[i])
    
    m <- MASS::rlm(log(weight) ~ log(length) + factor(occasion),
                   df_s)
    
    return(mutate(df_s, rlm_weight = m$w))
  }
  
  df_w <- do.call(bind_rows, list_df)  
  
  if (any(with(df_w, rlm_weight) < xi)) {
    message("One or more data points have suspicious length or weight entries; check column 'rlm_weight'")
    df_q <- filter(df_w, rlm_weight < xi)
    return(df_q)
  }
  
  ## multiple captures of the same individuals within an occasion
  if (rm_multi) {
    df_multi <- df_w %>% 
      group_by(tag_id, occasion) %>% 
      summarize(n_obs = n()) %>% 
      ungroup() %>% 
      filter(n_obs > 1) %>% 
      arrange(occasion) %>% 
      data.table::data.table()
    
    if (nrow(df_multi) > 0) {
      message("Multiple (re)captures within an occasion exist; only last captures were retained")
      df_out <- df_w %>% 
        mutate(datetime = paste(date, time),
               datetime = as.POSIXct(datetime,
                                     format = "%m/%d/%Y %H:%M:%OS")) %>% 
        relocate(datetime) %>% 
        group_by(tag_id, occasion) %>% 
        slice(which.max(datetime)) %>% 
        ungroup()
      
      attr(df_out, 'multi_recap') <- df_multi
    } else {
      df_out <- df_w
    }
    
    return(df_out)
  } else {
    message("No changes were made to the data")
    return(data)
  }
  
}



# get first ---------------------------------------------------------------
# 
# getf <- function(x) min(which(x == 1))
# 
# get_nonna <- function(x) min(which(!is.na(x)))
# 





