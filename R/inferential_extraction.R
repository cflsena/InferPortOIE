getTransitiveInference <- function(pattern)
{
  tran_pattern <- getLemmas(pattern)
  status <- FALSE
  
  if (grepl(IS_A, tran_pattern))
    status <- TRUE
  else if (grepl(PART_OF, tran_pattern) & grepl(SIN, tran_pattern))
    status <- TRUE
  else if (grepl(LOC, tran_pattern) & grepl(PART_OF, tran_pattern))
    status <- TRUE
  else if (grepl(LOC, tran_pattern) & grepl(SIN, tran_pattern))
    status <- TRUE
  
  status
}

getSymmetricalInference <- function(pattern)
{
  sym_pattern <- getLemmas(pattern)
  status <- 0
  
  if (grepl(SYM_3, sym_pattern))
    status <- 2
  else if (grepl(SYM_1, sym_pattern))
    status <- 2
  else if (grepl(SYM_2, sym_pattern))
    status <- 2
  else if (grepl(SYM_4, sym_pattern))
    status <- 1
  
  status
}

getInference <-
  function(left_arg,
           relations,
           right_arg,
           sentence_infos)
  {
    sym_left <- c()
    sym_right <- c()
    sym_rel <- c()
    
    tran_left <- c()
    tran_right <- c()
    tran_rel <- c()
    
    sym_inf <- 0
    tran_inf <- 0
    
    #symmetrical inference
    sym_pos <- 1
    for (iCount in 1:length(relations[[1]])) {
      for (jCount in 1:length(left_arg[[iCount]])) {
        for (kCount in 1:length(right_arg[[iCount]])) {
          pattern <-
            getLemmas(paste0(left_arg[[iCount]][jCount], relations[[1]][iCount], right_arg[[iCount]][kCount]))
          status <- getSymmetricalInference(pattern)
          if (status == 1) {
            sym_inf <- 1
            sym_left[sym_pos] <- right_arg[[iCount]][kCount]
            sym_rel[sym_pos] <- relations[[1]][iCount]
            sym_right[sym_pos] <- left_arg[[iCount]][jCount]
          } else if (status == 2) {
            sym_inf <- 1
            splited_rel <- str_split(relations[[1]][iCount], " ")[[1]]
            if (length(splited_rel) > 2) {
              aux <-
                paste0(sentence_infos[[1]][(min(relations[[2]][[iCount]]) + 1):(max(relations[[2]][[iCount]]))], collapse = "")
              sym_left[sym_pos] <-
                paste0(aux, " ", right_arg[[iCount]][kCount])
              sym_rel[sym_pos] <-
                sentence_infos[[1]][min(relations[[2]][[iCount]])]
              sym_right[sym_pos] <- left_arg[[iCount]][jCount]
            } else {
              sym_left[sym_pos] <- right_arg[[iCount]][kCount]
              sym_rel[sym_pos] <- relations[[1]][iCount]
              sym_right[sym_pos] <- left_arg[[iCount]][jCount]
            }
          }
          sym_pos <- sym_pos + 1
        }
      }
    }
    
    #transitive inference
    if (length(relations[[1]]) >= 2)
    {
      tran_pos <- 1
      for (iCount in 1:(length(relations[[1]]) - 1)) {
        for (jCount in iCount:length(relations[[1]])) {
          for (kCount in 1:length(left_arg[[iCount]])) {
            for (wCount in 1:length(right_arg[[iCount]])) {
              if (iCount != jCount) {
                rep_arg <-
                  str_split(right_arg[[iCount]][wCount], " ")[[1]] %in% str_split(left_arg[[jCount]][kCount], " ")[[1]]
                if (length(rep_arg[rep_arg != T]) == 0) {
                  pattern <-
                    paste0(left_arg[[iCount]][kCount],
                           relations[[1]][iCount],
                           left_arg[[jCount]][kCount],
                           relations[[1]][jCount],
                           right_arg[[jCount]][wCount])
                  status <- getTransitiveInference(pattern)
                  if (status) {
                    tran_inf <- 1
                    for (hCount in 1:length(left_arg[[iCount]])) {
                      for (lCount in 1:length(right_arg[[jCount]])) {
                        tran_left[tran_pos] <- left_arg[[iCount]][hCount]
                        tran_rel[tran_pos] <- relations[[1]][jCount]
                        tran_right[tran_pos] <-
                          right_arg[[jCount]][lCount]
                        tran_pos <- tran_pos + 1
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
    result <-
      list(
        list(sym_left, sym_rel, sym_right),
        list(tran_left, tran_rel, tran_right),
        sym_inf,
        tran_inf
      )
    result
  }