getRelation <- function (sentence_infos, pos_info)
{
  finals_index_rels <- list()
  
  relations <- c()
  
  sent <-
    getPattern(sentence_infos, 2, 1, length(sentence_infos[[2]]))
  
  rels <- gregexpr(PATTERN, sent)
  
  verbs <- getIndex(sentence_infos, "v-", 2)
  
  index_rels <- c()
  
  len_index_rels <- c()
  
  index_verbs <- c()
  
  if (length(verbs) == 0)
  {
    relations <- list(NULL, NULL, 0)
    
  } else{
    for (iCount in 1:length(verbs))
    {
      index_verbs[iCount]    <- verbs[iCount]
      index_rels[iCount]     <- rels[[1]][iCount]
      len_index_rels[iCount] <-
        attr(rels[[1]], "match.length")[iCount]
    }
    
    pos <- 1
    pos_last_elem <- 1
    
    for (iCount in 1:length(index_rels))
    {
      aux_rel <-
        substring(sent, index_rels[iCount], index_rels[iCount] + len_index_rels[iCount] -
                    1)
      
      num_tags <- length(gregexpr(" ", aux_rel, fixed = T)[[1]])
      
      if (iCount != 1)
      {
        if (index_verbs[iCount] - index_verbs[iCount - 1] == 1)
        {
          pos_last_elem <- index_verbs[iCount] + num_tags - 1
          
          if (grepl("v-", sentence_infos[[2]][pos_last_elem], fixed = T) &
              grepl(" ,", sentence_infos[[1]][pos_last_elem + 1], fixed = F)) {
            result <- getNewRelation (pos_last_elem + 1, sentence_infos, 1)
            relations[pos] <- paste0(relations[pos], result[[1]])
            finals_index_rels[[pos]] <-
              c(min(finals_index_rels[[pos]]), result[[2]])
            next
          } else {
            relations[pos] <-
              paste0(relations[pos],
                     getPattern(sentence_infos, 1, index_verbs[iCount], pos_last_elem))
            finals_index_rels[[pos]] <-
              c(min(finals_index_rels[[pos]]), pos_last_elem)
            next
          }
        } else if (index_verbs[iCount] - pos_last_elem == 1 ||
                   index_verbs[iCount] - pos_last_elem == 2)
        {
          if (grepl("( prp| adv)", sentence_infos[[2]][index_verbs[iCount] - 1], fixed = F) ||
              grepl("( que| se)", sentence_infos[[1]][index_verbs[iCount] -
                                                      1], fixed = F))
          {
            pos_last_elem <- index_verbs[iCount] + num_tags - 1
            
            if (grepl("v-", sentence_infos[[2]][pos_last_elem], fixed = T) &
                grepl(" ,", sentence_infos[[1]][pos_last_elem + 1], fixed = F)) {
              result <- getNewRelation (pos_last_elem + 1, sentence_infos, 1)
              relations[pos] <- paste0(relations[pos], result[[1]])
              finals_index_rels[[pos]] <-
                c(min(finals_index_rels[[pos]]), result[[2]])
              next
            } else {
              relations[pos] <-
                paste0(
                  relations[pos],
                  getPattern(sentence_infos, 1, index_verbs[iCount], pos_last_elem)
                )
              finals_index_rels[[pos]] <-
                c(min(finals_index_rels[[pos]]), pos_last_elem)
              next
            }
          }
        }
        pos <- pos + 1
        pos_last_elem <- index_verbs[iCount] + num_tags - 1
        
        if (length(finals_index_rels) == 0) {
          relations[pos] <-
            getPattern(sentence_infos, 1, index_verbs[iCount], pos_last_elem)
          finals_index_rels[[pos]] <-
            c(index_verbs[iCount], pos_last_elem)
        } else {
          index_previous_rel <- index_verbs[iCount - 1]
          index_token_comma <-
            grep(" ,", sentence_infos[[pos_info]][(index_previous_rel + 1)], fixed = T)
          if (length(index_token_comma) == 0) {
            if ((index_verbs[iCount] - index_verbs[iCount - 1]) == 2) {
              token_initial_rel <-
                str_split(relations[pos - 1], gsub(" ", "", sentence_infos[[1]][index_previous_rel]))[[1]][1]
              token_final_rel <-
                paste0(sentence_infos[[1]][c((index_previous_rel + 1),
                                             (index_verbs[iCount]:pos_last_elem))], collapse = " ")
              relations[pos] <-
                paste0(token_initial_rel,
                       sentence_infos[[1]][index_previous_rel],
                       " ",
                       token_final_rel)
              finals_index_rels[[pos]] <-
                c(
                  min(finals_index_rels[[pos - 1]]),
                  index_previous_rel,
                  (index_token_comma + 1),
                  (index_verbs[iCount]:pos_last_elem)
                )
            } else {
              index_token <- index_verbs[iCount] + 1
              token <- sentence_infos[[1]][index_token]
              if (grepl(" ,", token)) {
                result <- getNewRelation (index_token, sentence_infos, 1)
                relations[pos] <- result[[1]]
                finals_index_rels[[pos]] <- result[[2]]
                
              } else {
                relations[pos] <-
                  getPattern(sentence_infos,
                             1,
                             index_verbs[iCount],
                             pos_last_elem)
                finals_index_rels[[pos]] <-
                  c(index_verbs[iCount], pos_last_elem)
              }
            }
          } else {
            index_token_comma <- index_previous_rel + 1
            if ((index_verbs[iCount] - index_verbs[iCount - 1]) == 3) {
              token_initial_rel <-
                str_split(relations[pos - 1], gsub(" ", "", sentence_infos[[1]][index_previous_rel]))[[1]][1]
              token_final_rel <-
                paste0(sentence_infos[[1]][c((index_token_comma + 1),
                                             (index_verbs[iCount]:pos_last_elem))], collapse = " ")
              relations[pos] <-
                paste0(token_initial_rel,
                       sentence_infos[[1]][index_previous_rel],
                       " ",
                       token_final_rel)
              finals_index_rels[[pos]] <-
                c(
                  min(finals_index_rels[[pos - 1]]),
                  index_previous_rel,
                  (index_token_comma + 1),
                  (index_verbs[iCount]:pos_last_elem)
                )
            } else {
              index_token <- index_verbs[iCount] + 1
              token <- sentence_infos[[1]][index_token]
              if (grepl(" ,", token)) {
                result <- getNewRelation (index_token, sentence_infos, 1)
                relations[pos] <- result[[1]]
                finals_index_rels[[pos]] <- result[[2]]
                
              } else {
                relations[pos] <-
                  getPattern(sentence_infos,
                             1,
                             index_verbs[iCount],
                             pos_last_elem)
                finals_index_rels[[pos]] <-
                  c(index_verbs[iCount], pos_last_elem)
              }
            }
          }
        }
      } else {
        index_token <- index_verbs[iCount] + 1
        token <- sentence_infos[[1]][index_token]
        
        if (grepl(" ,", token)) {
          result <- getNewRelation (index_token, sentence_infos, 1)
          relations[pos] <- result[[1]]
          finals_index_rels[[pos]] <- result[[2]]
          
        } else {
          pos_last_elem <- index_verbs[iCount] + num_tags - 1
          relations[pos] <-
            getPattern(sentence_infos, 1, index_verbs[iCount], pos_last_elem)
          finals_index_rels[[pos]] <-
            c(index_verbs[iCount], pos_last_elem)
        }
      }
    }
    relations <- relations[!is.na(relations)]
    
    for (iCount in 1:length(relations)) {
      for (jCount in 1:length(relations)) {
        if (iCount != jCount) {
          repeated_rel <-
            str_split(relations[iCount], " ")[[1]] %in% str_split(relations[jCount], " ")[[1]]
          if (length(repeated_rel[repeated_rel != T]) == 0) {
            relations <- relations[-iCount]
            finals_index_rels <- finals_index_rels[-iCount]
          }
        }
      }
    }
    
    finals_index_rels <- Filter(length, finals_index_rels)
    finals_rel <- list(relations, finals_index_rels, 1)
    finals_rel
  }
}

getNewRelation <- function (index_token, sentence_infos, pos_info)
{
  result <- list()
  flag <- 0
  
  if (length(grep(" ,", sentence_infos[[1]][(index_token + 1):length(sentence_infos[[pos_info]])], fixed = F)) == 0) {
    result <- list(sentence_infos[[1]][index_token - 1], index_token - 1)
  } else {
    pos_token <-
      grep(" ,", sentence_infos[[1]][1:length(sentence_infos[[pos_info]])], fixed = F)
    pos_token <- pos_token[length(pos_token)]
    
    if (grepl("v-", sentence_infos[[2]][pos_token + 1], fixed = T)) {
      new_rel <-
        as.String(sentence_infos[[2]][((pos_token + 1):(length(sentence_infos[[pos_info]]) -
                                                          1))])
      flag <- 1
    }
    else
      new_rel <-
      as.String(sentence_infos[[2]][c((index_token - 1), ((pos_token + 1):(length(
        sentence_infos[[pos_info]]
      ) - 1)))])
    
    new_rel <- gsub("\n", "", new_rel, fixed = F)
    new_rel <- paste0(new_rel, " ")
    new_rel <- str_trim(new_rel, "left")
    
    new_pattern_rel <- gregexpr(PATTERN, new_rel)
    
    index_verb    <- index_token - 1
    index_rel     <- new_pattern_rel[[1]][1]
    len_index_rel <- attr(new_pattern_rel[[1]], "match.length")[1]
    
    aux_rel <-
      substring(new_rel, index_rel, index_rel + len_index_rel - 1)
    
    if (flag) {
      tags_index <-
        c(((pos_token + 1):(length(
          sentence_infos[[pos_info]]
        ) - 1)))
      tags_index_pattern <-
        tags_index[1:(length((str_split(aux_rel, " ")[[1]])) - 1)]
      result <-
        list(paste0(sentence_infos[[1]][c(index_token - 1, tags_index_pattern)], collapse = " "),
             c(index_token - 1, tags_index_pattern))
    } else {
      tags_index <-
        c((index_token - 1), ((pos_token + 1):(length(
          sentence_infos[[pos_info]]
        ) - 1)))
      tags_index_pattern <-
        tags_index[1:(length((str_split(aux_rel, " ")[[1]])) - 1)]
      result <-
        list(paste0(sentence_infos[[1]][c(tags_index_pattern)], collapse = " "),
             tags_index_pattern)
    }
  }
  result
}

mergeRelation <- function(relations, sentence_infos) {
  for (iCount in 1:length(relations[[1]])) {
    if (min(relations[[2]][[iCount]][1]) != 1) {
      if (identical(" n?o", sentence_infos[[1]][min(relations[[2]][[iCount]]) -
                                                1]) ||
          identical(" ", sentence_infos[[1]][min(relations[[2]][[iCount]]) -
                                             1])) {
        relations[[2]][[iCount]][1] <- min(relations[[2]][[iCount]]) - 1
        relations[[1]][iCount] <-
          paste0(sentence_infos[[1]][min(relations[[2]][[iCount]])], " ", relations[[1]][iCount])
      }
      
      if (identical(" n?o", sentence_infos[[1]][max(relations[[2]][[iCount]]) +
                                                1]) ||
          identical(" ", sentence_infos[[1]][max(relations[[2]][[iCount]]) +
                                             1])) {
        relations[[2]][[iCount]][length(relations[[2]][[iCount]])] <-
          max(relations[[2]][[iCount]]) + 1
        relations[[1]][iCount] <-
          paste0(relations[[1]][iCount], " ", sentence_infos[[1]][max(relations[[2]][[iCount]])])
      }
    }
  }
  relations
}

getLeftArgument <- function(sentence_infos, all_index, pos_info)
{
  finals_arg1 <- list()
  
  for (iCount in 1:length(all_index))
  {
    arg1 <- getPatternArg(sentence_infos, 3, all_index[[iCount]][[1]])
    
    if (length(strsplit(arg1, " ")[[1]]) == 1 &
        identical(arg1, "I-NP "))
      arg1 <- " B-NP"
    
    check_arg1 <- gregexpr(CHUNK, arg1)
    
    if (check_arg1[[1]][1] == -1) {
      next
    } else {
      np_left_most <- length(check_arg1[[1]])
      aux_arg1 <-
        substring(arg1,
                  check_arg1[[1]][np_left_most],
                  check_arg1[[1]][np_left_most] + attr(check_arg1[[1]], "match.length")[np_left_most])
      len_arg1 <- length(strsplit(arg1, " ")[[1]])
      
      if (grepl("O", aux_arg1)) {
        aux_arg1 <-
          substring(arg1,
                    check_arg1[[1]][np_left_most],
                    check_arg1[[1]][np_left_most] + attr(check_arg1[[1]], "match.length")[np_left_most] -
                      1)
        len_aux_arg1 <- length(strsplit(aux_arg1, " ")[[1]])
        pos_np <- all_index[[iCount]][[1]][len_arg1 - len_aux_arg1]
        pos_fin_np <- pos_np + len_aux_arg1
        
      } else {
        len_aux_arg1 <- length(strsplit(aux_arg1, " ")[[1]])
        pos_np <- all_index[[iCount]][[1]][len_arg1 - len_aux_arg1 + 1]
        pos_fin_np <- pos_np + len_aux_arg1
      }
      
      if (len_aux_arg1 == 1 & np_left_most > 1)
      {
        if (grepl(" que| se", sentence_infos[[1]][pos_fin_np - 1])) {
          aux_arg1 <-
            substring(arg1,
                      check_arg1[[1]][np_left_most - 1],
                      check_arg1[[1]][np_left_most - 1] + attr(check_arg1[[1]], "match.length")[np_left_most -
                                                                                                  1] - 1)
          len_aux_arg1 <- length(strsplit(aux_arg1, " ")[[1]])
          pos_np <-
            all_index[[iCount]][[1]][(len_arg1 - 1) - len_aux_arg1 + 1]
          pos_fin_np <- pos_np + len_aux_arg1
          
          if ((
            grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_fin_np - 1]) ||
            grepl(" que| se", sentence_infos[[1]][pos_fin_np - 1]) ||
            grepl(" prp| art| det| spec", sentence_infos[[2]][pos_fin_np -
                                                              1])
          ) & length(check_arg1[[1]]) > 2) {
            aux_arg1 <-
              substring(arg1,
                        check_arg1[[1]][np_left_most - 2],
                        check_arg1[[1]][np_left_most - 2] + attr(check_arg1[[1]], "match.length")[np_left_most -
                                                                                                    2] - 1)
            len_aux_arg1 <- length(strsplit(aux_arg1, " ")[[1]])
            pos_np <-
              all_index[[iCount]][[1]][(len_arg1 - 2) - len_aux_arg1 + 1]
            pos_fin_np <- pos_np + len_aux_arg1
            finals_arg1[[iCount]] <-
              getPattern(sentence_infos, 1, pos_np, pos_fin_np - 2)
          } else if (!(
            grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_fin_np - 1]) ||
            grepl(" que| se", sentence_infos[[1]][pos_fin_np - 1]) ||
            grepl(" prp| art| det| spec", sentence_infos[[2]][pos_fin_np -
                                                              1])
          ))
            finals_arg1[[iCount]] <-
            getPattern(sentence_infos, 1, pos_np, pos_fin_np - 1)
          else
            finals_arg1[[iCount]] <- ""
          next
        } else if ((
          grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_fin_np - 1]) ||
          grepl(" prp| art| det| spec", sentence_infos[[2]][pos_fin_np -
                                                            1])
        )) {
          aux_arg1 <-
            substring(arg1,
                      check_arg1[[1]][np_left_most - 1],
                      check_arg1[[1]][np_left_most - 1] + attr(check_arg1[[1]], "match.length")[np_left_most -
                                                                                                  1] - 1)
          len_aux_arg1 <- length(strsplit(aux_arg1, " ")[[1]])
          pos_np <-
            all_index[[iCount]][[1]][(len_arg1 - 1) - len_aux_arg1 + 1]
          pos_fin_np <- pos_np + len_aux_arg1
          
          if ((
            grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_fin_np - 1]) ||
            grepl(" que| se", sentence_infos[[1]][pos_fin_np - 1]) ||
            grepl(" prp| art| det| spec", sentence_infos[[2]][pos_fin_np -
                                                              1])
          ) & length(check_arg1[[1]]) > 2) {
            aux_arg1 <-
              substring(arg1,
                        check_arg1[[1]][np_left_most - 2],
                        check_arg1[[1]][np_left_most - 2] + attr(check_arg1[[1]], "match.length")[np_left_most -
                                                                                                    2] - 1)
            len_aux_arg1 <- length(strsplit(aux_arg1, " ")[[1]])
            pos_np <-
              all_index[[iCount]][[1]][(len_arg1 - 2) - len_aux_arg1 + 1]
            pos_fin_np <- pos_np + len_aux_arg1
            finals_arg1[[iCount]] <-
              getPattern(sentence_infos, 1, pos_np, pos_fin_np - 2)
          } else if (!(
            grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_fin_np - 1]) ||
            grepl(" que| se", sentence_infos[[1]][pos_fin_np -
                                                  1]) ||
            grepl(" prp| art| det| spec", sentence_infos[[2]][pos_fin_np -
                                                              1])
          ))
            finals_arg1[[iCount]] <-
            getPattern(sentence_infos, 1, pos_np, pos_fin_np - 1)
          else
            finals_arg1[[iCount]] <- ""
          next
        } else if (grepl(" \\,| \\(", sentence_infos[[1]][(pos_fin_np -
                                                           1) + 1], fixed = F) &
                   grepl(" prop", sentence_infos[[2]][(pos_fin_np - 1) +
                                                      2]) &
                   grepl(" \\,| \\)", sentence_infos[[1]][(pos_fin_np -
                                                           1) + 3])) {
          if (len_aux_arg1 > 1) {
            finals_arg1[[iCount]] <-
              getPatternArg(sentence_infos, 1, c(pos_np:(pos_fin_np - 1)))
            finals_arg1[[iCount]][2] <-
              getPatternArg(sentence_infos, 1, c(pos_np:(pos_fin_np - 2), (pos_fin_np -
                                                                             1) + 2))
          } else {
            finals_arg1[[iCount]] <-
              getPatternArg(sentence_infos, 1, c(pos_np:(pos_fin_np - 1)))
            finals_arg1[[iCount]][2] <-
              getPatternArg(sentence_infos, 1, pos_fin_np - 1 + 2)
          }
          
          last_pos <- (pos_fin_np - 1) + 2
          flag <- 3
          while (flag) {
            if (grepl(" \\,| \\(| e", sentence_infos[[1]][last_pos + 1], fixed = F) &
                grepl(" prop", sentence_infos[[2]][last_pos + 2]) &
                grepl(" \\,| \\)| e", sentence_infos[[1]][last_pos + 3], fixed = F)) {
              if (len_aux_arg1 > 1)
                finals_arg1[[iCount]][flag] <-
                  getPatternArg(sentence_infos, 1, c(pos_np:(pos_fin_np - 2), (last_pos +
                                                                                 2)))
              else
                finals_arg1[[iCount]][flag] <-
                  getPatternArg(sentence_infos, 1, last_pos + 2)
              
              last_pos <- last_pos + 2
              flag <- flag + 1
            } else
              flag <- 0
          }
          next
        }
        
        if ((
          grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_fin_np - 1]) ||
          grepl(" que| se", sentence_infos[[1]][pos_fin_np - 1]) ||
          grepl(" prp| art| det| spec", sentence_infos[[2]][pos_fin_np -
                                                            1])
        ) & len_aux_arg1 > 1)
          finals_arg1[[iCount]] <-
            getPattern(sentence_infos, 1, pos_np, pos_fin_np - 2)
        else if (!(
          grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_fin_np - 1]) ||
          grepl(" que| se", sentence_infos[[1]][pos_fin_np - 1]) ||
          grepl(" prp| art| det| spec", sentence_infos[[2]][pos_fin_np -
                                                            1])
        ))
          finals_arg1[[iCount]] <-
            getPattern(sentence_infos, 1, pos_np, pos_fin_np - 1)
        else
          finals_arg1[[iCount]] <- ""
        
      } else {
        if (grepl(" \\,| \\(| e", sentence_infos[[1]][(pos_fin_np - 1) + 1], fixed = F) &
            grepl(" prop", sentence_infos[[2]][(pos_fin_np - 1) + 2]) &
            grepl(" \\,| \\)| e", sentence_infos[[1]][(pos_fin_np - 1) +
                                                      3])) {
          if (len_aux_arg1 > 1) {
            finals_arg1[[iCount]] <-
              getPatternArg(sentence_infos, 1, c(pos_np:(pos_fin_np - 1)))
            finals_arg1[[iCount]][2] <-
              getPatternArg(sentence_infos, 1, c(pos_np:(pos_fin_np - 2), (pos_fin_np -
                                                                             1) + 2))
          } else {
            finals_arg1[[iCount]] <-
              getPatternArg(sentence_infos, 1, c(pos_np:(pos_fin_np - 1)))
            finals_arg1[[iCount]][2] <-
              getPatternArg(sentence_infos, 1, pos_fin_np - 1 + 2)
          }
          
          last_pos <- (pos_fin_np - 1) + 2
          flag <- 3
          while (flag) {
            if (grepl(" \\,| \\(| e", sentence_infos[[1]][last_pos + 1], fixed = F) &
                grepl(" prop", sentence_infos[[2]][last_pos + 2]) &
                (
                  grepl(" \\,| \\)| e", sentence_infos[[1]][last_pos + 3], fixed = F) ||
                  grepl(" prp", sentence_infos[[2]][last_pos + 3], fixed = F)
                )) {
              if (len_aux_arg1 > 1)
                finals_arg1[[iCount]][flag] <-
                  getPatternArg(sentence_infos, 1, c(pos_np:(pos_fin_np - 2), (last_pos +
                                                                                 2)))
              else
                finals_arg1[[iCount]][flag] <-
                  getPatternArg(sentence_infos, 1, last_pos + 2)
              
              last_pos <- last_pos + 2
              flag <- flag + 1
            } else
              flag <- 0
          }
          next
        }
        
        if ((
          grepl(" pron-indp| pron-det| adv", sentence_infos[[2]][pos_fin_np - 1]) ||
          grepl(" que| se", sentence_infos[[1]][pos_fin_np - 1]) ||
          grepl(" prp| art| det| spec", sentence_infos[[2]][pos_fin_np -
                                                            1])
        ) & len_aux_arg1 > 1)
          finals_arg1[[iCount]] <-
            getPattern(sentence_infos, 1, pos_np, pos_fin_np - 2)
        else if (!(
          grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_fin_np - 1]) ||
          grepl(" que| se", sentence_infos[[1]][pos_fin_np - 1]) ||
          grepl(" prp| art| det| spec", sentence_infos[[2]][pos_fin_np -
                                                            1])
        ))
          finals_arg1[[iCount]] <-
            getPattern(sentence_infos, 1, pos_np, pos_fin_np - 1)
        else
          finals_arg1[[iCount]] <- ""
      }
    }
  }
  finals_arg1
}

getRightArgument <- function (sentence_infos, all_index, pos_info)
{
  finals_arg2 <- list()
  np_right_most <- 1
  
  for (iCount in 1:length(all_index))
  {
    arg2 <- getPatternArg(sentence_infos, 3, all_index[[iCount]][[1]])
    
    if (identical(strsplit(arg2, " ")[[1]][1], "I-NP")) {
      arg2 <- strsplit(arg2, " ")[[1]]
      arg2[1] <- "B-NP"
      arg2 <- paste0(paste0(arg2, " "), collapse = "")
    }
    
    check_arg2 <- gregexpr(CHUNK, arg2)
    
    if (check_arg2[[1]][1] == -1) {
      next
    } else{
      aux_arg2 <-
        substring(arg2,
                  check_arg2[[1]][np_right_most],
                  check_arg2[[1]][np_right_most] + attr(check_arg2[[1]], "match.length")[np_right_most] -
                    1)
      len_aux_arg2 <- length(strsplit(aux_arg2, " ")[[1]])
      pos_np <- all_index[[iCount]][[1]][len_aux_arg2]
      pos_ini_np <- all_index[[iCount]][[1]][c(1:(len_aux_arg2 - 1))]
      
      if (len_aux_arg2 == 1 & length(check_arg2[[1]]) > 1) {
        if (grepl(" que| se", sentence_infos[[1]][pos_np])) {
          aux_arg2 <-
            substring(
              arg2,
              check_arg2[[1]][np_right_most],
              check_arg2[[1]][np_right_most] + check_arg2[[1]][np_right_most + 1] + attr(check_arg2[[1]], "match.length")[np_right_most +
                                                                                                                            1] - 2
            )
          len_aux_arg2 <- length(strsplit(aux_arg2, " ")[[1]])
          pos_np <- all_index[[iCount]][[1]][len_aux_arg2]
          pos_ini_np <-
            all_index[[iCount]][[1]][c(2:(len_aux_arg2 - 1))]
          
          aux_arg2 <-
            substring(arg2,
                      check_arg2[[1]][np_right_most + 1],
                      check_arg2[[1]][np_right_most + 1] + attr(check_arg2[[1]], "match.length")[np_right_most +
                                                                                                   1] - 1)
          len_aux_arg2 <- length(strsplit(aux_arg2, " ")[[1]])
          
          if ((
            grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_np]) ||
            grepl(" que| se", sentence_infos[[1]][pos_np]) ||
            grepl(" prp| art| det| spec", sentence_infos[[2]][pos_np])
          ) & length(check_arg2[[1]]) > 2) {
            aux_arg2 <-
              substring(
                arg2,
                check_arg2[[1]][np_right_most],
                check_arg2[[1]][np_right_most] + check_arg2[[1]][np_right_most + 2] + attr(check_arg2[[1]], "match.length")[np_right_most +
                                                                                                                              2] - 3
              )
            len_aux_arg2 <- length(strsplit(aux_arg2, " ")[[1]])
            pos_np <- all_index[[iCount]][[1]][len_aux_arg2]
            pos_ini_np <-
              all_index[[iCount]][[1]][c(3:(len_aux_arg2 - 1))]
            
            aux_arg2 <-
              substring(arg2,
                        check_arg2[[1]][np_right_most + 2],
                        check_arg2[[1]][np_right_most + 2] + attr(check_arg2[[1]], "match.length")[np_right_most +
                                                                                                     2] - 1)
            len_aux_arg2 <- length(strsplit(aux_arg2, " ")[[1]])
            
          }
          
          if ((
            grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_np]) ||
            grepl(" que| se", sentence_infos[[1]][pos_np]) ||
            grepl(" prp| art| det| spec", sentence_infos[[2]][pos_np])
          ) & len_aux_arg2 > 1)
            finals_arg2[[iCount]] <-
            getPatternArg(sentence_infos, 1, pos_ini_np)
          else if (!(
            grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_np]) ||
            grepl(" que| se", sentence_infos[[1]][pos_np]) ||
            grepl(" prp| art| det| spec", sentence_infos[[2]][pos_np])
          ))
            finals_arg2[[iCount]] <-
            getPatternArg(sentence_infos, 1, c(pos_ini_np, pos_np))
          else
            finals_arg2[[iCount]] <- ""
          next
          
        } else if ((
          grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_np]) ||
          grepl(" prp| art| det| spec", sentence_infos[[2]][pos_np])
        )) {
          aux_arg2 <-
            substring(
              arg2,
              check_arg2[[1]][np_right_most],
              check_arg2[[1]][np_right_most] + check_arg2[[1]][np_right_most + 1] + attr(check_arg2[[1]], "match.length")[np_right_most +
                                                                                                                            1] - 2
            )
          len_aux_arg2 <- length(strsplit(aux_arg2, " ")[[1]])
          pos_np <- all_index[[iCount]][[1]][len_aux_arg2]
          pos_ini_np <-
            all_index[[iCount]][[1]][c(2:(len_aux_arg2 - 1))]
          
          aux_arg2 <-
            substring(arg2,
                      check_arg2[[1]][np_right_most + 1],
                      check_arg2[[1]][np_right_most + 1] + attr(check_arg2[[1]], "match.length")[np_right_most +
                                                                                                   1] - 1)
          len_aux_arg2 <- length(strsplit(aux_arg2, " ")[[1]])
          
          if ((
            grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_np]) ||
            grepl(" que| se", sentence_infos[[1]][pos_np]) ||
            grepl(" prp| art| det| spec", sentence_infos[[2]][pos_np])
          ) & length(check_arg2[[1]]) > 2)
          {
            aux_arg2 <-
              substring(
                arg2,
                check_arg2[[1]][np_right_most],
                check_arg2[[1]][np_right_most] + check_arg2[[1]][np_right_most + 2] + attr(check_arg2[[1]], "match.length")[np_right_most +
                                                                                                                              2] - 3
              )
            len_aux_arg2 <- length(strsplit(aux_arg2, " ")[[1]])
            pos_np <- all_index[[iCount]][[1]][len_aux_arg2]
            pos_ini_np <-
              all_index[[iCount]][[1]][c(3:(len_aux_arg2 - 1))]
            
            aux_arg2 <-
              substring(arg2,
                        check_arg2[[1]][np_right_most + 2],
                        check_arg2[[1]][np_right_most + 2] + attr(check_arg2[[1]], "match.length")[np_right_most +
                                                                                                     2] - 1)
            len_aux_arg2 <- length(strsplit(aux_arg2, " ")[[1]])
            
          }
          
          if ((
            grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_np]) ||
            grepl(" que| se", sentence_infos[[1]][pos_np]) ||
            grepl(" prp| art| det| spec", sentence_infos[[2]][pos_np])
          ) & len_aux_arg2 > 1)
            finals_arg2[[iCount]] <-
            getPatternArg(sentence_infos, 1, pos_ini_np)
          else if (!(
            grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_np]) ||
            grepl(" que| se", sentence_infos[[1]][pos_np]) ||
            grepl(" prp| art| det| spec", sentence_infos[[2]][pos_np])
          ))
            finals_arg2[[iCount]] <-
            getPatternArg(sentence_infos, 1, c(pos_ini_np, pos_np))
          else
            finals_arg2[[iCount]] <- ""
          next
          
        } else if (grepl(" \\,| \\(| e", sentence_infos[[1]][pos_np + 1], fixed = F) &
                   grepl(" prop| n", sentence_infos[[2]][pos_np + 2], fixed = F) &
                   grepl(" \\,| \\)| e", sentence_infos[[1]][pos_np + 3], fixed = F)) {
          if (len_aux_arg2 > 1) {
            finals_arg2[[iCount]] <-
              getPatternArg(sentence_infos, 1, c(pos_ini_np, pos_np))
            finals_arg2[[iCount]][2] <-
              getPatternArg(sentence_infos, 1, c(pos_ini_np, pos_np + 2))
          } else {
            finals_arg2[[iCount]] <-
              getPatternArg(sentence_infos, 1, c(pos_ini_np, pos_np))
            finals_arg2[[iCount]][2] <-
              getPatternArg(sentence_infos, 1, pos_np + 2)
          }
          
          last_pos <- (pos_np) + 2
          flag <- 3
          while (flag) {
            if (grepl(" \\,| \\(| e", sentence_infos[[1]][last_pos + 1], fixed = F) &
                grepl(" prop| n", sentence_infos[[2]][last_pos + 2]) &
                (
                  grepl(" \\,| \\)| e", sentence_infos[[1]][last_pos + 3], fixed = F) ||
                  grepl(" prp", sentence_infos[[2]][last_pos + 3], fixed = F)
                )) {
              if (len_aux_arg2 > 1)
                finals_arg2[[iCount]][flag] <-
                  getPatternArg(sentence_infos, 1, c(pos_ini_np, last_pos + 2))
              else
                finals_arg2[[iCount]][flag] <-
                  getPatternArg(sentence_infos, 1, last_pos + 2)
              
              last_pos <- last_pos + 2
              flag <- flag + 1
            } else
              flag <- 0
          }
          next
        }
        
        if ((
          grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_np]) ||
          grepl(" que| se", sentence_infos[[1]][pos_np]) ||
          grepl(" prp| art| det| spec", sentence_infos[[2]][pos_np])
        ) & len_aux_arg2 > 1)
          finals_arg2[[iCount]] <-
            getPatternArg(sentence_infos, 1, pos_ini_np)
        
        if (grepl("B-NP", strsplit(arg2, " ")[[1]][1])) {
          flag <- 1
          flag1 <- 0
          while (flag) {
            if (grepl(" B-NP", sentence_infos[[3]][pos_np + flag]) &
                !grepl(" que| se", sentence_infos[[1]][pos_np + flag])
                &
                !grepl(" pron-indp| pron-det| prp| art| det| spec",
                       sentence_infos[[2]][pos_np + flag]))
              flag <- flag + 1
            else {
              if (flag != 1) {
                pos_np <- pos_np + flag - 1
                flag1 <- 1
              }
              flag <- 0
            }
          }
          
          if (grepl(" \\,| \\(| e", sentence_infos[[1]][pos_np + 1], fixed = F) &
              grepl(" prop| n", sentence_infos[[2]][pos_np + 2], fixed = F) &
              grepl(" \\,| \\)| e", sentence_infos[[1]][pos_np + 3], fixed = F)) {
            if (flag1) {
              finals_arg2[[iCount]] <-
                getPatternArg(sentence_infos, 1, c(pos_ini_np:(pos_np)))
              finals_arg2[[iCount]][2] <-
                getPatternArg(sentence_infos, 1, c(pos_ini_np:(pos_np - 1), (pos_np + 2)))
            } else {
              finals_arg2[[iCount]] <-
                getPatternArg(sentence_infos, 1, c(pos_ini_np, pos_np))
              finals_arg2[[iCount]][2] <-
                getPatternArg(sentence_infos, 1, pos_np + 2)
            }
            
            last_pos <- pos_np + 2
            flag <- 3
            while (flag) {
              if (grepl(" \\,| \\(| e", sentence_infos[[1]][last_pos + 1], fixed = F) &
                  grepl(" prop| n", sentence_infos[[2]][last_pos + 2]) &
                  (
                    grepl(" \\,| \\)| e", sentence_infos[[1]][last_pos + 3], fixed = F) ||
                    grepl(" prp", sentence_infos[[2]][last_pos + 3], fixed = F)
                  )) {
                if (flag1)
                  finals_arg2[[iCount]][flag] <-
                    getPatternArg(sentence_infos,
                                  1,
                                  c(pos_ini_np:(pos_np - 1), last_pos + 2))
                else
                  finals_arg2[[iCount]][flag] <-
                    getPatternArg(sentence_infos, 1, last_pos + 2)
                
                last_pos <- last_pos + 2
                flag <- flag + 1
              } else
                flag <- 0
            }
            next
          }
          
          finals_arg2[[iCount]] <-
            getPatternArg(sentence_infos, 1, c(pos_ini_np:(pos_np)))
        } else if (!(
          grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_np]) ||
          grepl(" que| se", sentence_infos[[1]][pos_np]) ||
          grepl(" prp| art| det| spec", sentence_infos[[2]][pos_np])
        ))
          finals_arg2[[iCount]] <-
            getPatternArg(sentence_infos, 1, c(pos_ini_np, pos_np))
        else
          finals_arg2[[iCount]] <- ""
        
      } else if ((
        grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_np]) ||
        grepl(" que| se", sentence_infos[[1]][pos_np]) ||
        grepl(" prp| art| det| spec", sentence_infos[[2]][pos_np])
      ) & len_aux_arg2 > 1)
      {
        finals_arg2[[iCount]] <- getPatternArg(sentence_infos, 1, pos_ini_np)
        
      } else if (grepl(" \\,| \\(| e", sentence_infos[[1]][pos_np + 1], fixed = F) &
                 grepl(" prop| n", sentence_infos[[2]][pos_np + 2], fixed = F) &
                 grepl(" \\,| \\)| e", sentence_infos[[1]][pos_np +
                                                           3], fixed = F)) {
        if (len_aux_arg2 > 1) {
          finals_arg2[[iCount]] <-
            getPatternArg(sentence_infos, 1, c(pos_ini_np, pos_np))
          finals_arg2[[iCount]][2] <-
            getPatternArg(sentence_infos, 1, c(pos_ini_np, pos_np + 2))
        } else {
          finals_arg2[[iCount]] <-
            getPatternArg(sentence_infos, 1, c(pos_ini_np, pos_np))
          finals_arg2[[iCount]][2] <-
            getPatternArg(sentence_infos, 1, pos_np + 2)
        }
        
        last_pos <- pos_np + 2
        flag <- 3
        while (flag) {
          if (grepl(" \\,| \\(| e", sentence_infos[[1]][last_pos + 1], fixed = F) &
              grepl(" prop| n", sentence_infos[[2]][last_pos + 2]) &
              (
                grepl(" \\,| \\)| e", sentence_infos[[1]][last_pos + 3], fixed = F) ||
                grepl(" prp", sentence_infos[[2]][last_pos + 3], fixed = F)
              )) {
            if (len_aux_arg2 > 1)
              finals_arg2[[iCount]][flag] <-
                getPatternArg(sentence_infos, 1, c(pos_ini_np, last_pos + 2))
            else
              finals_arg2[[iCount]][flag] <-
                getPatternArg(sentence_infos, 1, last_pos + 2)
            
            last_pos <- last_pos + 2
            flag <- flag + 1
          } else
            flag <- 0
        }
        next
      } else if (!(
        grepl(" pron-indp| pron-det", sentence_infos[[2]][pos_np]) ||
        grepl(" que| se", sentence_infos[[1]][pos_np]) ||
        grepl(" prp| art| det| spec", sentence_infos[[2]][pos_np])
      ))
      {
        finals_arg2[[iCount]] <-
          getPatternArg(sentence_infos, 1, c(pos_ini_np, pos_np))
      } else {
        finals_arg2[[iCount]] <- ""
      }
    }
  }
  finals_arg2
}

saveExtractions <-
  function (left_arg,
            sym_left,
            tran_left,
            relations,
            sym_rel,
            tran_rel,
            right_arg,
            sym_right,
            tran_right,
            path,
            file_name,
            sentence,
            index,
            sym_inf,
            tran_inf)
  {
    #normal extractions
    flag <- 0
    for (iCount in 1:length(relations)) {
      for (jCount in 1:length(left_arg[[iCount]])) {
        for (kCount in 1:length(right_arg[[iCount]])) {
          if (flag != 0) {
            data_frame_normal <-
              data.frame("", "", left_arg[[iCount]][jCount], relations[iCount], right_arg[[iCount]][kCount], 1, 0, 0)
            write.table(
              data_frame_normal,
              file = paste0(path, file_name),
              sep = ";",
              col.names = F,
              row.names = F,
              append = T
            )
          } else {
            data_frame_normal <-
              data.frame(index,
                         sentence,
                         left_arg[[iCount]][jCount],
                         relations[iCount],
                         right_arg[[iCount]][kCount],
                         1,
                         0,
                         0)
            write.table(
              data_frame_normal,
              file = paste0(path, file_name),
              sep = ";",
              col.names = F,
              row.names = F,
              append = T
            )
            flag <- 1
          }
        }
      }
    }
    
    #tran extractions
    if (tran_inf != 0) {
      for (iCount in 1:length(tran_rel[[1]])) {
        data_frame_tran <-
          data.frame("", "", tran_left[[1]][iCount], tran_rel[[1]][iCount], tran_right[[1]][iCount], 0, 1, 0)
        write.table(
          data_frame_tran,
          file = paste0(path, file_name),
          sep = ";",
          col.names = F,
          row.names = F,
          append = T
        )
      }
    }
    
    #sym extractions
    if (sym_inf != 0) {
      for (iCount in 1:length(sym_rel[[1]])) {
        data_frame_sym <-
          data.frame("", "", sym_left[[1]][iCount], sym_rel[[1]][iCount], sym_right[[1]][iCount], 0, 0, 1)
        write.table(
          data_frame_sym,
          file = paste0(path, file_name),
          sep = ";",
          col.names = F,
          row.names = F,
          append = T
        )
      }
    }
  }

saveEmpytExtractions <-
  function (sentence, index, path, file_name) {
    write.table(
      data.frame(index, sentence, ".", ".", ".", ".", ".", ".", ".", ".", "."),
      file = paste0(path, file_name),
      sep = ";",
      col.names = F,
      row.names = F,
      append = T
    )
  }