sepInfo <- function(info)
{
  result <- strsplit(info, ", ")
  
  for (iCount in 2:length(result[[1]])) {
    result[[1]][iCount] <- paste("", result[[1]][iCount])
    result[[2]][iCount] <- paste("", result[[2]][iCount])
    result[[3]][iCount] <- paste("", result[[3]][iCount])
  }
  
  result
}

getIndex <- function(sentence_infos, pattern_info, pos_info)
{
  ## Get index of chunks in the sentence
  index <- grep(pattern_info, sentence_infos[[pos_info]], fixed = T)
  index
}

getPattern <-
  function(sentence_infos,
           pos_info,
           left_limit,
           right_limit)
  {
    result <-
      as.String(sentence_infos[[pos_info]][left_limit:right_limit])
    result <- gsub("\n", "", result, fixed = F)
    result <- paste0(result, " ")
    result <- str_trim(result, "left")
    result
  }

getPatternArg <- function(sentence_infos, pos_info, interval)
{
  if (length(interval) == 2)
    if (interval[1] == interval[2])
      interval <- interval[1]
    
    result <- as.String(sentence_infos[[pos_info]][interval])
    result <- gsub("\n", "", result, fixed = F)
    result <- paste0(result, " ")
    result <- str_trim(result, "left")
    result
}

getLeftInterval <- function (sentence_infos, relations, pos_info)
{
  index_arg1 <- list()
  pos_iCount <- 1
  for (iCount in 1:length(relations[[1]]))
  {
    index_token <- min(relations[[2]][[iCount]]) - 1
    token <- sentence_infos[[1]][index_token]
    token_tag <- sentence_infos[[2]][index_token]
    
    if (index_token == 0) {
      index_arg1[[pos_iCount]] <- list(0)
      pos_iCount <- pos_iCount + 1
      next
    } else if (grepl(" ,", token, fixed = T)) {
      aux_index_token <-
        grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
      if (length(aux_index_token) == 0) {
        index_token_initial_parenthesis <-
          grep(" (", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)[1]
        if (is.na(index_token_initial_parenthesis))
          index_arg1[[pos_iCount]] <- list(c(1:(index_token - 1)))
        else {
          index_token_final_parenthesis <-
            grep(" )", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
          index_token_final_parenthesis <-
            index_token_final_parenthesis[length(index_token_final_parenthesis)]
          if (length(index_token_final_parenthesis) == 0)
            index_arg1[[pos_iCount]] <-
            list(c(1:(
              index_token_initial_parenthesis - 1
            )))
          else {
            index_arg1[[pos_iCount]] <-
              list(c(1:(
                index_token_initial_parenthesis - 1
              )))
            if (!grepl("( ,| ;| :| .)", sentence_infos[[pos_info]][index_token_final_parenthesis +
                                                                   1], fixed = F))
              index_arg1[[pos_iCount]] <-
                list(c(
                  1:(index_token_initial_parenthesis - 1),
                  (index_token_final_parenthesis + 1):(index_token - 1)
                ))
          }
        }
      } else {
        index_token_initial_parenthesis <-
          grep(" (", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
        if (length(index_token_initial_parenthesis) == 0) {
          aux_index_token <- aux_index_token[1]
          index_arg1[[pos_iCount]] <- list(c(1:(aux_index_token - 1)))
        } else {
          index_token_initial_parenthesis <- index_token_initial_parenthesis[1]
          aux_index_token <-
            grep(" ,", sentence_infos[[pos_info]][1:(index_token_initial_parenthesis -
                                                       1)], fixed = T)
          if (length(aux_index_token) == 0) {
            index_token_final_parenthesis <-
              grep(" )", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
            if (length(index_token_final_parenthesis) == 0)
              index_arg1[[pos_iCount]] <-
                list(c(1:(
                  index_token_initial_parenthesis - 1
                )))
            else {
              index_arg1[[pos_iCount]] <-
                list(c(1:(
                  index_token_initial_parenthesis - 1
                )))
              index_token_final_parenthesis <-
                index_token_final_parenthesis[length(index_token_final_parenthesis)]
              aux_index_token <-
                grep(" ,", sentence_infos[[pos_info]][(index_token_final_parenthesis + 1):(index_token -
                                                                                             1)], fixed = T)
              if (length(aux_index_token) == 0)
                index_arg1[[pos_iCount]] <-
                list(c(
                  1:(index_token_initial_parenthesis - 1),
                  (index_token_final_parenthesis + 1):(index_token - 1)
                ))
              else {
                aux_index_token <- aux_index_token[1]
                if (!grepl("( \\,| \\;| \\:| \\.)",
                           sentence_infos[[pos_info]][index_token_final_parenthesis + 1],
                           fixed = F))
                  index_arg1[[pos_iCount]] <-
                    list(c(
                      1:(index_token_initial_parenthesis - 1),
                      (index_token_final_parenthesis + 1):((
                        index_token_final_parenthesis + aux_index_token
                      ) - 1
                      )
                    ))
              }
            }
          } else {
            aux_index_token <- aux_index_token[1]
            index_arg1[[pos_iCount]] <-
              list(c(1:(aux_index_token - 1)))
          }
        }
      }
    } else if (grepl(" )", token, fixed = T)) {
      index_token_initial_parenthesis <-
        grep(" (", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
      if (length(index_token_initial_parenthesis) == 0) {
        index_token_comma <-
          grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
        if (length(index_token_comma) == 0)
          index_arg1[[pos_iCount]] <- list(c(1:(index_token - 1)))
        else {
          index_token_comma <- index_token_comma[length(index_token_comma)]
          index_arg1[[pos_iCount]] <- list(c(1:(index_token_comma - 1)))
        }
      } else {
        index_token_initial_parenthesis <- index_token_initial_parenthesis[1]
        index_token_comma <-
          grep(" ,", sentence_infos[[pos_info]][1:(index_token_initial_parenthesis -
                                                     1)], fixed = T)
        if (length(index_token_comma) == 0)
          index_arg1[[pos_iCount]] <-
          list(c(1:(
            index_token_initial_parenthesis - 1
          )))
        else {
          index_token_comma <- index_token_comma[length(index_token_comma)]
          index_arg1[[pos_iCount]] <-
            list(c((index_token_comma + 1):(index_token_initial_parenthesis - 1)
            ))
        }
      }
      
    } else if (grepl(" \\e| como| mas| por?m| entretanto| contuto| todavia| no_entanto",
                     token,
                     fixed = F)) {
      if (pos_iCount != 1) {
        if (length(index_arg1[[pos_iCount - 1]]) &
            index_arg1[[pos_iCount - 1]][[1]][1] != 0)
          index_arg1[[pos_iCount]] <- index_arg1[[pos_iCount - 1]]
        else {
          index_token_comma <-
            grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)[1]
          if (is.na(index_token_comma))
            index_arg1[[pos_iCount]] <- list(c(1:(index_token - 1)))
          else
            index_arg1[[pos_iCount]] <-
              list(c(1:(index_token_comma - 1)))
        }
      } else {
        index_token_comma <-
          grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)[1]
        if (is.na(index_token_comma))
          index_arg1[[pos_iCount]] <- list(c(1:(index_token - 1)))
        else
          index_arg1[[pos_iCount]] <-
            list(c(1:(index_token_comma - 1)))
      }
    } else if (grepl(" que| se", sentence_infos[[pos_info]][index_token], fixed = F)) {
      index_token_comma <-
        grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
      index_token_comma <-
        index_token_comma[length(index_token_comma)]
      index_token_final_parenthesis <-
        grep(" )", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
      index_token_final_parenthesis <-
        index_token_final_parenthesis[length(index_token_final_parenthesis)]
      
      if (length(index_token_comma) == 0 ||
          is.na(index_token_comma))
        index_token_comma <- 0
      if (length(index_token_final_parenthesis) == 0 ||
          is.na(index_token_final_parenthesis))
        index_token_final_parenthesis <- 0
      
      if (index_token_comma == 0 &
          index_token_final_parenthesis == 0) {
        index_arg1[[pos_iCount]] <- list(c(1:(index_token - 1)))
      } else {
        if (index_token_comma > index_token_final_parenthesis) {
          index_token_comma <-
            grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
          if (length(index_token_comma) == 1)
            index_arg1[[pos_iCount]] <-
              list(c((index_token_comma + 1):(index_token - 1)))
          else {
            index_last_token_comma <- index_token_comma[length(index_token_comma)]
            index_previous_token_comma <-
              index_token_comma[length(index_token_comma) - 1]
            if ((index_token - index_last_token_comma) == 1) {
              if (index_previous_token_comma > index_token_final_parenthesis)
                index_arg1[[pos_iCount]] <-
                  list(c((index_previous_token_comma + 1):(index_last_token_comma - 1)
                  ))
              else
                index_arg1[[pos_iCount]] <-
                  list(c((index_token_final_parenthesis + 1):(index_last_token_comma - 1)
                  ))
            } else {
              index_arg1[[pos_iCount]] <-
                list(c((index_last_token_comma + 1):(index_token - 1)))
            }
          }
        } else {
          index_arg1[[pos_iCount]] <-
            list(c((index_token_final_parenthesis + 1):(index_token - 1)
            ))
        }
      }
    } else {
      index_token_comma <-
        grep(" ,", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
      index_token_comma <-
        index_token_comma[length(index_token_comma)]
      index_token_final_parenthesis <-
        grep(" )", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
      index_token_final_parenthesis <-
        index_token_final_parenthesis[length(index_token_final_parenthesis)]
      
      if (length(index_token_comma) == 0)
        index_token_comma <- 0
      if (length(index_token_final_parenthesis) == 0)
        index_token_final_parenthesis <- 0
      
      if (index_token_final_parenthesis > index_token_comma) {
        index_token_initial_parenthesis <-
          grep(" (", sentence_infos[[pos_info]][1:(index_token - 1)], fixed = T)
        if (length(index_token_initial_parenthesis) == 0) {
          index_token_comma <-
            grep(" ,", sentence_infos[[pos_info]][1:(index_token_final_parenthesis -
                                                       1)], fixed = T)
          if (length(index_token_comma) == 0)
            index_arg1[[pos_iCount]] <-
              list(c(1:(
                index_token_final_parenthesis - 1
              )))
          else {
            index_token_comma <- index_token_comma[length(index_token_comma)]
            index_arg1[[pos_iCount]] <-
              list(c((index_token_comma + 1):(index_token_final_parenthesis - 1)
              ))
          }
        } else {
          index_token_initial_parenthesis <- index_token_initial_parenthesis[1]
          index_token_comma <-
            grep(" ,", sentence_infos[[pos_info]][1:(index_token_initial_parenthesis -
                                                       1)], fixed = T)
          if (length(index_token_comma) == 0)
            index_arg1[[pos_iCount]] <-
            list(c(
              1:(index_token_initial_parenthesis - 1),
              (index_token_final_parenthesis + 1):(index_token)
            ))
          else {
            index_token_comma <- index_token_comma[length(index_token_comma)]
            index_arg1[[pos_iCount]] <-
              list(c((index_token_comma + 1):(index_token_initial_parenthesis + 1)
              ))
            index_arg1[[pos_iCount]] <-
              list(c((index_token_comma + 1):(index_token_initial_parenthesis + 1),
                     (index_token_final_parenthesis + 1):(index_token)
              ))
          }
        }
        
      } else if (index_token_comma > index_token_final_parenthesis) {
        if (length(c((index_token_comma + 1):index_token)) == 1 &
            grepl(
              " \\e| como| mas| por?m| entretanto| contuto| todavia| no_entanto",
              token_tag,
              fixed = F
            )) {
          if (pos_iCount > 1) {
            if (length(index_arg1[[pos_iCount - 1]]))
              index_arg1[[pos_iCount]] <- index_arg1[[pos_iCount - 1]]
            else {
              aux_index_token_comma <-
                grep(" ,", sentence_infos[[pos_info]][1:(index_token_comma - 1)], fixed = T)
              if (length(aux_index_token_comma) == 0)
                index_arg1[[pos_iCount]] <-
                  list(c(1:(index_token_comma - 1)))
              else {
                aux_index_token_comma <- aux_index_token_comma[1]
                index_arg1[[pos_iCount]] <-
                  list(c(1:(
                    aux_index_token_comma - 1
                  )))
              }
            }
          } else {
            aux_index_token_comma <-
              grep(" ,", sentence_infos[[pos_info]][1:(index_token_comma - 1)], fixed = T)
            if (length(aux_index_token_comma) == 0)
              index_arg1[[pos_iCount]] <-
                list(c(1:(index_token_comma - 1)))
            else {
              aux_index_token_comma <- aux_index_token_comma[1]
              index_arg1[[pos_iCount]] <-
                list(c(1:(aux_index_token_comma - 1)))
            }
          }
        } else
          index_arg1[[pos_iCount]] <-
            list(c((index_token_comma + 1):index_token))
      } else {
        index_arg1[[pos_iCount]] <- list(c(1:index_token))
      }
    }
    pos_iCount <- pos_iCount + 1
  }
  index_arg1
}

getRightInterval <- function (sentence_infos, relations, pos_info)
{
  index_arg1 <- list()
  
  for (iCount in 1:length(relations[[1]]))
  {
    index_token <- max(relations[[2]][[iCount]]) + 1
    token <- sentence_infos[[1]][index_token]
    token_tag <- sentence_infos[[2]][index_token]
    max_num_tokens <- length(sentence_infos[[pos_info]])
    
    if (grepl(" ,", token, fixed = T)) {
      aux_index_token <-
        grep(" ,", sentence_infos[[pos_info]][(index_token + 1):max_num_tokens], fixed = T)
      if (length(aux_index_token) == 0) {
        index_token_initial_parenthesis <-
          grep(" (", sentence_infos[[pos_info]][c((index_token + 1):max_num_tokens)], fixed = T)[1]
        if (is.na(index_token_initial_parenthesis)) {
          index_arg1[[iCount]] <- list(c((index_token + 1):max_num_tokens))
        } else {
          index_token_final_parenthesis <-
            grep(" )", sentence_infos[[pos_info]][c((index_token + 1):max_num_tokens)], fixed = T)
          index_token_final_parenthesis <-
            index_token_final_parenthesis[length(index_token_final_parenthesis)]
          if (length(index_token_final_parenthesis) == 0) {
            index_arg1[[iCount]] <-
              list(c((index_token + 1):(index_token + index_token_initial_parenthesis -
                                          1)
              ))
          } else {
            index_token_verb <-
              grep("v-", sentence_infos[[2]][c((index_token + 1):(index_token + index_token_initial_parenthesis -
                                                                    1))], fixed = T)
            if (length(index_token_verb) == 0) {
              index_arg1[[iCount]] <-
                list(c((index_token + 1):(
                  index_token + index_token_initial_parenthesis - 1
                )
                ))
              if (!grepl("( ,| ;| :| .)", sentence_infos[[pos_info]][index_token +
                                                                     index_token_final_parenthesis + 1], fixed = F))
                index_arg1[[iCount]] <-
                  list(c((index_token + 1):(
                    index_token + index_token_initial_parenthesis - 1
                  ),
                  (index_token + index_token_final_parenthesis + 1):(max_num_tokens - 1)
                  ))
            } else {
              index_arg1[[iCount]] <-
                list(c((index_token + 1):(
                  index_token + index_token_initial_parenthesis - 1
                )
                ))
              if (!grepl("( ,| ;| :| .)", sentence_infos[[pos_info]][index_token +
                                                                     index_token_final_parenthesis + 1], fixed = F))
                index_arg1[[iCount]] <-
                  list(c((index_token + 1):(
                    index_token + index_token_initial_parenthesis - 1
                  ),
                  (index_token + index_token_final_parenthesis + 1):(max_num_tokens - 1)
                  ))
            }
          }
        }
      } else {
        index_token_initial_parenthesis <-
          grep(" (", sentence_infos[[pos_info]][(index_token + 1):(max_num_tokens)], fixed = T)
        if (length(index_token_initial_parenthesis) == 0) {
          aux_index_token <- aux_index_token[length(aux_index_token)]
          index_arg1[[iCount]] <-
            list(c((index_token + aux_index_token + 1):(max_num_tokens - 1)
            ))
        } else {
          index_token_initial_parenthesis <- index_token_initial_parenthesis[1]
          index_token_final_parenthesis <-
            grep(" )", sentence_infos[[pos_info]][(index_token + 1):(max_num_tokens -
                                                                       1)], fixed = T)
          if (length(index_token_final_parenthesis) == 0) {
            aux_index_token <-
              grep(" ,", sentence_infos[[pos_info]][(index_token + 1):(index_token + index_token_initial_parenthesis -
                                                                         1)], fixed = T)
            if (length(aux_index_token) == 0) {
              index_arg1[[iCount]] <-
                list(c((index_token + 1):(
                  index_token + index_token_initial_parenthesis - 1
                )
                ))
            } else {
              aux_index_token <- aux_index_token[length(aux_index_token)]
              index_arg1[[iCount]] <-
                list(c((index_token + aux_index_token + 1):(
                  index_token + index_token_initial_parenthesis - 1
                )
                ))
            }
          } else {
            aux_index_token <-
              grep(" ,", sentence_infos[[pos_info]][(index_token + index_token_final_parenthesis +
                                                       1):(max_num_tokens)], fixed = T)
            if (length(aux_index_token) == 0) {
              aux_index_token <-
                grep(" ,", sentence_infos[[pos_info]][(index_token + 1):(index_token + index_token_initial_parenthesis -
                                                                           1)], fixed = T)
              if (length(aux_index_token) == 0) {
                index_arg1[[iCount]] <-
                  list(c((index_token + 1):(
                    index_token + index_token_initial_parenthesis - 1
                  )
                  ))
                if (!grepl("( \\,| \\;| \\:| \\.)",
                           sentence_infos[[pos_info]][index_token + index_token_final_parenthesis +
                                                      1],
                           fixed = F))
                  index_arg1[[iCount]][2] <-
                    list(c((
                      index_token + index_token_final_parenthesis + 1
                    ):(max_num_tokens - 1)
                    ))
              } else {
                aux_index_token <- aux_index_token[length(aux_index_token)]
                index_arg1[[iCount]] <-
                  list(c((index_token + aux_index_token + 1):(
                    index_token + index_token_initial_parenthesis - 1
                  )
                  ))
                if (!grepl("( \\,| \\;| \\:| \\.)",
                           sentence_infos[[pos_info]][index_token + index_token_final_parenthesis +
                                                      1],
                           fixed = F))
                  index_arg1[[iCount]][2] <-
                  list(c((
                    index_token + index_token_final_parenthesis + 1
                  ):(max_num_tokens - 1)
                  ))
              }
            } else {
              aux_index_token <- aux_index_token[length(aux_index_token)]
              index_arg1[[iCount]] <-
                list(c((index_token + aux_index_token + 1):(max_num_tokens - 1)
                ))
            }
          }
        }
      }
    } else if (grepl(" (", token, fixed = T)) {
      index_token_final_parenthesis <-
        grep(" )", sentence_infos[[pos_info]][c((index_token + 1):(max_num_tokens))], fixed = T)
      if (length(index_token_final_parenthesis) == 0) {
        index_token_comma <-
          grep(" ,", sentence_infos[[pos_info]][c((index_token + 1):(max_num_tokens))], fixed = T)
        if (length(index_token_comma) == 0) {
          index_arg1[[iCount]] <- list(c((index_token + 1):(max_num_tokens - 1)))
        } else {
          index_token_comma <- index_token_comma[1]
          index_arg1[[iCount]] <-
            list(c((index_token + 1):(index_token_comma - 1)))
        }
      } else {
        index_token_final_parenthesis <-
          index_token_final_parenthesis[length(index_token_final_parenthesis)]
        index_token_comma <-
          grep(" ,", sentence_infos[[pos_info]][c((index_token + index_token_final_parenthesis +
                                                     1):(max_num_tokens))], fixed = T)
        if (length(index_token_comma) == 0) {
          index_arg1[[iCount]] <-
            list(c((index_token + index_token_final_parenthesis + 1):(max_num_tokens -
                                                                        1)
            ))
        } else {
          index_token_comma <- index_token_comma[1]
          index_arg1[[iCount]] <-
            list(c((index_token + index_token_final_parenthesis + 1):(index_token +
                                                                        index_token_comma - 1)
            ))
        }
      }
    } else if (grepl(" que| se", sentence_infos[[pos_info]][index_token], fixed = F)) {
      index_token_comma <-
        grep(" ,", sentence_infos[[pos_info]][(index_token):(max_num_tokens - 1)], fixed = T)
      index_token_comma <- index_token_comma[1]
      index_token_initial_parenthesis <-
        grep(" (", sentence_infos[[pos_info]][(index_token):(max_num_tokens - 1)], fixed = T)
      index_token_initial_parenthesis <-
        index_token_initial_parenthesis[1]
      
      if (length(index_token_comma) == 0 ||
          is.na(index_token_comma))
        index_token_comma <- 1000
      if (length(index_token_initial_parenthesis) == 0 ||
          is.na(index_token_initial_parenthesis))
        index_token_initial_parenthesis <- 1000
      
      if (length(index_token_comma) == 1000 &
          index_token_initial_parenthesis == 1000) {
        index_arg1[[iCount]] <- list(c((index_token + 1):(max_num_tokens - 1)))
      } else {
        if (index_token_comma > index_token_initial_parenthesis) {
          index_token_comma <-
            grep(" ,", sentence_infos[[pos_info]][(index_token):(max_num_tokens - 1)], fixed = T)
          if (length(index_token_comma) == 1)
            index_arg1[[iCount]] <-
              list(c((index_token + 1):((index_token - 1) + index_token_comma - 1
              )))
          else {
            index_previous_token_comma <- index_token_comma[1]
            index_last_token_comma <- index_token_comma[2]
            if ((index_previous_token_comma - index_token) == 1) {
              if (index_last_token_comma < index_token_initial_parenthesis)
                index_arg1[[iCount]] <-
                  list(c(((index_token - 1) + index_previous_token_comma + 1):((index_token -
                                                                                  1) + index_last_token_comma - 1)
                  ))
              else
                index_arg1[[iCount]] <-
                  list(c(((index_token - 1) + index_previous_token_comma + 1):((index_token -
                                                                                  1) + index_token_initial_parenthesis - 1
                  )
                  ))
            } else
              index_arg1[[iCount]] <-
              list(c((index_token + 1):(index_token - 1) + index_previous_token_comma -
                       1
              ))
          }
        } else {
          index_arg1[[iCount]] <-
            list(c((index_token + 1):((index_token - 1) + index_token_initial_parenthesis -
                                        1
            )
            ))
        }
      }
    } else {
      index_token_comma <-
        grep(" ,", sentence_infos[[pos_info]][(index_token):(max_num_tokens - 1)], fixed = T)
      index_token_comma <- index_token_comma[1]
      index_token_initial_parenthesis <-
        grep(" (", sentence_infos[[pos_info]][(index_token):(max_num_tokens - 1)], fixed = T)
      index_token_initial_parenthesis <-
        index_token_initial_parenthesis[1]
      
      if (length(index_token_comma) == 0 ||
          is.na(index_token_comma))
        index_token_comma <- 1000
      if (length(index_token_initial_parenthesis) == 0 ||
          is.na(index_token_initial_parenthesis))
        index_token_initial_parenthesis <- 1000
      
      if (index_token_initial_parenthesis < index_token_comma) {
        index_token_final_parenthesis <-
          grep(" )", sentence_infos[[pos_info]][(index_token):(max_num_tokens - 1)], fixed = T)
        index_token_final_parenthesis <-
          index_token_final_parenthesis[1]
        if (length(index_token_final_parenthesis) == 0)
          index_arg1[[iCount]] <-
          list(c(
            index_token:(index_token + index_token_initial_parenthesis - 1)
          ))
        else {
          if (length(grep(" \\,| \\.", sentence_infos[[1]][(index_token - 1) + index_token_final_parenthesis +
                                                           1], fixed = F)) > 0 ||
              length(grep(" adv| conj-s| conj-c| intj", sentence_infos[[2]][(index_token -
                                                                             1) + index_token_final_parenthesis + 1], fixed = F)) > 0)
            index_arg1[[iCount]] <-
              list(c(
                index_token:((index_token - 1) + index_token_initial_parenthesis - 1
                )
              ))
          else {
            index_token_comma <-
              grep(" ,", sentence_infos[[pos_info]][((index_token - 1) + index_token_final_parenthesis +
                                                       1):(max_num_tokens - 1)], fixed = T)
            if (length(index_token_comma) == 0)
              index_arg1[[iCount]] <-
                list(c(
                  index_token:((index_token - 1) + index_token_initial_parenthesis - 1
                  ),
                  ((index_token - 1) + index_token_final_parenthesis + 1
                  ):(max_num_tokens - 1)
                ))
            else{
              index_token_comma <- index_token_comma[1]
              index_arg1[[iCount]] <-
                list(c(
                  index_token:((index_token - 1) + index_token_initial_parenthesis - 1
                  ),
                  ((index_token - 1) + index_token_final_parenthesis + 1
                  ):((index_token - 1) + (index_token_final_parenthesis + 1) + index_token_comma -
                       1
                  )
                ))
            }
          }
        }
      } else if (index_token_comma < index_token_initial_parenthesis) {
        index_arg1[[iCount]] <-
          list(c(index_token:((index_token - 1) + index_token_comma - 1
          )))
      } else {
        index_arg1[[iCount]] <- list(c(index_token:(max_num_tokens - 1)))
      }
    }
  }
  index_arg1
}

clearTriples <- function(left_arg, relations, right_arg)
{
  result <- list()
  
  aux_left_arg <- list()
  aux_rel <- list(NULL, NULL)
  aux_right_arg <- list()
  lim <- 0
  pos_iCount <- 1
  
  if (length(left_arg) < length(right_arg))
    lim <- length(left_arg)
  else
    lim <- length(right_arg)
  
  for (iCount in 1:lim)
  {
    if (!(
      identical(left_arg[[iCount]], "") ||
      identical(left_arg[[iCount]], NULL) ||
      identical(left_arg[[iCount]], as.character(NA))
    ) &
    !(
      identical(right_arg[[iCount]], "") ||
      identical(right_arg[[iCount]], NULL) ||
      identical(right_arg[[iCount]], as.character(NA))
    )) {
      aux_left_arg[[pos_iCount]] <- left_arg[[iCount]]
      aux_right_arg[[pos_iCount]] <- right_arg[[iCount]]
      aux_rel[[1]][pos_iCount] <- relations[[1]][iCount]
      aux_rel[[2]][pos_iCount] <- relations[[2]][iCount]
      pos_iCount <- pos_iCount + 1
    }
  }
  
  if (length(aux_left_arg) == 0 ||
      length(aux_right_arg) == 0 || length(aux_rel) == 0)
    result <- NULL
  else
    result <- list(aux_left_arg, aux_rel, aux_right_arg)
  
  result
}

clearInfTriples <- function(left_arg, relations, right_arg)
{
  result <- list()
  
  for (iCount in 1:length(relations[[1]]))
  {
    if ((
      identical(left_arg[[1]][iCount], "") ||
      identical(left_arg[[1]][iCount], NULL) ||
      identical(left_arg[[1]][iCount], as.character(NA))
    ) ||
    (
      identical(right_arg[[1]][iCount], "") ||
      identical(right_arg[[1]][iCount], NULL) ||
      identical(right_arg[[1]][iCount], as.character(NA))
    )) {
      left_arg[[1]] <- left_arg[[1]][-iCount]
      right_arg[[1]] <- right_arg[[1]][-iCount]
      relations[[1]] <- relations[[1]][-iCount]
    }
  }
  
  if (length(left_arg) == 0 ||
      length(right_arg) == 0 || length(relations[[1]]) == 0)
    result <- NULL
  else
    result <- list(left_arg, relations, right_arg)
  
  result
}