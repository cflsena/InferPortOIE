## Sets work space
work_space <- "/home/cflsena/Documentos/Github/R/InferPortOIE"
setwd(work_space)

## List used packages
#list.of.packages <- c("rJava", "stringr", "NLP", "stringi")

## Returns a list of uninstalled packages
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

## Install the new packages
#if(length(new.packages))
#  install.packages(new.packages)

## Reads the set of sentences
# dataset <-
#   read.table(
#     "Sentences/test.txt",
#     sep = "\t",
#     header = F,
#     encoding = "UTF-8",
#     quote = ""
#   )

dataset <-
  read.table(
    "Sentences/wikipedia_final_200.txt",
    sep = "\t",
    header = F,
    encoding = "UTF-8",
    quote = ""
  )

path_save_file = "Output/"
file_name = "output.csv"

## Create folder to save csv file
if (!file.exists(path_save_file))
  dir.create(file.path(work_space, path_save_file))

## Load used packages
library(rJava)
library(stringr)
library(stringi)
library(NLP)

## Load R files
source(file = "R/sentence_information.r")
source(file = "R/extractor.r")
source(file = "R/extraction_tasks.r")
source(file = "R/inferential_extraction.R")
source(file = "R/patterns.r")

for (iCount in 1:length(dataset[,]))
{
  result <- getSentenceInformation(dataset, iCount)
  result <- sepInfo(result[1,])
  relations <- getRelation(result, 2)
  
  if (relations[[3]] == 0) {
    saveEmpytExtractions(c(as.String(dataset[iCount,])), iCount, path_save_file, file_name)
    next
  }
  
  relations <- mergeRelation(relations, result)
  
  sym_inf <- 0
  sym_triples_clean <- c()
  sym_left <- c()
  sym_rel <- c()
  sym_right <- c()
  
  tran_inf <- 0
  tran_triples_clean <- c()
  tran_left <- c()
  tran_rel <- c()
  tran_right <- c()
  
  all_left_indexes <- getLeftInterval(result, relations, 1)
  all_right_indexes <- getRightInterval(result, relations, 1)
  
  left_arg <- getLeftArgument(result, all_left_indexes, 1)
  right_arg <- getRightArgument(result, all_right_indexes, 1)
  
  if (length(left_arg) == 0 || length(right_arg) == 0) {
    saveEmpytExtractions(c(as.String(dataset[iCount,])), iCount, path_save_file, file_name)
    next
  }
  
  triples_clean <- clearTriples(left_arg, relations, right_arg)
  
  if (is.null(triples_clean)) {
    saveEmpytExtractions(c(as.String(dataset[iCount,])), iCount, path_save_file, file_name)
    next
  }
  
  left_arg <- triples_clean[[1]]
  relations <- triples_clean[[2]]
  right_arg <- triples_clean[[3]]
  
  #inferences triples
  inference <- getInference(left_arg, relations, right_arg, result)
  
  #symmetric triples
  if (inference[[3]] != 0) {
    sym_inf <- 1
    sym_triples_clean <-
      clearInfTriples(inference[[1]][1], inference[[1]][2], inference[[1]][3])
    sym_left <- sym_triples_clean[[1]]
    sym_rel <- sym_triples_clean[[2]]
    sym_right <- sym_triples_clean[[3]]
  }
  
  #transitive triples
  if (inference[[4]] != 0) {
    tran_inf <- 1
    tran_triples_clean <-
      clearInfTriples(inference[[2]][1], inference[[2]][2], inference[[2]][3])
    tran_left <- tran_triples_clean[[1]]
    tran_rel <- tran_triples_clean[[2]]
    tran_right <- tran_triples_clean[[3]]
  }
  
  #save the triples in the csv file
  saveExtractions (
    left_arg,
    sym_left,
    tran_left,
    relations[[1]],
    sym_rel,
    tran_rel,
    right_arg,
    sym_right,
    tran_right,
    path_save_file,
    file_name,
    c(as.String(dataset[iCount,])),
    iCount,
    sym_inf,
    tran_inf
  )
}
