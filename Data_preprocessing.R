library(tidyverse)
options(stringsAsFactors = FALSE)
# Categories of all the features
Feature_cate <- read.table("./Data/Features_Description.txt", sep = "\t", header = TRUE)
Feature_cate <- Feature_cate[, 1:2]

# All Features
Feature_info <- read.table("./Data/Features.txt", sep = "\t", header = TRUE)
# Feature_info <- Feature_info %>%
#   dplyr::select(c(colnames(Feature_info)[1:4], Feature_cate$Feature))
Feature_info[,unlist(lapply(Feature_info, is.numeric))] <- 
  round(Feature_info[,unlist(lapply(Feature_info, is.numeric))], 3)
rownames(Feature_info) <- NULL
Feature_info$Gene = as.character(Feature_info$Gene)
Feature_info$Entry = as.character(Feature_info$Entry)
All_features <- Feature_cate$Feature
All_Categories <- Feature_cate$Category

# Prediction and Ligandability
Preds <- read.table("./Data/Prioritization.txt", sep = "\t", header = TRUE)
Preds[,unlist(lapply(Preds, is.numeric))] <- 
  round(Preds[,unlist(lapply(Preds, is.numeric))], 3)
Preds$Gene = as.character(Preds$Gene)
Preds$Entry = as.character(Preds$Entry)

# for (i in colnames(Preds)[9:19]){
#   Preds[[i]] <- as.factor(Preds[[i]])
# }

# Preds <- Preds %>%
#   left_join(., Feature_info %>%
#               dplyr::select(c(1:2)),
#             by = "Gene") %>%
#   dplyr::select(c(1,length(colnames(.)),(3:length(colnames(.))-1)))

# E2_Accessibility
E2_table <- read.table("./Data/E2_Acc.txt", sep = "\t", header = TRUE)
E2_table$Gene = as.character(E2_table$Gene)
E2_table$Entry = as.character(E2_table$Entry)


