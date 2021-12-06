# Solana-van-Maanen-
Code Scalable Product Duplicate Detection 2021

install.packages('rjson'); library('rjson')
install.packages('jsonlite'); library('jsonlite')
install.packages('tidyjson'); library('tidyjson')
install.packages('tokenizers'); library('tokenizers')
install.packages('proxy'); library('proxy')
install.packages('dplyr'); library('dplyr')
install.packages('caret'); library('caret')

setwd('Desktop/CSforBA') ##Set working directory

data <- fromJSON(file='data.json') ##Import json file 
k1 <- length(data) ##1262 observations with nested duplicates 
new_data
k2 <- length(new_data) ##1624 observations without nested duplicates 

##PART 1 - SHINGLING 
feature_content <- function(x) {
  text <- gsub("[[:punct:]]", "", x) %>% tolower()
  text <- gsub("\\s+", " ", text) %>% str_trim()  
  word <- strsplit(text, " ") %>% unlist()
}

shops <- lapply(1:k2, function(x) new_data[x][[1]]$shop) %>% unique() %>% unlist()
all_modelID <- lapply(1:k2, function(x) new_data[x][[1]]$modelID)

all_titles <- lapply(1:k2, function(x) new_data[[x]]$title)
all_titles_per_word <- unlist(lapply(all_titles,feature_content))
stopwords_titles <- c("best", "buy","neweggcom","amazoncom","thenerdsnet","television","tv","hdtv","led","lcd",
                      "ledlcd","diagonal","diag","class","model","size","with")
titles_all <- all_titles_per_word[!all_titles_per_word %in% stopwords_titles] %>% unique()

title_per_tv <- lapply(1:k2, function(x) new_data[x][[1]]$title)
title_per_tv_per_word <- lapply(title_per_tv,feature_content)

##PART 2 - CREATE CHARACTERISTIC MATRIX 
#Define dictionary 
television_dictionary <- titles_all #or:shingles_all_modelID as dict and shingles_per_tv as inpunt
#Create a characteristic matrix CM
CM <- lapply(title_per_tv_per_word, function(set, dict) {
  as.integer(dict %in% set)
}, dict = television_dictionary) %>% as.data.frame() 
CM

##PART 3 - JACCARD SIMILARITY
#Calculate the Jaccard Index (similarity)
JS <- function(a,b) {
  non_zero <- which(a|b)
  set_intersect <- sum(a[non_zero] & b[non_zero])
  set_union <- length(non_zero)
  return(set_intersect/set_union)
}
#Calculate the pairwise similarity by creating the Jaccard Similarity distance matrix dis1
pr_DB$set_entry(FUN = JS, names = c("JaccardSimilarity"))
dis1 <- dist(t(CM), method = "JaccardSimilarity")
#Delete the new entry
pr_DB$delete_entry("JaccardSimilarity")
s1 <- as.data.frame(as.matrix(dis1))

##PART 4 - MIN HASHING
nrowsCM <- nrow(CM); ncolsCM <- ncol(CM)
nsignature <- 4 #Define the number of hash functions (signature number)
prime <- nrowsCM #Define the prime number
#Generate for each hash function unique coefficients for a and b respectively  
set.seed(12345)
coeff_a <- as.numeric(sample(nrowsCM, nsignature)) ##4 coefficients 
coeff_b <- as.numeric(sample(nrowsCM, nsignature)) ##4 coefficients 

#Check if the hash functions give permutations
permutation <- lapply(1:nsignature, function(s) {
  hashf <- numeric(length = length(nrowsCM))
  for(i in 1:nrowsCM) {
    hashf[i] <- (coeff_a[s] * i + coeff_b[s]) %% prime
  }
  return(hashf)
})

#Convert to data frame 
permutation_df <- structure(permutation, names = paste0( "hash_", 1:length(permutation))) %>%
  as.data.frame()

##PART 5 - CREATE SIGNATURE MATRIX 
#Obtain the non zero rows' index for all columns
rows_non_zero <- lapply(1:ncolsCM, function(j) {
  return(which(CM[, j] != 0))
})

#Initialize signature matrix
MS <- matrix(data = NA, nrow = nsignature, ncol = ncolsCM)
#For each column (television)
for (i in 1:ncolsCM) {
  #For each hash function (signature)'s value 
  for(s in 1:nsignature) {
    MS[s,i] <- min(permutation_df[, s][rows_non_zero[[i]]])
  }
}

#Set names for clarity
colnames(MS) <- paste("television", 1:k2, sep = "_")
rownames(MS) <- paste("minhash", 1:nsignature, sep = "_")  
MS

##PART 6 - SIGNATURE SIMILARITY 
SS <- function(x, y) mean(x == y) #Signature similarity
#Calculate the pairwise similarity by creating the Signature Similarity distance matrix dis2
pr_DB$set_entry(FUN = SignatureIndex, names = c("SigSimilarity"))
dis2 <- dist(t(MS), method = "SigSimilarity" )
#Delete the new entry
pr_DB$delete_entry("SigSimilarity") 
s2 <- as.data.frame(as.matrix(dis2))

##PART 7 - LOCALITY SENSITIVE HASHING 
nbands <- 2 #Set number of bands
nrows <- nrow(MS)/bands #Set number of rows
LSH_df <- as.data.frame(MS) %>% 
  mutate(band = rep(1:nbands, each = nrows)) %>% 
  select(band, everything())

##PART 8 - EXTRACT CANDIDATE PAIRS
#If 3 out of 3 rows of two columns are the same within a band, 
#a candidate pair is obtianed
bandn1 <- LSH_df[1:2,1:k2+1] ##Define band 1 
bandn2 <- LSH_df[3:4,1:k2+1] ##Define band 2 

cp_bandn1 <- lapply(1:(length(bandn1)-1), function(i) all(bandn1[i]==bandn1[i+1]))
indices_cp_bandn1 <- which(cp_bandn1==TRUE) %>% as.data.frame() %>% t()

cp_bandn2 <- lapply(1:(length(bandn2)-1), function(i) all(bandn2[i]==bandn2[i+1]))
indices_cp_bandn2 <- which(cp_bandn2==TRUE) %>% as.data.frame() %>% t()

unique_cp <- unique(c(indices_cp_bandn1,indices_cp_bandn2))

s1_scores_cp <- lapply(1:length(unique_cp), 
function(y) s1[unique_cp[y],unique_cp[y]+1]) %>% as.data.frame() %>% t()
rownames(s1_scores_cp) <- unique_cp
s1_scores_cp

s2_scores_cp <- lapply(1:length(unique_cp), 
function(y) s2[unique_cp[y],unique_cp[y]+1]) %>% as.data.frame() %>% t()
rownames(s2_scores_cp) <- unique_cp
s2_scores_cp

modelid_cp_1 <- lapply(1:length(unique_cp), 
function(i) unlist(all_modelID[unique_cp[i]])) %>% as.data.frame %>% t()
modelid_cp_2 <- lapply(1:length(unique_cp), 
function(i) unlist(all_modelID[unique_cp[i]+1])) %>% as.data.frame %>% t()

target_df <- lapply(1:length(unique_cp), function(i) 
modelid_cp_1[i] == modelid_cp_2[i]) %>% as.data.frame %>% t() %>% as.numeric

##PART 9 - INSTALL DATA FRAME SUITABLE FOR CLASSIFICATION 

df <- data.frame(as.matrix(s1_scores_cp),as.matrix(s2_scores_cp),as.matrix(target_df))
#set names for clarity 
colnames(df) <- c('s1 score','s2 score','target')

##PART 10 - EVALUATION: BOOTSTRAPPING 

target <- target_df

bs1_train <- df[1:144,]; bs1_test <- df[(144+1):231,1:2]
true1 <- df[(144+1):231,3]

bs2_train <- df[22:165,]; bs2_test <- rbind(as.matrix(df[1:(22-1),1:2]), as.matrix(df[(165+1):231,1:2]))
true2 <- c(df[1:(22-1),3],df[(165+1):231,3])

bs3_train <- df[44:187,]; bs3_test <- rbind(as.matrix(df[1:(44-1),1:2]), as.matrix(df[(187+1):231,1:2]))
true3 <- c(df[1:(44-1),3],df[(187+1):231,3])

bs4_train <- df[66:209,]; bs4_test <- rbind(as.matrix(df[1:(66-1),1:2]), as.matrix(df[(209+1):231,1:2]))
true4 <- c(df[1:(66-1),3],df[(209+1):231,3])

bs5_train <- df[88:231,]; bs5_test <- df[1:(88-1),1:2]
true5 <- df[1:(88-1),3]

#Bootstrap 1 

set.seed(123)
model_svm1 <- train(as.factor(target)~., data = bs1_train, method = "svmLinear",
                   trControl = trainControl("cv", number = 5),
                   preProcess = c("center","scale"))

#Make predictions on the test data
predicted_svm1 <- model_svm1 %>% predict(bs1_test); predicted_svm1
accuracy1 <- mean(predicted_svm1 == true1)

#Bootstrap 2

set.seed(123)
model_svm2 <- train(as.factor(target)~., data = bs2_train, method = "svmLinear",
                    trControl = trainControl("cv", number = 5),
                    preProcess = c("center","scale"))

#Make predictions on the test data
predicted_svm2 <- model_svm2 %>% predict(bs2_test); predicted_svm2
accuracy2 <- mean(predicted_svm2 == true2)

#Bootstrap 3

set.seed(123)
model_svm3 <- train(as.factor(target)~., data = bs3_train, method = "svmLinear",
                    trControl = trainControl("cv", number = 5),
                    preProcess = c("center","scale"))

#Make predictions on the test data
predicted_svm3 <- model_svm3 %>% predict(bs3_test); predicted_svm3
accuracy3 <- mean(predicted_svm3 == true3)

#Bootstrap 4

set.seed(123)
model_svm4 <- train(as.factor(target)~., data = bs4_train, method = "svmLinear",
                    trControl = trainControl("cv", number = 5),
                    preProcess = c("center","scale"))

#Make predictions on the test data
predicted_svm4 <- model_svm4 %>% predict(bs4_test); predicted_svm4
accuracy4 <- mean(predicted_svm4 == true4)

#Bootstrap 5

set.seed(123)
model_svm5 <- train(as.factor(target)~., data = bs5_train, method = "svmLinear",
                    trControl = trainControl("cv", number = 5),
                    preProcess = c("center","scale"))

#Make predictions on the test data
predicted_svm5 <- model_svm5 %>% predict(bs5_test); predicted_svm5
accuracy5 <- mean(predicted_svm5 == true5)

#Derive the F1 measure 
accuracies <- sum(accuracy1,accuracy2,accuracy3,accuracy4,accuracy5)

F1 <- accuracies/5 



