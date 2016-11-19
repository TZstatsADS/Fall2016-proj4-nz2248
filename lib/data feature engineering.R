setwd("/Users/zuonianyao/Documents/GR5243/Project4/data")
library("rhdf5")
library(data.table)
library(dplyr)
rm(list=ls())

data_name <- as.matrix(list.files(pattern = "h5",recursive = TRUE))
lyric <- apply(data_name,1,h5read,"analysis")

list_ele <- lyric[[1]]
names(list_ele)

get_name <- function(name, num){
  return(paste(name, num, sep="_"))
}

feature_name <- function(list_ele){
  name_data <- c()
  vec <- c("median", "min","quant1","mean", "quant3", 
           "max", "sd", "len")
  
  data <- list_ele$bars_confidence
  new_name <- sapply("bars_conf", vec, FUN=get_name)
  name_data <- c(name_data, new_name)
  
  data <- list_ele$bars_start
  new_name <- sapply("bars_start", vec, FUN=get_name)
  name_data <- c(name_data, new_name)
  
  data <- list_ele$beats_confidence
  new_name <- sapply("beats_conf", vec, FUN=get_name)
  name_data <- c(name_data, new_name)
  
  data <- list_ele$beats_start
  new_name <- sapply("beats_start", vec, FUN=get_name)
  name_data <- c(name_data, new_name)
  
  data <- list_ele$sections_confidence
  new_name <- sapply("sec_conf", vec, FUN=get_name)
  name_data <- c(name_data, new_name)
  
  data <- list_ele$sections_start
  new_name <- sapply("sec_start", vec, FUN=get_name)
  name_data <- c(name_data, new_name)
  
  data <- list_ele$segments_confidence
  new_name <- sapply("seg_conf", vec, FUN=get_name)
  name_data <- c(name_data, new_name)
  
  data <- list_ele$segments_loudness_max
  new_name <- sapply("seg_loud_max", vec, FUN=get_name)
  name_data <- c(name_data, new_name)
  
  data <- list_ele$segments_loudness_max_time
  new_name <- sapply("seg_loud_max_t", vec, FUN=get_name)
  name_data <- c(name_data, new_name)
  
  data <- list_ele$segments_loudness_start
  new_name <- sapply("seg_loud_start", vec, FUN=get_name)
  name_data <- c(name_data, new_name)
  
  data <- list_ele$segments_start
  new_name <- sapply("seg_start", vec, FUN=get_name)
  name_data <- c(name_data, new_name)
 
  data <- list_ele$tatums_confidence
  new_name <- sapply("tat_conf", vec, FUN=get_name)
  name_data <- c(name_data, new_name)
  
  data <- list_ele$tatums_start
  new_name <- sapply("tat_start", vec, FUN=get_name)
  name_data <- c(name_data, new_name)
  
  data <- list_ele$segments_pitches
  mt_vec <- sapply("seg_pit", 1:dim(data)[1], FUN=get_name)
  for(name in mt_vec){
    new_name <- sapply(name, vec, FUN=get_name)
    name_data <- c(name_data, new_name)
  }
  
  
  data <- list_ele$segments_timbre
  mt_vec <- sapply("seg_tim", 1:dim(data)[1], FUN=get_name)
  for(name in mt_vec){
    new_name <- sapply(name, vec, FUN=get_name)
    name_data <- c(name_data, new_name)
  }
  
  return(name_data)
}
ft_name <- feature_name(lyric[[1]])

summa <- function(data){
  if(length(data)==0){return(rep(0,8))}
  sum(identical(data, numeric(0)))
  mean_data <- mean(data,na.rm=T)
  median_data <- median(data, na.rm=T)
  quant_1 <- quantile(data, 0.25, na.rm=T)
  quant_3 <- quantile(data, 0.75, na.rm = T)
  min_data <- min(data, na.rm=T)
  if(is.infinite(min_data)){
    min_data <- 0
  }
  max_data <- max(data, na.rm=T)
  if(is.infinite(max_data)){
    max_data <- 0
  }
  sd_data <- sd(data,na.rm=T)
  len_data <- length(data)
  new_data <- c(median_data, min_data, quant_1, mean_data,
                quant_3, max_data, sd_data, len_data)
  return(new_data)
}


feature_extract <- function(list_ele){
  whole_data <-c()
  
  data <- list_ele$bars_confidence
  new_data <- summa(data)
  whole_data <- c(whole_data, new_data)
  
  data <- list_ele$bars_start
  new_data <- summa(data)
  whole_data <- c(whole_data, new_data)
  
  data <- list_ele$beats_confidence
  new_data <- summa(data)
  whole_data <- c(whole_data, new_data)
  
  data <- list_ele$beats_start
  new_data <- summa(data)
  whole_data <- c(whole_data, new_data)
  
  data <- list_ele$sections_confidence
  new_data <- summa(data)
  whole_data <- c(whole_data, new_data)
  
  data <- list_ele$sections_start
  new_data <- summa(data)
  whole_data <- c(whole_data, new_data)
  
  data <- list_ele$segments_confidence
  new_data <- summa(data)
  whole_data <- c(whole_data, new_data)
  
  data <- list_ele$segments_loudness_max
  new_data <- summa(data)
  whole_data <- c(whole_data, new_data)
  
  data <- list_ele$segments_loudness_max_time
  new_data <- summa(data)
  whole_data <- c(whole_data, new_data)
  
  data <- list_ele$segments_loudness_start
  new_data <- summa(data)
  whole_data <- c(whole_data, new_data)
  
  data <- list_ele$segments_start
  new_data <- summa(data)
  whole_data <- c(whole_data, new_data)
  
  data <- list_ele$tatums_confidence
  new_data <- summa(data)
  whole_data <- c(whole_data, new_data)
  
  data <- list_ele$tatums_start
  new_data <- summa(data)
  whole_data <- c(whole_data, new_data)
  
  data <- list_ele$segments_pitches
  data <- data.frame(data)
  new_data <- as.vector(unlist(apply(data,1, FUN=summa)))
  whole_data <- c(whole_data, new_data)
  
  
  data <- list_ele$segments_timbre
  data <- data.frame(data)
  new_data <- as.vector(unlist(apply(data,1, FUN=summa)))
  whole_data <- c(whole_data, new_data)
  
  return(as.vector(whole_data))
}


lyric_feature <- lapply(lyric, FUN=feature_extract)
result <- as.numeric(unlist(lyric_feature[[1]]))
for(i in 2:length(lyric_feature)){
  result <- rbind(result, as.numeric(unlist(lyric_feature[[i]])))
}
colnames(result) <- ft_name
row.names(result) <- NULL
df <- data.frame(result)
scaled_df <- df %>% mutate_each_(funs(scale),vars=names(df)) 
scaled_df[is.na(scaled_df)] <- 0

######################
### Topic Modeling ###
######################

load("/Users/zuonianyao/Documents/GR5243/Project4/lyr.RData")
lyr <- lyr[-c(2,3,6:30)]

id <- lyr[,1]
lyr <- lyr[,-1]
vocab <- names(lyr)

mt <- as.matrix(lyr)
mt_t <- t(mt)
lyr_t <- as.data.frame(mt_t, row.names = vocab)
names(lyr_t) <- id




get_doc <- function(vec){
  get_index <- function(idx){
    if(vec[idx]!=0){
      return(rep(idx,vec[idx]))
    }
  }
  index <- unlist(apply(as.array(1:length(vec)),1, FUN=get_index))
  return(rbind(as.integer(index - 1), as.integer(rep(1, length(index)))))
}
doc <- lapply(lyr_t, get_doc)


K <- 20
G <- 1000
alpha <- 0.02
eta <- 0.02
library(lda)
fit <- lda.collapsed.gibbs.sampler(documents = doc, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
save.image("../fit.rdata")


theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

term.frequency <- apply(lyr, 2, sum)
doc.length <- sapply(doc, function(x) sum(x[2, ])) 
MovieReviews <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

library(LDAvis)

# create the JSON object to feed the visualization:
json <- createJSON(phi = MovieReviews$phi, 
                   theta = MovieReviews$theta, 
                   doc.length = MovieReviews$doc.length, 
                   vocab = MovieReviews$vocab, 
                   term.frequency = MovieReviews$term.frequency)

serVis(json, out.dir = './vis', open.browser = FALSE)



#############
# The previous parts are done.
#############


#######################
# Try another LDA way #
#######################
library(topicmodels)
k <- 20 #number of topic
lda <- LDA(lyr[rowSums(lyr)!=0,], k )
term <- terms(lda, 5000) # First 5000 word for each topic

# make a data frame with topics as cols, docs as rows and
# cell values as posterior topic distribution for each document
gammaDF <- as.data.frame(lda@gamma) 
names(gammaDF) <- c(1:k)
toptopics <- as.data.frame(cbind(document = row.names(gammaDF), 
                                 topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
scaled_df$topic <- toptopics$topic
df$topic <- toptopics$topic

##########################################
# Use the LDA topics to train hd5 analysis
##########################################
library(xgboost)
h<-sample(nrow(df), 500)
dval<-xgb.DMatrix(data=data.matrix(df[h,1:296]),
                  label=df[h,297], missing = NaN)
dtrain<-xgb.DMatrix(data=data.matrix(df[-h,1:296]),
                    label=df[-h,297], missing = NaN)
watchlist<-list(val=dval,train=dtrain)

datatrain <-xgb.DMatrix(data=data.matrix(df[,1:296]), 
                        label=df$topic)

xgb_params <- list( objective = "multi:softmax", 
                    booster = "gbtree",
                    num_class           = 21,
                    eta                 = 0.02, 
                    max_depth           = 6, 
                    subsample           = 0.8,
                    colsample_bytree    = 0.4
)
xgb_model <- xgb.train(
  params              = xgb_params, 
  data                = datatrain, 
  nrounds             = 50, 
  verbose             = 1,  
  print_every_n       = 10,
  nthread             = 2,
  early_stopping_rounds = 30)

setwd("/Users/zuonianyao/Documents/GR5243/Project4/TestSongFile100")
test_name <- as.matrix(list.files(pattern = "h5",recursive = TRUE))
test <- apply(test_name,1,h5read,"analysis")
test_feature <- lapply(test, FUN=feature_extract)
test_res <- as.numeric(unlist(test_feature[[1]]))
for(i in 2:length(test_feature)){
  test_res <- rbind(test_res, as.numeric(unlist(test_feature[[i]])))
}
colnames(test_res) <- ft_name
row.names(test_res) <- NULL
test_df <- data.frame(test_res)

test_cls <- predict(xgb_model, data.matrix(test_df))

lyr_sim <- lyr
load("/Users/zuonianyao/Documents/GR5243/Project4/lyr.RData")
vocab_full <- names(lyr)[2:length(lyr)]

sub <- matrix(rep(NA, (length(lyr)-1) * (dim(lyr)[1])), nrow = dim(lyr)[1])
get_top_word <- function(i){
  vec <- sub[i,]
  idx <- test_cls[i]
  words <- as.vector(term[,idx])
  location <- match(words, vocab_full)
  vec[location[1:100]] <- c(1:100)
  return(vec)
}

try <- sapply(1:100, get_top_word)
try_t <- as.data.frame(t(as.matrix(try)))
names(try_t) <- vocab_full

get_name <- function(i){
  return(paste0("testsong",i, rep=""))
}
id <- apply(as.array(1:100), 1, FUN=get_name)

sub2 <- cbind(id, try_t)
names(sub2)[1] <- "dat2$track_id"

write.csv(sub2, "../ZUO_Nianyao_Sub.csv")





###################
### Association ###
###################
library(ggplot2)
library(tm)
library(RColorBrewer)
tdm <- lyr_t
word.freq <- sort(rowSums(as.matrix(tdm)), decreasing = T)
pal <- brewer.pal(9, "BuGn")[-(1:4)]
library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)

library(arules)
rules <- apriori(scaled_df)
# rules with rhs containing "Survived" only
rules <- apriori(titanic.raw,
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"),
                                   + default="lhs"),
                 control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# [1] 2 4 7 8
# remove redundant rules
ules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)



id <- read.table("../common_id.txt")

cluster <- kmeans(scaled_df, 20)
cluster <- kmeans(df, 12)
df$cluster <- cluster$cluster
df$id <- id$V1
df$id <- as.character(df$id)

names(lyr)[1] <- "id"
df_sim <- df[,c("id","cluster")]
new_df <- merge(df_sim, lyr, by = "id")

lyr <- read.csv("../lyr.csv")
