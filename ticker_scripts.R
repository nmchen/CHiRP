# setwd("D:/Documents/Graduate Work/Projects/MATH 285J Financial Project/Yearly")


# decided jingyi is right
# better to organize all dates/files in order, which will allow for easier retrieval of date information.
index_data <- function(){
  # this needs to start in the Yearly folder.
  
  
  df <- data.frame(matrix(ncol = 6, nrow = 0))
  
  index = 1
  for(year in 2000:2021){
    setwd(as.character(year)) #find data
    files = list.files() 
    files = files[grepl(".csv.gz", files)]
    
    for(f in files){ # loop through all files
      filepath = paste0(year, "/", f)
      date = gsub(".csv.gz", "", f)
      year = substr(date, 1, 4)
      month = substr(date, 5, 6)
      day = substr(date, 7, 8)
      row = c(index, date, filepath, year, month, day)
      df = rbind(df, row)
      
      index = index + 1
      
    }
    setwd("..")
  }
  colnames(df) <- c("index", "date", "filepath", "year", "month", "day")
  return(df)
    
  
}



# retrieves tickers that have shown up in every data.
sharedtickers <- function(range, bonusdays = -1){  #range should ben an array, like 2019:2021. Min is 2000, max 2021.
  #data = read.csv("2020/20200102.csv")
  #tickers = data$ticker
  first = TRUE
  for(year in range){
    print(year) # announce year being processed
    setwd(as.character(year)) #find data
    files = list.files() 
    files = files[grepl(".csv", files)]
    for(f in files){ # loop through all files
      data = read.csv(f)
      if(first){ # if this is the first file looked at, take its list without comparison (since there's nothing to compare it to yet)
        tickers = data$ticker
        first = FALSE
      }else{
        tickers = tickers[tickers %in% data$ticker] # on subsequent loops, strip out any ticker that doesn't appear in the newest data.
      }
      #print(length(tickers))
    }
  

    
    
    
    setwd("..")
    #break()
  }
  
  year = range[length(range)] + 1
  if(bonusdays >= 1 & year <= 2021){ #take a look at next year, grab a few more days.
    
    
    print(year) # announce year being processed
    setwd(as.character(year)) #find data
    files = list.files() 
    files = files[grepl(".csv", files)]
    counter = 0
    for(f in files){ # loop through all files
      if(counter >= bonusdays){
        break()
      }
      data = read.csv(f)
      if(first){ # if this is the first file looked at, take its list without comparison (since there's nothing to compare it to yet)
        tickers = data$ticker
        first = FALSE
      }else{
        tickers = tickers[tickers %in% data$ticker] # on subsequent loops, strip out any ticker that doesn't appear in the newest data.
      }
      #print(length(tickers))
      counter = counter + 1
    }
    setwd("..")
    
  }
  return(tickers)
}

#retrieves every ticker that has ever appeared in the data.
alltickers <- function(range){ #range should ben an array, like 2019:2021. Min is 2000, max 2021.
  first = TRUE
  tickers = c()
  for(year in range){
    print(year)
    setwd(as.character(year))
    files = list.files()
    files = files[grepl(".csv", files)]
    for(f in files){

      #print(f)
      data = read.csv(f)
      extratickers = data$ticker[!data$ticker %in% tickers]
      tickers = c(tickers, extratickers)
      print(length(tickers))

    }
    
    
    
    
    setwd("..")
    #break()
  }
  return(tickers)
  
}



# scans data, gives a dataframe of booleans to help assess how often tickers show up in data.
alltickers_frequency <- function(alltickers){
  data = read.csv("2000/20000103.csv") 
  df = data.frame(alltickers %in% data$ticker)
  #df = data.frame()
  rownames(df) = alltickers
  #print(df)
  
  for(year in 2020:2021){
    print(year)
    setwd(as.character(year))
    files = list.files()
    files = files[grepl(".csv", files)]
    for(f in files){
      print(f)
      data = read.csv(f)
      t = data$ticker
      newcol = alltickers %in% t
      df[f] = newcol
      
    }
    
    
    
    
    setwd("..")
    #break()
  }
  return(df)
}



# helper functions
getyear <- function(date){ #given a string of form #YYYYMMDD get YYYY
  return(substr(date, 1, 4))
}
getmonth <- function(date){ #given a string of form #YYYYMMDD get MM
  return(substr(date, 5, 6))
}
getmonth <- function(date){ #given a string of form #YYYYMMDD get DD
  return(substr(date, 7, 8))
}

date_to_index <- function(date, dataindex){
  dataindex = as.numeric(dataindex[dataindex$date == date,]$index[1])
  return(dataindex)
}

index_to_date <- function(index, dataindex){
  dataindex = as.numeric(dataindex[dataindex$index == index,]$date[1])
  return(dataindex)
}


# this function goes through the desired date range and collects the relevant data from the relevant tickers.
# range should be a vector containing years to take from.
# tickers should be the relevant tickers.
# if no tickers specified, it will automatically scan for tickers that show up in all relevant data.
# specifying tickers will save a small amount of time.
# column is the column to take frome ach day's dataset. Default is OPCL (open-close log return)
collectdata <- function(range, tickers = "", column = "OPCL", bonusdays = -1){ 
  #numdays says how much of the data to take.
  if(setequal(tickers,"")){
    if(bonusdays != -1){
      tickers = sharedtickers(range, bonusdays = bonusdays)
    }else{
      tickers = sharedtickers(range)
    }
  }
  
  
  
  df = data.frame(tickers) #essentially a dummy column to establish correct number of rows
  rownames(df) <- tickers #set proper rownames
  
  print(dim(df))
   #first = TRUE
  
  
  for(year in range){
    print(year) # announce year being processed
    setwd(as.character(year)) #find data
    files = list.files() 
    files = files[grepl(".csv", files)]
    counter = 0
    for(f in files){ # loop through all files
      data = read.csv(f)
      data = data[data$ticker %in% tickers, ] # keep only tickers we care about
      data = data[match(tickers, data$ticker),] # reorder rows of tickers. I THINK this row should do nothing because the data is consistent (?) but doesn't hurt to be safe.

      date = substr(f, 1, nchar(f) - 7) #remove .csv bit for better naming
      #print(date)
      
      df[date] <- data[column] #add column
      
   
    }
    
    setwd("..") #return to parent folder
    
  }
  year = range[length(range)] + 1
  if(bonusdays != -1 & year <= 2021){
    
    
    
    
    print(year) # announce year being processed
    setwd(as.character(year)) #find data
    files = list.files() 
    files = files[grepl(".csv", files)]
    
    counter = 0
    for(f in files){ # loop through all files
      if(counter >= bonusdays){
        break()
      }
      data = read.csv(f)
      data = data[data$ticker %in% tickers, ] # keep only tickers we care about
      data = data[match(tickers, data$ticker),] # reorder rows of tickers. I THINK this row should do nothing because the data is consistent (?) but doesn't hurt to be safe.
      
      date = substr(f, 1, nchar(f) - 7) #remove .csv bit for better naming
      #print(date)
      
      df[date] <- data[column] #add column
      counter = counter + 1
      
    }
    
    setwd("..") #return to parent folder
    
    
  }
  
  
  
  df <- df[, -1] #remove first column (which just contains ticker names)
  df <- df[, -1] #remove next column as well (which is a duplicate column)
  

  return(df)
  
}




preprocess_returns <- function(returns){
  #nr = nrow(returns)
  columns_to_keep = c()
  for(c in 1:ncol(returns)){
    zeroreturns = returns[,c] <= -5 
    zeroreturns[is.na(zeroreturns)] <- TRUE
    if(sum(zeroreturns)/ncol(returns) <= .1){
      columns_to_keep = c(columns_to_keep, c)
    }
  }
  
  returns = returns[,columns_to_keep]
  
  
  rows_to_keep = c()
  for(r in 1:nrow(returns)){
    
    zeroreturns = returns[r, ] <= -5 
    zeroreturns[is.na(zeroreturns)] <- TRUE
    
    if(sum(zeroreturns)/nrow(returns) <= .5){
      rows_to_keep = c(rows_to_keep, r)
    }
  }
  
  returns = returns[rows_to_keep, ]
  
}







# fill NA values with the values from previous day
fill_with_previous <- function(day) {
  for (i in 2:length(day)) {
    if (is.na(day[i]) || is.nan(day[i])) {
      day[i] <- day[i - 1]
    }
  }
  return(day)
}

replace_na_with_0 <- function(day) {
  for (i in 1:length(day)) {
    if (is.na(day[i]) || is.nan(day[i])) {
      day[i] <- 0
    }
  }
  return(day)
}

# handle multiple data processing steps
prepare_data <- function(data){
  
  #basic bad data trimming
  data <- preprocess_returns(data)
  

  
  # NA values get replaced w/ previous values
  data <- t(apply(data, 1, fill_with_previous))
  data <- t(apply(data, 1, replace_na_with_0))
  data <- as.data.frame(data)
  
  #winsorizing
  for(row in nrow(data)){
    rowdata = as.numeric(data[row,]) # throws a fit if I don't vectorize it
    q95 = quantile(rowdata, probs = .95)
    q05 = quantile(rowdata, probs = .05)
    data[row, data[row, ] > q95] <- q95 # replace outlier values with the 95/5th quantile values
    data[row, data[row, ] > q05] <- q05
  }
  return(data)
}




prepare_projection <- function(S,E){
  # given the matrices S and E, grab the relevant data for forecasting
  # specifically, for every day, record
  # A) the date
  # B) the date's column of S, E 
  # C) the sum of the last 5 columns, not including current day (-5:-2) (4 columns)
  # D) the sum of the last 21 columns, not including last week (-21:-6) (16 columns)
  # E) date's index 
  # These should all be columns, since we're doing this on each cluster. 
  # bind them into a dataframe, associate that DF with the particular day. This should now be a Cx6 DF
  # where C = number clusters. 
  # add it to a list object.
  
  # starting at the most recent time, roll backward until you no longer have enough columns.
  
  N = ncol(S)
  c = N
  range1 = c-1
  range2 = (c - 6):(c - 3)
  range3 = (c - 22):(c - 7)
  proj_data <- list()
  while(c > 22){ 

    S1 = S[,c]
    S2 = rowSums(S[,range2])
    S3 = rowSums(S[,range3])
    E1 = E[,c]
    E2 = rowSums(E[,range2])
    E3 = rowSums(E[,range3])
    
    mat = cbind(S1, S2, S3, E1, E2, E3)
    colnames(mat) = c("S1", "S2", "S3", "E1", "E2", "E3")
    rownames(mat) <- rownames(S)
    
    date = colnames(S)[c]
    
    
    
    proj_data[[c]] = mat # not sure if I should label it with the DATE, the INDEX or the COUNTER.  
    
    
    # adjust ranges downward. 
    c = c - 1
    range1 = range1 - 1
    range2 = range2 - 1
    range3 = range3 - 1
  }
  
  return(proj_data)
  
  
}

prepare_projection_by_cluster <- function(S,E){
  # copy of prepare_projection, but it organizes matrices differently
  # specifically rather than have it indexed by dates, it indexes by cluster
  #
  # given the matrices S and E, grab the relevant data for forecasting
  # specifically, for every day, record
  # A) the date
  # B) the date's column of S, E 
  # C) the sum of the last 5 columns, not including current day (-5:-2) (4 columns)
  # D) the sum of the last 21 columns, not including last week (-21:-6) (16 columns)
  # E) date's index 
  # These should all be columns, since we're doing this on each cluster. 
  # bind them into a dataframe, associate that DF with the particular day. This should now be a Cx6 DF
  # where C = number clusters. 
  # add it to a list object.
  
  # starting at the most recent time, roll backward until you no longer have enough columns.
  ncluster = nrow(S)
  clusterIDs = rownames(S)
  proj_data = list()
  for(cluster in clusterIDs){
    tempdf = data.frame(matrix(nrow = 0, ncol = 6))
    colnames(tempdf) = c("S1", "S2", "S3", "E1", "E2", "E3")
    proj_data[[cluster]] = tempdf
  }
  N = ncol(S)
  c = N
  range1 = c
  range2 = (c - 5):(c - 2)
  range3 = (c - 21):(c - 6)
  
  
  datevec = c()
  while(c > 21){ 
    
    S1 = S[,c]
    S2 = rowSums(S[,range2])
    S3 = rowSums(S[,range3])
    E1 = E[,c]
    E2 = rowSums(E[,range2])
    E3 = rowSums(E[,range3])
    
    mat = cbind(S1, S2, S3, E1, E2, E3)
    colnames(mat) = c("S1", "S2", "S3", "E1", "E2", "E3")
    rownames(mat) <- rownames(S)
    
    datevec = c(datevec, colnames(S)[c])
    
    
    for(cluster in clusterIDs){
      proj_data[[cluster]] = rbind(proj_data[[cluster]], mat[cluster,]) # not sure if I should label it with the DATE, the INDEX or the COUNTER.  
    }
    
    
    
    # adjust ranges downward. 
    c = c - 1
    range1 = range1 - 1
    range2 = range2 - 1
    range3 = range3 - 1
  }
  
  for(cluster in clusterIDs){
    rownames(proj_data[[cluster]]) = datevec
    
    colnames(proj_data[[cluster]]) = c("S1", "S2", "S3", "E1", "E2", "E3")
  }
  
  
  return(proj_data)
  
  
}

cluster_data <- function(data, k = 100){
  # we perform k-means here
  set.seed(285)
  k <- 100
  print(paste0("Beginning clustering with k = ", k))
  min_cluster_size <- 5
  km <- kmeans(data_twoyears, centers = k, nstart = 10)
  
  # how clusters are assigned initially
  assignments <- km$cluster 
  centroids <- km$centers
  
  # repeatedly reassign until no cluster is smaller than min required size
  repeat {
    cluster_sizes <- table(assignments)
    # these are the small clusters with < 5 elements in them
    small_clusters <- as.numeric(names(cluster_sizes[cluster_sizes < min_cluster_size]))
    
    if (length(small_clusters) == 0) {
      break
    }
    
    # loop through each small cluster
    for (sc in small_clusters) {
      members <- which(assignments == sc)
      
      # loop through each ticker in current small cluster and resign them to the nearest cluster
      for (m in members) {
        point <- data_twoyears[m, ]
        
        # compute distances to other centroids
        dists <- apply(centroids, 1, function(center) sum((point - center)^2))
        # exclude current small cluster itself
        dists[sc] <- Inf
        # find nearest other cluster
        repeat {
          new_cluster <- which.min(dists)
          if (as.numeric(cluster_sizes[as.character(new_cluster)]) >= min_cluster_size) {
            break
          } else {
            dists[new_cluster] <- Inf
          }
        }
        # reassign
        assignments[m] <- new_cluster
      }
    }
    # update centroids
    clustered_data <- split(as.data.frame(data_twoyears), assignments)
    centroids <- t(sapply(clustered_data, colMeans))
  }
  
  n <- nrow(data_twoyears)
  new_k <- length(unique(assignments)) # this is the updated k clusters
  
  # recompute cluster centroids using new assignments
  clustered_data <- split(as.data.frame(data_twoyears), assignments)
  new_centroids <- t(sapply(clustered_data, colMeans))
  rownames(new_centroids) <- paste0("C", 1:new_k)
  
  # compute Euclidean distances to each new centroid
  distances <- matrix(NA, nrow = n, ncol = new_k)
  
  for (i in 1:new_k) {
    centroid_mat <- matrix(rep(new_centroids[i, ], each = n), nrow = n)
    distances[, i] <- sqrt(rowSums((data_twoyears - centroid_mat)^2))
  }
  
  distances <- replace(distances, distances == 0, .000001)
  
  # compute weight matrix
  inv_dists <- 1 / distances
  weights_mat <- inv_dists / rowSums(inv_dists)  # normalize
  
  rownames(weights_mat) <- rownames(data_twoyears)
  colnames(weights_mat) <- rownames(new_centroids)
  
  return(weights_mat)
  
}








# this is not used. we use jingyi's code instead
quantile_analysis <- function(pred_fret_list, true_fret_list, hedged_fret_list, quantile){
  # run a quantile analysis, i.e sharpe ratios if you ONLY consider stocks for the highest quantile% of forecasts.
  # quantile should be a value in (0, 1]. If a number >1 is inputted, we will automatically divide by 100 on the 
  # assumption somebody is trying to put in a percentage.
  print(paste0("Running Quantile Analysis for top ", quantile, " quantiles"))
  return_summaries = c()
  hedged_return_summaries = c()
  
  if(quantile > 1){
    quantile = quantile/100
  }
  
  for(name in names(pred_fret_list)){
    
    
    
    t = as.numeric(true_fret_list[[name]])
    p = as.numeric(pred_fret_list[[name]])
    h = as.numeric(hedged_fret_list[[name]])
    p.abs = abs(p)
    h.abs = abs(h)
    
    p.quant = quantile(p.abs, probs = quantile)
    h.quant = quantile(h.abs, probs = quantile)
    
    
    pnl = sign(p) * t
    sharpe <- mean(pnl, na.rm = TRUE) / sd(pnl, na.rm = TRUE) #* sqrt(252)
    return_summaries = c(return_summaries, sharpe)
    
    hedged_pnl = sign(h) * t
    hedged_sharpe =  mean(hedged_pnl, na.rm = TRUE) / sd(hedged_pnl, na.rm = TRUE)
    
    hedged_return_summaries = c(hedged_return_summaries, hedged_sharpe )
    
  }
  results = list()
  results[["hedged"]] <- hedged_return_summaries
  results[["unhedged"]] <- return_summaries
  
  return(results)
  
}

#this function returns the cumulative sum vector of a vector of numbers
# specifically: given a input vector v, returns a vector w
# such that w[i] = sum(v[1:i])
cum_sum_vector <- function(vec){
  w = c()
  for(i in 1:length(vec)){
    w = c(w, sum(vec[1:i]))
  }
  return(w)
}


#given a vector
#generate a new vector with entries "col1" if <0, "col2" otherwise.
sharp_coloring <- function(vec, col1 = "red", col2 = "black"){
  colors = vec
  colors[vec < 0] <- col1
  colors[vec>=0] <- col2
  return(colors)
}




load_data <- function(year){
  
  
  pred = readRDS(paste0(year, "_pred_fret_compilation.rds"))
  hedged = readRDS(paste0(year, "_hedged_fret_compilation.rds"))
  true = readRDS(paste0(year, "_true_fret_compilation.rds"))
  spy = readRDS(paste0(year, "_spy_return_compilation.rds"))
  
  returns = list()
  returns[["pred"]] <- pred
  returns[["hedged"]] <- hedged
  returns[["true"]] <- true
  returns[["spy"]] <- spy
  
  return(returns)
  
}

library(e1071)
# mihai's code
sharpe_test <-  function(v){
  sharpe_ratio = mean(v)/sd(v)
  Ti = length(v)
  g3 = skewness(v) ;
  g4 = kurtosis ( v )
  p1 = pnorm (sharpe_ratio/sqrt((1 - g3*sharpe_ratio + (g4-1) * 
                                      (sharpe_ratio^2 / 4 ))/(Ti - 1)))
  return(min(p1 ,1 - p1) * 2)
}


sharpe_test_pos <-  function(v){ #one tailed test
  sharpe_ratio = mean(v)/sd(v)
  Ti = length(v)
  g3 = skewness(v) ;
  g4 = kurtosis ( v )
  p1 = pnorm (sharpe_ratio/sqrt((1 - g3*sharpe_ratio + (g4-1) * 
                                   (sharpe_ratio^2 / 4 ))/(Ti - 1)))
  return(1 - p1)
}

