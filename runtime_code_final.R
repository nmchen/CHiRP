## Running Code

#install.packages("rpca")
library(rpca)

starttime = Sys.time()

# using https://cran.r-project.org/web/packages/rpca/rpca.pdf for robust PCA
#source("/Users/shuiyouchuan/Desktop/285/ticker_scripts.R")

# basic outline: we'll run this per-year, starting at the earliest day in a given year
# Let year by Y
# gather years Y - 2, Y - 1, Y
# for the first month, run the analysis using 2 years of data. Keep the clustering to avoid recalcuating, just keep adding data.
# this is a bit of a simplification, but it should work ok.


data_index = index_data()

# this still only handles one year at a time, implementing a rolling window for that year
# Nothing's really stopping us from doing it longer, this is just a little easier to code.
#year = as.numeric(readline("What year do you want to analyze? Can be between 2002 and 2021:   "))
#if(year < 2002 | year>2021){
#  print("No!")
#  stop("bad year")
#}

#define year to analyze 
year = 2016

startday = 1 #what day to start at, relative to the start of the year?

recluster_period = 7 #how often to recluster, in days.
#If you don't want it to EVER recluster, set it to -1
# setting it to 7 means that every 7 days it will attempt reclustering.

# define range
range = (year-2):year
#if(year != 2021){
#  bonusyear = year + 1
#  # acquire shared tickers
#  tickers <- sharedtickers(range, bonusdays = 21)
#  # collect data
#  
#  data <- collectdata(range, tickers, column = "OPCL")
#  extra_data <- collectdata(bonusyear, column = "OPCL", numdays = 21)
#  data = cbind(data, extra_data)
#}else{
#  # acquire shared tickers
#  
#  tickers = sharedtickers(range)
#  # collect data
#  data <- collectdata(range, tickers, column = "OPCL")
#}
# acquire shared tickers
tickers = sharedtickers(range, bonusdays = 21)
# collect data
data <- collectdata(range, tickers, column = "OPCL", bonusdays = 21)




dayone_index = as.numeric(data_index[data_index$year == year, 1][1]) # numerical index of day one.

data_twoyears = data[,getyear(colnames(data)) < year] #get the first two years of data, which are also our initial rolling window.
days_in_year = ncol(data[,getyear(colnames(data)) == year]) #how many days are in the CURRENT year.


# preprocess data
# using thresholds given by Mihai in pdf- I don't think any of the data gets filtered out
data <- prepare_data(data)
data_twoyears <- prepare_data(data_twoyears)
windowsize = ncol(data_twoyears)




h_forecasts = c(1,5) # how far do we want to forecast? 

pred_fret_compilation = list() # this will be a list, containing a list per h value.
true_fret_compilation = list()
hedged_fret_compilation = list()
spy_return_compilation = list()


for(h in h_forecasts){
  # initialize with some empty lists. 
  # this technically means these lists have empty elements, but that doesn't matter that much.
  pred_fret_compilation[[h]] = list()
  true_fret_compilation[[h]] = list()
  hedged_fret_compilation[[h]] = list()
  spy_return_compilation[[h]] = 0 #can't load an empty vector as a list entry
  
}



#handle the case of having a non-1 startdate.
if(startday == 1){ # if we're not delaying our start date, most of this part is not relevant. 
  weights_mat = cluster_data(data_twoyears)
  
}else{
  print(paste0("Beginning at iteration ", as.character(startday)))
  
  if(recluster_period <= 0){ #if we're not reclustering, then just grab the clusters from day 1.
    weights_mat = cluster_data(data_twoyears)
  }else if((startday - 1) %% recluster_period != 0){
  
    # we need to simulate the most *recent* reclustering.
    # we recluster every time (day - 1) %% recluster_period == 0, so
    # find the largest day that is <= startday where reclustering happened.
    tempday = startday- ((startday - 1) %% recluster_period)
    
    index_window = (dayone_index + tempday - windowsize - 1):(dayone_index + tempday - 2) # index of our data
    daterange = data_index[index_window,]
    daterange = daterange$date
    
    data_window = prepare_data(data[, colnames(data) %in% daterange])
    data_window = apply(data_window, 2, as.numeric)
    
    weights_mat = cluster_data(data_window)
  }
  #note that if (startday - 1)%%recluster_period == 0, then we can just do nothing.
  # clustering will be handled by normal reclustering code.
}


total_iterations = days_in_year
#how many days forward do you want to run analysis for. this should be set to days_in_year, except when testing. 
# for 2021, it should be days_in_year - 21 instead, since we don't have enough data to look forward past 2021.

print((dayone_index + (startday - 1)):(dayone_index+total_iterations + (startday - 2)))
for(day in (dayone_index + (startday - 1)):(dayone_index+(total_iterations-1) + (startday - 1))){
  
  
  print(paste0("Iteration ", day - dayone_index + 1, " of ", total_iterations))
  print(Sys.time() - starttime) 
  # gather data, per our rolling window
  index_window = (day - windowsize):(day - 1) # index of our data
  daterange = data_index[index_window,]
  daterange = daterange$date
  
  # grab data from appropriate dates, do some basic processing
  data_window = prepare_data(data[, colnames(data) %in% daterange])
  data_window = apply(data_window, 2, as.numeric)
  
  
  # perform periodic reclustering, every `recluster_period` days. Note that the first clustering is day 1.
  if( (day - dayone_index) %% recluster_period == 0 & recluster_period > 0 & (day - dayone_index) != 0){
    weights_mat <- cluster_data(data_window)
    print(Sys.time() - starttime) 
  } 

  
  cluster_matrix <- t(weights_mat) %*% data_window #weight the data according to the clusters
  
  print(paste("earliest date in window is ", daterange[1]))
  print(paste("latest date in window is ", daterange[length(daterange)]))
  
  # perform RPCA analysis
  PCA <- rpca(cluster_matrix)
  L <- PCA$L
  S <- PCA$S
  E = cluster_matrix - L - S
  rownames(L) = rownames(cluster_matrix)
  colnames(L) = colnames(cluster_matrix)
  rownames(S) = rownames(cluster_matrix)
  colnames(S) = colnames(cluster_matrix)
  rownames(E) = rownames(cluster_matrix)
  colnames(E) = colnames(cluster_matrix)
  
  
  # --------
  
  # extract features (S1, S2, S3, E1, E2, E3）
  proj_data <- prepare_projection_by_cluster(S, E)
  n_days <- length(proj_data)
  
  
  # gather some *future* data, weighted by cluster, in order to compare the accuracy of our predictions
  
  future_window = (day):(day + 21) # a month or so. Make sure this window is larger than your max h.
  future_range = data_index[future_window,]
  future_range = future_range$date
  
  future_data = prepare_data(data[, colnames(data) %in% future_range])
  future_data = apply(future_data, 2, as.numeric)
  future_matrix <- t(weights_mat) %*% future_data
  
  # set forecast
  for(h in h_forecasts){
    #forecasting h days ahead.
    print(paste0("Forecasting value ", h))
    
    # load in previous data.
    pred_fret_list = pred_fret_compilation[[h]]
    true_fret_list = true_fret_compilation[[h]]
    hedged_fret_list = hedged_fret_compilation[[h]]
    spy_returns = spy_return_compilation[[h]]
    if(is.numeric(spy_returns) & spy_returns[1] == 0){ #first initialization check
      spy_returns = c()
    }
    
    
    # list of clusters
    cluster_list = rownames(cluster_matrix)
    
    
    

    #---
    
    pred_fret= c() # vector of fret predictions; pred_fret[1] will be the predicted fret of C1, etc
    true_fret = c() # true fret, from future data
    # run linear predictions
    for(cluster in cluster_list){
      
      #h = 1 #for now, we're only projecting 1 day ahead. 
      # build a linear regression model for each cluster based on the previous data
      # stored in proj_data.
      
      
      days_to_model = 500 #how far back to look in the linear model? note this isn't the same thing as how many days are included.
      # if you include too many, we're probably not going to get a good estimate
      # recent data is more important than long ago date
      
      # if you have a forecast of 21 days in the future, we have to leave out the recent 21 days of history, 
      # so we'll end up modeling based on data from 500 - 21 days
      
      # get data for this cluster
      cdata = proj_data[[cluster]]
      
      # extract the dates
      dates = rownames(cdata)
      
      # stores the projected h + 1 future data for every row of cdata.
      yvec = c()
      # loop over the dates, find the appropriate future projections.
      for(d in dates){
        yval = 0
        for(hstep in 1:h){
        
          dateindex = date_to_index(d, data_index) #find index of the relevant day
          dateindex = dateindex + hstep #increment that by h, which represents moving forward h business days
          newdate = as.character(index_to_date(dateindex, data_index)) # find the corresponding day
          
          # check to see if we've exceeded range of cluster_matrix; if so, reference values from future_matrix instead.
          if(as.numeric(newdate) > as.numeric(colnames(cluster_matrix)[ncol(cluster_matrix)])){
            addval = future_matrix[cluster, newdate]
          }else{
            addval = cluster_matrix[cluster, newdate]
          }
          yval = yval + addval #
        }
        
        yvec = c(yvec, yval)
        
      }
      
      
      
      # trim down to recent data
      cdata = cdata[h:days_to_model,] 
      yvec = yvec[h:days_to_model]
      
      
      
      # yes it's kind of wasteful to gather all the data and then just use part of it. 
      # oh well. it was easier to write this way.
      
      # generates linear model
      lm_model <- lm(yvec ~ ., data = cdata)
      summary(lm_model)
      
      
      
      # generate current S1, S2, S3, E1, E2, E3
      S1_current = S[cluster, ncol(S) - 1]
      S2_current = sum(S[cluster, (ncol(S) - 6):(ncol(S) - 2)])
      S3_current = sum(S[cluster, (ncol(S) - 22):(ncol(S) - 7)])
      E1_current = E[cluster, ncol(E) - 1]
      E2_current = sum(E[cluster, (ncol(E) - 6):(ncol(E) - 2)])
      E3_current = sum(E[cluster, (ncol(E) - 22):(ncol(E) - 7)])
      
      current_data = c(S1_current, S2_current, S3_current, E1_current, E2_current, E3_current)
      
      
      # predict
      current_data = as.data.frame(t(current_data))
      colnames(current_data) <- c("S1", "S2", "S3", "E1", "E2", "E3")
      
      predicted_fret = predict(lm_model, newdata = current_data)
      # actual data
      true_return = future_matrix[cluster, h]
      
      pred_fret = c(pred_fret, predicted_fret)
      true_fret = c(true_fret, true_return)
    }
    date = as.character(index_to_date(day, data_index)) #day is an index, date is a date in the form of YYYYMMDD
    pred_fret_list[[date]] <- pred_fret
    true_fret_list[[date]] <- true_fret
    
    spy_val = data["SPY", as.character(index_to_date(day + h, data_index))] # NOTE PLEASE DOUBLE CHECK THIS IS ACTUALLY THE CORRECT SPY VALUE
    # IT WOULD BE REALLY EASY TO MESS EVERYTHING UP IF YOU SLIPPED IN THE WRONG DATE
    spy_returns = c(spy_returns, spy_val)
    # build hedged returns by subtracting off the SPY return 
    hedged_fret_list[[date]] = pred_fret  - spy_val
    
    
    
    pred_fret_compilation[[h]] <- pred_fret_list 
    true_fret_compilation[[h]] <- true_fret_list 
    hedged_fret_compilation[[h]] <- hedged_fret_list
    spy_return_compilation[[h]] <- spy_returns
  }

  
}
print("Saving Data...")
saveRDS(pred_fret_compilation, paste0(year, "_pred_fret_compilation.rds"))
saveRDS(true_fret_compilation, paste0(year, "_true_fret_compilation.rds"))
saveRDS(hedged_fret_compilation, paste0(year, "_hedged_fret_compilation.rds"))
saveRDS(spy_return_compilation, paste0(year, "_spy_return_compilation.rds"))
print(paste0("Data saved to ", getwd()))
