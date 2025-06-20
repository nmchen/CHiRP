library(RColorBrewer)

## compile quantile plot for every year
h_forecasts = c(1, 5)
quantile_sharpe_list = list()
for(h in h_forecasts){
  quantile_sharpe_list[[h]] = as.data.frame(matrix(ncol = 4, nrow = 0))
}

years = 2002:2020
for(year in years){

  output_width = 750
  output_height = 450
  

  loaded_data <- load_data(year)
  pred_fret_compilation = loaded_data[["pred"]]
  hedged_fret_compilation = loaded_data[["hedged"]]
  true_fret_compilation = loaded_data[["true"]]
  spy_return_compilation = loaded_data[["spy"]]


  
  
  hedged_return_compilation = list()
  return_compilation = list()
  
  sharpes = list()
  hedged_sharpes = list()
  for(h in h_forecasts){
    #sharpe_summaries = c()
    #hedged_sharpe_summaries = c()
    #return_summaries = c()
    #hedged_return_summaries = c()
    pnl_vec = c()
    hedged_pnl_vec = c()
    pred_fret_list <- pred_fret_compilation[[h]]
    true_fret_list <- true_fret_compilation[[h]]
    hedged_fret_list <- hedged_fret_compilation[[h]]
    spy_returns <- spy_return_compilation[[h]]
    
    
    for(name in names(pred_fret_list)){
      
      #print(name)
      p = as.numeric(pred_fret_list[[name]])
      t = as.numeric(true_fret_list[[name]])
      hg = as.numeric(hedged_fret_list[[name]])
      
      pnl = sum(sign(p) * t)
      
      pnl_vec = c(pnl_vec, pnl)
      #sharpe <- mean(pnl, na.rm = TRUE) / sd(pnl, na.rm = TRUE) #* sqrt(252)
      #sharpe_summaries = c(sharpe_summaries, sharpe)
      #return_summaries = c(return_summaries, mean(exp(1)^pnl - 1))
      
      # okay I'm not *super* certain about this. But given that each data is OPCL = log(OP/CL)
      # I think to get the 'naive' returns for a day, we should undo the log
      # which would essentially tell us the "percent" return, i.e if OP<CLit'd be <100% of original
      # we then subtract 1 to get percent change
      # then we can just sum up percent changes, which simulates a simple
      # portfolio strategy of just investing some constant amount, every day, split equally amongst
      # all clusters according to predicted results.
      
      hedged_pnl = sum(sign(hg) * t)
      hedged_pnl_vec = c(hedged_pnl_vec, hedged_pnl)
      #hedged_sharpe =  mean(hedged_pnl, na.rm = TRUE) / sd(hedged_pnl, na.rm = TRUE)
      
      #hedged_return_summaries = c(hedged_return_summaries, mean(exp(1)^hedged_pnl - 1))
      
      
      
      #hedged_sharpe_summaries = c(hedged_sharpe_summaries, hedged_sharpe )
      
    }
    
    
    print(paste0("Hedged sharpe test for h = ", h, " is p =  ", sharpe_test(hedged_pnl_vec)))
    
    hedged_return_compilation[[h]] <- hedged_pnl_vec
    return_compilation[[h]] <- pnl_vec
    
    sharpes[[h]] = mean(pnl_vec)/sd(pnl_vec) * sqrt(252)
    hedged_sharpes[[h]] = mean(hedged_pnl_vec)/sd(hedged_pnl_vec) * sqrt(252)
    
    plot(1:length(pnl_vec), pnl_vec, xlab = "Days Since Year Start", ylab = "PnL", 
         main = paste0("Unhedged ", year, ", h = ", h), col = sharp_coloring(pnl_vec), pch = 20)
    plot(1:length(hedged_pnl_vec), hedged_pnl_vec, xlab = "Days Since Time Period Start", 
         ylab = "PnL", main = paste0("Hedged ", year, ", h = ", h), col = sharp_coloring(hedged_pnl_vec), pch = 20)
    
    
    
    pnl_filename = paste0("hedged_", year, "_h", h, ".png")
    png(pnl_filename, width = output_width, height = output_height, pointsize = font_size)
    plot(1:length(hedged_pnl_vec), hedged_pnl_vec, xlab = "Days Since Time Period Start", 
         ylab = "PnL", main = paste0("Hedged ", year, ", h = ", h), col = sharp_coloring(hedged_pnl_vec), pch = 20)
    dev.off()
    
    
    
    hedged_cum_pnl <- cum_sum_vector(hedged_pnl_vec)
    plot(1:length(hedged_cum_pnl), hedged_cum_pnl, type ="l", xlab = "days since start", ylab = "PnL", main = "Cumulative PnL")
    
    cum_pnl_filename = paste0("cumulative_hedged_", year, "_h", h, ".png")
    png(cum_pnl_filename, width = output_width, height = output_height, , pointsize = font_size)
    plot(1:length(hedged_cum_pnl), hedged_cum_pnl, type ="l", xlab = "days since start", ylab = "PnL", main = "Cumulative PnL", cex.names = 2, cex.lab=2, cex.axis=2, cex.main=2.5, cex.sub=2)
    dev.off()
    
    
    # build quantile portfolios
    # aim for four quantiles: top 25%, 50%, 75%, and 100% of predictions
    quantile_levels <- c(0.25, 0.5, 0.75, 1)
    # storing matrix
    pnl_by_quantile <- matrix(NA, nrow = length(quantile_levels), ncol = length(names(hedged_fret_list)))
    rownames(pnl_by_quantile) <- paste0("Top ", quantile_levels * 100, "%")
    
    i = 1
    for (name in names(hedged_fret_list)) {
      preds <- hedged_fret_list[[name]]
      actuals <- true_fret_list[[name]]
      # rank by absolute magnitude
      ranked_idx <- order(abs(preds), decreasing = TRUE)
      
      for (q in seq_along(quantile_levels)) {
        cutoff <- ceiling(quantile_levels[q] * length(preds))
        selected_idx <- ranked_idx[1:cutoff]
        
        pnl_val <- sum(sign(preds[selected_idx]) * actuals[selected_idx])
        #sharpe <- mean(pnl, na.rm = TRUE) / sd(pnl, na.rm = TRUE)
        pnl_by_quantile[q, i] <- pnl_val
      }
      i = i + 1
    }
    
    quantile_sharpe_vec = c()
    quantile_sharpe_test = c()
    for(r in 1:nrow(pnl_by_quantile)){
      row = pnl_by_quantile[r,]
      sharpe = mean(row)/sd(row) * sqrt(252)
      quantile_sharpe_vec  = c(quantile_sharpe_vec, sharpe)
      
      quantile_sharpe_test = c(quantile_sharpe_test, sharpe_test(row))
    }
    
    quantile_sharpe_list[[h]] = rbind(quantile_sharpe_list[[h]], quantile_sharpe_vec)
    
    print(paste0("Quantile sharpes for h = ", h, ", year = ", year, " are = ", round(quantile_sharpe_test, 3), " with signifiance ", round(quantile_sharpe_test, 4)))
    
    
    
    barplot(quantile_sharpe_vec, names.arg = rownames(pnl_by_quantile),
            main = paste0("Sharpe Ratio by Quantile Portfolio, ", year, " h = ", h),
            ylab = "Sharpe Ratio", col = "lightblue")
    
    
    quantile_filename = paste0("quantile_", year, "_h", h, ".png")
    png(quantile_filename, width = output_width, height = output_height, pointsize = font_size)
    barplot(quantile_sharpe_vec, names.arg = rownames(pnl_by_quantile),
            main = paste0("Sharpe Ratio by Quantile Portfolio, ", year, " h = ", h),
            ylab = "Sharpe Ratio", col = "lightblue")
    dev.off()
    
    
    
    
  }


}
for(h in h_forecasts){
  colnames(quantile_sharpe_list[[h]]) = c("25%", "50%", "75%", "100%")
  rownames(quantile_sharpe_list[[h]]) = years
  
  quantile_file = paste0("quantile_performance_per_year_h=", h, ".png")
  png(filename = quantile_file, width = 1000, height = 380)
  barplot(as.matrix(t(quantile_sharpe_list[[h]])), beside = TRUE, col = brewer.pal(4, "Spectral"), xaxt = "n", ylab = "Sharpe", xlab = "Year", main = paste0("Quantile Performance per Year, h = ", h))
  legend("topleft",  legend = c("25%", "50%", "75%", "100%"), fill = brewer.pal(4, "Spectral"))
  text(x = 1:length(years)*5 - 2,
       ## Move labels to just below bottom of chart.
       y = par("usr")[3] - 0.45,
       ## Use names from the data list.
       labels = years,
       ## Change the clipping region.
       xpd = NA,
       ## Rotate the labels by 35 degrees.
       srt = 90,
       ## Adjust the labels to almost 100% right-justified.
       adj = .5,
       ## Increase label size.
       cex = 1.3)
  dev.off()
}




