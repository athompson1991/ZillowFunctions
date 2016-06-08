get_zillow_data <- function(quote_type = "AllHomes", loc_type = "State", filename = "out.csv"){
  quote_types <- c("AllHomes", "Condominium", "SingleFamilyResidence"
                   ,"1bedroom","2bedroom","3bedroom", "4bedroom", "5BedroomOrMore"
  )
  loc_types <- c("State", "Metro", "County", "City", "Zip", "Neighborhood")


  if(!(quote_type %in% quote_types) | !(loc_type %in% loc_types)){
    e = simpleError("badQuote")
    good_quote <- paste("Valid Quotes:", paste(quote_types, sep = "", collapse = ", "), sep = " ")
    good_loc <- paste("Valid Locations:", paste(loc_types, sep = "", collapse = ", "), sep = " ")
    good_all <- paste(good_quote, "\n", good_loc, sep = "", collapse = "")
    tryCatch(stop(e), error = function(e) e, finally = cat(good_all))
    break
  }

  if(quote_type == "Condominium"){
    quote_type <- "Condominum"
  }

  url_head <- "http://files.zillowstatic.com/research/public/"
  pasted <- paste(url_head, loc_type, "/", loc_type,"_Zhvi_", quote_type, ".csv", sep = "")
  temp_csv <- paste(tempfile(),".csv",sep = "")
  download.file(pasted,destfile = temp_csv)
  raw_data <- read.csv(temp_csv)
  fixed_data <- fix_zillow_data(raw_data)
  fixed_data <- do_returns(fixed_data)
  fixed_data$info$unique_id <- paste(fixed_data$info$RegionID, fixed_data$info$RegionName, sep = "_")
  fixed_data <- do_returns(fixed_data)
  fixed_data
}


fix_zillow_data <- function(d, na_rm = T){
  colname_logic <- unlist(lapply(strsplit(colnames(d), ""), function(x) "X" %in% x))
  info_cols <- d[ ,colnames(d)[!(colname_logic)]]
  data_cols <- t(d[ ,colnames(d)[colname_logic]])
  colnames(data_cols) <- d$RegionName
  dates <- as.Date(paste(gsub("X","",row.names(data_cols)),"01",sep = "."), "%Y.%m.%d")
  out_list <- list(info = info_cols, time_series = zoo::zoo(data_cols, order.by = dates))
  if(na_rm){
    good_cols <- which(unlist(apply(out_list$time_series, 2, function(x) all(!(is.na(x))))))
    out_list$info <- out_list$info[good_cols,]
    out_list$time_series <- out_list$time_series[,good_cols]
  }
  return(out_list)
}

do_returns <- function(fixed_obj){
  timeseries <- fixed_obj$time_series
  fixed_obj[["returns"]] <- xts::as.xts(diff(log(timeseries))[-1, ])
  fixed_obj
}


calc_basic_data <- function(obj, start = NA, end = NA){
  if(is.na(start)){
    start <- start(obj$returns)
  }else{
    start <- start
  }
  if(is.na(end)){
    end = end(obj$returns)
  }else{
    end <- end
  }
  date_range <- paste(start, end, sep = "/")
  print(date_range)
  returns_df <- obj$returns[date_range]
  prices <- xts::as.xts(obj$time_series)[date_range]
  mean_mth_ret <- apply(returns_df, 2, mean)
  mean_ann_ret <- mean_mth_ret * 12
  sd_mth_ret <- apply(returns_df, 2, sd)
  sd_ann_ret <- sd_mth_ret * sqrt(12)
  sharpe_ratio <- mean_ann_ret / sd_ann_ret
  last_price <- apply(prices, 2, xts::last)
  out <- data.frame(
    row.names = obj$info$unique_id
    ,mean_mth_ret
    ,mean_ann_ret
    ,sd_mth_ret
    ,sd_ann_ret
    ,sharpe_ratio
    ,last_price
  )
  out
}

performance_chart <- function(returns, from = "2014-01-01", to = "2015-11-01"){
  returns <- window(returns, start = as.Date(from), end = as.Date(to))
  performance <- (exp(cumsum(returns)) - 1) * 100
  plot_range <- range(performance)
  xts::plot.xts(performance[ ,1]
           , type = "n"
           , main = "Performance"
           , xlab = NA
           , ylab = "Percentage Growth"
           , las = 1
           , ylim = plot_range)
  for(i in 1:ncol(performance)) lines(performance[ ,i], col = rgb(0,0,0,0.2))

}
