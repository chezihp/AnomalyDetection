# based on https://bocoup.com/weblog/padding-time-series-with-r
get_all_times <- function(data, attr, by) {
  by = switch(by,
              day = 'day',
              hr = 'hour',
              min = 'min',
              sec = 'sec'
  )
  sorted.data <- data[order(data[[attr]]),]
  info.data.length <- length(sorted.data[[attr]])
  
  info.date.min <- sorted.data[[attr]][1]
  info.date.max <- sorted.data[[attr]][info.data.length]
  
  all.dates       <- seq(info.date.min, info.date.max, by=by)
  all.dates.frame <- data.frame(list(time=all.dates))
  names(all.dates.frame) <- attr
  merged.data <- merge(all.dates.frame, sorted.data, all=T)
  merged.data
}

zero_pad_data <- function(data, attr='timestamp', value='count', by='hr') {
  merged.data <- get_all_times(data, attr, by)
  merged.data[[value]][which(is.na(merged.data[[value]]))] <- 0
  merged.data
}

interpolate_pad_data <- function(data, attr='timestamp', value='count', by='hr') {
  all_times <- get_all_times(data, attr, by)
  if(any(is.na(all_times[[value]])))
    all_times[[value]] <- na.approx(all_times[[value]])
  all_times
}
