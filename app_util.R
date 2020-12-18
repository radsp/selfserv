# Function to parse the dates from calendar (retrieved as text)
# and convert to date object. Dates length of two represent
# range, and length one is just one time point
get_tt <- function(tt_from_cal, res) {
  ttc <- as.Date(tt_from_cal)
  interval <- if_else(res %in% "month", "1 month", "1 year")
  if (length(ttc) > 1) {
    tt <- seq.Date(from = ttc[1], to = ttc[2], by = interval)
  } else {
    tt <- ttc[1]
  }
  return(tt)
}

# dat_scatter <- function()