# IMPORTING LIBRARIES ---------------------------------------------------------

library(dplyr)

# SUBSET DATA -----------------------------------------------------------------

# The following function will read in a filename (string) and a ratio (0.0-1.0)
# and will return a subset of the data. The default ratio is 0.25 or 25% of the
# data. This can be changed to 1.0 to return all the data with the required
# columns.

subset_cols <- function(filename, ratio = 0.25) {
  
  print(paste0("Reading file: ", filename))
  
  data <- read.csv(filename)
  
  cols <- c("protocol_type",
            "service",
            "flag",
            "land",
            "logged_in",
            "duration",
            "dst_bytes",
            "wrong_fragment",
            "num_compromised",
            "serror_rate",
            "srv_rerror_rate",
            "src_bytes",
            "dst_host_srv_diff_host_rate")
  
  data <- data %>%
    sample_frac(ratio) %>%
    select(cols)
  
  return(data)
   
}

# TESTING ---------------------------------------------------------------------

# filename <- "kddcup.data.name.attack.csv"
# sample <- subset_cols(filename)
# nrow(sample)
