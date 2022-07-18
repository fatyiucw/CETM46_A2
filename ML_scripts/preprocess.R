# IMPORTING LIBRARIES ---------------------------------------------------------

library(dplyr)

# FOR VISUALISATION -----------------------------------------------------------

## READING DATA ---------------------------------------------------------------

filename_ddos <- "Friday-WorkingHours-Afternoon-DDos.pcap_ISCX.csv"
filename_port <- "Friday-WorkingHours-Afternoon-PortScan.pcap_ISCX.csv"
ddos <- read.csv(filename_ddos)
port <- read.csv(filename_port)

## COMBINING DATA -------------------------------------------------------------

data <- rbind(ddos, port)

# write.csv(data, "full_data.csv", row.names=FALSE)

subset <- data %>%
  sample_frac(0.25)

# write.csv(subset, "25_perc_data.csv", row.names = FALSE)

subset <- read.csv("data/25_perc_data.csv")

for_plot <- subset[, c(1,3,5,7, 85)]

# write.csv(for_plot, "25_for_plot.csv", row.names = FALSE)
