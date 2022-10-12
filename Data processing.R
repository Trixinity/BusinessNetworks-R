# Necessary packages
library(igraph, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# Read dataset from Securities Data Company (SDC) Platinum
sdc <- readRDS("SDC_data_2021.rds")

# Filter the set with only Ford and their deal_number
ford_dealnumber <- sdc %>% filter(type == "Joint Venture",
    participants == "Ford Motor Co") %>%
        select(deal_number)
vector_dealnumber <- as.vector(t(ford_dealnumber))

# List of companies which have/had a Joint Venture with Ford.
ford_jointventures <- sdc %>% filter(deal_number %in% vector_dealnumber,
    SIC_primary != "999A", #Remove countries from Joint Ventures
    participants != "Ford Motor Co") #Removes Ford from the list.