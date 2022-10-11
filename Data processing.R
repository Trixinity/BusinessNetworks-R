#necessary packages
library(igraph, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# Read dataset from Securities Data Company (SDC) Platinum
sdc <- readRDS("SDC_data_2021.rds")

# Filter the set with only Ford and their alliance_SIC_code
ford_alliances <- sdc %>% filter(status == "Completed/Signed",
    date_terminated == "",
    type == "Joint Venture",
    participants == "Ford Motor Co") %>%
        select(participants, date_announced, type, SIC_primary,
        participant_nation, deal_number)
