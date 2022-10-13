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
    participants != "Ford Motor Co") %>% #Removes Ford from the list
    select(participants, business_description, participant_nation)
ford_jointventures <- ford_jointventures %>%
    distinct(participants, .keep_all = TRUE) # Removes duplicates

# Filter the set with only BMW and their deal_number
bmw_dealnumber <- sdc %>% filter(type == "Joint Venture",
    participants == "Bayerische Motoren Werke AG") %>%
        select(deal_number)
vector_dealnumber_bmw <- as.vector(t(bmw_dealnumber))

# List of companies which have/had a Joint Venture with BMW.
bmw_jointventures <- sdc %>% filter(deal_number %in% vector_dealnumber_bmw,
    SIC_primary != "999A", #Remove countries from Joint Ventures
    participants != "Bayerische Motoren Werke AG") %>% #Removes BMW
    select(participants, business_description, participant_nation)
bmw_jointventures <- bmw_jointventures %>%
    distinct(participants, .keep_all = TRUE) # Removes duplicates

# Filter the set with only Toyota and their deal_number
toyota_dealnumber <- sdc %>% filter(type == "Joint Venture",
    participants == "Toyota Motor Corp") %>%
        select(deal_number)
vector_dealnumber_toyota <- as.vector(t(toyota_dealnumber))

# List of companies which have/had a Joint Venture with Toyota.
toyota_jointventures <- sdc %>% filter(
    deal_number %in% vector_dealnumber_toyota,
    SIC_primary != "999A", #Remove countries from Joint Ventures
    participants != "Toyota Motor Corp") %>% #Removes Toyota from the list
    select(participants, business_description, participant_nation)
toyota_jointventures <- toyota_jointventures %>%
    distinct(participants, .keep_all = TRUE) # Remove duplicates

# Create adjacency matrices of the dataframes
