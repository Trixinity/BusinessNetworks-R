# Necessary packages
library(igraph, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(reshape2, warn.conflicts = FALSE)

# Read dataset from Securities Data Company (SDC) Platinum
sdc <- readRDS("SDC_data_2021.rds")

# function getting dataframe of companies 
get_dataframe <- function(company_name) {
    # Get the deal numbers made of the company
    company_dealnumber <- sdc %>% filter(type == "Joint Venture",
        participants == company_name) %>%
            select(deal_number)
    company_dealnumber <- as.vector(t(company_dealnumber))

    # Get the data frame of join venture deals of company_name
    dataframe <- sdc %>% filter(deal_number %in% company_dealnumber,
    SIC_primary != "999A", #Remove countries from Joint Ventures
    participants != company_name) %>% #Removes company_name from the list
        select(participants, business_description, participant_nation)
    return(dataframe)
}

# Function to get a matrix
get_matrix <- function(dataframe_name, company_name) {
    vector <- dataframe_name %>%
        select(participants)
    vector <- as.vector(t(vector))

    # Setting up the data frame for an adjecency matrix
    length <- length(na.omit(vector))
    vector_company <- rep(c(company_name), each=length)
    dataframe <- data.frame(vector_company, vector)
    matrix <- get.adjacency(graph.edgelist(as.matrix(dataframe),
        directed = FALSE))
    return(matrix)
}

# Get data frames of join ventures deals with company_name
ford_jointventures <- get_dataframe("Ford Motor Co") # Ford
bmw_jointventures <- get_dataframe("Bayerische Motoren Werke AG") # BMW
toyota_jointventures <- get_dataframe("Toyota Motor Corp") # Toyota

# get matrices
matrix_ford <- get_matrix(ford_jointventures, "Ford Motor Co")
graph_ford <- graph.adjacency(matrix_ford, mode="undirected")
set.seed(689)
plot(ig)

#sry <- ddply(matrix_dataframe, .(vector_ford,vector_ford_jv),
#    summarize, Frequency=length(vector_ford))
