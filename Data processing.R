# Necessary packages
library(igraph, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(plyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(reshape2, warn.conflicts = FALSE)

# Read dataset from Securities Data Company (SDC) Platinum
sdc <- readRDS("SDC_data_2021.rds")

# function getting dataframe of companies 
get_dataframe <- function(company_name) {
    # Get the deal numbers made of the company
    company_dealnumber <- sdc %>% filter(type == "Joint Venture",
        status != "Pending", participants == company_name) %>%
            select(deal_number)
    company_dealnumber <- as.vector(t(company_dealnumber))

    # Get the data frame of join venture deals of company_name
    dataframe <- sdc %>% filter(deal_number %in% company_dealnumber,
    SIC_primary != "999A", #Remove countries from Joint Ventures
    participants != company_name) %>% #Removes company_name from the list
        select(participants)
    return(dataframe)
}

# Get dataframe of edges with weights
edges_dataframe <- function(dataframe_name, company_name) {
    to <- as.vector(t(dataframe_name %>%
        select(participants)))
    from <- rep(c(company_name), each=length(na.omit(to)))
    edges_dataframe <- data.frame(from, to)

    edges_with_weights <- ddply(edges_dataframe, .(from,to),
        summarize, Frequency=length(to))
    colnames(edges_with_weights) <- c("from", "to", "weight")

    return(edges_with_weights)
}

# Get ties between companies we already have
network_between_companies <- function(company_1, company_2, company_3) {
    companies_together <- unique(as.vector(t(rbind(company_1, company_2,
        company_3))))
    companies_together <- companies_together[! companies_together %in%
        c("Ford Motor Co", "Bayerische Motoren Werke AG", "Toyota Motor Corp",
            "Undisclosed JV Partner")]
    edges_together <- data.frame()

    for (company in companies_together) {
        company_dealnumber <- sdc %>% filter(type == "Joint Venture",
            status != "Pending", participants == company) %>%
                select(deal_number)
        company_dealnumber <- as.vector(t(company_dealnumber))

        dataframe <- sdc %>% filter(deal_number %in% company_dealnumber,
            SIC_primary != "999A", participants != company,
            participants %in% companies_together) %>%
                select(participants)

        to <- as.vector(t(dataframe))
        from <- rep(c(company), each=length(na.omit(to)))
        edges_dataframe <- data.frame(from, to)

        edges_with_weights <- ddply(edges_dataframe, .(from,to),
            summarize, Frequency=length(to))

        edges_together <- rbind(edges_together, edges_with_weights)
        edges_together <- edges_together %>% filter()
    }
    colnames(edges_together) <- c("from", "to", "weight")
    return(edges_together)
}

# Get data frames of join ventures deals with company_name
ford_jointventures <- get_dataframe("Ford Motor Co") # Ford
bmw_jointventures <- get_dataframe("Bayerische Motoren Werke AG") # BMW
toyota_jointventures <- get_dataframe("Toyota Motor Corp") # Toyota

# Get dataframe of edges with weights
edges_ford <- edges_dataframe(ford_jointventures, "Ford Motor Co")
edges_bmw <- edges_dataframe(bmw_jointventures, "Bayerische Motoren Werke AG")
edges_toyota <- edges_dataframe(toyota_jointventures, "Toyota Motor Corp")

# Get ties between companies we already have
edges_list <- network_between_companies(ford_jointventures, bmw_jointventures,
    toyota_jointventures)

# Append the edges dragframes into one big dataframe
edges_ford_bmw_toyota_df <- rbind(edges_ford, edges_bmw, edges_toyota,
    edges_list)
# Write appended data as .csv
write.csv(edges_ford_bmw_toyota_df, "edges_bmw_ford_toyota.csv")

# Plotting the graph (example)
# g <- graph_from_data_frame(d = edges_ford_bmw_toyota_df, directed = FALSE)
# g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
# E(g)$weight <- E(g)$weight + 1
# V(g)$color <- "red"
# coords <- layout_with_fr(g)
# plot(g, layout = coords, vertex.label = NA, vertex.size = 5,
#     edge.width = E(g)$weight/5)

# Calculating the degree centrality of graph with Ford example
# deg <- degree(g)
# deg[order(deg, decreasing = TRUE)[1:5]]