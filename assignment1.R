# To install "I-Graph" package in R
install.packages("igraph","data.table")
library(igraph)

#Load the graphs in R 
jazzNetGraph <- read_graph("jazz.net", format = "pajek")
celegansMetabolicGraph <- read_graph("celegans_metabolic.net",format = "pajek")
higgsSocialNetworkGraph <- read_graph("higgs-social_network.edgelist", format = "edgelist")
higgsMenNetworkGraph <- read_graph("higgs-mention_network.edgelist", format = "edgelist")
higgsRetweetNetworkGraph <- read_graph("higgs-retweet_network.edgelist", format = "edgelist")
erdosRenyi01Graph <- erdos.renyi.game(2000, 0.01)
erdosRenyi005Graph <- erdos.renyi.game(2000, 0.005)
erdosRenyi0025Graph <- erdos.renyi.game(2000, 0.0025)

#Create a Data Frame as " Network Type n m c d l L ccl ccg " 
dataFrame <- data.frame("Network" = character(8),
                 "Type" = character(8),
                 "n" = integer(8),
                 "m" = integer(8),
                 "cStrong" =integer(8),
                 "cWeak" = integer(8),
                 "d" = integer(8),
                 "l" = double(8),
                 "L" = integer(8),
                 "ccLocal" = double(8),
                 "ccGlobal" = double(8),
                 stringsAsFactors = FALSE)
# Jazz Network 
dataFrame$Network[1] <- "Jazz muscians network "
dataFrame$Type[1] <- if(is_directed(jazzNetGraph) == TRUE) "DIRECTED" else "UNDIRECTED"
dataFrame$n[1] <- vcount(jazzNetGraph)
dataFrame$m[1] <- ecount(jazzNetGraph)
dataFrame$c_strong[1] <- count_components(jazzNetGraph, mode = "strong")
dataFrame$c_weak[1] <- count_components(jazzNetGraph, mode = "weak")
dataFrame$d[1] <- max(degree(jazzNetGraph))
dataFrame$l[1] <- mean_distance(jazzNetGraph)
dataFrame$L[1] <- diameter(jazzNetGraph)
dataFrame$cc_local[1] <- transitivity(jazzNetGraph, type = "localaverageundirected")
dataFrame$cc_global[1] <- transitivity(jazzNetGraph, type = "globalundirected")

# C.elegans metabolic network
dataFrame$Network[2] <- "C.elegans metabolic network"
dataFrame$Type[2] <- if(is_directed(celegansMetabolicGraph) == TRUE) "DIRECTED" else "UNDIRECTED"
dataFrame$n[2] <- vcount(celegansMetabolicGraph)
dataFrame$m[2] <- ecount(celegansMetabolicGraph)
dataFrame$c_strong[2] <- count_components(celegansMetabolicGraph, mode = "strong")
dataFrame$c_weak[2] <- count_components(celegansMetabolicGraph, mode = "weak")
dataFrame$d[2] <- max(degree(celegansMetabolicGraph))
dataFrame$l[2] <- mean_distance(celegansMetabolicGraph)
dataFrame$L[2] <- diameter(celegansMetabolicGraph)
dataFrame$cc_local[2] <- transitivity(celegansMetabolicGraph, type = "localaverageundirected")
dataFrame$cc_global[2] <- transitivity(celegansMetabolicGraph, type = "globalundirected")

# Higgs Twitter-Soc-Net network
dataFrame$Network[3] <- "Higgs Twitter-Soc-Net network"
dataFrame$Type[3] <- if(is_directed(higgsSocialNetworkGraph) == TRUE) "DIRECTED" else "UNDIRECTED"
dataFrame$n[3] <- vcount(higgsSocialNetworkGraph)
dataFrame$m[3] <- ecount(higgsSocialNetworkGraph)
dataFrame$c_strong[3] <- count_components(higgsSocialNetworkGraph, mode = "strong")
dataFrame$c_weak[3] <- count_components(higgsSocialNetworkGraph, mode = "weak")
dataFrame$d[3] <- max(degree(higgsSocialNetworkGraph))
dataFrame$l[3] <- NA
dataFrame$L[3] <- NA
dataFrame$cc_local[3] <- transitivity(higgsSocialNetworkGraph, type = "localaverageundirected")
dataFrame$cc_global[3] <- transitivity(higgsSocialNetworkGraph, type = "globalundirected")

#  Higgs Twitter-Men-Net network
dataFrame$Network[4] <- "Higgs Twitter-Men-Net network"
dataFrame$Type[4] <- if(is_directed(higgsMenNetworkGraph) == TRUE) "DIRECTED" else "UNDIRECTED"
dataFrame$n[4] <- vcount(higgsMenNetworkGraph)
dataFrame$m[4] <- ecount(higgsMenNetworkGraph)
dataFrame$c_strong[4] <- count_components(higgsMenNetworkGraph, mode = "strong")
dataFrame$c_weak[4] <- count_components(higgsMenNetworkGraph, mode = "weak")
dataFrame$d[4] <- max(degree(higgsMenNetworkGraph))
dataFrame$l[4] <- NA
dataFrame$L[4] <- NA
dataFrame$cc_local[4] <- transitivity(higgsMenNetworkGraph, type = "localaverageundirected")
dataFrame$cc_global[4] <- transitivity(higgsMenNetworkGraph, type = "globalundirected")

# Higgs Twitter-Retweet-Net network
dataFrame$Network[5] <- "Higgs Twitter-Retweet-Net network"
dataFrame$Type[5] <- if(is_directed(higgsRetweetNetworkGraph) == TRUE) "DIRECTED" else "UNDIRECTED"
dataFrame$n[5] <- vcount(higgsRetweetNetworkGraph)
dataFrame$m[5] <- ecount(higgsRetweetNetworkGraph)
dataFrame$c_strong[5] <- count_components(higgsRetweetNetworkGraph, mode = "strong")
dataFrame$c_weak[5] <- count_components(higgsRetweetNetworkGraph, mode = "weak")
dataFrame$d[5] <- max(degree(higgsRetweetNetworkGraph))
dataFrame$l[5] <- NA
dataFrame$L[5] <- NA
dataFrame$cc_local[5] <- transitivity(higgsRetweetNetworkGraph, type = "localaverageundirected")
dataFrame$cc_global[5] <- transitivity(higgsRetweetNetworkGraph, type = "globalundirected")

#Erdos Renyi 0.01 network 
dataFrame$Network[6] <- "Erdos Renyi 0.01 network"
dataFrame$Type[6] <- if(is_directed(erdosRenyi01Graph) == TRUE) "DIRECTED" else "UNDIRECTED"
dataFrame$n[6] <- vcount(erdosRenyi01Graph)
dataFrame$m[6] <- ecount(erdosRenyi01Graph)
dataFrame$c_strong[6] <- count_components(erdosRenyi01Graph, mode = "strong")
dataFrame$c_weak[6] <- NA
dataFrame$d[6] <- max(degree(erdosRenyi01Graph))
dataFrame$l[6] <- max(degree(erdosRenyi01Graph))
dataFrame$L[6] <- mean_distance(erdosRenyi01Graph)
dataFrame$cc_local[6] <- transitivity(erdosRenyi01Graph, type = "localaverageundirected")
dataFrame$cc_global[6] <- transitivity(erdosRenyi01Graph, type = "globalundirected")

#Erdos Renyi 0.005 network
dataFrame$Network[7] <- "Erdos Renyi 0.005 network"
dataFrame$Type[7] <- if(is_directed(erdosRenyi005Graph) == TRUE) "DIRECTED" else "UNDIRECTED"
dataFrame$n[7] <- vcount(erdosRenyi005Graph)
dataFrame$m[7] <- ecount(erdosRenyi005Graph)
dataFrame$c_strong[7] <- count_components(erdosRenyi005Graph, mode = "strong")
dataFrame$c_weak[7] <- NA
dataFrame$d[7] <- max(degree(erdosRenyi005Graph))
dataFrame$l[7] <- max(degree(erdosRenyi005Graph))
dataFrame$L[7] <- mean_distance(erdosRenyi005Graph)
dataFrame$cc_local[7] <- transitivity(erdosRenyi005Graph, type = "localaverageundirected")
dataFrame$cc_global[7] <- transitivity(erdosRenyi005Graph, type = "globalundirected")

#Erdos Renyi 0.0025 network
dataFrame$Network[8] <- "Erdos Renyi 0.0025 network"
dataFrame$Type[8] <- if(is_directed(erdosRenyi0025Graph) == TRUE) "DIRECTED" else "UNDIRECTED"
dataFrame$n[8] <- vcount(erdosRenyi0025Graph)
dataFrame$m[8] <- ecount(erdosRenyi0025Graph)
dataFrame$c_strong[8] <- count_components(erdosRenyi0025Graph, mode = "strong")
dataFrame$c_weak[8] <- NA
dataFrame$d[8] <- max(degree(erdosRenyi0025Graph))
dataFrame$l[8] <- max(degree(erdosRenyi0025Graph))
dataFrame$L[8] <- mean_distance(erdosRenyi0025Graph)
dataFrame$cc_local[8] <- transitivity(erdosRenyi0025Graph, type = "localaverageundirected")
dataFrame$cc_global[8] <- transitivity(erdosRenyi0025Graph, type = "globalundirected")

kable(dataFrame, format = "markdown")
