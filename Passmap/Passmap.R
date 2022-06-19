nodes <- read_csv("asgmt_nodes.csv")
edges <- read_csv("asgmt_edges.csv")

g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

V(g)$name <- nodes$media 

plot(g)
par(mar=c(0,0,0,0))

edge_density(g)
centr_degree(g, mode = 'all', normalized = TRUE)
