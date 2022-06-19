library(igraph)

names <- c("john", "michael", "scottie", "charles",
           "hakeem", "karl", "david", "clyde",
           "shawn", "reggie", "gary", "mitch")

id <- c(0:11)
nodes <- data.frame(names, id)

g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
g2 <- graph_from_data_frame(d = edges2, vertices = nodes, directed = TRUE)

V(g)
E(g)
E(g)$weight
g[]
g2[]

plot(g)
par(mar=c(0,0,0,0))
plot(g)
plot(g, edge.width = E(g)$weight, edge.arrow.size = 0.5)
strength(g)
V(g)$size <- log(strength(g))
vertex_attr(g)
plot(g, edge.width = E(g)$weight, edge.arrow.size = 0.5)
V(g)$size <- log(strength(g))*4
plot(g, edge.width = E(g)$weight*.2, edge.arrow.size = 0.5)
plot(g, edge.width = E(g)$weight*.2, edge.arrow.size = 0.5, layout = layout_in_circle)
plot(g, edge.width = E(g)$weight*.2, edge.arrow.size = 0.5, layout = layout_as_tree)
plot(g, edge.width = E(g)$weight*.2, edge.arrow.size = 0.5, layout = layout_as_star)

V(g2)$size <- log(strength(g2))*4
plot(g2, edge.width = E(g2)$weight*.2, edge.arrow.size = 0.5, layout = layout_in_circle)

starting <- c("john", "michael", "scottie",
              "charles", "hakeem")
bench <- c("karl", "david", "clyde", "shawn",
           "reggie", "gary", "mitch")
V(g)$color <- NA
V(g)$color[V(g)$name %in% starting] <-"red"
V(g)$color[V(g)$name %in% bench] <-"green"
vertex_attr(g)
plot(g, edge.width = E(g2)$weight*.2, edge.arrow.size = 0.5, layout = layout_in_circle)

edge_density(g)
edge_density(g2)

degree(g, v=V(g))
degree(g, v=V(g), mode = 'in')
degree(g, v=V(g), mode = 'out')
degree(g, v=V(g), mode = 'all')

centr_degree(g, mode = 'all', normalized = TRUE)
centr_degree(g2, mode = 'all', normalized = TRUE)






