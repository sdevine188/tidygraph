library(tidyverse)
library(ggraph)
library(tidygraph)
library(igraph)


# https://www.data-imaginist.com/2017/ggraph-introduction-layouts/
# https://www.data-imaginist.com/2017/ggraph-introduction-edges/
# https://www.data-imaginist.com/2017/ggraph-introduction-nodes/
# https://jeremydfoote.com/Communication-and-Social-Networks/resources/ggraph_walkthrough.html
# https://tidygraph.data-imaginist.com/
# https://medium.com/slalom-technology/an-intro-to-graph-theory-and-analysis-using-tidygraph-d5199490963


# setwd
setwd("C:/Users/Stephen/Desktop/R/tidygraph")


#//////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////


# tutorial
# https://medium.com/slalom-technology/an-intro-to-graph-theory-and-analysis-using-tidygraph-d5199490963


supplier_producer <- data.frame(
        supplier = c('Supp. A', 'Supp. A', 'Supp. B', 
                     'Supp. B', 'Supp. C', 'Supp. D'),
        producer = c('Manuf. A', 'Manuf. B', 'Manuf. A', 
                     'Manuf. C', 'Manuf. B', 'Manuf. D')
)
supplier_producer 


#///////////////////


producer_distributor <- data.frame(
        producer = c('Manuf. A', 'Manuf. A', 'Manuf. B', 
                     'Manuf. B', 'Manuf. C', 'Manuf. D'),
        distributor = c('Dist. A', 'Dist. B', 'Dist. B', 
                        'Dist. C', 'Dist. B', 'Dist. C')
)
producer_distributor


#///////////////////


distributor_customer <- data.frame(
        distributor = c('Dist. A', 'Dist. A', 'Dist. B', 
                        'Dist. C', 'Dist. C', 'Dist. C'),
        customer = c('Cust. A', 'Cust. B', 'Cust. C', 
                     'Cust. C', 'Cust. D', 'Cust. E')
)
distributor_customer


#///////////////////////////////////////////////////////////////////////////////////


# combine three df into a single df
# note that from/to names can be important if the network has meaningful directionality (eg directed vs undirected graph)
network_df <- rbind(
        supplier_producer %>% 
                rename(from = supplier, to = producer),
        producer_distributor %>% 
                rename(from = producer, to = distributor),
        distributor_customer %>% 
                rename(from = distributor, to = customer)
)
network_df

# convert df into a tbl_graph
# note that tbl_graph splits the network_df into two tbls - one for nodes and one for edges
# note the edges tbl uses numeric id for the nodes, based on the node's row number in the node tbl (eg supp A is node/row 1)
network_grph <- as_tbl_graph(network_df)
network_grph

# add numeric ids
# note that activate() changes which of the two tbls are active/accessible; the other is more "appended in background"
# so the mutate() will work on whichever tbl is active
network_grph <- network_grph %>%
        activate(nodes) %>%
        mutate(id = row_number())
network_grph

# eg mutate edges tbl
network_grph %>%
        activate(edges) %>%
        mutate(id = row_number())

# add distance var to edges tbl
network_grph <- network_grph %>%
        activate(edges) %>%
        mutate(distance = from + to)
network_grph

# visualize as sankey plot
library(networkD3)
create_sankey <- function(grph_object, colors){
        nodes <- grph_object %>%
                activate(nodes) %>%
                data.frame()
        
        edges <- grph_object %>%
                activate(edges) %>%
                mutate(source = from - 1,
                       target = to - 1) %>%
                data.frame() %>%
                select(source, target, distance)
        
        network_p <- sankeyNetwork(Links = edges, Nodes = nodes,
                                   Source = 'source', Target = 'target', 
                                   Value = 'distance', NodeID = 'name',
                                   fontSize = 14, nodeWidth = 20, 
                                   sinksRight = FALSE, 
                                   colourScale = colors
        )
        print(network_p)
        
}
#please put the colourScale declaration on a single line!!!
colourScale ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF",
        "#35B779FF","#1F9E89FF","#26828EFF",
        "#31688EFF","#3E4A89FF","#482878FF","#440154FF"])'
p <- create_sankey(grph_object = network_grph, colors = colourScale)
p


#/////////////////////////////////////////////////////////////////////////////////


# find all nodes down from a given node (eg we have a bad supplier and want to find downstream partners)
# bfs_dist is breadth-first algorithm
bad_supplier = 'Supp. D'

bad_supplier_id <- network_grph %>%
        activate(nodes) %>%
        filter(name == bad_supplier) %>%
        pull(id)
bad_supplier_id

# bfs_results is the number of edges from bad_supplier_id to given node
network_grph <- network_grph %>%
        activate(nodes) %>%
        mutate(bfs_results = bfs_dist(root = bad_supplier_id, 
                                      mode = 'out'))
network_grph


#////////////////////////////////////////////////////////////////////////////////////


# scenario: identify the most connected distributor

# find number of neighbors for a given distributor
network_grph %>%
        activate(nodes) %>%
        morph(to_local_neighborhood,  
              node = 1, 
              order = 1, 
              mode = 'all') %>%
        mutate(neighbor = TRUE) %>%
        unmorph() %>%
        data.frame() %>%
        filter(neighbor == TRUE,
               id != 1) %>%
        nrow()

distributor_id <- network_grph %>%
        activate(nodes) %>%
        filter(str_detect(name, 'Dist.')) %>%
        data.frame() %>%
        pull(id)

distributor_id 


# get count of neighbors for all distributors
for(i in distributor_id){
        
        n_neighbors <- network_grph %>%
                activate(nodes) %>%
                morph(to_local_neighborhood,  
                      node = i, 
                      order = 1, 
                      mode = 'all') %>%
                mutate(neighbor = TRUE) %>%
                unmorph() %>%
                data.frame() %>%
                filter(neighbor == TRUE,
                       id != i) %>%
                nrow()
        
        distributor_name <- network_grph %>%
                activate(nodes) %>%
                filter(id == i) %>%
                pull(name)
        
        print(paste0('Distributor: ', distributor_name, 
                     '   Neighbors: ', n_neighbors))
        
}


#//////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////


# tidygraph intro ####

# https://tidygraph.data-imaginist.com/

play_gnp(10, 0.5) %>% 
        activate(nodes) %>% 
        mutate(degree = centrality_degree()) %>% 
        activate(edges) %>% 
        mutate(centrality = centrality_edge_betweenness()) %>% 
        arrange(centrality)

# note there are a lot of centrality score options
?centrality_degree


#//////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////



# tutorial
# https://jeremydfoote.com/Communication-and-Social-Networks/resources/ggraph_walkthrough.html


# create an igraph
G_igraph <- erdos.renyi.game(50, .4)
G_igraph


# convert to tbl_graph
# the tbl_graph consits of two tibbles - one for nodes and one for edges (use activate(nodes/edges) to select btw them)
G <- as_tbl_graph(G_igraph)
G


# mutate a table
create_notable('zachary') %>%
        activate(nodes) %>%
        mutate(degree = centrality_degree())

# plot histogram for measure of betweenness
create_notable('zachary') %>%
        activate(edges) %>%
        mutate(bw = centrality_edge_betweenness()) %>%
        as_tibble() %>%
        ggplot() +
        geom_histogram(aes(x=bw)) +
        theme_minimal()


#//////////////////////////////


# plots

nodes = read_csv('https://raw.githubusercontent.com/jdfoote/Communication-and-Social-Networks/spring-2021/resources/school_graph_nodes.csv')
edges = read_csv('https://raw.githubusercontent.com/jdfoote/Communication-and-Social-Networks/spring-2021/resources/school_graph_edges.csv')

G = graph_from_data_frame(d=edges, v = nodes) %>% as_tbl_graph()

G


# plot coloring edges based on connected node similarity
G %>% 
        activate(edges) %>%
        filter(type == 'friendship') %>%
        mutate(drinking_diff = abs(.N()$alcohol_use[from] - .N()$alcohol_use[to])) %>%
        ggraph(layout = 'stress') + 
        geom_edge_fan(aes(color = drinking_diff), width = .5) +
        geom_node_point(aes(color=alcohol_use), size = 3) + 
        scale_color_viridis() + 
        scale_edge_color_viridis()


G %>%
        ggraph(layout = 'stress') +
        geom_edge_fan(aes(color=type),width = .5) +
        geom_node_point(aes(color=alcohol_use), size = 3) +
        scale_color_viridis()


G %>%
        ggraph(layout = 'stress') +
        geom_edge_fan(aes(color=type),width = .5) +
        geom_node_point(aes(color=alcohol_use), size = 3) +
        scale_color_viridis() +
        scale_edge_color_manual(values = c('friendship' = '#ceb888', 'primary_school' = 'lightgray'))


G %>%
        ggraph(layout = 'stress') +
        geom_edge_fan(aes(color=type), width = .5) +
        geom_node_point(aes(color=alcohol_use, size = delinquency)) +
        scale_color_viridis() +
        scale_edge_color_manual(values = c('friendship' = '#ceb888', 'primary_school' = 'gray'))
