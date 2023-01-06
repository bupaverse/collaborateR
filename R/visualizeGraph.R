#' @title Function to visualize the graph
#'
#' @description Uses igraph's plot to plot the graph
#'
#' @param graph the graph object to visualize
#' @param anonymize whether or not to anonymize node names
#' @param nodeWeightScaling scale the node weight by multiplying by this param
#' @param edgeWeightScaling scale the edge weight by multiplying by this param
#' @return Visualized graph
#' @export
#'
visualize_graph <- function(graph, anonymize = FALSE, nodeWeightScaling= 35, edgeWeightScaling = 50){
    graph <- create_igraph_object(graph, anonymize, nodeWeightScaling, edgeWeightScaling)

    plot(graph, vertex.label.color="black", #vertex.label.dist=1.5,
         #vertex colour: 1 = pink,  2 = blue; since Cluster is False = 0 , the total is 1 and the vertices will be pink
         vertex.color=c( "pink", "skyblue")[1+V(graph)$cluster ],
         vertex.size = V(graph)$weight,
         vertex.label = V(graph)$programmerName,
         #collabType should be ordered vector so we can select the proper colour based on the numeric value of the ordering
         edge.color=c("green","orange","blue")[as.numeric(E(graph)$collabType)],
         edge.width = E(graph)$weight)#,
    #layout = layout_with_fr(graph))
}
