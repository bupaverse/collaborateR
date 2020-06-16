#Test file to visualize the graph
#Normally the choice of visualization (package) lies with the end user
#The end user can request the processed nodes and edges dataframe from the output file
#And uses them to visualize it with whatever package and tweaks he pleases

#' Function that creates an igraph object from the graph parameter
#'
#' @param graph A graph, created with build_graph
#' @param anonymize Anonimize output
#' @param nodeWeightScaling Scale node weights
#' @param edgeWeightScaling Scale edge weights
#'
#' @return igraph object of the graph
#'
#' @importFrom igraph graph_from_data_frame
#' @importFrom igraph V
#' @importFrom igraph E
#' @importFrom igraph `V<-`
#' @importFrom igraph `E<-`
#'
#'
#' @export
#'
create_igraph_object <- function(graph, anonymize = FALSE, nodeWeightScaling= 35, edgeWeightScaling = 50){
  edges <- g$edges
  nodes <- g$nodes
  if(anonymize){
    nodes <- anonymizeNodeNames(nodes)
  }


  graph <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

  V(graph)$weight <- V(graph)$weight*nodeWeightScaling
  E(graph)$weight <- E(graph)$weight*edgeWeightScaling

  graph
}




