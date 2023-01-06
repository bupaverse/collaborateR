#This file handles all graph output:
# -write it to CSV
# -ask processed nodes and edges dataframe to visualise it (incl anonymization option)
# -...
#There is an option to anonymize the output
#source("R/visualization.R")

#' @title Write graph to 2 separate CSV files: one for the nodes, one for the edges
#'
#' @param graph : graph of which we want to write the nodes and edges to CSV
#' @param nodesFilename : filename for the CSV that will contain the nodes
#' @param edgesFilename : filename for the CSV that will contain the edges
#' @param anonymization : default FALSE meaning programmer names will not be anonymized

#'
#' @return CSV files
#' @export
#'
write_graph_to_csv <- function(graph,nodesFilename = "nodes.csv",edgesFilename="edges.csv", anonymization = FALSE){
  print("Writing the graph to CSV")

  #write the nodes to a CSV file
  writeNodesCSV(graph$nodes, nodesFilename, anonymization)

  #write the edges to a CSV file
  writeEdgesCSV(graph$edges,edgesFilename)
}


#Function that writes the nodes to a CSV file, structured:
#ID, Label, Weight, Cluster
# @param nodes : dataframe of the nodes
# @param filename: the name of the CSV file to write to
# @param anonymization: whether or not programmer names should be anonymized
writeNodesCSV <- function(nodes, filename, anonymization){



  #check whether the programmer names should be anonymized
  if(anonymization){
    nodes <- anonymizeNodeNames(nodes)
  }

  #rewrite the columnnames
  nodesFrame <- setNames(nodes, c("ID","Label","Weight","Cluster"))

  write.csv(nodesFrame,filename, row.names = FALSE)
}

#Function that writes the edges to a CSV file, structured:
#Source, Target, Weight, Type, CollaborationType
#@param edges : dataframe of the edges
#@param filename: the name of the CSV file to write to
writeEdgesCSV <- function(edges, filename){


  #rewrite the columnnames
  edgesFrame <- setNames(edges, c("Source","Target","Weight","Type", "CollaborationType"))
  write.csv(edgesFrame,filename, row.names = FALSE)
}


#Function that anonymizes the nodes' names
#@param nodes : dataframe containing the nodes
#@returns : the same dataframe but the Label column has been anonymized
anonymizeNodeNames <- function(nodes){
  #if the node is a cluster, also add the number of programmers in the cluster
  nodes %>% filter(cluster) %>% mutate("numberOfMembers" = str_count(programmerName,fixed("+"))+1) %>%
    select(programmerID,numberOfMembers) -> clusters

  nodes$programmerName <- paste0(rep("P",length(nodes$programmerName)),c(1:length(nodes$programmerName)))
  clusters %>% inner_join(nodes) %>% mutate(programmerName = paste0(programmerName," (",numberOfMembers,")")) %>%
    select(programmerID,programmerName) -> clusters

  nodes %>% left_join(clusters, by = "programmerID") %>% mutate(programmerName = ifelse(is.na(programmerName.y),programmerName.x,programmerName.y)) %>%
    select(-programmerName.x,-programmerName.y) -> nodes

  #reorder the columns
  nodes <- nodes[c("programmerID", "programmerName", "weight","cluster")]

  nodes
}
