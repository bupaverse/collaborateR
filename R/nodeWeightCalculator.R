#Functions to calculate the weight of the nodes

#' @importFrom igraph betweenness
#' @importFrom igraph eigen_centrality
#Function to calculate the node weights
#@param log : dataframe of the VCS log
#@param nodes: dataframe of the nodes
#@param edges: dataframe of the edges
#@param FSW: frequency significance weight -> how much does the FS attribute to the final node weight
#@param BCW: betweenness centrality weight -> how much does the BC attribute to the final node weight
#@param ECW: eigenvector centrality weight -> how much does the EC attribute to the final node weight
#@param DCW: degree centrality  weight -> how much does the DC attribute to the final node weight
calculateNodeWeights <- function(log,nodes, edges, FSW, BCW, ECW, DCW, fileImportance){
  print("Calculating the node weights")


  #TO DO:delete
 # FSW = 0.15
#  BCW = 0.25
  #ECW = 0.3
  #DCW = 0.3

  ####


  #Calculate FS
  frequencySignificance <- calculateUnaryFrequencySignificance(log, nodes,fileImportance)

  #Calculate BC
  betweennessCentrality <- calculateBetweennessCentrality(nodes,edges)

  #Calculate EC
  eigenvectorCentrality <- calculateEigenvectorCentrality(nodes,edges)

  #Calculate DC
  degreeCentrality <- calculateDegreeCentrality(nodes,edges)

  #Calculate final node weight: calculate the weighted sum , using the weight params passed as args
  nodes %>% inner_join(frequencySignificance) %>%
    inner_join(betweennessCentrality) %>% inner_join(eigenvectorCentrality) %>% inner_join(degreeCentrality) %>%
    mutate(weight  = FSW * NUFS + BCW * BC + ECW * EC + DCW * DC) %>% select(-NUFS, -BC, -EC, -DC) -> nodes

  nodes
}


#Calculates the unary frequency significance as the sum of the file importances of the files this programmer worked on +normalized
#@param log: dataframe of the log
#@param nodes: dataframe of the graph nodes
#@param fileImportance : dataframe with the fileID and the importance value
#@returns dataframe with programmerID and normalized unary frequency significance
calculateUnaryFrequencySignificance <- function(log, nodes, fileImportance){
  #Get programmers and what files they worked on
  log %>% select(programmerID, fileId) %>% unique() %>%
    inner_join(fileImportance) %>%
    group_by(programmerID) %>%
    summarise(UFS = sum(importance), numberOfFiles = n()) %>%
    mutate(NUFS = ifelse(numberOfFiles == 0, 0,UFS/numberOfFiles)) %>%
    select(-UFS, -numberOfFiles) %>%
    inner_join(nodes, by = c("programmerID")) %>% select(-programmerName, -weight, -cluster) -> frequencySignificance

  frequencySignificance


}

#Calculate the betweenness centrality value of a node using igraph library
#@param nodes : dataframe of the graph nodes
#@param edges : dataframe of the graph edges
#@returns dataframe with programmerID + BC : normalized betweenness centrality of the node
calculateBetweennessCentrality <- function(nodes, edges){
  #Make an igraph graph object
  graph <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

  V(graph)$betweenness <- betweenness(graph,normalized = FALSE)

  betweennessCentrality <- data.frame("programmerID" = as.integer(V(graph)$name), "BC" = V(graph)$betweenness)

  #normalize this based on min and max
  betweennessCentrality <- normalizeBasedOnMinAndMax(betweennessCentrality,"BC")

  betweennessCentrality
}

#Calculate the eigenvector centrality value of a node using igraph library
#@param nodes : dataframe of the graph nodes
#@param edges : dataframe of the graph edges
#@returns  dataframe with programmerID + EC : eigenvector centrality value for the node
calculateEigenvectorCentrality <- function(nodes, edges){
  #Make an igraph graph object
  graph <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

  eigenvectorCentrality <- data.frame("programmerID" = as.integer(V(graph)$name), "EC" = eigen_centrality(graph)$vector)

  #Normalize this base on min and max
  eigenvectorCentrality <- normalizeBasedOnMinAndMax(eigenvectorCentrality,"EC")

  eigenvectorCentrality
}

#Calculates the degree centrality for the nodes (normalized)
#@param nodes: dataframe of the graph nodes
#@param edges: dataframe of the graph edges
#@returns dataframe with programmerID and DC: degree centrality of that node
calculateDegreeCentrality <- function(nodes, edges){


  inverseDisjunctDegreeCentrality <- calculateInverseDisjunctDegreeCentrality(nodes,edges)
  pairProgDegreeCentrality <- calculatePairProgrDegreeCentrality(nodes,edges)

  inner_join(inverseDisjunctDegreeCentrality,pairProgDegreeCentrality) %>%
    mutate(DC = inverseDisjunctDC - pairProgDC) %>%
    mutate(DC = (DC+1)/2) %>%
    select(-inverseDisjunctDC, -pairProgDC) -> degreeCentrality


  degreeCentrality

}

#Function that calculates the inverse degree centrality solely for distinct edges (so no pair programming edges are taken into account)
#Calculates the inverse because the less people a programmer collabs with the more important he is
#@param nodes: dataframe of the graph nodes
#@param edges: dataframe of the graph edges
#@returns dataframe with the programmerID and inverseDisjunctDC : the inverse degree centrality of the disjunct programming edges
calculateInverseDisjunctDegreeCentrality <- function(nodes,edges){


  numberOfNodesNodeCanBeConnectedTo <- length(nodes$programmerID) - 1

  #This concerns only the disjunct edges
  edges %>% filter(collabType != "pair programming") %>% select(sourceNodeID,targetNodeID)-> disjunctEdges
  #I want every edge mentioned 2 (undirected so in both directions)
  data.frame("sourceNodeID" = disjunctEdges$targetNodeID, "targetNodeID" = disjunctEdges$sourceNodeID) %>%
  full_join(disjunctEdges) -> disjunctEdges

  #!inner_join drops all nodes that have 0 incident disjunct programming edges !! Deal with this later
  nodes %>% select(programmerID) %>% inner_join(disjunctEdges, by = c("programmerID" = "sourceNodeID")) %>% unique() %>%
    group_by(programmerID) %>% summarise(nIncidentLinks = n()) %>%
    mutate(inverseDisjunctDC = 1 - (nIncidentLinks / numberOfNodesNodeCanBeConnectedTo)) %>% select(-nIncidentLinks)-> inverseDisjunctDegreeCentrality


  #Deal with the nodes that didnt have any incident disjunct programming edges
  nodes %>% select(programmerID) %>% left_join(inverseDisjunctDegreeCentrality) %>%
    mutate(inverseDisjunctDC = ifelse(is.na(inverseDisjunctDC), 0, inverseDisjunctDC)) -> inverseDisjunctDegreeCentrality

  inverseDisjunctDegreeCentrality

}



#Calculates the degree centrality of the pair programming edges
#@param nodes: dataframe of the graph nodes
#@param edges: dataframe of the graph edges
#@returns dataframe with programmerID and pairProgDC : the degree centrality of the pair programming edges
calculatePairProgrDegreeCentrality <- function(nodes,edges){

  numberOfNodesNodeCanBeConnectedTo <- length(nodes$programmerID) - 1

  #This concerns only the pair programming edges
  edges %>% filter(collabType != "disjunct programming") %>% select(sourceNodeID, targetNodeID) -> pairProgEdges
  #I want every edge mentioned 2 (undirected so in both directions)
  data.frame("sourceNodeID" = pairProgEdges$targetNodeID, "targetNodeID" = pairProgEdges$sourceNodeID) %>%
    full_join(pairProgEdges) -> pairProgEdges

  #!inner_join drops all nodes that have 0 incident pair programming edges !! Deal with this later
  nodes %>% select(programmerID) %>% inner_join(pairProgEdges, by = c("programmerID" = "sourceNodeID")) %>% unique() %>%
    group_by(programmerID) %>% summarise(nIncidentLinks = n()) %>%
    mutate(pairProgDC = (nIncidentLinks / numberOfNodesNodeCanBeConnectedTo)) %>% select(-nIncidentLinks)-> pairProgDegreeCentrality

  #Deal with the nodes that didnt have any incident pair programming edges
  nodes %>% select(programmerID) %>% left_join(pairProgDegreeCentrality) %>%
    mutate(pairProgDC = ifelse(is.na(pairProgDC), 0, pairProgDC)) -> pairProgDegreeCentrality

  pairProgDegreeCentrality

}
