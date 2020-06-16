#Code to simplify the graph


simplifyGraph <- function(log, nodes, edges, simplificationCutoffParam, AgVP, AgCP, AbVP, AbCP){
  #Initialize the params if missing

  if(missing(AgVP)){
    #Calculate the aggregation victim param
    AgVP <- calculateAggregationVictimParam(nodes)
  }
  if(missing(AgCP)){
    #Calculate the aggregation correlation param
    AgCP <- calculateAggregationCorrelationParam(nodes,edges)
  }
  if(missing(AbVP)){
    #Calculate the abstraction victim param
    AbVP <- calculateAbtractionVictimParam(nodes)
  }
  if(missing(AbCP)){
    #Calculate the abstraction correlation param
    AbCP <- calculateAbstractionCorrelationParam(nodes,edges)
  }


  #Now start the simplification
  print("Simplification phase 1: Edge filtering")
  edges <- edgeFiltering(nodes,edges,simplificationCutoffParam)
  print("Simplification phase 2: Aggregation")
  returnlist <- aggregation(nodes, edges,AgVP, AgCP)
  vizEdges <- returnlist$vizEdges
  vizNodes <- returnlist$vizNodes

  #abstraction
  print("Simplification phase 3: Abstraction")
  returnlist <- abstraction(vizNodes,vizEdges,AbVP,AbCP)
  vizNodes <- returnlist$nodes
  vizEdges <- returnlist$edges

  returnlist <- resetDataTypes(vizNodes,vizEdges)
  vizNodes <- returnlist$nodes
  vizEdges <- returnlist$edges


  returnlist <- list("nodes" = vizNodes,"edges" = vizEdges)
  returnlist
}

#Function that makes sure all the colums are the right datatype
resetDataTypes <- function(vizNodes,vizEdges){
  vizNodes <- vizNodes %>% mutate(programmerName = as.factor(programmerName))
  vizEdges <- vizEdges %>% mutate(type = as.factor(type), collabType = factor(collabType,ordered = TRUE, levels = c("disjunct programming","pair programming","pair and disjunct programming")))

  returnlist <- list("nodes" = vizNodes,"edges" = vizEdges)
  returnlist

}


#Function that calculates the aggregation victim param as the avg node weight
#@param nodes: dataframe of the graph nodes
#@returns the default aggregation victim param = average weight of a node
calculateAggregationVictimParam <- function(nodes){
  AgVP <- mean(nodes$weight)

  AgVP
}


#Function that calculates the aggregation correlation param as the absolute value of the difference between the avg node weight and avg edge weight
#@param nodes : dataframe of the graph nodes
#@param edges : dataframe of the graph edges
#@returns aggregation correlation param = absolute difference between the avg node weight and avg edge weight
calculateAggregationCorrelationParam <- function(nodes, edges){
  AgCP <- abs(mean(nodes$weight) -  mean(edges$weight))

  AgCP
}

#Function that calculates the abstraction victim param as the avg node weight
#@param nodes: dataframe of the graph nodes
#@returns the default abstraction victim param = average weight of a node
calculateAbtractionVictimParam <- function(nodes){
  AbVP <- mean(nodes$weight)

  AbVP
}

#Function that calculates the abstraction correlation param as the absolute value of the difference between the avg node weight and avg edge weight
#@param nodes : dataframe of the graph nodes
#@param edges : dataframe of the graph edges
#@returns abstraction correlation param = absolute difference between the avg node weight and avg edge weight
calculateAbstractionCorrelationParam <- function(nodes, edges){
  AbCP <- abs(mean(nodes$weight) -  mean(edges$weight))

  AbCP
}


#Filter the edges that are least important to a node
#@param nodes : dataframe of the graph nodes
#@param edges : dataframe of the graph edges
#@param simplificationCutoffParam : for a node: edges whose normalized weight (normalized locally using the weights of the other incident edges) is < this param will be candidates for removal
#@returns edges dataframe with the edges where both nodes agree on removal are removed from
edgeFiltering <- function(nodes,edges, simplificationCutoffParam){
  #Collect all edges
  edges %>% select(sourceNodeID,targetNodeID, weight)-> chronologicEdges
  #I want every edge mentioned 2 (undirected so in both directions)
  data.frame("sourceNodeID" = chronologicEdges$targetNodeID, "targetNodeID" = chronologicEdges$sourceNodeID, "weight" = chronologicEdges$weight) %>%
    full_join(chronologicEdges) %>% unique() -> directedEdges



  #!inner_join drops all nodes that have 0 incident edges !! Deal with this later
  nodes %>% select(programmerID) %>% inner_join(directedEdges, by = c("programmerID" = "sourceNodeID")) %>% unique() %>%
    #We only want nodes with > 1 incident edge
    group_by(programmerID) %>% count() %>% filter(n > 1) %>% select(-n)-> candidateNodes

  candidateNodes %>% inner_join(directedEdges,by = c("programmerID" = "sourceNodeID")) %>%
   group_by(programmerID) %>% mutate(max = max(weight), min = min(weight)) %>%
    #only consider nodes where not all edges have the same weight
    filter(max != min) %>%
    mutate(normWeight = ((weight - min)/(max-min))) %>%
    #edges where normWeight < cutoffParam are the candidates for filtering
    filter(normWeight < simplificationCutoffParam) %>% select(programmerID, targetNodeID)-> candidateEdges


    #only edges that appear in both directions (where both nodes agree on filtering) will be filtered
    edges %>% inner_join(candidateEdges, by = c("sourceNodeID" = "programmerID", "targetNodeID" = "targetNodeID")) -> direction1
    edges %>% inner_join(candidateEdges, by = c("sourceNodeID" = "targetNodeID", "targetNodeID" = "programmerID")) -> direction2
    direction1 %>% inner_join(direction2) %>% select(sourceNodeID, targetNodeID) -> edgesToFilter

    #Now filter the edges from the dataframe
    edges %>% anti_join(edgesToFilter,by = c("sourceNodeID" = "sourceNodeID","targetNodeID" = "targetNodeID" )) -> edges


  edges
}

#@return list with 2 elements: nodes contains the nodes dataframe, edges the edges dataframe
aggregation <- function(nodes, edges,AgVP, AgCP){
  #Initial cluster building
  clusters <- initialClusterBuilding(nodes,edges,AgVP, AgCP)
  #Cluster merging
  clusters <- clusterMerging(clusters, nodes, edges, AgCP)


  #Build cluster graph nodes
  returnlist <- buildClusterNodes(clusters, nodes,edges)


  returnlist
}



#Build the initial clusters
#@param nodes dataframe of the nodes
#@param edges : dataframe of the edges
#@param AgVP: aggregation victim parameter
#@param AgCP: aggregation correlation parameter
#@returns clusters: dataframe with programmerID, clusterID en aggregationEdgeID, that gives a first notion of groups/clusters of different nodes (when they have the same clusterID)
initialClusterBuilding <- function(nodes, edges,AgVP, AgCP){

  #find the candidate nodes
  nodes %>% filter(weight < AgVP) -> candidateNodes


  #Collect all edges
  directedEdges <- transformToDirectedEdges(edges)


  #search an edge strong enough for aggregation
  candidateNodes %>% select(programmerID, weight) %>% rename("nodeWeight" = weight) %>%
    inner_join(directedEdges, by = c("programmerID" = "sourceNodeID")) %>% unique() %>%
    #calculate distance significance for every edge
    mutate(distSign = abs(edgeWeight - nodeWeight)) %>%
    #only keep the edges that are strong enough to aggregate
    filter(distSign > AgCP) %>%
    #for every node we need only the strongest edge
    group_by(programmerID) %>%
    arrange(-distSign) %>%
    slice(1) -> clusterNodes



    clusters <- mergeInitialClusters(clusterNodes)
    clusters
}

#Function that transforms the undirected edges dataframe to a directed on
#every edge is mentioned twice
transformToDirectedEdges <- function(edges){
  #Collect all edges
  edges %>% select(sourceNodeID,targetNodeID, weight)-> chronologicEdges
  #I want every edge mentioned 2 (undirected so in both directions)
  data.frame("sourceNodeID" = chronologicEdges$targetNodeID, "targetNodeID" = chronologicEdges$sourceNodeID, "weight" = chronologicEdges$weight) %>%
    full_join(chronologicEdges) %>% unique() %>% rename("edgeWeight" = weight)-> directedEdges

  directedEdges

}

#function that groups the clustersnodes if the have an aggregation edges between them
#@param clusterNodes : dataframe that contains the nodes and their aggregation edges that qualify for clustering
#@returns clusters: dataframe with programmerID, clusterID en aggregationEdgeID, that gives a first notion of groups/clusters of different nodes (when they have the same clusterID)
mergeInitialClusters <- function(clusterNodes){
  clusters <- data.frame("programmerID" = clusterNodes$programmerID,
                         "clusterID" = rep(0,length(clusterNodes$programmerID)),
                         "aggregationEdge" =rep(0,length(clusterNodes$programmerID)))

  currentClusterID <- 1

  #We need to loop over de clusterNodes dataframe and edit the corresponding node in the clusters dataframe
  for(i in seq_len(nrow(clusterNodes))){
    #if the targetnode is not a cluster: ie if the ID doesnt exist in the dataframe, OR it does exist but has a clusterID == 0
    #-> programmerID becomes a new cluster, give a unique clusterID
    if(!(clusterNodes$targetNodeID[i] %in% clusters$programmerID)){
      #give new cluster a unique ID
      clusters$clusterID[clusters$programmerID == clusterNodes$programmerID[i]] <- currentClusterID
      clusters$aggregationEdge[clusters$programmerID == clusterNodes$programmerID[i]] <- clusterNodes$targetNodeID[i]
      currentClusterID <- currentClusterID + 1

    } else if(clusters$clusterID[clusters$programmerID == clusterNodes$targetNodeID[i]] != 0){
      #if the target node is already a cluster -> add source node to this cluster
      #source node gets same cluster ID as target node
      clusters$clusterID[clusters$programmerID == clusterNodes$programmerID[i]] <- clusters$clusterID[clusters$programmerID == clusterNodes$targetNodeID[i]]
      clusters$aggregationEdge[clusters$programmerID == clusterNodes$programmerID[i]] <- clusterNodes$targetNodeID[i]


    } else {
      #there are no connected clusters yet, source node becomes a new cluster
      clusters$clusterID[clusters$programmerID == clusterNodes$programmerID[i]] <- currentClusterID
      clusters$aggregationEdge[clusters$programmerID == clusterNodes$programmerID[i]] <- clusterNodes$targetNodeID[i]
      currentClusterID <- currentClusterID + 1
    }
  }




  clusters
}


#Function that tries to merge the different already existing clusters if possible
#@param clusters dataframe with the existing clusters
#@param nodes dataframe that contains the graph nodes
#@param edges dataframe that contains the graph edges
#@param AgCP aggregation correlation parameter
#@return dataframe with the programmerID and the clusterID it belongs to
clusterMerging <- function(clusters, nodes, edges, AgCP){
  nodeWeights <- nodes %>% select(programmerID,weight)
  directedEdges <- transformToDirectedEdges(edges)

  #First aggregation edges
  #Find candidates : target node has to be a cluster
  clusters %>% inner_join(clusters,by = c("aggregationEdge" = "programmerID")) %>%
    #target node should belong to a different cluster than the source
    filter(clusterID.x != clusterID.y) %>%
    #we need edge and node weights
    inner_join(nodeWeights, by = c("programmerID" = "programmerID")) %>%
    rename("nodeWeight" = "weight") %>%
    inner_join(directedEdges, by = c("programmerID" = "sourceNodeID", "aggregationEdge" = "targetNodeID")) %>%
    #calculate the distance significance
    mutate("DS" = abs(edgeWeight - nodeWeight)) %>%
    #per cluster find the largest DS
    group_by(clusterID.x) %>% arrange(-DS) %>% slice(1) %>%
    select(clusterID.x, clusterID.y) %>% ungroup() -> aggrEdgesClusters

  #Handle non aggregation edges
  #For the x clusters we found merging partners, now check the other clusters
  #First filter out the clusters we already found partners for
  clusters %>% filter(!(clusterID %in% aggrEdgesClusters$clusterID.x)) -> nonAggrEdgesClusters
  #filter from the edges, the edges that appear in nonAggrEdgesClusters
  directedEdges %>% anti_join(nonAggrEdgesClusters, by = c("sourceNodeID" = "programmerID", "targetNodeID" = "aggregationEdge")) %>%
    #we only need the edges that start from a cluster we still need to check
    filter(sourceNodeID %in% nonAggrEdgesClusters$programmerID) -> nonAggrEdgesToCheck

    #We need the cluster ID of the sourceNode
    clusters %>% select(programmerID, clusterID) -> clusterIDs
    nonAggrEdgesToCheck %>%
      inner_join(clusterIDs, by = c("sourceNodeID" = "programmerID")) %>%
      rename("clusterID.source" = "clusterID") %>%
      #now we need the targetNode cluster ID
      left_join(clusterIDs, by = c("targetNodeID" = "programmerID")) %>%
      rename("clusterID.target" = "clusterID") %>%
      #target node has to be a cluster
      filter(!is.na(clusterID.target)) %>%
      #they cannot be the same clusterID
      filter(!(clusterID.source == clusterID.target)) %>%
      #add target node weight
      inner_join(nodeWeights, by = c("targetNodeID" = "programmerID")) %>%
      rename("nodeWeight" = "weight") %>%
      #calculate DS
      mutate("DS" = abs(edgeWeight - nodeWeight)) %>%
      #DS > AgCP
      filter(DS > AgCP) %>%
      #per cluster find the largest DS
      group_by(clusterID.source) %>% arrange(-DS) %>% slice(1) %>%
      select(clusterID.source,clusterID.target) %>% ungroup() -> nonAggrEdgesClusters



    #cluster couples may appear twice in switched form : set the smaller ID left and the largest right
    columnsAscending(nonAggrEdgesClusters,"clusterID.source","clusterID.target") %>% unique() -> nonAggrEdgesClusters
    columnsAscending(aggrEdgesClusters,"clusterID.x","clusterID.y") %>% unique() -> aggrEdgesClusters


    #Get list with programmerID and clusterID (merge the different clusterIDs that belong to one cluster)
    clusters <- combineClusters(clusters,nonAggrEdgesClusters,aggrEdgesClusters)


  clusters
}

#Function that sets the right clusterIDs voor the clusternodes
#@param clusters  dataframe that contains nodeIDs and clusterIDs
#@param tupleList1 list of tuples of clusterIDs to merge
#@param tupleList2 list of tuples of clusterIDs to merge
#@return dataframe with the nodeID and the clusterID, so we can identify the different nodes that belong to the same cluster
combineClusters <- function(clusters,tupleList1,tupleList2){

  #Get list of all distinct clusterIDs from the tuples lists
  d1 <- data.frame("clusterID" =unlist(tupleList1, use.names = FALSE))
  d2 <- data.frame("clusterID" =unlist(tupleList2, use.names = FALSE))
  distinctClusterIDs <- rbind(d1,d2) %>% unique() %>% arrange(clusterID)

  #Make dataframe that will hold the merged clusterIDs
  clusterMerges <- data.frame("clusterID" = distinctClusterIDs$clusterID,
                         "mergeID" = rep(0,length(distinctClusterIDs$clusterID)))

  #Now we need to loop like we did before, get tuples of clusters the same cluster ID
  #Merge the tuples lists and arrange them
  tupleList1 %>% rename("clusterID.x" = "clusterID.source", "clusterID.y" = "clusterID.target") -> tupleList1
  rbind(tupleList1,tupleList2) %>% unique() %>% arrange(-clusterID.y, -clusterID.x)-> tuplesList

  #Now fill the new dataframe
  #first find the max current cluster ID
  if(nrow(clusters) == 0){
    currentClusterID <- 1
  } else {
    currentClusterID <- max(clusters$clusterID) + 1
  }

  for(i in seq_len(nrow(tuplesList))){
    if(clusterMerges$mergeID[clusterMerges$clusterID == tuplesList$clusterID.y[i]] == 0){
      #Check if the x also doesnt have a cluster ID yet
      if(clusterMerges$mergeID[clusterMerges$clusterID == tuplesList$clusterID.x[i]] == 0){
        #Assign a completely new cluster ID
        clusterMerges$mergeID[clusterMerges$clusterID == tuplesList$clusterID.y[i]] <- currentClusterID
        clusterMerges$mergeID[clusterMerges$clusterID == tuplesList$clusterID.x[i]] <- currentClusterID
        currentClusterID <- currentClusterID +1
      } else {
        #The x cluster already has an ID, take this one to fill up the y
        clusterMerges$mergeID[clusterMerges$clusterID == tuplesList$clusterID.y[i]] <- clusterMerges$mergeID[clusterMerges$clusterID == tuplesList$clusterID.x[i]]

      }
    } else {
      #Take this clusterID and assign it to the x
      clusterMerges$mergeID[clusterMerges$clusterID == tuplesList$clusterID.x[i]] <- clusterMerges$mergeID[clusterMerges$clusterID == tuplesList$clusterID.y[i]]
    }
  }

  #Now overwrite the clusterIDs from clusters
  clusters %>% left_join(clusterMerges, by = c("clusterID" = "clusterID")) %>%
    mutate(mergeID = ifelse(is.na(mergeID), clusterID, mergeID )) %>%
    select(programmerID, mergeID) %>%
    rename("clusterID" = "mergeID") -> clusters


  #Remove clusters that consist of only 1 node
  clusters %>% group_by(clusterID) %>% count() %>% filter(n > 1) -> realClustersID
  clusters %>% filter(clusterID %in% realClustersID$clusterID) -> clusters

  clusters
}


#Function that build the cluster nodes as we will see them in the visualisation
#Also calculates the clusternode weight and new edge weights and types
#@param clusters  dataframe with programmerID and clusterID, that indicates which nodes belong to which cluster
#@param nodes dataframe of the graph nodes
#@param edges dataframe of the graph edges
#@return list with 2 elements: nodes contains the nodes dataframe, edges the edges dataframe
buildClusterNodes <- function(clusters,nodes,edges){


  #First we handle the nodes
  #New dataframe the contains for every cluster a vizualisation node ID
  #Get total number of clusters
  totalNumberOfClusterNodes <- length(clusters$clusterID %>% unique())
  uniqueClusterIDs <- clusters$clusterID %>% unique()
  vizNodeIDCounter <- max(nodes$programmerID) +1

  #Dataframe that contains the ID of the clusternode in the visualization and the corresponding clusterID to find the nodes belonging to the cluster
  if(totalNumberOfClusterNodes > 0){
  vizClusterNodesToClusterIDs <- data.frame("vizNodeID" = seq(from=vizNodeIDCounter,to=vizNodeIDCounter+totalNumberOfClusterNodes-1) , "clusterID" = uniqueClusterIDs)
  } else {
    #There are no clusternodes
    vizClusterNodesToClusterIDs <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("vizNodeID", "clusterID"))

  }

  #List of clusterIDs and which programmerIDs belong to a cluster, incl programmer info
  clusterProgrammers <- clusters %>% inner_join(nodes) %>% mutate(cluster=TRUE)
  #Remove these clusterprogrammers from the OG nodes list (vizualisation)
  vizNodes <- nodes %>% filter(!(programmerID %in% clusterProgrammers$programmerID))

  #Calculate new clusterNode weight and label
  vizClusterNodesToClusterIDs <- calculateClusterWeightAndLabel(clusterProgrammers, vizClusterNodesToClusterIDs)
  #Add these vizClusterNodes to the vizNodes
  vizClusterNodesToClusterIDs %>% select(-clusterID) %>% rename("programmerID"="vizNodeID", "programmerName"="label","weight"="clusterNodeWeight" ) %>%
    mutate("cluster"=TRUE) %>% full_join(vizNodes, by=c("programmerID"="programmerID","programmerName"="programmerName","weight"="weight","cluster"="cluster")) -> vizNodes

  #Reorder the colums
  vizNodes <- vizNodes[c("programmerID", "programmerName", "weight","cluster")]
  ################################################
  #EDGES
  ################################################
  #Now do the cluster edges
  #Collect the edges of which both source and target node belong to the same cluster
  clusterProgrammers %>% select(programmerID, clusterID) -> progrClusterIDs
  edges %>%
    left_join(progrClusterIDs, by = c("sourceNodeID"="programmerID")) %>%
    rename("clusterID.sourceNode"="clusterID") %>%
    left_join(progrClusterIDs, by = c("targetNodeID"="programmerID"))%>%
                rename("clusterID.targetNode"="clusterID") -> edges
  edges %>% filter(!is.na(clusterID.sourceNode)) %>% filter(!is.na(clusterID.targetNode)) %>%
    filter(clusterID.sourceNode == clusterID.targetNode) -> inClusterEdges

  #Remove inclusterEdges from the vizEdges list
  vizEdges <- edges %>% anti_join(inClusterEdges, by = c("sourceNodeID"="sourceNodeID","targetNodeID"="targetNodeID"))
  #Handle outClusterEdges : between regular neighbour node or cluster neighbournode
  vizEdges <- handleOutClusterEdges(vizEdges,edges, vizClusterNodesToClusterIDs)


  #return vizNodes and vizEdges
  returnlist <- list("vizNodes" = vizNodes, "vizEdges" = vizEdges)


  returnlist
}

#Function that handles edges that are between 2 clusters or between a regular node and a cluster
#@param vizEdges: edges that will appear in the visualization later
#@param edges dataframe of graph edges
#@param vizClusterNodesToClusterIDs link between clusterID and the node ID of the cluster in the visualization
#@return ataframe with the edges as how they appear in the ending visualization
handleOutClusterEdges <- function(vizEdges, edges,vizClusterNodesToClusterIDs){
  #Edges between a regular node and a cluster
  vizEdges <- handleOutClusterEdgesRegular(vizEdges,edges,vizClusterNodesToClusterIDs)

  #Edges between 2 clusters
  vizEdges <- handleOutClusterEdgesInterCluster(vizEdges,edges,vizClusterNodesToClusterIDs)


  vizEdges
}


#Function that handles the edges that are between two clusters
#@param vizEdges: edges that will appear in the visualization later
#@param edges dataframe of graph edges
#@param vizClusterNodesToClusterIDs link between clusterID and the node ID of the cluster in the visualization
#@return ataframe with the edges as how they appear in the ending visualization
handleOutClusterEdgesInterCluster <- function(vizEdges,edges,vizClusterNodesToClusterIDs){
  #Get the edges that run between 2 clusters, those clusters must have different clusterIDS
  edges %>% filter((!is.na(clusterID.sourceNode)&(!is.na(clusterID.targetNode)))&(clusterID.sourceNode != clusterID.targetNode)) -> edgesBetweenClusters

  #If there are no edges between clusters, return vizEdges
  if(nrow(edgesBetweenClusters) > 0){

  inverseEdges <- data.frame("sourceNodeID" = edgesBetweenClusters$targetNodeID,
             "targetNodeID" = edgesBetweenClusters$sourceNodeID,
             "weight"=edgesBetweenClusters$weight,
             "type"=edgesBetweenClusters$type,
             "collabType"= edgesBetweenClusters$collabType,
             "clusterID.sourceNode"= edgesBetweenClusters$clusterID.targetNode,
             "clusterID.targetNode" = edgesBetweenClusters$clusterID.sourceNode)

  directedEdgesBetweenClusters <- edgesBetweenClusters %>% full_join(inverseEdges)

  #There must be one edge between the two clusters in the end visualization
  vizInterClusterEdges <- edgesBetweenClusters %>% select(clusterID.sourceNode,clusterID.targetNode) %>% unique() %>%
    #We need to know how the clusterIDs relate to the IDs of the clusters in the visualization
    inner_join(vizClusterNodesToClusterIDs, by=c("clusterID.sourceNode" = "clusterID")) %>% select(-label,-clusterNodeWeight) %>%
    rename( "vizSourceNodeID" = "vizNodeID") %>%
    inner_join(vizClusterNodesToClusterIDs, by=c("clusterID.targetNode" = "clusterID"))%>% select(-label,-clusterNodeWeight) %>%
    rename( "vizTargetNodeID" = "vizNodeID")


  #Remove duplicates that are switched tuples
  vizInterClusterEdges %>% mutate("duplicateTest" = paste(pmin(clusterID.sourceNode,clusterID.targetNode),pmax(clusterID.sourceNode,clusterID.targetNode))) %>%
    group_by(grp = duplicateTest) %>%
    distinct(grp, .keep_all = TRUE) %>%
    ungroup() %>%
    select(-grp, -duplicateTest) -> vizInterClusterEdges

  #Future optional addition: Order columns ascending : lowest ID is placed left (clusterID.sourceNode, clusterID.targetNode)
  ####

  #Determine the weight of the inter cluster edge
  vizInterClusterEdges <- determineInterClusterWeight(directedEdgesBetweenClusters, vizInterClusterEdges)


  #Determine the collab type of the inter cluster edge
  vizInterClusterEdges <- determineCollabTypeInterClusterEdge(directedEdgesBetweenClusters,vizInterClusterEdges)


  #remove the original edges from the visualization and replace them with the new cluster edges
  directedEdgesBetweenClusters %>% select(sourceNodeID,targetNodeID) -> edgesToRemove
  vizEdges %>% anti_join(edgesToRemove,by=c("sourceNodeID"="sourceNodeID","targetNodeID"="targetNodeID")) -> vizEdges
  #Add the new cluster to cluster edges
  vizInterClusterEdges %>% ungroup() %>% select(-"clusterID.sourceNode", -"clusterID.targetNode") -> vizInterClusterEdges

  vizEdges %>% full_join(vizInterClusterEdges, by = c( "sourceNodeID"="vizSourceNodeID","targetNodeID"="vizTargetNodeID", "weight"="clusterEdgeWeight",
                                                       "collabType"="clusterEdgeCollabType")) -> vizEdges

  #All the new edges are of type "undirected
  vizEdges <- vizEdges %>% mutate(type = ifelse(is.na(type),"undirected", as.character(type)))
  }
  vizEdges <- vizEdges %>% select(-"clusterID.sourceNode",-"clusterID.targetNode")
  vizEdges
}







#Function that determines the new collaboration type of the inter cluster edges
#@param directedEdgesBetweenClusters dataframe of the edges between clusters, but each edgeis mentioned twice: one for each direction
#@param vizInterClusterEdges dataframe of the intercluster edges that will appear in the ending visualization
#@return  dataframe of the inter cluster edges extended with the intercluster edge collaboration type
determineCollabTypeInterClusterEdge <- function(directedEdgesBetweenClusters,vizInterClusterEdges){


  directedEdgesBetweenClusters %>% group_by(clusterID.sourceNode,clusterID.targetNode) %>%
    summarize("numberPP" = sum(collabType=="pair programming"),
              "numberDP" = sum(collabType=="disjunct programming"),
              "numberPADP" = sum(collabType=="pair and disjunct programming")) %>%
    group_by(clusterID.sourceNode,clusterID.targetNode) %>%
    summarize("clusterEdgeCollabType" = ifelse(numberPADP > 0,
                                               "pair and disjunct programming",
                                               ifelse( numberPP == 0,
                                                       "disjunct programming",
                                                       ifelse(numberDP == 0,
                                                              "pair programming",
                                                              "pair and disjunct programming")))) %>%
    inner_join(vizInterClusterEdges) -> vizInterClusterEdges

  vizInterClusterEdges

}

#Function that determines the weight of an edge between two clusters
#@param directedEdgesBetweenClusters dataframe of the edges between clusters, but each edgeis mentioned twice: one for each direction
#@param vizInterClusterEdges dataframe of the intercluster edges that will appear in the ending visualization
#@return dataframe of the inter cluster edges extended with the intercluster edge weight
determineInterClusterWeight <- function(directedEdgesBetweenClusters,vizInterClusterEdges){
  #Get heaviest weight
  directedEdgesBetweenClusters %>% group_by(clusterID.sourceNode,clusterID.targetNode) %>% arrange(-weight) %>% slice(1) %>%
    select(clusterID.sourceNode,clusterID.targetNode,weight) %>% rename("heaviestWeight" = weight) %>%
    inner_join(vizInterClusterEdges) -> vizInterClusterEdges
  #calculate the avg weight
  directedEdgesBetweenClusters %>% group_by(clusterID.sourceNode,clusterID.targetNode) %>% summarize("avgWeight" = mean(weight)) %>%
    inner_join(vizInterClusterEdges) %>%
    mutate("clusterEdgeWeight" = (avgWeight + heaviestWeight)/2) %>% select(-avgWeight,-heaviestWeight) -> vizInterClusterEdges


  vizInterClusterEdges
}



#Function that handles edges between a regular node and a cluster
#@param vizEdges: edges that will appear in the visualization later
#@param edges dataframe of graph edges
#@param vizClusterNodesToClusterIDs link between clusterID and the node ID of the cluster in the visualization
#@return dataframe that contains all normal edges incl edge from a regular node to cluster + weight and collabType
handleOutClusterEdgesRegular <- function(vizEdges,edges,vizClusterNodesToClusterIDs){
  #Handle edges where only one side is a cluster, the other is clusterID NA
  edges %>% filter((is.na(clusterID.sourceNode) & !is.na(clusterID.targetNode))|(is.na(clusterID.targetNode) & !is.na(clusterID.sourceNode))) %>%
    mutate("regularNodeID" = ifelse(is.na(clusterID.sourceNode), sourceNodeID,targetNodeID),
           "clusterID" = ifelse(is.na(clusterID.sourceNode),clusterID.targetNode,clusterID.sourceNode),
           "clusterNodeID" = ifelse(is.na(clusterID.sourceNode), targetNodeID,sourceNodeID)) -> edgesBetweenRegularAndCluster

  #There must be one edge between the regular node and cluster in the end visualization
  vizRegularToClusterEdges <- edgesBetweenRegularAndCluster %>% select(regularNodeID,clusterID) %>% unique() %>%
    inner_join(vizClusterNodesToClusterIDs, by = ("clusterID"="clusterID")) %>% select(-label,-clusterNodeWeight)


  #Determine the new edge weight
  vizRegularToClusterEdges <- determineRegularNodeToClusterEdgeWeight(edgesBetweenRegularAndCluster,vizRegularToClusterEdges)

  #Determine collaboration type
  vizRegularToClusterEdges <- determineCollabTypeRegularNodeToClusterEdge(edgesBetweenRegularAndCluster,vizRegularToClusterEdges)

  #remove the original edges from the visualization and replace them with the new cluster edges
  edgesBetweenRegularAndCluster %>% select(sourceNodeID,targetNodeID) -> edgesToRemove
  vizEdges %>% anti_join(edgesToRemove, by=c("sourceNodeID"="sourceNodeID","targetNodeID"="targetNodeID")) -> vizEdges
  #Add the new regular node to cluster edges
  if(nrow(vizRegularToClusterEdges) > 0){
  vizEdges %>% full_join(vizRegularToClusterEdges, by=c( "sourceNodeID"="regularNodeID","targetNodeID"="vizNodeID", "weight"="clusterEdgeWeight",
                                                         "collabType"="clusterEdgeCollabType"))  %>% select(-clusterID) -> vizEdges
  }

  #All the new edges are of type "undirected"
  vizEdges <- vizEdges %>% mutate(type = ifelse(is.na(type),"undirected", as.character(type)))

  vizEdges
}

#Function that determines the weight of the new edge that connects a regular node to a cluster
determineRegularNodeToClusterEdgeWeight <- function(edgesBetweenRegularAndCluster,vizRegularToClusterEdges){
  #Determine the new edge weight
  #Group the edges that go from this regular node the the same cluster
  #Search for these edges the heaviest edge and the average edge weight
  edgesBetweenRegularAndCluster %>% group_by(regularNodeID,clusterID) %>% arrange(-weight) %>% slice(1) %>%
    select(regularNodeID,clusterID, weight) %>% rename("heaviestWeight" = weight) %>%
    inner_join(vizRegularToClusterEdges) -> vizRegularToClusterEdges
  #Now get the avg weight
  edgesBetweenRegularAndCluster %>% group_by(regularNodeID,clusterID) %>% summarize("avgWeight" = mean(weight)) %>%
    inner_join(vizRegularToClusterEdges) %>%
    #calculate the final edge weight
    mutate("clusterEdgeWeight" = (avgWeight + heaviestWeight)/2) %>% select(-avgWeight,-heaviestWeight) -> vizRegularToClusterEdges

   vizRegularToClusterEdges
}

#Function that calculates the collaboration type of the new edge between a regular node and a cluster node
#Returns dataframe with added collaboration type for the cluster edges
determineCollabTypeRegularNodeToClusterEdge <- function(edgesBetweenRegularAndCluster,vizRegularToClusterEdges){

  edgesBetweenRegularAndCluster %>% group_by(regularNodeID,clusterID) %>%
    summarize("numberPP" = sum(collabType=="pair programming"),
              "numberDP" = sum(collabType=="disjunct programming"),
              "numberPADP" = sum(collabType=="pair and disjunct programming")) %>%
    group_by(regularNodeID,clusterID) %>% summarize("clusterEdgeCollabType" = ifelse(numberPADP > 0,
                                                                                     "pair and disjunct programming",
                                                                                     ifelse( numberPP == 0,
                                                                                             "disjunct programming",
                                                                                             ifelse(numberDP == 0,
                                                                                                    "pair programming",
                                                                                                    "pair and disjunct programming")))) -> clusterEdgeCollabTypes
   vizRegularToClusterEdges %>% inner_join(clusterEdgeCollabTypes, by = c("regularNodeID"="regularNodeID",
                                                                          "clusterID"="clusterID"))  -> vizRegularToClusterEdges

  vizRegularToClusterEdges

}

#Function that calculates the cluster node weight and label
#@param clusterProgrammers : dataframe of programmerNodes that belong to clusters
#@param vizClusterNodesToClusterIDs : dataframe of vizClusterIDs and clusterIDS
#@return dataframe with the vizNodeID, the corresponding clusterID, label and weight
calculateClusterWeightAndLabel <- function(clusterProgrammers, vizClusterNodesToClusterIDs){
  #Get the heaviest node for every cluster
  clusterProgrammers %>% group_by(clusterID) %>% arrange(-weight) %>% slice(1) %>% select(clusterID, weight) %>% rename("heaviestWeight" = weight) %>%
    inner_join(vizClusterNodesToClusterIDs) -> vizClusterNodesToClusterIDs

  #Get average node weight in a cluster
  clusterProgrammers %>% group_by(clusterID) %>% summarize("avgWeight" = mean(weight)) %>% inner_join(vizClusterNodesToClusterIDs) %>%
    #calculate the final node weight for the cluster
    mutate("clusterNodeWeight" = (avgWeight + heaviestWeight)/2) %>% select(-avgWeight,-heaviestWeight) -> vizClusterNodesToClusterIDs

  #Now calculate the cluster label
  clusterProgrammers %>% group_by(clusterID) %>% summarise("label" = paste0(programmerName,collapse="+")) %>%
    inner_join(vizClusterNodesToClusterIDs) -> vizClusterNodesToClusterIDs

  vizClusterNodesToClusterIDs
}


#Abstraction phase:
#abstract insignificant nodes that does not have an edge strong enough for aggregation
#@param vizNodes dataframe of the nodes that appear in the visualization
#@param vizEdges dataframe of the edges that appear in the visualization
#@param AbVP abstraction victim parameter: determines which nodes are candidates for abstraction
#@param AbCP abstraction correlation parameter : determines how strong an edge must be to prevent the abstraction
#@return list with 2 elements: the data frame of the nodes and the one of the edges
abstraction <- function(vizNodes,vizEdges,AbVP,AbCP){
  #This is only for the non cluster nodes that are weak
  vizNodes %>% filter(!cluster) %>% select(programmerID,weight) %>% filter(weight < AbVP) -> regularNodes

  #Collect all the edges these programmers are involved in
  vizEdges %>% filter((sourceNodeID %in% regularNodes$programmerID)) %>% mutate("abstractionCandidate" = sourceNodeID) -> candidateEdges
  vizEdges %>% filter(targetNodeID %in% regularNodes$programmerID) %>% mutate("abstractionCandidate" = targetNodeID) %>%
    full_join(candidateEdges) -> candidateEdges

  #For each node we want to know if all edges are weak
  candidateEdges %>% rename("edgeWeight" = "weight") %>%
    #We need to know the node weight also
    inner_join(regularNodes, by = c("abstractionCandidate" = "programmerID")) %>%
    rename("nodeWeight" = "weight") %>%
    #calculate the distance significance for every edge
    mutate("DS" = abs(edgeWeight - nodeWeight)) %>%
    #look now if there is an edge strong enough
    mutate("doNotAbstract" = (DS > AbCP)) %>%
    group_by(abstractionCandidate) %>% summarise("result" = sum(doNotAbstract)) %>%
    #ony the nodes with 0 strong edges will get abstracted
    filter(result == 0) %>% ungroup() %>% select(abstractionCandidate) -> nodesToAbstract

    returnlist <- abstractTheseNodes(nodesToAbstract, vizNodes, vizEdges)

  returnlist
}

#Function that will remove these nodes and their incident edges from the visualization
#@param nodesToAbstract ist of nodeIDs to remove from the visualization
#@param vizNodes dataframe of the nodes that appear in the visualization
#@param vizEdges dataframe of the edges that appear in the visualization
#@return list with 2 elements: the data frame of the nodes and the one of the edges
abstractTheseNodes <- function(nodesToAbstract, vizNodes, vizEdges){
  #remove the nodes
  vizNodes <- vizNodes %>% anti_join(nodesToAbstract, by = c("programmerID" = "abstractionCandidate"))

  #remove the edges for with the source or target node is a node that we want to abstract
  vizEdges <- vizEdges %>% filter(!(sourceNodeID %in% nodesToAbstract$abstractionCandidate)) %>%
    filter(!(targetNodeID %in% nodesToAbstract$abstractionCandidate))

  returnlist <- list("nodes" = vizNodes,"edges" = vizEdges)
  returnlist
}
