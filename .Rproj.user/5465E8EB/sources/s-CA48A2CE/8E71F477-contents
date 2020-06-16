
#Returns a dataframe with the edges:
#sourceNodeID
#targetNodeID
#type
#undirected
#collabtype
getBaseGraphEdges <- function(log){
  pairProgEdges <- getPairProgrammingEdges(log)
  disjunctEdges <- getDisjunctCollaborationEdges(log)


  #To merge the two dataframes, make sure the programmer with the lowest index is always programmer x
  pairProgEdges <- columnsAscending(pairProgEdges, "sourceNodeID", "targetNodeID")
  disjunctEdges <- columnsAscending(disjunctEdges, "sourceNodeID", "targetNodeID")

  #Now merge the dataframes:
  #allEdges <- merge(pairProgEdges, disjunctEdges, by = c("sourceNodeID", "targetNodeID", "weight","type"), all = TRUE)
  allEdges <- full_join(pairProgEdges, disjunctEdges, by = c("sourceNodeID", "targetNodeID", "weight","type"))
  allEdges <- allEdges %>% mutate(collabType = ifelse((collabType.x == "pair programming") & (collabType.y == "disjunct programming"),
                                                      "pair and disjunct programming",
                                                      NA))  %>%
    mutate(collabType = ifelse((is.na(collabType) & is.na(collabType.x)), "disjunct programming", collabType)) %>%
    mutate(collabType = ifelse((is.na(collabType) & is.na(collabType.y)), "pair programming", collabType)) %>%
    select(-collabType.x, - collabType.y)

  #make the collabType a ordered factor
  allEdges <- allEdges %>% mutate(collabType = factor(collabType, ordered = TRUE, levels = c("disjunct programming","pair programming","pair and disjunct programming")))

  allEdges

}


#Return dataframe with sourceNodeID, targetNodeID, weight = 1, type = "undirected" and collabType = "pair programming"
getPairProgrammingEdges <- function(log){
  #For every commit the pairs of programmers that contributed to it
  #commitProgrammerPairs <- log %>% select(commitID, programmerID) %>% unique()

  #edges <- inner_join(commitProgrammerPairs,commitProgrammerPairs, by = "commitID") %>%
  #  filter(as.numeric(programmerID.x) < as.numeric(programmerID.y))

  edges <- getPairProgrammingCommits(log)
  edges <- data.frame("sourceNodeID" = edges$programmerID.x, "targetNodeID" = edges$programmerID.y, "weight" = 1, "type" = "undirected","collabType" = "pair programming")
  edges <- edges %>% unique()
  edges
}

getDisjunctCollaborationEdges <- function(log){
 #for each pair of programmers -> collect their commits
  # Compare the commits (if they are NOT BOTH pair programming commits involving these programmers)
  #If the 2 commits have 1 file in common --> add to the list
  #If the list > 0 --> an edge exists between the two programmers

    #List of commit pairs that have files in common
    #filter != and not < cause we dont know the way the tuples will be ordered later on
    commitsWithFilesInCommon <- getCommitsWithFilesInCommon(log) %>% select(-fileId) %>% unique()


    #Get carthesic product of progr commit pairs, with condition:
    #remove the combinations where for the programmer pair the 2 selected commits are both pair programming commits
    #where the 2 programmers in question are involved in
    programmerCommitPairs <- getCPProgrCommitPairsNotTwicePairprogr(log)


    #if the commit pair from programmerCommitPairs appears in commitsWithFilesInCommon -> there is an edge between the two programmers
    #We do this by an inner join
    edges <- inner_join(programmerCommitPairs,commitsWithFilesInCommon, by = c("commitID.x" = "commitID.x", "commitID.y" = "commitID.y")) %>%
      select(programmerID.x, programmerID.y) %>% unique()
    #delete duplicates that are switched tuples
    edges <- edges %>% mutate("duplicateTest" = paste(pmin(programmerID.x,programmerID.y), pmax(programmerID.x,programmerID.y)))
    edges <- edges %>%
      group_by(grp = duplicateTest) %>%
      distinct(grp, .keep_all = TRUE) %>%
      ungroup() %>%
      select(-grp, -duplicateTest)


    #return
    edges <- data.frame("sourceNodeID" = edges$programmerID.x, "targetNodeID" = edges$programmerID.y, "weight" = 1, "type" = "undirected","collabType" = "disjunct programming")

    edges <- edges %>% unique()
    edges

}
