#Functions to calculate the weights of the edges
#Function to calculate the edge weights
#@param log : event log of the VCS log
#@param edges: dataframe of the edges currently in the graph
#@param FSW : frequency significance weight -> how much does the FS attribute to the final edge weight
#@param PCW : proximity correlation weight  -> how much does the PC attribute to the final edge weight
#@param PCtimeBetweenPairProg : for proximity correlation: param time that must be within 2 pair programming commits to count as a new pair programming session, default = 30
#@param PCtime_units : for proximity correlation: time unit of param timeBetweenPairProg, default "days"
#@param FScollabTimespan : for frequency significance : param timespan that indicates that 2 commits within this span are part of the same collaboration session (recursive), default = 7
#@param FStime_units : for frequency significance: time unit of param FScollabTimespan, default "days"
calculateEdgeWeights <- function(log, edges, FSW, PCW, fileImportance, PCtimeBetweenPairProg, PCtime_units,FScollabTimespan, FStime_units){
  print("Calculating the edge weights")

  #TO DO: delete
 # FSW = 0.4
 #PCW = 0.6
 #PCtimeBetweenPairProg = 30
 #PCtime_units = "days"
  #FScollabTimespan = 7
  #FStime_units = "days"
  ######

  #Calculate the frequency significance
  frequencySignificance <- calculateFrequencySignificance(log,edges, fileImportance,FScollabTimespan, FStime_units )


  #Calculate the proximity correlation for edges
  proximityCorrelation <- calculateProximityCorrelation(log, edges, fileImportance, PCtimeBetweenPairProg, PCtime_units)


  #left join with edges: fill in the FS and PC where possible, where NA -> 0
  edges %>% left_join(frequencySignificance, by = c("sourceNodeID","targetNodeID")) %>%
    mutate(FS = ifelse(is.na(FS), 0, FS)) %>%
    left_join(proximityCorrelation,by = c("sourceNodeID","targetNodeID") ) %>%
    mutate(PC = ifelse(is.na(PC),0,PC)) -> edges



  #Normalize the FS and PC
  edges <- normalizeBasedOnMinAndMax(edges, "FS")
  edges <- normalizeBasedOnMinAndMax(edges, "PC")

  #Calculate the weighted sum , using the weight params passed as args
  edges %>% mutate(weight = FSW * FS + PCW * PC) %>% select(-FS, -PC) -> edges


  #clean up edges with weight 0
  edges %>% filter(weight > 0) -> edges

  edges
}


#Function that calculates for every edge the frequency significance
#@param log
#@param edges : dataframe of the edges
#@param fileImportance : dataframe with the file importance
#@param collabSpan : param timespan that indicates that 2 commits within this span are part of the same collaboration session (recursive)
#@param time_units : time unit of param collabSpan, default "days"
#@returns dataframe with sourceNodeID, targetNodeID, FS (frequency significance for that edge)
calculateFrequencySignificance <- function(log, edges, fileImportance,collabTimespan,time_units){


  #get commit timestamps
  commitTimestamps <- getCommitTimestamps(log)

  #This concerns disjunct programming only
  disjunctEdges <- edges %>% filter(! collabType == "pair programming")

  #Get commits with files in common (Attention: each couple is mentioned twice)
  commitsWithFilesInCommon <- getCommitsWithFilesInCommon(log)
  #Get carthesic product of programmer commit pairs, condition: the commits are not both pair programming where the two programmers are in involved
  progrCommitPairs <- getCPProgrCommitPairsNotTwicePairprogr(log)

  #only the programmer pairs that appear in disjunctEdges are of interest
  disjunctEdges %>% inner_join(progrCommitPairs, by = c("sourceNodeID" = "programmerID.x", "targetNodeID" = "programmerID.y")) %>%
  #Only commit pairs with files in common are of interest : innerjoin on commit ID with commitsWithFilesInCommon
  inner_join(commitsWithFilesInCommon, by = c("commitID.x", "commitID.y")) %>%
    #expand with the timestamps of the commits
  inner_join(commitTimestamps, by = c("commitID.x" = "commitID")) %>%
    rename(timestamp.x = timestamp) %>%
    inner_join(commitTimestamps, by = c("commitID.y" = "commitID")) %>%
    rename(timestamp.y = timestamp) %>%
    #we dont need a lot of the info in the dataframe now
    select(-weight,-type,-collabType, -commitID.x, -commitID.y) %>% unique() %>%
  #now start the actual calculation
  #Calculate the number of collaboration blocks
  #We need the timestamps in 1 column
  gather(key = programmer, value = timestamp, timestamp.x, timestamp.y) %>%
    #we still need to know which timestamp belongs to which programmer
    mutate(programmer = ifelse(programmer == "timestamp.x",sourceNodeID, targetNodeID) ) %>%
    unique() %>%
    #Begin calculation per edge per file
    group_by(sourceNodeID, targetNodeID, fileId) %>%
    #next programmer+timestamp
    arrange(timestamp) %>%
    mutate(next_P = lead(programmer),
           next_timestamp = lead(timestamp)) %>%
    filter(!is.na(next_timestamp)) %>%
    #calculate time difference
    mutate(timeDiff = ifelse(timestamp < next_timestamp,
                             as.double(next_timestamp - timestamp, units = time_units),
                             as.double(timestamp - next_timestamp, units = time_units))) %>%
    filter(!((programmer == next_P)&(timeDiff <= collabTimespan))) %>%
    mutate(block = ifelse(timeDiff > collabTimespan,0,1)) %>%
    mutate(next_block = lead(block)) %>%
    filter(!((block == 1) & (next_block == 1))) %>%
    summarise(numberOfCollabBlocks = sum(block)) %>% arrange(-numberOfCollabBlocks) %>% ungroup() %>%
    #now we need the file importance
    inner_join(fileImportance, by = "fileId") %>%
    mutate(FSFile = numberOfCollabBlocks * importance) %>%
    group_by(sourceNodeID, targetNodeID) %>%
    summarise(FS = sum(FSFile)) %>%
    ungroup() -> frequencySignificance

  frequencySignificance
}




#function that calculates for every edge the proximity correlation
#@param log
#@param edges : dataframe of the edges
#@param fileImportance : dataframe with the file importance
#@param timeBetweenPairProg : param time that must be within 2 pair programming commits to count as a new pair programming session, default = 30
#@param time_units : time unit of param timeBetweenPairProg, default "days"
#@returns dataframe with sourceNodeID, targetNodeID, PC (proximity correlation for that edge)
calculateProximityCorrelation <- function(log,edges, fileImportance,timeBetweenPairProg, time_units){


  pairProgrammingCommits <- getPairProgrammingCommits(log)
  filesPerCommit <- log %>% select(commitID, fileId, timestamp) %>% unique()

  #this concerns pair programming only, so filter disjunct
  edges %>% filter(collabType != "disjunct programming") %>%
    select(sourceNodeID, targetNodeID) %>%
    #get the pair programming commits for the edges
    inner_join(pairProgrammingCommits, by=c("sourceNodeID" = "programmerID.x", "targetNodeID"= "programmerID.y")) %>%
    #link the files to the commits
    inner_join(filesPerCommit) %>%
    #add the file importance
    inner_join(fileImportance) -> edgePairProgrCommitFiles


    #calculate intermediate proximity correlation (every file importance once)
    edgePairProgrCommitFiles %>%
      select(sourceNodeID, targetNodeID, fileId, importance) %>%
      unique() %>%
      group_by(sourceNodeID, targetNodeID) %>%
      #sum of the importance of all the files of those programmers
      summarize(PC = sum(importance))  -> proximityCorrelation


  #Correct the proximity correlation forfiles that were worked on more than once,
    #if the time span between modified is large enough
    edgePairProgrCommitFiles %>%
    #start calculating the proximity correlation
    group_by(sourceNodeID, targetNodeID, fileId) %>%
      arrange(timestamp) %>%
    #There is always al least one time stamp
    mutate(next_timestamp = lead(timestamp)) %>%
    #Filter where there is no next
    filter(!is.na(next_timestamp)) %>%
      #only keep the row if the time span is big enough
      filter(as.double(next_timestamp - timestamp, units = time_units) >= timeBetweenPairProg) %>%
      #sum of importance over all files
      group_by(sourceNodeID, targetNodeID) %>%
      mutate(extraPC = sum(importance)) %>%
      select(sourceNodeID, targetNodeID, extraPC) %>% unique() -> extraProximityCorrelation

    #Add the extra proximity to the proximity correlation value
    proximityCorrelation %>%
      left_join(extraProximityCorrelation) %>%
      #where there is no extraPC set 0
      mutate(extraPC = ifelse(is.na(extraPC), 0, extraPC)) %>%
      mutate(PC = PC+ extraPC) %>%
      select(-extraPC)  %>%
      ungroup()-> proximityCorrelation


  proximityCorrelation

}
