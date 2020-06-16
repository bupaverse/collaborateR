
#Function that reads the VCS eventlog and turns it into a dataframe
#@param filePath xes file
#How to transform a VCS log into an event log that is supported by the program:
#*Case ID = File ID
#*Activity Type = "Commit"
#Time stamp = time stamp of the commit
#Resource = programmer that committed the file
#Other required attributes: modifier status, revision number
#
#@return log dataframe that contains: fileId, event, filePath, modifierStatus, programmerName, commitID, timestamp, programmerID
#Every observation is one commit by one programmer on one file at a certain time

#' @title Read Version Control System Event log
#'
#' @description XES-files with at least the following attributes
#'
#' - Activity Type = "Commit"
#' - Time stamp = time stamp of the commit
#' - Resource = programmer that committed the file
#" - Other required attributes: modifier status, revision number
#'
#' @param filePath Path to XES-file
#'
#'
#' @return Data.frame that contains: fileId, event, filePath, modifierStatus, programmerName, commitID, timestamp, programmerID
#' @export
#'
read_vcs_eventlog <- function(filePath){
  #read log
  log <- read_xes(filePath)
  #cast to dataframe
  df_log <- as.data.frame(log)
  #clean dataframe
  log <- df_log %>% select(fileId = CASE_concept_name, event, filePath, modifierStatus, programmerName = resource_id, commitID = revision, timestamp)
  #give every programmer a unique ID
  programmers <- log$programmerName %>% unique()
  log <- full_join(log,data.frame("programmerID"=1:length(programmers),"programmerName"=programmers))
  log
}

#Function to get commits on which the programmers did pair programming
#@returns a dataframe with commitID, programmerID.X and programmerID.y
#of programmers that worked together (=did pair programming) on that commit
getPairProgrammingCommits <- function(log){
#For every commit the pairs of programmers that contributed to it
commitProgrammerPairs <- log %>% select(commitID, programmerID) %>% unique()

edges <- inner_join(commitProgrammerPairs,commitProgrammerPairs, by = "commitID") %>%
  filter(as.numeric(programmerID.x) < as.numeric(programmerID.y))

edges
}

#Get dataframe with commitID.x, fileId, commitID.y
#Attention! Pairs are mentioned TWICE!
#Example 308  1   310   an 310   1   308
getCommitsWithFilesInCommon <- function(log){
  #List of commit pairs that have files in common
  commitFilePairs <- log %>% select(commitID, fileId) %>% unique()
  #filter != and not < cause we dont know the way the tuples will be ordered later on
  commitsWithFilesInCommon <- inner_join(commitFilePairs, commitFilePairs, by = "fileId") %>% filter(commitID.x != commitID.y)  %>% unique()

  commitsWithFilesInCommon

}

#Function to get the carthesic product of the programmerId, commitID pairs
#@returns dataframe programmerID.x, commitID.x, programmerID.y, commitID.y
#Meaning programmer x worked on commit x, y on commit y
getCarthesisProductProgrCommitPairs <- function(log){
  programmerCommitPairs <- log %>% select(programmerID, commitID) %>% unique()
  programmerCommitPairs <- merge(programmerCommitPairs, programmerCommitPairs, by = NULL, all = TRUE) %>% filter(programmerID.x < programmerID.y) %>%
    filter(commitID.x != commitID.y)


  programmerCommitPairs
}

#Function to get the carthesic product of programmer commit pairs, but with the condition that the programmers in question did not do pair programming on both the commits mentioned
#@returns dataframe programmerID.x, commitID.x, programmerID.y, commitID.y
getCPProgrCommitPairsNotTwicePairprogr <- function(log){
  programmerCommitPairs <- getCarthesisProductProgrCommitPairs(log)

  #remove the combinations where for the programmer pair the 2 selected commits are both pair programming commits
  #where the 2 programmers in question are involved in
  programmerCommitPairs <- programmerCommitPairs %>% mutate("duplicateTest" = paste(pmin(programmerID.x,programmerID.y), pmin(commitID.x,commitID.y),pmax(programmerID.x,programmerID.y), pmax(commitID.x,commitID.y)))
  programmerCommitPairs <- programmerCommitPairs[!(duplicated(programmerCommitPairs$duplicateTest) | duplicated(programmerCommitPairs$duplicateTest, fromLast = TRUE)), ]
  programmerCommitPairs <- programmerCommitPairs %>% select(-duplicateTest)

  programmerCommitPairs
}


#@returns dataframe with commitID and timestamp
getCommitTimestamps <- function(log){
  log %>% select(commitID, timestamp) %>% unique()
}
