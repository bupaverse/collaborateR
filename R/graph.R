
#' Build Collaborator Graph
#' @param log Data.frame that contains: fileId, event, filePath, modifierStatus, programmerName, commitID, timestamp, programmerID
#' @param fileImportance  dataframe with 2 columns "fileID" and "importance", optional argument
#' @param AgVP aggregation victim parameter: nodes with a weight < AgVP will be candidates for aggregation, the higher the AgVP the more clusters there will be. Value must be [0,1]
#' @param AgCP aggregation correlation parameter: a node will be clustered if it is a candidate (see AgVP) and has an edge with a distance significance > AgCP, the lower AgCP the higher the chance of clustering. Value must be [0,1]
#' @param AbVP abstraction victim parameter: nodes with weight < AbVP will be candidates for removal, the higher AbVP the more nodes can be removed. Value must be [0,1]
#' @param AbCP abstraction correlation parameter: nodes that are candidate for abstraction (see AbVP) will be abstracted if all their edges have a distance significance < AbCP, the higher the AbCP the more nodes will be removed. Value must be [0,1]
#' @param useFileImportance  whether or not we want to use different importance values for the files. If set to FALSE but fileImportance arg was given, the fileImportance arg will be used nevertheless
#' @param BFSW  binary frequency significance weight -> how much does the FS attribute to the edge weight
#' @param PCW  proximity correlation weight -> how much does the PC attribute to the edge weight
#BFSW + PCW = 1
#' @param UFSW  unary frequency significance weight -> how much does the UFS attribute to the node weight
#' @param BCW  betweenness centrality weight -> how much does the BC attribute to the node weight
#' @param ECW  eigenvector centrality weight -> how much does the EC attribute to the node weight
#' @param DCW  degree centrality weight -> how much does the DC attribute to the node weight
#UFSW + BCW + ECW + DCW = 1
#' @param PCtimeBetweenPairProg  for proximity correlation: param time that must be within 2 pair programming commits to count as a new pair programming session, default = 30
#' @param PCtime_units for proximity correlation: time unit of param timeBetweenPairProg, default "days"
#' @param FScollabTimespan  for frequency significance : param timespan that indicates that 2 commits within this span are part of the same collaboration session (recursive), default = 7
#' @param FStime_units  for frequency significance: time unit of param FScollabTimespan, default "days"
#' @param SCP SCP
#'
#' @return A graph object
#' @export
#'
build_graph <- function(log, fileImportance,
                        AgVP, AgCP, AbVP, AbCP, useFileImportance = TRUE,
                        BFSW = 0.4, PCW = 0.6, UFSW = 0.15, BCW = 0.25, ECW = 0.3, DCW = 0.3,
                        PCtimeBetweenPairProg = 30, PCtime_units = "days",
                        FScollabTimespan = 7, FStime_units = "days", SCP = 0.5){
  if(missing(fileImportance)){
    #calculate default file importance
    if(useFileImportance){
    print("Calculating file importance")
    fileImportance <- calculateFileImportance(log)
    } else {
      fileImportance <- doNotUseFileImportance(log)
    }
  }

  graph <- collaborationGraph_factory$new(log, fileImportance, BFSW, PCW,UFSW, BCW, ECW, DCW,
                                          PCtimeBetweenPairProg,PCtime_units,FScollabTimespan,FStime_units, SCP,AgVP, AgCP, AbVP, AbCP )
  graph
}


#Function that calculates the fileImportance as the ration:
# #of months in which the file was changed / # months it exists
calculateFileImportance <- function(log){


  #calculate number of months in which file changed
  log %>% select(fileId,timestamp, modifierStatus) %>%
    filter(modifierStatus %in% c("Added","Modified")) %>%
    mutate(timestamp = floor_date(timestamp, unit = "month")) %>%
    select(-modifierStatus) %>%
    unique() %>%
    group_by(fileId) %>% summarize(monthsChanged = n()) -> numberOfMonthsChanged

  #We lost the files that were only deleted
  log %>% select(fileId) %>% unique() %>% left_join(numberOfMonthsChanged, by="fileId") %>%
    mutate(monthsChanged = ifelse(is.na(monthsChanged),0,monthsChanged)) -> numberOfMonthsChanged


  #calculate number of months in existence
  #numberOfMonthsInExistence <- data.frame(fileId = numberOfMonthsChanged$fileId, monthsExistence = rep(102, length(numberOfMonthsChanged$fileId)))
  numberOfMonthsInExistence <- calculateNumberOfMonthsInExistence(log)


  #calculate the importance value
  inner_join(numberOfMonthsChanged, numberOfMonthsInExistence) %>%
    #File importance cannot be > 1, otherwise mismatch between months and days due to rounding
    mutate(importance = ifelse((monthsChanged / monthsExistence) > 1, 1, monthsChanged / monthsExistence)) %>%
    select(-monthsChanged, -monthsExistence) -> fileImportance

  fileImportance
}


#Function that calculates how many months the file existed
#@returns dataframe with file ID and the number of months in existence
calculateNumberOfMonthsInExistence <- function(log){
  start_log <- min(log$timestamp)
  end_log <- max(log$timestamp)
  time_units <- "days"
  existenceLog <- as.double(end_log - start_log, units = time_units)
  listOfFileIDs <- log %>% select(fileId) %>% unique()

  log %>% filter(modifierStatus != "Modified") %>%
    distinct(fileId,modifierStatus, timestamp) %>%
    #sort by file
    group_by(fileId) %>%
    arrange(timestamp) %>%
    #register the first and last status and timestamp
    mutate(first_status = first(modifierStatus),
           last_status = last(modifierStatus),
           first_timestamp = first(timestamp),
           last_timestamp = last(timestamp)) %>%
    #what is the next status and timestamp?
    mutate(next_status = lead(modifierStatus),
           next_timestamp = lead(timestamp)) %>%
    #filter where there is no next status
    filter(!is.na(next_status)) %>%
    #filter rows that are not active time
    filter(!(modifierStatus == "Deleted" & next_status == "Added")) %>%
    filter(!(modifierStatus == "Deleted" & next_status == "Deleted")) %>%
    #Added > added does count as active time!
    #Calculate the active time between the timestamp and the next
    mutate(active_time = as.double(next_timestamp - timestamp, units = time_units)) %>%
    group_by(fileId, first_status, first_timestamp, last_status, last_timestamp) %>%
    summarize(active_time = sum(active_time))  %>%
    #if necessary add begin and end tailing time
    mutate(pre_time = ifelse(first_status =="Deleted", as.double(first_timestamp - start_log, units = time_units), 0),
         post_time = ifelse(last_status == "Added", as.double(end_log - last_timestamp, units = time_units), 0)) %>%
    #add pre and post time to the total active time
    mutate(total_active_time = active_time + pre_time + post_time) %>%
    ungroup() %>%
    distinct(fileId, total_active_time)-> filesAddedAndDeleted


    #add the files that were only modified: have the lifespan of the log
    filesAddedAndDeleted %>%
      select(fileId, total_active_time) %>%
      full_join(listOfFileIDs) %>%
      mutate(total_active_time = ifelse(is.na(total_active_time),existenceLog,total_active_time)) %>%
    #active time is in days, I want it in months, take 365 / 12 = one month
    mutate(monthsExistence = ceiling((total_active_time *12)/365)) %>%
      select(-total_active_time) -> numberOfMonthsInExistence

    numberOfMonthsInExistence

}


#Function that sets the importance value of all files to 1
doNotUseFileImportance <- function(log){
    fileId <- NULL

    log %>% select(fileId) %>% unique() -> fileIDs
    fileImportance <- data.frame("fileId" = fileIDs, "importance" = 1)

    fileImportance
}
