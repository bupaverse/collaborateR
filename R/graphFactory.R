#this file contains a graph object, that respresent the collaboration graph


collaborationGraph_factory <- R6Class(
  "Collaboration Graph",
  #############################################
  #private section
  private = list(
    #private variables
    ..fileImportance = data.frame(),  #2 columns: fileId and importance
    ..edges = data.frame(),
    ..nodes = data.frame(),
    ..BFSW = 0,
    ..PCW = 0 ,
    ..UFSW = 0,
    ..BCW = 0,
    ..ECW = 0,
    ..DCW = 0,
    ..simplificationCutoffParam = 0,

    #edge weight: Proximity correlation and Frequency significance params
    ..PCtimeBetweenPairProg = 30,
    ..PCtime_units = "days",
    ..FScollabTimespan = 7,
    ..FStime_units = "days",



    #Private functions

    buildGraph = function(log, fileImportance, BFSW, PCW, UFSW, BCW, ECW, DCW,SCP,AgVP, AgCP, AbVP, AbCP,
                          PCtimeBetweenPairProg,PCtime_units,
                          FScollabTimespan,FStime_units){
      #set the weights
      private$..BFSW <- BFSW
      private$..PCW <- PCW
      private$..UFSW <- UFSW
      private$..BCW <- BCW
      private$..ECW <- ECW
      private$..DCW <- DCW
      private$..simplificationCutoffParam <- SCP





      #set the fileImportance
      private$..fileImportance <- fileImportance

      #Set the params for frequency significance and proximity correlation of the edge weights
      private$..PCtimeBetweenPairProg <- PCtimeBetweenPairProg
      private$..PCtime_units <- PCtime_units
      private$..FScollabTimespan <- FScollabTimespan
      private$..FStime_units <- FStime_units

      private$buildBaseGraph(log)
      private$calculateWeights(log)
      private$simplification(log,AgVP, AgCP, AbVP, AbCP)

      print("Done building the graph")
    },

    #function to build the base graph
    buildBaseGraph = function(log){
      #build the nodes
      private$buildNodesList(log)
      #build the edges
      private$buildEdgesList(log)
    },

    #function to build the nodes list
    buildNodesList = function(log){
      #every programmer becomes a node
      print("Building the nodes")
      t <- log %>% select(programmerID, programmerName) %>% unique()
      private$..nodes <-  data.frame("programmerID" = t$programmerID, "programmerName" = t$programmerName, "weight" = 1, "cluster" = FALSE)

    },

    #function to build the edges list
    buildEdgesList = function(log){
      print("Building the edges")

      private$..edges <- getBaseGraphEdges(log)


    },

    #Function to calculate the weights of the nodes and edges
    calculateWeights = function(log){
      private$GFcalculateEdgeWeights(log)
      private$GFcalculateNodeWeights(log)
    },

    #Function to calculate the edge weights
    GFcalculateEdgeWeights = function(log){
      private$..edges <- calculateEdgeWeights(log,private$..edges, private$..BFSW, private$..PCW, private$..fileImportance, private$..PCtimeBetweenPairProg, private$..PCtime_units,
                                              private$..FScollabTimespan, private$..FStime_units)
    },

    #fuction to calculate the node weights
    GFcalculateNodeWeights = function(log){
      private$..nodes <- calculateNodeWeights(log,private$..nodes, private$..edges, private$..UFSW, private$..BCW, private$..ECW, private$..DCW, private$..fileImportance)

    },

    #Function to simplify the graph
    simplification = function(log,AgVP, AgCP, AbVP, AbCP){
      print("Simplifying the graph")

      returnlist <- simplifyGraph(log, private$..nodes, private$..edges,private$..simplificationCutoffParam,
                                  AgVP, AgCP, AbVP, AbCP)

      private$..edges <- returnlist$edges
      private$..nodes <- returnlist$nodes
    }


  ),

  #############################################
  #public section

  public = list(
    initialize = function(log, fileImportance, BFSW, PCW, UFSW, BCW, ECW, DCW, PCtimeBetweenPairProg,PCtime_units,
                          FScollabTimespan,FStime_units, SCP, AgVP, AgCP, AbVP, AbCP){
      #check
      if(!missing(log)){
        private$buildGraph(log, fileImportance, BFSW, PCW, UFSW, BCW, ECW, DCW, SCP, AgVP, AgCP, AbVP, AbCP,
                           PCtimeBetweenPairProg,PCtime_units,
                           FScollabTimespan,FStime_units)
      }
      else{
        stop("Please provide the log as argument!")
      }
    }
  ),
  #############################################
  #active bindings
  active = list(
    #function returns a df of the nodes
    nodes = function(newNodes){
      if(!missing(newNodes)){
        private$..nodes <- newNodes
        message("Warning: setting the nodes manually is not adviced. Unwanted behaviour may occur!")

      } else {
        private$..nodes
      }
    },

    #function returns a df of the edges
    edges = function(newEdges){
      if(!missing(newEdges)){
        private$..edges <- newEdges
        message("Warning: setting the edges manually is not adviced. Unwanted behaviour may occur!")

      } else {
        private$..edges
      }
    }

  )
)
