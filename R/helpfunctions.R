
#swap the columns values if the value of the second column > the first column
#precondition: column values are numeric or ordinal
#@param data : dataframe
#@param column1 : first column name, that should contain the lowest value
#@param column2 : second column name, that should contain the highest value
columnsAscending <- function(data, column1, column2){
  column1 <- rlang::sym(column1)
  column2 <- rlang::sym(column2)

  data <- data %>% mutate(swaphelp = !!column1)
  data <- data %>% mutate(!!column1 := ifelse(!!column1 > !!column2,
                                        !!column2,
                                        !!column1),
                          !!column2 := ifelse(swaphelp > !!column2,
                                              swaphelp,
                                              !!column2)) %>%
    select(-swaphelp)


  data
}



#Function that normalizes the values in the specified column based on the min and max value of that column; column will be overwritten
#@param data: dataframe
#@param column: column name of which we want to normalize the values
normalizeBasedOnMinAndMax <- function(data,column){
  column <- rlang::sym(column)

  max <- data %>% select(!!column) %>% max()
  min <- data %>% select(!!column) %>% min()

  if(max == min){
    data <- data %>% mutate(!!column := 1/length(!!column))
  } else {
    data <- data %>% mutate(!!column := ((!!column - min)/(max-min)))
  }

  data

}
