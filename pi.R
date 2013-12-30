
# reset variablesre
rm(list=ls())

#
#
# x and y can take values from [ -radius , +radius ]
#
is_inside <- function( x, y, radius ) {
    
  # check
  if ( x > radius || x > radius ) {
    NULL
  } else {
    inside = (sqrt(x**2 + y**2) <= radius)
    data.frame(x,y,inside)
  }
}

#
#
# determines pi value
#
determine_pi <- function(radius,sample_size=1000000,start_seed=TRUE,seed=1321123131) {
  
  # get data points
  points = get_square_points(radius*2,sample_size,start_seed,seed)
  
  # check which points are inside circle
  frame = is_inside( points$x, points$y, radius )
  
  # get frequency
  freq = as.data.frame(table(frame$inside))
  
  # initialize variables
  key <- c()
  value <- c()
  
  # true occurences
  true = freq[freq$Var1==TRUE,][,2]
  # false occurences
  false = freq[freq$Var1==FALSE,][,2]
  if (is.na(true)) {true=0}
  if (is.na(false)) {false=0}
  # percentage of trues
  perct = true/ (true + false)
  
  # how many are true
  key <- c( key, "true" )
  value <- c( value, true)
  
  # how many are false
  key <- c( key, "false" )
  value <- c( value, false)
  
  # percentage of true
  key <- c( key, "percentage" )
  value <- c( value, perct )
  
  # value of pi
  key <- c( key, "pi" )
  if (true + false == 0) {
    value <- c( value, NA )
  } else {
    value <- c( value, (radius*2)**2/(radius**2)* perct)
  }
  
  # return result
  data.frame(key,value)
}

#
#
# get a data frame with x and y columns that fit inside a  
#
get_square_points <- function( side, sample_size=1000,start_seed=TRUE,seed=1321123131 ) {

  radius = side / 2
  # set seed to better control the deviation
  if ( sample_size > 1 && start_seed ) {
    set.seed(seed)
  }
  
  # initialize variables
  x <- rnorm(sample_size,0,side*2) %% side - radius
  y <- rnorm(sample_size,0,side*2) %% side - radius
  
  # create values' matrix
  data.frame( x, y )
}









