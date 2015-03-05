###########################################################################
#                                                                         #
#  Marijuana arrests in Chicago Part III                                  #
#  Helpers                                                                #
#  Coded by Scarlett Swerdlow                                             #
#  scarlett.swerdlow@gmail.com                                            #
#  February 8, 2015                                                       #
#                                                                         #
###########################################################################

########################################################
#  Function to split data field in city crime dataset  #
########################################################

splitDate <- function( df ) {
  
  df <- within( df, {
    Month <- as.numeric( substr( Date, 1, 2 ) )
    Day <- as.numeric( substr( Date, 4, 5 ) )
    Year <- as.numeric( substr( Date, 7, 10 ) )
    Hour <- as.numeric( substr( Date, 12, 13 ) )
    Minute <- as.numeric( substr( Date, 15, 16 ) )
    AMPM <- substr( Date, 21, 22 )
  } )
  
  df$Hour <- ifelse( df$AMPM == "PM", df$Hour + 12, df$Hour )
  
  return( df )
}

######################################################
#  Function to collapse dataframe by community area  #
######################################################

collapseDFByComm <- function( df ) {
  
  Community <- c()
  Arrests2013 <- c()
  Arrests2014 <- c()
  Change <- c()
  
  for (comm in unique( df$Community.Area ) ) {
    Arrests2013Count <- df$freq[ df$Community.Area == comm & df$Year == 2013 ]
    Arrests2014Count <- df$freq[ df$Community.Area == comm & df$Year == 2014 ]
    Community <- append( Community, comm )
    Arrests2013 <- append( Arrests2013, Arrests2013Count )
    Arrests2014 <- append( Arrests2014, Arrests2014Count )
    Change <- append( Change, ( Arrests2014Count - Arrests2013Count ) / Arrests2013Count )
  }
  
  rv <- data.frame( Community, Arrests2013, Arrests2014, Change )
  return( rv )
}

############################################
#  Function to collapse dataframe by race  #
############################################

collapseDFByRace <- function( df ) {
 
  RaceGroup <- c()
  RacePop <- c()
  RaceArrests2013 <- c()
  RaceArrests2014 <- c()
  
  # Exclude Other because it is not a majority race in any community
  races = c( "Asian", "Black", "Latino", "White" )
  
  for ( race in races ) {
    pop <- sum( df$Pop_total[ df$Majority == race ] )
    arrests2013 <- sum( df$Arrests2013[ df$Majority == race ] )
    arrests2014 <- sum( df$Arrests2014[ df$Majority == race ] )
    RaceGroup <- append( RaceGroup, race )
    RacePop <- append( RacePop, pop )
    RaceArrests2013 <- append( RaceArrests2013, arrests2013 )
    RaceArrests2014 <- append( RaceArrests2014, arrests2014 )
  }
  
  rv <- data.frame( RaceGroup, RacePop, RaceArrests2013, RaceArrests2014 )
  return( rv )
}

#######################################
# Function to identify majority race  #
#######################################

idMajorityRace <- function( record ) {
  
  # Figure out where to start and end
  start <- which( names(record) == "Asian" )
  end <- start + 4
  
  # Column number of race with majority share of community population
  majorityIndex <- which.max( record[ start:end ] ) + start - 1
  
  majorityRace <- names( record[ majorityIndex ] )
  
  return( majorityRace )
}

########################
#  Multiplot function  #
########################

# From ggplot2
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


