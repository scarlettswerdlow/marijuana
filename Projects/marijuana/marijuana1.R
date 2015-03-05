###########################################################################
#                                                                         #
#  Marijuana arrests in Chicago Part I                                    #
#  Coded by Scarlett Swerdlow                                             #
#  scarlett.swerdlow@gmail.com                                            #
#  January 30, 2015                                                       #
#                                                                         #
###########################################################################

######################################################
#  Read in, clean, and subset marijuana arrest data  #
######################################################

mj_arrests <- read.csv( "~/marijuana/marijuana.csv" )

mj_arrests <- within( mj_arrests, {
  Month <- as.numeric( substr( Date, 1, 2 ) )
  Day <- as.numeric( substr( Date, 4, 5 ) )
  Year <- as.numeric( substr( Date, 7, 10 ) )
  Hour <- as.numeric( substr( Date, 12, 13 ) )
  Minute <- as.numeric( substr( Date, 15, 16 ) )
  AMPM <- substr( Date, 21, 22 )
} )

mj_arrests$Hour <- ifelse( mj_arrests$AMPM == "PM", mj_arrests$Hour + 12, mj_arrests$Hour )

mj_arrests <- subset( mj_arrests, mj_arrests$Year > 2010 & 
                        mj_arrests$Year < 2015 & 
                        mj_arrests$Arrest == "true" )

count <- length( mj_arrests$ID )

write.csv( mj_arrests, "marijuana1_map.csv" )

