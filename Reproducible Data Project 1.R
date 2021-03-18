########1#########2#########3#########4#########5#########6#########7###########
#                                                                              #
#  Reproducible Data Project 1 (03/017/2021)                                   #
#                                                                              #
#  This project analyzes data from a personal activity monitoring device.      #
#  The data consist of two months of data (October and November 2012)          #
#  for one individual and include the number of steps taken in 5 minute        #
#  intervals each day.  The project is for a Reproducible Research course.     #
#                                                                              #
#########1#########2#########3#########4#########5#########6#########7##########

   library(dplyr)
   library(ggplot2)

#  read in the activity data
#  create a dataset with no NAs

   act_dat <- read.csv("data/activity.csv") 

   head(act_dat, n=30)
   tail(act_dat, n=30)
   
   table(act_dat$date)
   
   summary(act_dat$steps)
   
   act_noNA <- na.omit(act_dat)
   
   
#  defince multiplot function
#
#  ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#  - cols:   Number of columns in layout
#  - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
#  If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#  then plot 1 will go in the upper left, 2 will go in the upper right, and
#  3 will go all the way across the bottom.
   
   
   multiplot <- function(...,
                         plotlist = NULL,
                         file,
                         cols = 1,
                         layout = NULL) {
      library(grid)
      
      # Make a list from the ... arguments and plotlist
      plots <- c(list(...), plotlist)
      
      numPlots = length(plots)
      
      # If layout is NULL, then use 'cols' to determine layout
      if (is.null(layout)) {
         # Make the panel
         # ncol: Number of columns of plots
         # nrow: Number of rows needed, calculated from # of cols
         layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                          ncol = cols,
                          nrow = ceiling(numPlots / cols))
      }
      
      if (numPlots == 1) {
         print(plots[[1]])
         
      } else {
         # Set up the page
         grid.newpage()
         pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
         
         # Make each plot, in the correct location
         for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <-
               as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]],
                  vp = viewport(
                     layout.pos.row = matchidx$row,
                     layout.pos.col = matchidx$col
                  ))
         }
      }
   }
   
   
   
   
#  Step 1      
#  calculate the total number of steps taken per day
#  create a histogram of total steps   
#  compute the mean and median   
     
   steps_tot <- aggregate(x = act_noNA$steps,    
                          by = list(act_noNA$date), 
                          FUN = sum)              

   ggplot(steps_tot, aes(x = x)) +                           
      geom_histogram(bins=20, na.rm=TRUE) +
      labs(
         x="Number of Steps",
         y="Frequency",
         title = "Distribution of Total Steps Taken Per Day"
      ) 
   
   mean(steps_tot$x)
   median(steps_tot$x)

      
#  Step 2
#  caclulate the average number of steps taken for each 5-minute interval
#  create a time series plot of averages
#  calcluate the maximum number of steps taken during a 5-minute intervale 
     
   steps_avg <- aggregate(x = act_noNA$steps,    
                          by = list(act_noNA$interval), 
                          FUN = mean)              
   
   
   ggplot(steps_avg, aes(x=Group.1, y=x)) + 
      geom_line() + 
      labs(x="Time Interval", y="Average Number of Steps",
           title="Average Number of Steps by Time Interval")
   
   max(steps_avg$x)
   
   
#  Step 3
#  caclulate the number of missing values for steps
#  copy steps variable to imp variable   
#  impute the NAs with the median for stpes   
#  create histogram of total steps with imputed data
#  compute the mean and median   
        
   NAlist <- is.na(act_dat$steps)   

   table(NAlist)   

   act_dat$imp <- act_dat$steps
   
   act_dat$imp[is.na(act_dat$imp)] = median(act_noNA$steps)
   
   steps_imp <- aggregate(x = act_dat$imp,    
                          by = list(act_dat$date), 
                          FUN = sum)              
   
   ggplot(steps_imp, aes(x = x)) +                           
      geom_histogram(bins=20, na.rm=TRUE) +
      labs(
         x="Number of Steps",
         y="Frequency",
         title = "Distribution of Total Steps Taken Per Day (Imputed Data)"
      ) 
   
   mean(steps_imp$x)
   median(steps_imp$x)
   
   
#  Step 4
#  create variable "day" that contains the day of the week for each date
#  create variable "cat" that has value "weekend" if day of the week is a weekend,
#  and has value of "weekday" otherwise.   
#  caclulate the average number of steps taken for each 5-minute interval for
#  weekends and for weekdays   
#  create a time series plot of averages for weekends versus weekdays

   act_dat$day <- weekdays(as.Date(act_dat$date))
   
   act_dat$cat <- ifelse(grepl("Sunday|Saturday", act_dat$day), "weekend", "weekday")   
   

   act_day <- subset(act_dat, act_dat$cat=="weekday")
   act_end <- subset(act_dat, act_dat$cat=="weekend")
   
   steps_day <- aggregate(x = act_day$imp,    
                          by = list(act_day$interval), 
                          FUN = mean)              

   steps_end <- aggregate(x = act_end$imp,    
                          by = list(act_end$interval), 
                          FUN = mean)              
  
   ylim <- max(steps_end$x, steps_day$x)
   
   p1 <- ggplot(steps_end, aes(x=Group.1, y=x)) + 
      geom_line() + 
      labs(x="Time Interval", y="Average Number of Steps",
           title="Average Number of Steps on Weekends by Time Interval") +
      ylim(0, ylim)
   
   p2 <- ggplot(steps_day, aes(x=Group.1, y=x)) + 
      geom_line() + 
      labs(x="Time Interval", y="Average Number of Steps",
           title="Average Number of Steps on Weekdays by Time Interval") +
      ylim(0, ylim)
   
   multiplot(p1, p2, cols=1)
