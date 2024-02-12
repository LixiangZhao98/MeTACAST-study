library(ggplot2)
library(reshape2)
library(glue)
library(dplyr)
library(ggpubr)
  #we create some empty data frames to hold all events, the summary of time data, and the error data
  fullTimeData <- read.table("logfiles/log/_globalTimeDummy.txt",header=TRUE,sep="\t",fill=TRUE,blank.lines.skip=TRUE,as.is=TRUE)
  summaryTimeData <- read.table("logfiles/log/_summaryTimeDummy.txt",header=TRUE,sep="\t",fill=TRUE,blank.lines.skip=TRUE,as.is=TRUE)

  fullErrorData <-read.table("logfiles/log/_globalErrorDummy.txt",
                           header=TRUE,
                           sep="\t",
                           fill=TRUE,
                           blank.lines.skip=TRUE,
                           as.is=TRUE
                           )


  #now reading the logfiles
  files <- (Sys.glob("logfiles/log/*.csv"))
  print("Reading logfiles..." )
  
  for (file in files){

    data <- read.table(file,header=TRUE,sep=",",fill=TRUE,blank.lines.skip=TRUE,as.is=TRUE)
    #we remove the last line that contains the error data
    events <-tail(data, 2) 
    
    #from the events we extract the time the trial took
    endTime <- as.numeric(events[events$Lable=="End","Time"])
    
    
    #now because the last line has 10 entries and not 8 as the header suggests we have to read the last line again
    s<-nrow(data)
    lastline <- read.table(file,header=FALSE,sep=",",skip=s,col.names=c("V1","TruePositives","V2","TrueNegatives","V3","FalseNegatives","V4","FalsePositives"))
    
    #now generate some new data frames from the extracted data
    participantId <- data[2,2]
    techniqueId <- data[2,4]
    datasetId <- data[2,3]
    repetitionId <- data[2,5]
    
    #one for holding all the timing information
    summaryTime <- data.frame(participantId,techniqueId,datasetId,repetitionId,c(endTime))
    colnames(summaryTime) <- c("ParticipantID","TechniqueID","DatasetID","RepetitionID","Time")
    
    #one for the error data
    div <- 1000 #the division factor
    fp <- lastline$FalsePositives / div
    tp <- lastline$TruePositives / div
    fn <- lastline$FalseNegatives / div
    tn <- lastline$TrueNegatives / div
    error <- data.frame(participantId,techniqueId,datasetId,repetitionId,tp,tn,fp,fn)
    colnames(error) <- c("ParticipantID","TechniqueID","DatasetID","RepetitionID","TP","TN","FP","FN")
    
      
    
    #add the data from this logfile to the global tables
    fullTimeData <- rbind(fullTimeData,events)
    fullErrorData <- rbind(fullErrorData,error)
    summaryTimeData <- rbind(summaryTimeData,summaryTime)

  }

  print("done reading logfiles. Now combining and preparing the data")
  
  #some data massaging here

  #1: add a column in seconds for plotting purposes
  summaryTimeData$TimeInS = summaryTimeData$Time 

  #2: logtransform time before averaging
  summaryTimeData$LogTime=log(summaryTimeData$TimeInS)

  #3: make the ids a factor
  summaryTimeData$TechniqueID <- factor(summaryTimeData$TechniqueID)
  fullErrorData$TechniqueID <- factor(fullErrorData$TechniqueID)
  fullTimeData$TechniqueID <- factor(fullTimeData$TechniqueID)
  summaryTimeData$RepetitionID <- factor(summaryTimeData$RepetitionID)



  ###################################################


  createErrorStats <- function(errorDataSubset,filenamePrefix){

      ############## Calculate error stats ########################
      errorDataSubset$Precision =   errorDataSubset$TP / (errorDataSubset$TP + errorDataSubset$FP)
      errorDataSubset$Recall =      errorDataSubset$TP / (errorDataSubset$TP + errorDataSubset$FN)
      errorDataSubset$F1 = 2 * (errorDataSubset$Precision * errorDataSubset$Recall) / (errorDataSubset$Precision + errorDataSubset$Recall)
      errorDataSubset$MCC = ((errorDataSubset$TP * errorDataSubset$TN) - (errorDataSubset$FP * errorDataSubset$FN)) /
                            sqrt((errorDataSubset$TP + errorDataSubset$FP)*(errorDataSubset$TP + errorDataSubset$FN)*(errorDataSubset$TN+errorDataSubset$FP)*(errorDataSubset$TN+errorDataSubset$FN))
    
      #replace NAs with 0s
      e <- errorDataSubset
      e[is.na(e <- errorDataSubset)] <- 0
    
      errorMelt <- melt(e,id=c("ParticipantID","TechniqueID","DatasetID","RepetitionID"),measure.vars=c("F1","MCC"))
      errorPerParticipant <- as.data.frame(acast(errorMelt,ParticipantID ~ TechniqueID ~ variable,mean))
      #Selection technique: 0 MeTaPoint, 1 MeTaBrush, 2 MeTaPaint, 3 BaseLine
      colnames(errorPerParticipant) <- c("T0_F1","T1_F1","T2_F1","T3_F1","T0_MCC","T1_MCC","T2_MCC","T3_MCC")
    
      error_F1_mean0 <- bootstrapMeanCI(errorPerParticipant$T0_F1)
      cat("The mean F1 error rate for technique 0 is ", formatCI(error_F1_mean0, ""), ", ", sep = "")
      cat("\n")
      error_F1_mean1 <- bootstrapMeanCI(errorPerParticipant$T1_F1)
      cat("The mean F1 error rate for technique 1 is ", formatCI(error_F1_mean1, ""), ", ", sep = "")
      cat("\n")
      error_F1_mean2 <- bootstrapMeanCI(errorPerParticipant$T2_F1)
      cat("The mean F1 error rate for technique 2 is ", formatCI(error_F1_mean2, ""), ", ", sep = "")
      cat("\n")
      error_F1_mean3 <- bootstrapMeanCI(errorPerParticipant$T3_F1)
      cat("The mean F1 error rate for technique 3 is ", formatCI(error_F1_mean3, ""), ", ", sep = "")
      cat("\n")

      
      F1resultTable <- data.frame(error_F1_mean0,error_F1_mean1,error_F1_mean2,error_F1_mean3)
      colnames(F1resultTable) <- c("MeTaPoint","MeTaBrush","MeTaPaint","BaseLine")
      row.names(F1resultTable) <- c("mean_F1","lowerBound_CI","upperBound_CI")
    
      cat("F1 Table\n")
      print(F1resultTable)
      
      write.table(F1resultTable, paste(filenamePrefix, "Means_F1.csv", sep=""), sep=",")
    
      error_MCC_mean0 <- bootstrapMeanCI(errorPerParticipant$T0_MCC)
      cat("The mean MCC error rate for technique 0 is ", formatCI(error_MCC_mean0, ""), ", ", sep = "")
      cat("\n")
      error_MCC_mean1 <- bootstrapMeanCI(errorPerParticipant$T1_MCC)
      cat("The mean MCC error rate for technique 1 is ", formatCI(error_MCC_mean1, ""), ", ", sep = "")
      cat("\n")
      error_MCC_mean2 <- bootstrapMeanCI(errorPerParticipant$T2_MCC)
      cat("The mean MCC error rate for technique 2 is ", formatCI(error_MCC_mean2, ""), ", ", sep = "")
      cat("\n")
      error_MCC_mean3 <- bootstrapMeanCI(errorPerParticipant$T3_MCC)
      cat("The mean MCC error rate for technique 3 is ", formatCI(error_MCC_mean3, ""), ", ", sep = "")
      cat("\n")

      
      MCCresultTable <- data.frame(error_MCC_mean0,error_MCC_mean1,error_MCC_mean2,error_MCC_mean3)
        
      colnames(MCCresultTable) <-c("MeTaPoint","MeTaBrush","MeTaPaint","BaseLine")
      row.names(MCCresultTable) <- c("mean_MCC","lowerBound_CI","upperBound_CI")
    
      cat("-------------------------------------\n")
      cat("MCC Table\n")
      print(MCCresultTable)
      
      write.table(MCCresultTable, paste(filenamePrefix, "Means_MCC.csv", sep=""), sep=",")
      

      pdf(file=paste(filenamePrefix, "F1Distribution.pdf", sep=""))
        F1Distribution(errorDataSubset)
      dev.off()

      pdf(file=paste(filenamePrefix, "MCCDistribution.pdf", sep=""))
        MCCDistribution(errorDataSubset)
      dev.off()
      
      pdf(file=paste(filenamePrefix, "barChartF1.pdf", sep=""), width=8, height=2)
        barChartF1(F1resultTable)
      dev.off()
      
      pdf(file=paste(filenamePrefix, "barChartMCC.pdf", sep=""), width=8, height=2)
        barChartMCC(MCCresultTable)
      dev.off()

  }

  ############## Calculate time stats #########################
  
  createTimeStats <- function(summaryTimeDataSubset,filenamePrefix){

    
      timeMelt <- melt(summaryTimeDataSubset,id=c("ParticipantID","TechniqueID","DatasetID","RepetitionID"),measure.vars=c("LogTime"))
      participantPerTechnique <- as.data.frame(acast(timeMelt,ParticipantID ~ TechniqueID ~ variable,mean))
      colnames(participantPerTechnique) <- c("T0","T1","T2","T3")
    
      #now on to the confidence intervals
      mean0 <- exp(exactMeanCI(participantPerTechnique$T0))
      cat("The mean task completion time for technique 0 is ", formatCI(mean0, "s"), ". ", sep = "")
      cat("\n")
      mean1 <- exp(exactMeanCI(participantPerTechnique$T1))
      cat("The mean task completion time for technique 1 is ", formatCI(mean1, "s"), ". ", sep = "")
      cat("\n")
      mean2 <- exp(exactMeanCI(participantPerTechnique$T2))
      cat("The mean task completion time for technique 2 is ", formatCI(mean2, "s"), ". ", sep = "")
      cat("\n")
      mean3 <- exp(exactMeanCI(participantPerTechnique$T3))
      cat("The mean task completion time for technique 3 is ", formatCI(mean3, "s"), ". ", sep = "")
      cat("\n")

      
      resultTable <- data.frame(mean0,mean1,mean2,mean3)
      colnames(resultTable) <- c("MeTaPoint","MeTaBrush","MeTaPaint","BaseLine")
      row.names(resultTable) <- c("mean_time","lowerBound_CI","upperBound_CI")
    
      cat("Time Table\n")
      print(resultTable)
      
      write.table(resultTable, paste(filenamePrefix, "Means_time.csv", sep=""), sep=",")
      barChartTime(resultTable)
      
      
      pdf(file=paste(filenamePrefix, "boxplotTime.pdf", sep=""))
        boxplotTime(summaryTimeDataSubset)
      dev.off()
      
      pdf(file=paste(filenamePrefix, "logTimeDistribution.pdf", sep=""))
        logTimeDistribution(summaryTimeDataSubset)
      dev.off()
      
      pdf(file=paste(filenamePrefix, "boxplotTimePerDataset.pdf", sep=""))
        boxplotTimePerDataset(summaryTimeDataSubset)
      dev.off()
      
      
      pdf(file=paste(filenamePrefix, "barChartTime.pdf", sep=""), width=8, height=2)
        barChartTime(resultTable)
      dev.off()
      
      cat("Calculating differences\n")
      #now plot the differences:

      v1 <- participantPerTechnique$T0 - participantPerTechnique$T2
      v2 <- participantPerTechnique$T1 - participantPerTechnique$T0
      v3 <- participantPerTechnique$T1 - participantPerTechnique$T2
      v100 <- participantPerTechnique$T3 - participantPerTechnique$T1
      
      mean8 <- exp(exactMeanCI(v1))
      mean9 <- exp(exactMeanCI(v2))
      mean10 <- exp(exactMeanCI(v3))
      mean100 <- exp(exactMeanCI(v100))
      
      resultTableDifferences <- data.frame(mean8,mean9,mean10,mean100)  
      colnames(resultTableDifferences) <- c("MeTaPoint/MeTaPaint","MeTaBrush/MeTaPoint","MeTaBrush/MeTaPaint","Baseline/MeTaBrush")
      row.names(resultTableDifferences) <- c("mean_time","lowerBound_CI","upperBound_CI")
      
      
      
      cat("Time Table Differences\n")
      print(resultTableDifferences)   
      
      pdf(file=paste(filenamePrefix, "barChartTimeDatasetsDifference.pdf", sep=""), width=8, height=2)
        barChartTimeDifference(resultTableDifferences)
      dev.off()

      write.table(resultTableDifferences, paste(filenamePrefix, "Ratios_time.csv", sep=""), sep=",")
      
      
      #  ("MeTaPoint","MeTaBrush","MeTaPaint","BaseLine")
       v1 <- participantPerTechnique$T0 - participantPerTechnique$T1 #MeTaPoint - MeTaBrush
       v2 <- participantPerTechnique$T0 - participantPerTechnique$T2 #MeTaPoint - MeTaPaint
       v3 <- participantPerTechnique$T0 - participantPerTechnique$T3 #MeTaPoint - BaseLine
       v4 <- participantPerTechnique$T1 - participantPerTechnique$T2 #MeTaBrush - MeTaPaint
       v5 <- participantPerTechnique$T1 - participantPerTechnique$T3 #MeTaBrush - BaseLine
       v6 <- participantPerTechnique$T2 - participantPerTechnique$T3 #MeTaPaint - BaseLine
      
       mean11 <- exp(exactMeanCI(v1))
       mean12 <- exp(exactMeanCI(v2))
       mean13 <- exp(exactMeanCI(v3))
       mean14 <- exp(exactMeanCI(v4))
       mean15 <- exp(exactMeanCI(v5))
       mean16 <- exp(exactMeanCI(v6))
      
       resultTableDifferences <- data.frame(mean11,mean12,mean13,mean14,mean15,mean16)
       colnames(resultTableDifferences) <- c("MeTaPoint/MeTaBrush","MeTaPoint/MeTaPaint","MeTaPoint/BaseLine","MeTaBrush/MeTaPaint","MeTaBrush/BaseLine","MeTaPaint/BaseLine")
       row.names(resultTableDifferences) <- c("mean_time","lowerBound_CI","upperBound_CI")

       pdf(file=paste(filenamePrefix, "barChartTimeDatasetsDifference2.pdf", sep=""), width=8, height=3)
       barChartTimeDifference2(resultTableDifferences)
       dev.off()

      write.table(resultTableDifferences, paste(filenamePrefix, "Ratios_time2.csv", sep=""), sep=",")

  }



##############PLOTTING CODE BELOW



require(grid)

barChartMCC <- function(MCCresultTable){
  tr <- t(MCCresultTable)
  tr <- as.data.frame(tr)
  
  
  #now need to calculate one number for the width of the interval
  tr$CI2 <- tr$upperBound_CI - tr$mean_MCC
  tr$CI1 <- tr$mean_MCC - tr$lowerBound_CI
  
  #add a technique column
  tr$technique <- factor(c(0,1,2,3))
  
  
  
   g <- ggplot(tr, aes(x=technique, y=mean_MCC)) +
    geom_bar(stat="identity",fill = I("#CCCCCC")) +
    geom_errorbar(aes(ymin=mean_MCC-CI1, ymax=mean_MCC+CI2),
                  width=0,                    # Width of the error bars
                  size = 1.1
    ) +
    
    labs(x = "", y = "MCC score") +
    scale_x_discrete(name="",breaks=c("0","1","2","3"),labels=c("MeTaPoint","MeTaBrush","MeTaPaint","BaseLine")) +
    coord_flip() +
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),axis.title=element_text(size = rel(1.2), colour = "black"),axis.text=element_text(size = rel(1.2), colour = "black"),panel.grid.major = element_line(colour = "#DDDDDD"),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+
    geom_point(size=4, colour="black")         # dots

  print(g)
}

barChartF1 <- function(F1resultTable){
  tr <- t(F1resultTable)
  tr <- as.data.frame(tr)
  
  
  #now need to calculate one number for the width of the interval
  tr$CI2 <- tr$upperBound_CI - tr$mean_F1
  tr$CI1 <- tr$mean_F1 - tr$lowerBound_CI
  
  #add a technique column
  tr$technique <- factor(c(0,1,2,3))
  
  
  g <- ggplot(tr, aes(x=technique, y=mean_F1)) + 
    geom_bar(stat="identity",fill = I("#CCCCCC")) +
    geom_errorbar(aes(ymin=mean_F1-CI1, ymax=mean_F1+CI2),
                  width=0,                    # Width of the error bars
                  size = 1.1
    ) +

    labs(x = "", y = "F1 score") + 
    scale_x_discrete(name="",breaks=c("0","1","2","3"),labels=c("MeTaPoint","MeTaBrush","MeTaPaint","BaseLine")) +
    coord_flip() + 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),axis.title=element_text(size = rel(1.2), colour = "black"),axis.text=element_text(size = rel(1.2), colour = "black"),panel.grid.major = element_line(colour = "#DDDDDD"),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+
    geom_point(size=4, colour="black")         # dots
  
  print(g)
}


barChartTimeDifference <- function(resultTable){
  print("Creating difference time table")
  tr <- t(resultTable)
  tr <- as.data.frame(tr)
  
  
  #now need to calculate one number for the width of the interval
  tr$CI2 <- tr$upperBound_CI - tr$mean_time
  tr$CI1 <- tr$mean_time - tr$lowerBound_CI
  
  #add a technique column
  tr$technique <- factor(c(0,1,2,3))
  
  
  g <- ggplot(tr, aes(x=technique, y=mean_time)) + 
       #geom_bar(stat="identity",fill = I("#CCCCCC")) +
       geom_errorbar(aes(ymin=mean_time-CI1, ymax=mean_time+CI2),
                                           width=0,                    # Width of the error bars
                                           size = 1.1
                             ) +
     
         labs(x = "", y = "Ratio between completion times",title="no effect") + 
         scale_x_discrete(name="",breaks=c("0","1","2","3"),labels=c("MeTaPoint/MeTaPaint","MeTaPoint/MeTaBrush","MeTaPaint/MeTaBrush","MeTaBrush/Baseline")) +
         scale_y_continuous(limits = c(0.5,3)) +
         coord_flip() +
         theme(plot.title=element_text(hjust=.245),panel.background = element_rect(fill = 'white', colour = 'white'),axis.title=element_text(size = rel(1.2), colour = "black"),axis.text=element_text(size = rel(1.2), colour = "black"),panel.grid.major = element_line(colour = "#DDDDDD"),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+
         geom_point(size=4, colour="black") +        # dots
         geom_hline(yintercept = 1)
  
  print(g)
}

barChartTimeDifference2 <- function(resultTable){
  print("Creating difference time table")
  tr <- t(resultTable)
  tr <- as.data.frame(tr)
  
  
  #now need to calculate one number for the width of the interval
  tr$CI2 <- tr$upperBound_CI - tr$mean_time
  tr$CI1 <- tr$mean_time - tr$lowerBound_CI
  
  #add a technique column
  tr$technique <- factor(c(0,1,2,3,4,5))
  
  
  g <- ggplot(tr, aes(x=technique, y=mean_time)) + 
    #geom_bar(stat="identity",fill = I("#CCCCCC")) +
    geom_errorbar(aes(ymin=mean_time-CI1, ymax=mean_time+CI2),
                  width=0,                    # Width of the error bars
                  size = 1.1
    ) +
 
    labs(x = "", y = "Ratio between completion times",title="no effect") + 
    scale_x_discrete(name="",breaks=c("0","1","2","3","4","5"),labels=c("MeTaPoint/MeTaBrush","MeTaPoint/MeTaPaint","MeTaPoint/BaseLine","MeTaBrush/MeTaPaint","MeTaBrush/BaseLine","MeTaPaint/BaseLine")) +
    scale_y_continuous(limits = c(1,10)) +
    coord_flip() +
    theme(plot.title=element_text(hjust=.5),panel.background = element_rect(fill = 'white', colour = 'white'),axis.title=element_text(size = rel(1.2), colour = "black"),axis.text=element_text(size = rel(1.2), colour = "black"),panel.grid.major = element_line(colour = "#DDDDDD"),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+
    geom_point(size=4, colour="black") +        # dots
    geom_hline(yintercept = 1)
  
  print(g)
}
barChartTime <- function(resultTable){
  tr <- t(resultTable)
  tr <- as.data.frame(tr)
  
  
  #now need to calculate one number for the width of the interval
  tr$CI2 <- tr$upperBound_CI - tr$mean_time
  tr$CI1 <- tr$mean_time - tr$lowerBound_CI
  
  #add a technique column
  tr$technique <- factor(c(0,1,2,3))
  
  
  g <- ggplot(tr, aes(x=technique, y=mean_time)) + 
    geom_bar(stat="identity",fill = I("#CCCCCC")) +
    geom_errorbar(aes(ymin=mean_time-CI1, ymax=mean_time+CI2),
                  width=0,                    # Width of the error bars
                  size = 1.1
    ) +
 
    labs(x = "", y = "Completion time (in seconds)") + 
    scale_y_continuous(limits = c(0,60)) +
    scale_x_discrete(name="",breaks=c("0","1","2","3"),labels=c("MeTaPoint","MeTaBrush","MeTaPaint","BaseLine")) +
    coord_flip() +
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),axis.title=element_text(size = rel(1.2), colour = "black"),axis.text=element_text(size = rel(1.2), colour = "black"),panel.grid.major = element_line(colour = "#DDDDDD"),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+
    geom_point(size=4, colour="black")         # dots
  
  print(g)
}

barChartTimeDatasets <- function(resultTable){
  tr <- t(resultTable)
  tr <- as.data.frame(tr)
  
  
  #now need to calculate one number for the width of the interval
  tr$CI2 <- tr$upperBound_CI - tr$mean_time
  tr$CI1 <- tr$mean_time - tr$lowerBound_CI
  
  #add a technique column
  tr$technique <- factor(c(4,5,6,7))
  
  
  g <- ggplot(tr, aes(x=technique, y=mean_time)) + 
    geom_bar(stat="identity",fill = I("#CCCCCC")) +
    geom_errorbar(aes(ymin=mean_time-CI1, ymax=mean_time+CI2),
                  width=0,                    # Width of the error bars
                  size = 1.1
    ) +
  
    labs(x = "", y = "Completion time (in seconds)") + 
    scale_x_discrete(name="",breaks=c("4","5","6","7"),labels=c("Clusters","Shell","Rings","Simulation")) +
    coord_flip() + 
    theme(panel.background = element_rect(fill = 'white', colour = 'white'),axis.title=element_text(size = rel(1.2), colour = "black"),axis.text=element_text(size = rel(1.2), colour = "black"),panel.grid.major = element_line(colour = "#DDDDDD"),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+
    geom_point(size=4, colour="black")         # dots
  
  print(g)
}



boxplotTime <- function(summaryTimeDataSubset){
  
  g <- ggplot(summaryTimeDataSubset,aes(x=as.factor(TechniqueID),y=TimeInS,fill=as.factor(TechniqueID)))+
         geom_boxplot() +
         # labs(title="Overall time per technique") +
         labs(x = "Technique", y = "Time in s") +
         scale_x_discrete(name="",breaks=c("0","1","2","3"),labels=c("MeTaPoint","MeTaBrush","MeTaPaint","BaseLine"))
  print(g)
}

boxplotTimePerDataset <- function(summaryTimeDataSubset){
  g <- ggplot(summaryTimeDataSubset,aes(x=as.factor(TechniqueID),y=TimeInS,fill=as.factor(TechniqueID)))+
         geom_boxplot() +
         # labs(title="Overall time per technique") +
         labs(x = "Technique", y = "Time in s") +
         scale_fill_discrete(name="Technique",breaks=c("0","1","2","3"),labels=c("MeTaPoint","MeTaBrush","MeTaPaint","BaseLine"))+
         scale_x_discrete(name="",breaks=c("0","1","2","3"),labels=c("MeTaPoint","MeTaBrush","MeTaPaint","BaseLine")) +
         facet_grid(DatasetID~.)
  
  print(g)
}


logTimeDistribution <- function(summaryTimeDataSubset){
  g <- qplot(LogTime,data=summaryTimeDataSubset,facets=.~TechniqueID)
  print(g)
}

F1Distribution <- function(errorDataSubset){
  g <- qplot(F1,data=errorDataSubset,facets=.~TechniqueID)
  print(g)
}

MCCDistribution <- function(errorDataSubset){
  g <- qplot(MCC,data=errorDataSubset,facets=.~TechniqueID)
  print(g)
}


##############take just a subset of repetitions
#full data for 0123dataset, repetitions 2 and 3

cat("****************************************************\n")
cat("Preparing time data for 0123 datasets\n")
summaryTimeDataSubset <- summaryTimeData[ which(as.numeric(summaryTimeData$RepetitionID) > 0 &as.numeric(summaryTimeData$DatasetID)!=4), ]
createTimeStats(summaryTimeDataSubset,"resultFiles/log/time_0123Datasets_rep23")
cat("Preparing error data for 0123 datasets\n")
errorDataSubset <- fullErrorData[ which(as.numeric(fullErrorData$RepetitionID) > 0&as.numeric(summaryTimeData$DatasetID)!=4), ]
createErrorStats(errorDataSubset,"resultFiles/log/error_0123Datasets_rep23_")

cat("****************************************************\n")
#dataset0 only, rep 2 and 3
cat("Preparing time data for dataset 0\n")
summaryTimeDataSubset = subset(summaryTimeData, as.numeric(RepetitionID) > 0 & DatasetID == "0")
createTimeStats(summaryTimeDataSubset,"resultFiles/log/time_Dataset0_rep23")

cat("Preparing error data for dataset 0\n")
errorDataSubset <- subset(fullErrorData, as.numeric(fullErrorData$RepetitionID) >0 & DatasetID == "0")
createErrorStats(errorDataSubset,"resultFiles/log/error_Dataset0_rep23_")

cat("****************************************************\n")
#dataset1 only, rep 2 and 3
cat("Preparing time data for dataset 1\n")
summaryTimeDataSubset <- subset(summaryTimeData, as.numeric(RepetitionID) > 0 & DatasetID == "1")
createTimeStats(summaryTimeDataSubset,"resultFiles/log/time_Dataset1_rep23")
cat("Preparing error data for dataset 1\n")
errorDataSubset <- subset(fullErrorData, as.numeric(fullErrorData$RepetitionID) > 0 & DatasetID == "1")
createErrorStats(errorDataSubset,"resultFiles/log/error_Dataset1_rep23_")

cat("****************************************************\n")
#dataset2 only rep 2 and 3
cat("Preparing time data for dataset 2\n")
summaryTimeDataSubset <- subset(summaryTimeData, as.numeric(RepetitionID) > 0 & DatasetID == "2")
createTimeStats(summaryTimeDataSubset,"resultFiles/log/time_Dataset2_rep23")
cat("Preparing error data for dataset 2\n")
errorDataSubset <- subset(fullErrorData, as.numeric(fullErrorData$RepetitionID) > 0 & DatasetID == "2")
createErrorStats(errorDataSubset,"resultFiles/log/error_Dataset2_rep23_")

cat("****************************************************\n")
#dataset3 only rep 2 and 3
cat("Preparing time data for dataset 3\n")
summaryTimeDataSubset <- subset(summaryTimeData, as.numeric(RepetitionID) > 0 & DatasetID == "3")
createTimeStats(summaryTimeDataSubset,"resultFiles/log/time_Dataset3_rep23")
cat("Preparing error data for dataset 3\n")
errorDataSubset <- subset(fullErrorData, as.numeric(fullErrorData$RepetitionID) > 0 & DatasetID == "3")
createErrorStats(errorDataSubset,"resultFiles/log/error_Dataset3_rep23_")

cat("****************************************************\n")
#dataset4 only rep 2 and 3
cat("Preparing time data for dataset 4\n")
summaryTimeDataSubset <- subset(summaryTimeData, as.numeric(RepetitionID) > 0 & DatasetID == "4")
createTimeStats(summaryTimeDataSubset,"resultFiles/log/time_Dataset4_rep23")
cat("Preparing error data for dataset 4\n")
errorDataSubset <- subset(fullErrorData, as.numeric(fullErrorData$RepetitionID) > 0 & DatasetID == "4")
createErrorStats(errorDataSubset,"resultFiles/log/error_Dataset4_rep23_")

# a = subset(summaryTimeData, as.numeric(RepetitionID) > 0 & DatasetID == "0" & TechniqueID=="0")
# ggdensity(a$Time,
#           main = "Density plot of sepal length",
#           xlab = "Time")
# 
# ggqqplot(a$Time)