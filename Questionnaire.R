library(ggplot2)
library(reshape2)
library(glue)
library(dplyr)


#'Generate the 95% CI generated via bootstrapping
#
#'@param data the data to use during bootstrapping
#'@param statistic the statistical function to use
#'
#'@return an array containing (defaultStats, lowerBound, upperBound)
bootstrapCI_q = function(data, statistic)
{
  b   = boot(data, statistic=statistic, R=5000)
  cis = boot.ci(b, conf=0.95, type="bca")
  
  return(c(statistic(data), cis$bca[4], cis$bca[5]))
}

#'Generate the mean value of a list using the log anti log method (geometrical mean)
#'
#'@param x the original dataset
#'@param d the indice or list of indices to subsample 'x'
#'
#'@return exp(mean(log(x[d])))
geomMeanFunc = function(x, d)
{
  return(exp(mean(log(x[d]))))
}
#'Plot a list of bootstrap data
#'
#'@param d the list of data.frame object with each containing: a column mean, lower, and upper. A name column permits to name the bar chart (each name is a metric). An id column allows to gather objects with different ids under the same metric. 
#'         You can change the column names with the corresponding label
#'@param maxAxis the maximum axis value
#'@param isBarChart is the graph a bar chart? (default: FALSE)
#'
#'@return the plot object
#'
#'
#'
#'

plotStackedBarchart = function(d, maxAxis=NA, minAxis=NA, legendName="",
                               xLabel=value, yLabel=name, fillLabel=id, facetLabel=grid)
{
  xLabel     = deparse(substitute(xLabel))
  yLabel     = deparse(substitute(yLabel))
  fillLabel  = deparse(substitute(fillLabel))
  facetLabel = deparse(substitute(facetLabel))
  
  d[[fillLabel]] = factor(d[[fillLabel]])
  
  g = ggplot(d, aes(x = get(xLabel), y = get(yLabel), fill = factor(get(fillLabel), levels=unique(.data[[fillLabel]]), ordered=TRUE))) + 
    geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ factor(get(facetLabel), levels=unique(.data[[facetLabel]]), ordered=TRUE))
  
  if(is.na(maxAxis) && is.na(minAxis))
    g = g + expand_limits(y = 0)
  else
  {
    if(is.na(maxAxis))
      maxAxis = 0
    if(is.na(minAxis))
      minAxis = 0
    g = g + expand_limits(y = c(minAxis, maxAxis))
  }
  
  g = g + labs(x = "", y = "", col=legendName, fill=legendName) + 
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_brewer(palette="Paired") +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
          plot.margin=grid::unit(c(0,0,0,0), "mm"),
          axis.title = element_text(size = rel(1), colour = "black"),
          axis.text  = element_text(size = rel(1), colour = "black"),
          panel.grid.major   = element_line(colour = "#DDDDDD"),
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(),
          aspect.ratio = 3/4,
          legend.title = element_text(size=12, face="bold"), 
          legend.text  = element_text(size=10))
  
  return(g)
}


plotListBootstrap = function(d, maxAxis=NA, minAxis=NA, isBarChart=FALSE, legendName="",
                             meanLabel=mean, lowerLabel=lower, upperLabel=upper,
                             nameLabel=name, idLabel=id)
{
  lowerLabel = deparse(substitute(lowerLabel))
  upperLabel = deparse(substitute(upperLabel))
  meanLabel  = deparse(substitute(meanLabel))
  
  tr = data.frame(d)
  tr = tr %>% group_by(!!ensym(idLabel)) %>% mutate(id = seq.int(1, nrow(cur_data()), 1))
  #    tr = tr[with(tr,order(-id)),] ## Sorting
  tr$id = factor(tr$id, ordered=TRUE)
  g = ggplot(tr, aes(fill=factor(.data[[substitute(idLabel)]], levels=unique(.data[[substitute(idLabel)]]), ordered=TRUE), x=id, y=get(meanLabel), colour=.data[[substitute(idLabel)]]))
  
  if(isBarChart)
    g = g + geom_bar(width=0.5, stat="identity", position=position_dodge())
  if(is.na(maxAxis) && is.na(minAxis))
    g = g + expand_limits(y = 0)
  else
  {
    if(is.na(maxAxis))
      maxAxis = 0
    if(is.na(minAxis))
      minAxis = 0
    g = g + expand_limits(y = c(0, 6))
  }
  
  g = g + 
    scale_x_discrete(name="", labels=unique(tr[[substitute(nameLabel)]]), expand=c(0, 0)) +
    scale_y_continuous(name="") + 
    geom_errorbar(aes(ymin=get(lowerLabel), ymax=get(upperLabel)), width = 0, size = 0.5, position=position_dodge(width=0.5), stat="identity") +
    labs(x = "", y = "", col=legendName, fill=legendName) + 
    coord_flip() +
    scale_color_brewer(palette="Paired") +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
          plot.margin=grid::unit(c(0,0,0,0), "mm"),
          axis.title = element_text(size = rel(1), colour = "black"),
          axis.text  = element_text(size = rel(1), colour = "black"),
          panel.grid.major   = element_line(colour = "#DDDDDD"),
          panel.grid.major.y = element_blank(), 
      
         legend.title = element_text(size=13, face="bold"), 
          legend.text  = element_text(size=10),
          aspect.ratio = nrow(tr)/50) +
    geom_point(size=1.5, position=position_dodge(width=0.5))         # dots
  
  return(g)
}
meanFunc = function(x, d)
{
  return(mean(x[d]))
}

# Parse the Questionnaire
parseQuestionnaire = function()
{
  outputDir = "resultFiles/Questionnaire"
  dir       = "logfiles/Questionnaire"
  data      = NULL
  
  #Read the csv files and concatenate them.
  #They should have the same header
  for(fileName in list.files(path=dir, pattern="*.csv"))
  {
    glue("Parsing {fileName}...")
    #Parse
    csvData = read.csv(glue("{dir}/{fileName}"), sep=',', header=TRUE)
    
    #Concatenate
    if(is.null(data))
      data = csvData
    else if(names(data) == names(csvData)) #This should be true
      data = rbind(data, csvData)
    else #If csv files are not consistent: exit the script
    {
      print("Error: csv files are not consistent. Exiting...")
      stop()
    }
  }
  
  print("Bootstraping TLX data for task 1-4")
  tlxBootstrap1   = NULL
  techIDNames    = c("Point", "Brush", "Paint","Baseline") #ID as stored in the CSV
  techLabelNames = c("Po", "Br", "Pa","Ba") #Label to use for rendering
 
  for(i in 1:length(techIDNames))
  {
    techID    = techIDNames[i]
    techLabel = techLabelNames[i]
   
    metrics   = c("Performance_task1to4", "Mental_task1to4", "Physical_task1to4", "Temporal_task1to4", "Frustration_task1to4", "Effort_task1to4")
    ylablenames= c("Performance", "Mental", "Physical", "Temporal", "Frustration", "Effort")
     varNames  = paste(techID, metrics, sep='') 
    
    for(j in 1:length(metrics))
    {
      var    = varNames[j]
      metric = metrics[j]
      ylablename=ylablenames[j]
      #Perform the bootstrap
      print(glue("bootstraping var {var}"))
      bsData           = data.frame(t(c(bootstrapCI_q(data[[var]], meanFunc), var, ylablename, techLabel)))
      print(bsData) 
      bsData[1:3]      = as.double(bsData[1:3])
      rownames(bsData) = c(var)
      if(is.null(tlxBootstrap1))
        tlxBootstrap1 = bsData
      else
        tlxBootstrap1 = rbind(tlxBootstrap1, bsData)
    }
  }
  colnames(tlxBootstrap1) = c("mean", "lower", "upper", "varName", "metric", "tech")
  
  dataNames  = c("1", "2", "3", "4", "5")
  dataLabels = c("Disk", "Rings", "Shell","Strings","Filaments")
  
  
  print("Bootstraping TLX data for task 5")
  tlxBootstrap2   = NULL
  techIDNames    = c("Point", "Brush", "Paint","Baseline") #ID as stored in the CSV
  techLabelNames = c("Po", "Br", "Pa","Ba") #Label to use for rendering
  
  for(i in 1:length(techIDNames))
  {
    techID    = techIDNames[i]
    techLabel = techLabelNames[i]
    metrics   = c("Performance_task5", "Mental_task5", "Physical_task5", "Temporal_task5", "Frustration_task5", "Effort_task5")
    ylablenames= c("Performance", "Mental", "Physical", "Temporal", "Frustration", "Effort")
    varNames  = paste(techID, metrics, sep='') 
    
    for(j in 1:length(metrics))
    {
      var    = varNames[j]
      metric = metrics[j]
      ylablename=ylablenames[j]
      #Perform the bootstrap
      print(glue("bootstraping var {var}"))
      bsData           = data.frame(t(c(bootstrapCI_q(data[[var]], meanFunc), var, ylablename, techLabel)))
      print(bsData) 
      bsData[1:3]      = as.double(bsData[1:3])
      rownames(bsData) = c(var)
      if(is.null(tlxBootstrap2))
        tlxBootstrap2 = bsData
      else
        tlxBootstrap2 = rbind(tlxBootstrap2, bsData)
    }
  }
  colnames(tlxBootstrap2) = c("mean", "lower", "upper", "varName", "metric", "tech")
  
  dataNames  = c("1", "2", "3", "4", "5")
  dataLabels = c("Disk", "Rings", "Shell","Strings","Filaments")
  
  
  
  print("Bootstraping Paired-Wise TLX data for task1 to 4")
  pwTLXBootstrap1 = NULL
  techIDPairedNames = data.frame(c("Brush", "Point"),  #Name as stored in the CSV
                                 c("Point", "Paint"), 
                                 c("Point", "Baseline"), 
                                 c("Brush", "Paint"),
                                 c("Brush", "Baseline"),
                                 c("Paint", "Baseline"))
  
  techLabelPairedNames = data.frame(c("Br", "Po"), #Label to use for rendering
                                    c("Po", "Pa"), 
                                    c("Po", "Ba"), 
                                    c("Br", "Pa"),
                                    c("Br", "Ba"),
                                    c("Pa", "Ba"))
  for(i in 1:length(techIDPairedNames))
  {
    idPairedNames    = t(techIDPairedNames[i])
    labelPairedNames = t(techLabelPairedNames[i])
    varNames = c("Performance_task1to4", "Mental_task1to4", "Physical_task1to4", "Temporal_task1to4", "Frustration_task1to4", "Effort_task1to4")
    ylableNames = c("Performance", "Mental", "Physical", "Temporal", "Frustration", "Effort")
    a=1
     for(var in varNames)
    {
      print(glue("Bootstraping var {var}"))
      tech1  = paste(idPairedNames[1], var, sep='')
      tech2  = paste(idPairedNames[2], var, sep='')
      
      d1 = data[[tech1]] + 1 #Discard infinite values
      d2 = data[[tech2]] + 1 #Discard infinite values
      ratio = d1/d2
      ylableName=ylableNames[a]
      bsData = data.frame(t(c(bootstrapCI_q(ratio, geomMeanFunc), ylableName, paste(labelPairedNames[1], labelPairedNames[2], sep=" / "))))
      
      bsData[1:3]      = as.double(bsData[1:3])
      rownames(bsData) = paste(labelPairedNames[1], labelPairedNames[2], ylableName)
      a=a+1
      if(is.null(pwTLXBootstrap1))
        pwTLXBootstrap1 = bsData
      else
        pwTLXBootstrap1 = rbind(pwTLXBootstrap1, bsData)
      
    
    }
  }
  colnames(pwTLXBootstrap1) = c("mean", "lower", "upper", "varName", "tech")
  print(pwTLXBootstrap1)
  
  
  print("Bootstraping Paired-Wise TLX data for task5")
  pwTLXBootstrap2 = NULL
  techIDPairedNames = data.frame(c("Brush", "Point"),  #Name as stored in the CSV
                                 c("Point", "Paint"), 
                                 c("Point", "Baseline"), 
                                 c("Brush", "Paint"),
                                 c("Brush", "Baseline"),
                                 c("Paint", "Baseline"))
  
  techLabelPairedNames = data.frame(c("Br", "Po"), #Label to use for rendering
                                    c("Po", "Pa"), 
                                    c("Po", "Ba"), 
                                    c("Br", "Pa"),
                                    c("Br", "Ba"),
                                    c("Pa", "Ba"))
  for(i in 1:length(techIDPairedNames))
  {
    idPairedNames    = t(techIDPairedNames[i])
    labelPairedNames = t(techLabelPairedNames[i])
    varNames = c("Performance_task5", "Mental_task5", "Physical_task5", "Temporal_task5", "Frustration_task5", "Effort_task5")
    ylableNames = c("Performance", "Mental", "Physical", "Temporal", "Frustration", "Effort")
    a=1
      for(var in varNames)
    {
      print(glue("Bootstraping var {var}"))
      tech1  = paste(idPairedNames[1], var, sep='')
      tech2  = paste(idPairedNames[2], var, sep='')
      ylableName=ylableNames[a]
      d1 = data[[tech1]] + 1 #Discard infinite values
      d2 = data[[tech2]] + 1 #Discard infinite values
      ratio = d1/d2
      bsData = data.frame(t(c(bootstrapCI_q(ratio, geomMeanFunc), ylableName, paste(labelPairedNames[1], labelPairedNames[2], sep=" / "))))
      
      bsData[1:3]      = as.double(bsData[1:3])
      rownames(bsData) = paste(labelPairedNames[1], labelPairedNames[2], ylableName)
      a=a+1
      if(is.null(pwTLXBootstrap2))
        pwTLXBootstrap2 = bsData
      else
        pwTLXBootstrap2 = rbind(pwTLXBootstrap2, bsData)
    }
  }
  colnames(pwTLXBootstrap2) = c("mean", "lower", "upper", "varName", "tech")
  print(pwTLXBootstrap2)
  
  
  
  
  print("Counting the TechniqueRank_dataset results...")
  techIDNames    = c("Point", "Brush", "Paint","BaseLine") #ID as stored in the CSV
  techLabelNames = c("Po", "Br", "Pa","Ba") #Label to use for rendering
  TechniqueRank_dataset = NULL
  
  for(i in 1:length(techIDNames))
  {
    techName  = techIDNames[i]
    techLabel = techLabelNames[i] 
    
    print(glue("Counting for tech {techName}"))
    #Names
    Dataset1ColName = glue("D1{techName}Rank")
    Dataset2ColName = glue("D2{techName}Rank")
    Dataset3ColName = glue("D3{techName}Rank")
    Dataset4ColName = glue("D4{techName}Rank")
    Dataset5ColName = glue("D5{techName}Rank")
    
    #Count
    D1 = t(data.frame(c(sum(data[[Dataset1ColName]] == 1), "1st", "Disk", techLabel),
                      c(sum(data[[Dataset1ColName]] == 2), "2nd", "Disk", techLabel),
                      c(sum(data[[Dataset1ColName]] == 3), "3rd", "Disk", techLabel),
                      c(sum(data[[Dataset1ColName]] == 4), "4th", "Disk", techLabel)))
    
    D2 = t(data.frame(c(sum(data[[Dataset2ColName]] == 1), "1st", "Rings", techLabel),
                      c(sum(data[[Dataset2ColName]] == 2), "2nd", "Rings", techLabel),
                      c(sum(data[[Dataset2ColName]] == 3), "3rd", "Rings", techLabel),
                      c(sum(data[[Dataset2ColName]] == 4), "4th", "Rings", techLabel)))
    
    D3 = t(data.frame(c(sum(data[[Dataset3ColName]] == 1), "1st", "Shell", techLabel),
                      c(sum(data[[Dataset3ColName]] == 2), "2nd", "Shell", techLabel),
                      c(sum(data[[Dataset3ColName]] == 3), "3rd", "Shell", techLabel),
                      c(sum(data[[Dataset3ColName]] == 4), "4th", "Shell", techLabel)))
    
    D4 = t(data.frame(c(sum(data[[Dataset4ColName]] == 1), "1st", "Strings", techLabel),
                      c(sum(data[[Dataset4ColName]] == 2), "2nd", "Strings", techLabel),
                      c(sum(data[[Dataset4ColName]] == 3), "3rd", "Strings", techLabel),
                      c(sum(data[[Dataset4ColName]] == 4), "4th", "Strings", techLabel)))
    
    D5 = t(data.frame(c(sum(data[[Dataset5ColName]] == 1), "1st", "Filaments", techLabel),
                      c(sum(data[[Dataset5ColName]] == 2), "2nd", "Filaments", techLabel),
                      c(sum(data[[Dataset5ColName]] == 3), "3rd", "Filaments", techLabel),
                      c(sum(data[[Dataset5ColName]] == 4), "4th", "Filaments", techLabel)))
    
    subTechniqueRank_dataset = rbind(D1, D2, D3,D4,D5)
    TechniqueRank_dataset = rbind(TechniqueRank_dataset, subTechniqueRank_dataset)
  }
  
  TechniqueRank_dataset = data.frame(TechniqueRank_dataset)
  colnames(TechniqueRank_dataset) = c("value", "TechniqueRank_dataset", "metric", "tech")
  rownames(TechniqueRank_dataset) = NULL
  TechniqueRank_dataset$value = as.numeric(TechniqueRank_dataset$value) / nrow(data)
  print(TechniqueRank_dataset)
  
  
  print("Counting the general ranking results...")
  techIDNames    = c("Point", "Brush", "Paint","BaseLine") #ID as stored in the CSV
  techLabelNames = c("Po", "Br", "Pa","Ba") #Label to use for rendering
  rank = NULL
  
  for(i in 1:length(techIDNames))
  {
    techName  = techIDNames[i]
    techLabel = techLabelNames[i] 
    
    print(glue("Counting for tech {techName}"))
    #Names
    accColName = glue("acc{techName}Rank")
    tctColName = glue("tct{techName}Rank")
    genColName = glue("general{techName}Rank")
    feaColName=glue("featureSelection{techName}Rank")
    
    #Count
    subAcc = t(data.frame(c(sum(data[[accColName]] == 1), "1st", "Accuracy", techLabel),
                          c(sum(data[[accColName]] == 2), "2nd", "Accuracy", techLabel),
                          c(sum(data[[accColName]] == 3), "3rd", "Accuracy", techLabel),
                          c(sum(data[[accColName]] == 4), "4th", "Accuracy", techLabel)))
    
    subTct = t(data.frame(c(sum(data[[tctColName]] == 1), "1st", "Speed", techLabel),
                          c(sum(data[[tctColName]] == 2), "2nd", "Speed", techLabel),
                          c(sum(data[[tctColName]] == 3), "3rd", "Speed", techLabel),
                          c(sum(data[[tctColName]] == 4), "4th", "Speed", techLabel)))
    
    subGen = t(data.frame(c(sum(data[[genColName]] == 1), "1st", "Overall", techLabel),
                          c(sum(data[[genColName]] == 2), "2nd", "Overall", techLabel),
                          c(sum(data[[genColName]] == 3), "3rd", "Overall", techLabel),
                          c(sum(data[[genColName]] == 4), "4th", "Overall", techLabel)))
    
    subFea = t(data.frame(c(sum(data[[feaColName]] == 1), "1st", "Feature Selection", techLabel),
                          c(sum(data[[feaColName]] == 2), "2nd", "Feature Selection", techLabel),
                          c(sum(data[[feaColName]] == 3), "3rd", "Feature Selection", techLabel),
                          c(sum(data[[feaColName]] == 4), "4th", "Feature Selection", techLabel)))
    
    subRank = rbind(subTct,subAcc ,subFea,subGen)
    if(is.null(rank))
      rank = subRank
    else
      rank = rbind(rank, subRank)
  }
  
  rank = data.frame(rank)
  colnames(rank) = c("value", "rank", "metric", "tech")
  rownames(rank) = NULL
  rank$value = as.numeric(rank$value) / nrow(data)
  print(rank)
  

  #------------------------------------------------------------------------------
  #---------------------------Plot questionnaire data----------------------------
  #------------------------------------------------------------------------------
  
  #Overall TLX
  print(glue("Generating {outputDir}/tlx_task1to4.pdf"))
  g = plotListBootstrap(tlxBootstrap1, maxAxis=2, nameLabel=metric, idLabel=tech)
  ggsave(glue("{outputDir}/tlx_task1to4.pdf"), plot=g, device="pdf")
  
  print(glue("Generating {outputDir}/tlx_task5.pdf"))
  g = plotListBootstrap(tlxBootstrap2, maxAxis=2, nameLabel=metric, idLabel=tech)
  ggsave(glue("{outputDir}/tlx_task5.pdf"), plot=g, device="pdf")
  
  
  #PW comparisons
  print(glue("Generating {outputDir}/PWtlx_task1to4.pdf"))
  g = plotListBootstrap(pwTLXBootstrap1, nameLabel=varName, idLabel=tech) + geom_hline(yintercept = 1.0)
  ggsave(glue("{outputDir}/PWtlx_task1to4.pdf"), plot=g, device="pdf")
  
  print(glue("Generating {outputDir}/PWtlx_task5.pdf"))
  g = plotListBootstrap(pwTLXBootstrap2, nameLabel=varName, idLabel=tech) + geom_hline(yintercept = 1.0)
  ggsave(glue("{outputDir}/PWtlx_task5.pdf"), plot=g, device="pdf")
  
  
  # #Counting technique rank by dataset
  print(TechniqueRank_dataset)
  print(glue("Generating {outputDir}/TechniqueRankbydataset.pdf"))
  g = plotStackedBarchart(TechniqueRank_dataset, xLabel=tech, yLabel=value, fillLabel=TechniqueRank_dataset, facetLabel=metric)
  ggsave(glue("{outputDir}/TechniqueRank_dataset.pdf"), plot=g, device="pdf")
  # 
  #Counting Rank
  print(rank)
  print(glue("Generating {outputDir}/TechniqueRankGeneral.pdf"))
  g = plotStackedBarchart(rank, xLabel=tech, yLabel=value, fillLabel=rank, facetLabel=metric)
  ggsave(glue("{outputDir}/TechniqueRankGeneral.pdf"), plot=g, device="pdf")
}


parseQuestionnaire()