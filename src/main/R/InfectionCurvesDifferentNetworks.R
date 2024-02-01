enter_path_here <- "" #Here one needs to enter the path to the folder of the modeling results
setwd(enter_path_here)

files <- list.files(path=enter_path_here, pattern="*infectious-0.csv", full.names=FALSE, recursive=FALSE) #Right now, the curves are produced for a 0-day lag. The pattern needs to be adapted if the user is interested in alternative lags.
files <- files[files != paste0(enter_path_here, "_info.csv")]
pattern <- c("-0.1-", "-0.2-", "-0.3-")
files <- files[grepl(paste(pattern, collapse = "|"), files)]

#Reading in of the modeling results
dataSetFull <- data.frame()

for(i in 1:(length(files))){
  filesReduced <- files[i]
  
  for(file in filesReduced){
    dataSetNew <- read.csv(file)
    dataSetNew$ID <- seq.int(nrow(dataSetNew))
    dataSetNew$Network <- str_split(file, "-")[[1]][[1]]
    dataSetNew$BaseInfProb <- str_split(file, "-")[[1]][[2]]
    dataSetNew$DiseaseState <- str_split(file, "-")[[1]][[3]]
    dataSetNew <- pivot_longer(dataSetNew, cols = seed1:seed100)

    #Removing the runs where the infection dies out directly after the initial
    removed <- dataSetNew %>% group_by(name) %>% summarise(cumsuminfected = (max(value)))
    removed <- removed %>% filter(cumsuminfected == 1)
    test <- removed$name
    dataSetNew <- dataSetNew %>% filter(!name  %in% test)

    if (length(str_split(file, "-")[[1]]) == 5) {
    dataSetNew$lag <- str_split(file, "-")[[1]][[5]]
    } else {
      dataSetNew$lag <- 0
    }
    dataSetFull <- rbind(dataSetFull, dataSetNew)
  }
}
  
#Preparing the data for plotting
  dataSetGrouped <- dataSetFull %>% group_by(Network, BaseInfProb , DiseaseState, ID, lag) %>% summarise(mean = mean(value), sd = sd(value))
  dataSetGrouped <- dataSetGrouped %>% ungroup()
  dataSetGrouped <- dataSetGrouped %>% mutate(ymin = case_when(mean-sd > 0 ~ mean-sd, 
                                                               .default = 0))

#Creation and saving of plots
for(lag in unique(dataSetGrouped$lag)){
for(DisState in unique(dataSetGrouped$DiseaseState)){
for(InfProb in unique(dataSetGrouped$BaseInfProb)){
plot <- ggplot(dataSetGrouped %>% filter(lag == lag) %>% filter(BaseInfProb == InfProb) %>% filter(DiseaseState == DisState), aes(x=ID, y = mean)) +
    geom_line(aes(color = Network)) +
    theme_minimal() +
   # scale_y_log10() +
    xlab("Time step") +
    ylab("Infections") +
    theme(legend.position = "bottom", legend.title = element_blank())
  
ggsave(paste0(lag, "-", DisState, "-", InfProb, "-InfectionsDifferentScenarios.pdf"), plot, dpi = 500, w = 9, h = 3)   
}
}
}
