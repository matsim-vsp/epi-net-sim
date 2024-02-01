library(tidyverse)

enter_path_here <- "" #Here one needs to enter the path to the folder of the modeling results
setwd(enter_path_here)

#Reading of the modeling results
files <- list.files(path=enter_path_here, pattern="*-0.csv", full.names=FALSE, recursive=FALSE) #Right now, the curves are produced for a 0-day lag. The pattern needs to be adapted if the user is interested in alternative lags.
files <- files[files != "_info.csv"]
files <- files[!str_detect(files, "infectionChance")]
pattern <- c("-0.1-", "-0.2-", "-0.3-")
files <- files[grepl(paste(pattern, collapse = "|"), files)]

dataSetFull <- data.frame()

  for(file in files){
    dataSetNew <- read.csv(file)
    dataSetNew$ID <- seq.int(nrow(dataSetNew))
    dataSetNew$Network <- str_split(file, "-")[[1]][[1]]
    dataSetNew$BaseProb <- str_split(file, "-")[[1]][[2]]
    dataSetNew$Scenario <- str_split(file, "-")[[1]][[3]]
    dataSetNew$DiseaseState <- str_split(file, "-")[[1]][[4]]
    #dataSetNew$DiseaseState <- str_remove(dataSetNew$DiseaseState, ".csv")
    if (length(str_split(file, "-")[[1]]) == 5) {
     dataSetNew$lag <- str_split(file, "-")[[1]][[5]]
    } else {
      dataSetNew$lag <- 0
    }
    #dataSetNew$DiseaseState <- substr(dataSetNew$DiseaseState,1,nchar(dataSetNew$DiseaseState)-4)
    dataSetNew <- pivot_longer(dataSetNew, cols = seed1:seed100)

    if(unique(dataSetNew$DiseaseState) == "susceptible"){
    removed <- dataSetNew %>% group_by(name) %>% summarise(cumsuminfected = (min(value)))
    removed <- removed %>% filter(cumsuminfected == 9999)
    test <- removed$name
    } else{
    removed <- dataSetNew %>% group_by(name) %>% summarise(cumsuminfected = (max(value)))
    removed <- removed %>% filter(cumsuminfected == 1)
    test <- removed$name
    }
    dataSetNew <- dataSetNew %>% filter(!name  %in% test)
    dataSetFull <- rbind(dataSetFull, dataSetNew)
  }

#Data is prepared for plotting
  dataSetGrouped <- dataSetFull %>% group_by(Network, Scenario, DiseaseState, ID, lag, BaseProb) %>% summarise(mean = mean(value), sd = sd(value))
  dataSetGrouped <- dataSetGrouped %>% ungroup()
  dataSetGrouped <- dataSetGrouped %>% mutate(ymin = case_when(mean-sd > 0 ~ mean-sd, 
                                                              .default = 0)) %>%
                                        mutate(lag = case_when(lag == "0.csv" ~ 0, 
                                                                lag == "1.csv" ~ 1,
                                                                lag == "2.csv" ~ 2,))

#Plots are created and saved for the different networks/scenarios/infection probabilities per contact
  for(network in unique(dataSetGrouped$Network)) {
    for(scenario in unique(dataSetGrouped$Scenario)) {
      for(baseprob in unique(dataSetGrouped$BaseProb)) {
  plot <- ggplot(dataSetGrouped %>% filter(Network == network) %>% filter(Scenario == scenario) %>% filter(BaseProb == baseprob), aes(x=ID, y = mean)) +
    geom_line(aes(color = DiseaseState), size = 3) +
    geom_ribbon(aes(y = mean, ymin= ymin, ymax = mean+sd, fill = DiseaseState), alpha = 0.2) +
    theme_minimal() +
   scale_y_log10(breaks = c(1, 10, 100, 1000,10000), labels = c(1, 10, 100, 1000,10000)) +
    xlab("Time step") +
    ylab("Number of nodes") +
   # scale_y_log10() +
    xlim(c(0, 200)) +
  #  ylim(c(0,1000)) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    theme(axis.ticks.x = element_line(),
      axis.ticks.y = element_line(),
      axis.ticks.length = unit(5, "pt")) +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    theme(text = element_text(size = 45))

  file_name <- paste0(as.character(network),"-", unique(dataSetGrouped$lag), "lag-", as.character(scenario), "-", as.character(baseprob), "-SEIRcurve.pdf")
  ggsave(file_name, plot, dpi = 500, w = 9, h = 9)
      }
    }
  }
