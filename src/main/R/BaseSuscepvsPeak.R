library(tidyverse)

enter_path_here <- "" #Here one needs to enter the path to the folder of the modeling results
setwd(enter_path_here)

# Reading in of necessary modeling results
files <- list.files(path=enter_path_here, pattern="*infectious", full.names=FALSE, recursive=FALSE) 
files <- files[files != paste0(enter_path_here, "_info.csv")]

dataSetFull <- data.frame()
for(i in 1:(length(files))){
  filesReduced <- files[i]
  
  for(file in filesReduced){
    dataSetNew <- read.csv(file)
    dataSetNew$ID <- seq.int(nrow(dataSetNew))
    dataSetNew$Network <- str_split(file, "-")[[1]][[1]]
    dataSetNew$BaseSuscep <- str_split(file, "-")[[1]][[2]]
    dataSetNew$Strategy <- str_split(file, "-")[[1]][[3]]
    if (length(str_split(file, "-")[[1]]) == 5) {
    dataSetNew$lag <- str_split(file, "-")[[1]][[5]]
    } else {
      dataSetNew$lag <- 0
    }
    dataSetNew <- pivot_longer(dataSetNew, cols = seed1:seed100)

    #Removing the runs where the infection dies out directly after the initial seed 
    removed <- dataSetNew %>% group_by(name) %>% summarise(cumsuminfected = (max(value)))
    removed <- removed %>% filter(cumsuminfected == 1)
    test <- removed$name
    dataSetNew <- dataSetNew %>% filter(!name  %in% test)

    dataSetFull <- rbind(dataSetFull, dataSetNew)
  }
}

#Preparing the data for plotting
dataSetGrouped <- dataSetFull %>%
  group_by(BaseSuscep, Strategy, ID, lag, Network) %>%
  summarise(value = mean(value))
dataSetGrouped <-  dataSetGrouped %>%
  group_by(BaseSuscep, Strategy, lag, Network) %>%
  summarise(maxInfected = max(value), timePointMax = which.max(value))

# ggplot(dataSetGrouped, aes(x = Strategy, y = maxInfected)) +
#   geom_point() +
#   xlab("weight") +
#   theme_minimal() +
#   facet_wrap(~ BaseSuscep) +
#   #ggtitle(network_top) +
#   xlab("Considered Scenario") +
#   ylab("Peak height") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#ggsave(paste0(str_split(file, "-")[[1]][[1]], "ScenarioVsMaxInfected.pdf"), dpi = 500, w = 9, h = 6)   

# ggplot(dataSetGrouped, aes(x = Strategy, y = timePointMax)) +
# geom_point() +
# xlab("weight") +
# theme_minimal() +
# facet_wrap(~ BaseSuscep) +
# #ggtitle(network_top) +
# xlab("Considered Scenario") +
# ylab("Timing of peak") +
# theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#ggsave(paste0(str_split(file, "-")[[1]][[1]], "ScenarioVsPeakTiming.pdf"), dpi = 500, w = 9, h = 6)

# dataSetGrouped <- dataSetFull %>%
#   group_by(BaseSuscep, DiseaseState, name) %>%
#   summarise(maxInfected = max(value), sd = sd(value)) %>%
#   filter(maxInfected > 5)
# #
# ggplot(dataSetGrouped, aes(x = Strategy, y = maxInfected)) +
#   geom_boxplot() +
#   facet_wrap(~BaseSuscep) +
#   theme_minimal()

dataSetGrouped$BaseSuscep <- as.numeric(dataSetGrouped$BaseSuscep)

dataSetGrouped <- dataSetGrouped %>% filter(Strategy != "local2") %>% filter(Strategy != "global_local1_and_2") %>% filter(Strategy != "global_local1")

#Creating and saving a plot that displays the infection probability per single contact vs. the peak height
for(network in unique(dataSetGrouped$Network)){
  for(consideredlag in unique(dataSetGrouped$lag)) {
  ggplot(dataSetGrouped %>% filter(Network == network) %>% filter(lag == consideredlag), aes(x = BaseSuscep, y = maxInfected)) +
  geom_line(aes(color = Strategy), size = 4.5) +
  scale_color_brewer(palette = "Dark2") +
  ylab("Peak Height") +
  #scale_y_log10() +
  xlab("Infection Probability \n per Single Contact") +
  theme_minimal() +
  #ggtitle(network_top) +
  scale_x_continuous(breaks = seq(from = 0.1, to = 0.3, by = 0.05)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(text = element_text(size = 60)) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
        axis.ticks.y = element_line(),
        axis.ticks.length = unit(5, "pt")) +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 10))) +
  guides(fill = FALSE, color = FALSE)
  #guides(color=guide_legend(nrow=2, byrow=TRUE))
  
  if (length(str_split(file, "-")[[1]]) == 5){
  ggsave(paste0(as.character(network), "-", as.character(consideredlag), "-BaseSuscepVsMaxInfected.pdf"), dpi = 500, w = 9.5, h = 9.5) 
  }else { 
  ggsave(paste0(as.character(network), "-BaseSuscepVsMaxInfected.pdf"), dpi = 500, w = 9.5, h = 9.5)   
  }
}
}

#Creating and saving a plot that displays the infection probability per single contact vs. the time step of the peak
for(network in unique(dataSetGrouped$Network)){
  for(consideredlag in unique(dataSetGrouped$lag)){
    ggplot(dataSetGrouped %>% filter(Network == network) %>% filter(lag == consideredlag), aes(x = BaseSuscep, y = timePointMax)) +
    geom_line(aes(color = Strategy), size = 4.5) +
    scale_color_brewer(palette = "Dark2") +
    ylab("Time Step \n of Peak") +
    xlab("Infection Probability \nper Single Contact") +
    theme_minimal() +
   # scale_y_log10() +
    #ggtitle(network_top) +
    theme(text = element_text(size = 57)) +
    scale_x_continuous(breaks = seq(from = 0.1, to = 0.3, by = 0.05)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    theme(axis.ticks.x = element_line(),
          axis.ticks.y = element_line(),
          axis.ticks.length = unit(5, "pt")) +
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 70, b = 0, l = 10))) +
    guides(fill = FALSE, color = FALSE)
    #guides(color=guide_legend(nrow=2, byrow=TRUE))

    if (length(str_split(file, "-")[[1]]) == 5){
    ggsave(paste0(as.character(network), "-", as.character(consideredlag), "-BaseSuscepVsPeakTiming.pdf"), dpi = 500, w = 9.5, h = 9.5) 
    } else {
    ggsave(paste0(as.character(network), "-BaseSuscepVsPeakTiming.pdf"), dpi = 500, w = 9.5, h = 9.5)  
    }

  }
}
