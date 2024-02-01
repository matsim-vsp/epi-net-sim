library(tidyverse)

enter_path_here <- "" #Here one needs to enter the path to the folder of the modeling results
setwd(enter_path_here)

# Reading in of files, the user needs to change line 9 according to the network they are interested in
files <- list.files(path=enter_path_here, pattern="*recovered", full.names=FALSE, recursive=FALSE)
files <- files[files != paste0(enter_path_here, "_info.csv")]

dataSetFull <- data.frame()

for (i in 1:(length(files))){
  filesReduced <- files[i]
  for (file in filesReduced){
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
    dataSetFull <- rbind(dataSetFull, dataSetNew)
  }
}

#dataSetFull <- read.csv("/Users/sydney/Desktop/10000nodesdata/dataSetFullBaseSuscepvsPandemicsize.csv")

dataSetGrouped <- dataSetFull
noTimeSteps <- max(dataSetGrouped$ID)
dataSetGrouped <- dataSetGrouped %>% filter(ID == noTimeSteps) %>%
  filter(value > 1) %>%
  group_by(BaseSuscep, ID, Strategy, lag, Network) %>%
  summarise(mean = mean(value), sd = sd(value))
 dataSetGrouped <- dataSetGrouped %>% ungroup()


# ggplot(dataSetGrouped, aes(x = Strategy, y = mean)) +
#   geom_point() +
#   ylab("Pandemic size (# of recovered, after 200 steps") +
#   xlab("Considered szenario") +
#   theme_minimal() +
#   facet_wrap(~BaseSuscep) +
#   ggtitle(network_top) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#ggsave(paste0(str_split(file, "-")[[1]][[1]], "ScenarioVsPandemicSize.pdf"), dpi = 500, w = 9, h = 6)   

dataSetGrouped$BaseSuscep <- as.double(dataSetGrouped$BaseSuscep)

dataSetGrouped <- dataSetGrouped %>% filter(Strategy != "local2") %>% filter(Strategy != "global_local1_and_2") %>% filter(Strategy != "global_local1")

for (network in unique(dataSetGrouped$Network)) {
  for (consideredlag in unique(dataSetGrouped$lag)) {
    ggplot(dataSetGrouped %>% filter(Network == network) %>% filter(lag == consideredlag), aes(x = BaseSuscep, y = mean)) +
      geom_line(aes(color = Strategy), size = 4.5) +
      scale_color_brewer(palette = "Dark2") +
      ylab("Pandemic Size") +
      xlab("Infection Probability \n per Single Contact") +
      #scale_y_log10(breaks = c(1, 10, 100, 1000,10000), labels = c(1, 10, 100, 1000,10000)) +
      theme_minimal() +
      #ggtitle(network_top) +
      theme(text = element_text(size = 60)) +
      scale_x_continuous(breaks = seq(from = 0.1, to = 0.3, by = 0.05)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      theme(legend.position = "right", legend.title = element_blank()) +
      theme(axis.ticks.x = element_line(),
            axis.ticks.y = element_line(),
            axis.ticks.length = unit(5, "pt")) +
      theme(axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 10))) 

    if (length(str_split(file, "-")[[1]]) == 5) {
    ggsave(paste0(as.character(network), "-", as.character(consideredlag), "-BaseSuscepVsPandemicSize.pdf"), dpi = 500, w = 12.5, h = 9.5) 
    } else { 
    ggsave(paste0(as.character(network), "-Susceptible-BaseSuscepVsPandemicSize.pdf"), dpi = 500, w = 12.5, h = 9.5)   
    }

  }
}


