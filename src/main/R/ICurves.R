library(tidyverse)

enter_path_here <- "" #Here one needs to enter the path to the folder of the modeling results
setwd(enter_path_here)

#Reading in of the necessary data
files <- list.files(path=enter_path_here, pattern="*infectious-0.csv", full.names=FALSE, recursive=FALSE) #Right now, the curves are produced for a 0-day lag. The pattern needs to be adapted if the user is interested in alternative lags.
files <- files[files != "_info.csv"]
files <- files[!str_detect(files, "infectionChance")]
#pattern <- c("-0.1-", "-0.2-", "-0.3-")
pattern <- c("-0.3-")
files <- files[grepl(paste(pattern, collapse = "|"), files)]

dataSetFull <- data.frame()

  for(file in files){
    dataSetNew <- read.csv(file)
    dataSetNew$ID <- seq.int(nrow(dataSetNew))
    dataSetNew$Network <- str_split(file, "-")[[1]][[1]]
    dataSetNew$BaseProb <- str_split(file, "-")[[1]][[2]]
    dataSetNew$DiseaseState <- str_split(file, "-")[[1]][[3]]
    #dataSetNew$DiseaseState <- str_remove(dataSetNew$DiseaseState, ".csv")
    if (length(str_split(file, "-")[[1]]) == 5) {
     dataSetNew$lag <- str_split(file, "-")[[1]][[5]]
    } else {
      dataSetNew$lag <- 0
    }
    #dataSetNew$DiseaseState <- substr(dataSetNew$DiseaseState,1,nchar(dataSetNew$DiseaseState)-4)
    dataSetNew <- pivot_longer(dataSetNew, cols = seed1:seed100)

    #Removing the runs where the infection dies out directly after the initial seed 
    removed <- dataSetNew %>% group_by(name) %>% summarise(cumsuminfected = (max(value)))
    removed <- removed %>% filter(cumsuminfected == 1)
    test <- removed$name
    dataSetNew <- dataSetNew %>% filter(!name  %in% test)
    dataSetFull <- rbind(dataSetFull, dataSetNew)
  }

# The following line of codes plot the infectious curve for each seed
# This tests if the different seeds are converging
# for(network in unique(dataSetFull$Network)){
#     for(baseprob in unique(dataSetFull$BaseProb)){
#         for(strategy in unique(dataSetFull$DiseaseState)){
#             for(lag in unique(dataSetFull$lag)){
#             plot <- ggplot(dataSetFull %>% filter(Network == network) %>% filter(BaseProb == baseprob) %>% filter(DiseaseState == strategy), aes(x=ID, y = value)) +
#             xlab("Time step") +
#             ylab("Number of infectious agents") +
#             geom_line(aes(color = name), alpha = 0.2) +
#             theme_minimal() +
#             theme(legend.position = "bottom", legend.title = element_blank()) +
#             guides(fill=FALSE, color=FALSE) +
#             theme(text = element_text(size = 45)) +
#             theme(axis.ticks.x = element_line(),
#                 axis.ticks.y = element_line(),
#                 axis.ticks.length = unit(5, "pt"))

#             file_name <- paste0(as.character(network),"-", as.character(dataSetGrouped$lag), "-", as.character(baseprob), "-", as.character(strategy),  "-ICurvesDifferentSeeds.pdf")
#             ggsave(file_name, plot, dpi = 500, w = 15, h = 10)
#             }
#         }
#     }
# }

#Data pprep for plotting
  dataSetGrouped <- dataSetFull %>% group_by(Network, DiseaseState, ID, lag, BaseProb) %>% summarise(mean = mean(value), sd = sd(value))
  dataSetGrouped <- dataSetGrouped %>% ungroup()
  dataSetGrouped <- dataSetGrouped %>% mutate(ymin = case_when(mean-sd > 0 ~ mean-sd, 
                                                              .default = 0)) %>%
                                        mutate(lag = case_when(lag == "0.csv" ~ 0, 
                                                                lag == "1.csv" ~ 1,
                                                                lag == "2.csv" ~ 2,))

  dataSetGrouped <- dataSetGrouped %>% mutate(BaseProbLabel = case_when(
          BaseProb == 0.3 ~ "Infection probability = 0.3",
          BaseProb == 0.2 ~ "Infection probability = 0.2",
          BaseProb == 0.1 ~ "Infection probability = 0.1"))
  dataSetGrouped$BaseProbLabel <- as.factor(dataSetGrouped$BaseProbLabel)
  dataSetGrouped$BaseProbLabel <- ordered (dataSetGrouped$BaseProbLabel, levels = c("Infection probability = 0.3", "Infection probability = 0.2","Infection probability = 0.1"))


#Plots are created and saved for different networks
    for(network in unique(dataSetGrouped$Network)){
    plot <- ggplot(dataSetGrouped %>% filter(DiseaseState != "local2") %>% filter(DiseaseState != "global_local1_and_2") %>% filter(DiseaseState != "global_local1") %>% filter(Network == network), aes(x=ID, y = mean)) +
    geom_line(aes(color = DiseaseState), size = 4.5) +
    xlab("Time Step") +
    ylab("No. of \n Infectious Agents") +
   #scale_y_log10(breaks = c(1, 10, 100, 1000,10000), labels = c(1, 10, 100, 1000,10000)) +
    xlim(c(0, 110)) +
    theme_minimal() +
    theme(legend.position = "none", legend.title = element_blank()) +
    theme(axis.ticks.x = element_line(),
      axis.ticks.y = element_line(),
      axis.ticks.length = unit(5, "pt")) +
    scale_color_brewer(palette = "Dark2") +
    theme(text = element_text(size = 60)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(axis.title.x = element_text(margin = margin(t = 10, r = 20, b = 0, l = 10))) 
  #  theme(axis.title.y = element_text(margin = margin(t = 200, r = 0, b = 0, l = 20))) 

    file_name <- paste0(as.character(network),"-", unique(dataSetGrouped$lag), "lag-ICurves.pdf")
    ggsave(file_name, plot, dpi = 500, w = 9.5, h = 9.5)
    }

