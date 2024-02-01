enter_path_here <- "" #Here one needs to enter the path to the folder of the modeling results
setwd(enter_path_here)

files <- list.files(path=enter_path_here, pattern="*clusteringCoefficient*", full.names=FALSE, recursive=FALSE)

dataSetCoefficients <- data.frame(matrix(nrow = 0, ncol = 2))
colnames(dataSetCoefficients) <- c("fileName", "avgClusteringCoefficient")

  for (file in files) {
    dataSetNew <- read.csv(file)
    dataSetNew <- pivot_longer(dataSetNew, cols = seed1:seed5, names_to ="seed", values_to = "clusteringCoefficient")
    dataSetCoefficients[nrow(dataSetCoefficients) + 1, ] <- c(file, mean(dataSetNew$clusteringCoefficient))
    }


files <- list.files(path=enter_path_here, pattern="*shortestPath*", full.names=FALSE, recursive=FALSE)

dataSetPaths <- data.frame(matrix(nrow = 0, ncol = 2))
colnames(dataSetPaths) <- c("fileName", "avgshortestPath")

  for (file in files) {
    dataSetNew <- read.csv(file)
    dataSetNew <- pivot_longer(dataSetNew, cols = seed1:seed5, names_to ="seed", values_to = "shortestPath")
    dataSetPaths[nrow(dataSetPaths) + 1, ] <- c(file, mean(dataSetNew$shortestPath))
    }
