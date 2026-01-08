install.packages(c("readxl","dplyr")) 
library(readxl)
library(dplyr)
library(readr)

df <-read_excel("Desktop/LectureRoomHVAC_Dataset.xlsx")
View(df)

X <- df[,c("Room Size (m2)",
         "OccupancyRate (percentage)",
         "SessionDuration (hrs)",
         "OutdoorTemperature (Celsius)",
         "OutdoorHumidity (percentage)",
         "CO2_ppm (parts_per_million)")]

#Data Normalisation
X_Scale<-scale(X)

#Number of clusters
library(factoextra)
fviz_nbclust(X_Scale,kmeans, method = "wss")

set.seed(123)
k<-3
km<-kmeans(X_Scale,centers = k,nstart = 25)
df$clusterID<-km$cluster

#Cluster Interpretation
library(dplyr)
centroids <- aggregate(
  df[, c("Room Size (m2)", "OccupancyRate (percentage)", "SessionDuration (hrs)","OutdoorTemperature (Celsius)","OutdoorHumidity (percentage)","CO2_ppm (parts_per_million)")],
  by = list(clusterID = df$clusterID),
  FUN = mean)

print(centroids)

fviz_cluster(
  list(data = X_Scale, cluster = df$clusterID),
  geom = "point",
  ellipse = TRUE,
  show.clust.cent = TRUE
) +
  labs(title = "Cluster Visualisation")

write.csv(df, "LectureRoomHVAC_Clustering.csv", row.names = FALSE)