rm(list=ls())
setwd("C:/Users/DELL/Downloads/house-prices-advanced-regression-techniques")
data <- read.csv("C:/Users/DELL/Downloads/house-prices-advanced-regression-techniques/train.csv")
df <- as.data.frame(data)

#eliminar variable id
df <- df[,-which(names(df) %in% c('Id'))]

#caracter a factor
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)

#años a factor
cols <- c('YearBuilt', 'YearRemodAdd','YrSold','GarageYrBlt','MSSubClass')
df[,cols] <- lapply(df[,cols], as.factor)

#df con variables numericas
df_features <- df[sapply(df, is.numeric)]
summary(df_features)

#df con variables categoricas
df_factor <- df[sapply(df,is.factor)]
summary(df_factor)

###### ANÁLISIS DE LA CALIDAD DE LOS DATOS
summary(df)

#MISSING VALUES
library(DataExplorer)
plot_missing(df_features)

#IMPUTACIÓN DE DATOS MASVNRAREA
df_features <- df_features[!is.na(df_features$MasVnrArea), ]

#IMPUTACION DE DATOS LOTFRONTAGE
library(mice)
mice_imputed <- data.frame(
  original = df_features$LotFrontage,
  imputed_cart = complete(mice(df_features, method = "cart"))$LotFrontage)
View(mice_imputed)
library(ggplot2)
install.packages("cowplot")
library(cowplot)
h.orig <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#7FFFD4", color = "#FF7256", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()

h.cart <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#98F5FF", color = "#FF7F00", position = "identity") +
  ggtitle("Cart-imputed distribution") +
  theme_classic()

plot_grid(h.orig, h.cart, nrow = 1, ncol = 2)

df_features$LotFrontage <- mice_imputed$imputed_cart

#PCA
library(stats)
features_pca <- prcomp(df_features, center = T, scale. = T)
summary(features_pca)

loadings <- features_pca$rotation
for(i in 1:ncol(loadings)) {
  component_loadings <- loadings[,i]
  max_loading_var <- names(which.max(abs(component_loadings)))
  print(paste("Componente", i, ":", max_loading_var))
}

#VARIANZA EXPLICADA POR NUMERO DE COMPONENTES
PVE <- 100*features_pca$sdev^2/sum(features_pca$sdev^2)
par(mfrow = c(1,2))
plot(PVE, type = "o", 
     ylab = "PVE", 
     xlab = "Componente principal", 
     col = "#9B30FF")
plot(cumsum(PVE), type = "o", 
     ylab = "PVE acumulada", 
     xlab = "Componente principal", 
     col = "#FF82AB")

#GRAFICO DOS DIMENSIONES COMPONENTES PRINCIPALES
par(mfrow = c(1, 1))
biplot(features_pca, col = c("#1E90FF", "#FF6347"), cex = c(1.3), main = "Biplot PCA", xlim = c(-0.15, 0.25), ylim = c(-0.15, 0.15))


#K-MEANS 
library(factoextra)
library(cluster)
  
fviz_nbclust(df_features, kmeans, method = "wss")
#2 cluster
fviz_nbclust(df_features, kmeans, method = "silhouette")
#2 cluster

#grafico k-means
library(dplyr)
set.seed(1)
final <- kmeans(df_features, 2, nstart = 25)
print(final)
fviz_cluster(final, data = df_features)
df_features %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

#hierchical cluster DIANA
library(cluster)
diana_clust <- diana(df_features)
plot(diana_clust, which.plots=2, cex = 0.7)

clusters_diana <- cutree(as.hclust(diana_clust), k = 4)
df_diana <- df_features %>%
  mutate(Cluster_DIANA = clusters_diana) %>%
  group_by(Cluster_DIANA) %>%
  summarise_all("mean")
print(clusters_diana)
fviz_cluster(list(data = df_features, cluster = clusters_diana))
                
          
