#reevaluateCenters <- function(clusters, centers){
#  for(i in 1:nrow(centers)){
#    centers[i, ] <- c(mean(clusters$v[which(clusters$clNum == i)]),
#                      mean(clusters$e[which(clusters$clNum == i)]))
#  }
#  return(centers)
#}

makeClusters <- function(clusteredPoints, centers){
  for(i in 1:nrow(clusteredPoints)){
    clusteredPoints[i, 3] <- which.min(dist(rbind(clusteredPoints[i, 1:2], centers))[1:nrow(centers)])
  }
  return(clusteredPoints)
}

clusterDensity <- function(clusters, centers, rad = T){
  clDen <- 0
  if (rad) { 
    for (i in 1:nrow(centers)) {
      d <- rbind(centers[i, ], clusters[which(clusters$clNum == i), 1:2])
      clDen <- clDen + mean(dist(d)[1:(nrow(d)-1)])
    }
  } else {
    for (i in 1:nrow(centers)) {
      clDen <- clDen + mean(dist(clusters[which(clusters$clNum == i), 1:2]))
    }
  }
  return(clDen/nrow(centers))
}

plotClusters <- function(clusters, K, alg_option, quality1 = NULL, quality2 = NULL){
  plot(clusters[, 1:2],
       pch = 19,
       col = clusters$clNum,
       cex = 1.3,
       main = paste0("K-means.", alg_option, ". K = ", K))
  if ( (!is.null(quality1)) && (!is.null(quality2)) ) title(sub = paste("Avg cluster radius = ", round(quality1, digits = 4), 
                                                                        ". Avg internal distance = ", round(quality2, digits = 4)))
  points(clusters[which(is.na(clusters$clNum)), 1:2])
}

reevaluateCenterOfCluster <- function(cluster){
  return(c(mean(cluster[[1]]), mean(cluster[[2]])))
}

reevaluateCenters <- function(clusters, K, centers){
  for(i in 1:K){
    centers[i, ] <- reevaluateCenterOfCluster(clusters[which(clusters$clNum == i),])
  }
  return(centers)
}

initCenters <- function(sample, K, method){
  N <- nrow(sample)
  switch (method,
          random ={
            centers <- sample[sample(N, K),]
          },
          equidistant ={
            period <- floor(N/K)
            ind <- seq(from = floor(median(1:period)), to = N, by = period)
            ind <- match(sort(sample$v)[ind], sample$v)
            centers <- sample[ind,]
          }
  )
  return(centers)
}

initClusters <- function(sample, centers){
  clusters <- left_join(sample, cbind(centers, clNum = 1:nrow(centers)))
  return(clusters)
}

determineClusterForPoint <- function(centers, pointXY){
  return(which.min(dist(rbind(pointXY, centers))[1:nrow(centers)]))
}

kmeansOnePointAtATime <- function(clusteredPoints, centers){
  plotClusters(clusteredPoints, nrow(centers), alg_option = "Long")
  for (i in 1:nrow(clusteredPoints)){
    if(is.na(clusteredPoints[i, 3])){
      j <- determineClusterForPoint(centers, clusteredPoints[i, 1:2])
      clusteredPoints[i, 3] <- j
      centers[j, ] <- reevaluateCenterOfCluster(clusteredPoints[which(clusteredPoints$clNum == j), ])
      plotClusters(clusteredPoints, nrow(centers), alg_option = "Long")
    }
  }
  t <- 0
  repeat {
    clustersQuality <- clusterDensity(clusteredPoints, centers)
    for(i in 1:nrow(clusteredPoints)){
      j <- determineClusterForPoint(centers, clusteredPoints[i, 1:2])
      if(clusteredPoints[i, 3] != j){
        clusteredPoints[i, 3] <- j
        centers[j, ] <- reevaluateCenterOfCluster(clusteredPoints[which(clusteredPoints$clNum == j), ])
        plotClusters(clusteredPoints, nrow(centers),
                     alg_option = "Long",
                     quality1 = clustersQuality, 
                     quality2 = clusterDensity(clusteredPoints, centers, rad = F))
      }
    }
    if (clustersQuality == clusterDensity(clusteredPoints, centers)) t <- t+1
    else t <- 0
    if (t == 5) break
  }
  print(clusterDensity(clusteredPoints, centers))
  print(clusterDensity(clusteredPoints, centers, F))
  for(i in 1:10) plotClusters(clusteredPoints, nrow(centers),
                              alg_option = "Long",
                              quality1 = clustersQuality, 
                              quality2 = clusterDensity(clusteredPoints, centers, rad = F))
  return(clusteredPoints)
}

kmeansLazy <- function(clusteredPoints, centers){
  plotClusters(clusteredPoints, nrow(centers), alg_option = "Lazy")
  clusteredPoints <- makeClusters(clusteredPoints, centers)
  centers <- reevaluateCenters(clusteredPoints, nrow(centers), centers)
  clustersQuality <- clusterDensity(clusteredPoints, centers)
  plotClusters(clusteredPoints, nrow(centers),
               alg_option = "Lazy",
               quality1 = clustersQuality, 
               quality2 = clusterDensity(clusteredPoints, centers, rad = F))
  t <- 0
  
  repeat {
    clusteredPoints <- makeClusters(clusteredPoints, centers)
    centers <- reevaluateCenters(clusteredPoints, nrow(centers), centers)
    plotClusters(clusteredPoints, nrow(centers),
                 alg_option = "Lazy",
                 quality1 = clustersQuality, 
                 quality2 = clusterDensity(clusteredPoints, centers, rad = F))
    if (clustersQuality == clusterDensity(clusteredPoints, centers)) t <- t+1
    else t <- 0
    if (t == 5) break
    clustersQuality <- clusterDensity(clusteredPoints, centers)
  }
  for(i in 1:10) plotClusters(clusteredPoints, nrow(centers),
                              alg_option = "Lazy",
                              quality1 = clustersQuality, 
                              quality2 = clusterDensity(clusteredPoints, centers, rad = F))
  return(clusteredPoints)
}

mykmeans <- function(sample, K, center_method = "random", algoritm_option = "Lazy"){
  switch (center_method,
          random = {centers <- initCenters(sample, K, "random")},
          equidistant = {centers <- initCenters(sample, K, "equidistant")}
  )
  png(file="example%03d.png")
  palette(sample(rainbow(K)))
  switch (algoritm_option,
          Lazy = {clusters <- kmeansLazy(initClusters(sample, centers), centers)},
          Long = {clusters <- kmeansOnePointAtATime(initClusters(sample, centers), centers)}
  )
  palette("default")
  dev.off()
  return(clusters)
}

mykmeans_both <- function(sample, K, center_method = "random"){
  switch (center_method,
          random = {centers <- initCenters(sample, K, "random")},
          equidistant = {centers <- initCenters(sample, K, "equidistant")}
  )
  palette(sample(rainbow(K)))
  
  newdir <- paste0("randomLazy",K)
  dir.create(newdir)      
  cwd <- getwd()          
  setwd(newdir)
  
  png(file="rand_lazy%03d.png")
  
  clustered_points <- kmeansLazy(initClusters(sample, centers), centers)
  new_centers <- reevaluateCenters(clustered_points, K, centers)
  result <- c(clusterDensity(clustered_points, new_centers),
              clusterDensity(clustered_points, new_centers, F))
  dev.off()
  setwd(cwd)
  
  newdir <- paste0("randomLong",K)
  dir.create(newdir)      
  cwd <- getwd()          
  setwd(newdir)
  png(file="rand_long%03d.png")
  clustered_points <- kmeansOnePointAtATime(initClusters(sample, centers), centers)
  new_centers <- reevaluateCenters(clustered_points, K, centers)
  result <- c(result,
              clusterDensity(clustered_points, new_centers),
              clusterDensity(clustered_points, new_centers, F))
  dev.off()
  setwd(cwd)
  
  palette("default")
  
  return(result)
}
