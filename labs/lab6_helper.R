#reevaluateCenters <- function(clusters, centers){
#  for(i in 1:nrow(centers)){
#    centers[i, ] <- c(mean(clusters$v[which(clusters$clNum == i)]),
#                      mean(clusters$e[which(clusters$clNum == i)]))
#  }
#  return(centers)
#}

# reuire(purrr)

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
      dd <- mean(dist(clusters[which(clusters$clNum == i), 1:2]))
      if (is.finite(dd)) clDen <- clDen + dd
    }
  }
  return(clDen/nrow(centers))
}

plotClusters <- function(clusters, K, alg_option, quality1 = NULL, quality2 = NULL){
  plot(clusters[, 1:2],
       asp = 1, xlim = c(0, 1), ylim = c(0, 1),
       pch = 19,
       col = clusters$clNum,
       cex = 1.3,
       main = paste0(alg_option, ". K = ", K))
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
  centers <- reevaluateCenters(clusteredPoints, nrow(centers), centers)
  print(clusterDensity(clusteredPoints, centers))
  print(clusterDensity(clusteredPoints, centers, F))
  for(i in 1:10) plotClusters(clusteredPoints, nrow(centers),
                              alg_option = "Long",
                              quality1 = clusterDensity(clusteredPoints, centers), 
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
                              quality1 = clusterDensity(clusteredPoints, centers), 
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

library("purrr", lib.loc="~/R/R-3.5.2/library")

ind <- function(i, j, n){
  # if(j == i) return(0)
  # if(j < i) return(N*(j -1) + i - j - (j*(j-1))/2) 
  # N*(i -1) + j - i - (i*(i-1))/2 
  
  result <-n*(i -1) + j - i - (i*(i-1))/2
  result[which(j == i)] <- 0
  t <- j[which(j < i)]
  result[which(j < i)] <- (n*(t -1) + i - t - (t*(t-1))/2)
  return(result)
}


chooseStart <- function(unclustered_points, method = "minmedian"){
  switch (method,
          maxmedian = {
            ll <- dist(unclustered_points)
            j <- which.max(sapply(1:nrow(unclustered_points), 
                                  function(i) median(ll[ind(i, 1:nrow(unclustered_points), nrow(unclustered_points))])))
            return(unclustered_points[j,])
          },
          
          minmedian = {
            ll <- dist(unclustered_points)
            j <- which.min(sapply(1:nrow(unclustered_points), 
                                  function(i) median(ll[ind(i, 1:nrow(unclustered_points), nrow(unclustered_points))])))
            return(unclustered_points[j,])
          },
          
          random = {
            return(unclustered_points[sample.int(nrow(unclustered_points),
                                                 size = 1),])
          }
  )
}

formSphere <- function(unclustered_points, 
                       radius, 
                       cent_meth = "minmedian", 
                       i, 
                       visualise = T,
                       k = NULL){
  
  if (cent_meth == "seq") center <- unclustered_points[k,]
  else {
    center <- chooseStart(unclustered_points, cent_meth)
  }
  
  if (isTRUE(visualise)) plotClustSearch(unclustered_points, center, radius, i)
  
  repeat{
    ll <- dist(rbind(center, unclustered_points))
    indices <- which(ll[1:nrow(unclustered_points)] 
                     < radius)
    sphereCandidate <-  unclustered_points[indices,]
    center <- reevaluateCenterOfCluster(sphereCandidate)
    
    if (isTRUE(visualise)) plotClustSearch(unclustered_points, center, radius, i)
    
    if (isTRUE(all.equal(which(dist(rbind(center, unclustered_points))[1:nrow(unclustered_points)] 
                               < radius),
                         indices))) break
  }
  if (isTRUE(visualise)) points(sphereCandidate,
                                col = i,
                                pch = 19)
  
  return(sphereCandidate)
}


forel <- function(unclustered_points, radius, cent_meth = "minmedian"){
  number_of_unclustered_points <- nrow(unclustered_points)
  clusters <- data.frame(v = NULL, e = NULL, clNum = NULL)
  i <- 0
  
  
  repeat{
    i <- i + 1
    
    dd <- formSphere(unclustered_points, radius, cent_meth, i)
    clusters <- rbind(clusters,
                      cbind(dd, clNum = i))
    
    unclustered_points <- setdiff(unclustered_points, 
                                  dd)
    number_of_unclustered_points <- nrow(unclustered_points)
    
    if (number_of_unclustered_points == 1) {
      clusters <- rbind(clusters,
                        cbind(unclustered_points, clNum = i+1))
      unclustered_points <- setdiff(unclustered_points, unclustered_points)
      number_of_unclustered_points <- nrow(unclustered_points)
    }
    
    if (number_of_unclustered_points == 0) break
  }
  return(clusters)
}

###ØÒÎ ÇÄÅÑÜ ÏÐÎÈÑÕÎÄÈÒ??!! ÏÀÌÀÃÈÒÈ!!
forel2 <- function(unclustered_points, radius){
  
  number_of_unclustered_points <- nrow(unclustered_points)
  all_points <- unclustered_points
  
  for (j in 1:2) {
    plot(unclustered_points, 
         asp = 1, 
         xlim = c(0, 1), 
         ylim = c(0, 1),
         main = "FOREL 2")
  }
  
  clusters <- data.frame(v = NULL, e = NULL, clNum = NULL)
  
  i <- 0
  
  repeat{
    i <- i + 1
    cc <- list(3)
    for (k in 1:nrow(unclustered_points)) {
      cluster <- formSphere(unclustered_points, radius, 
                            cent_meth = "seq", i, 
                            k, visualise = F)
      cc[[k]] <- cluster
    }
    
    {
      ww <- unique(cc)
      
      ws <- map(ww, reevaluateCenterOfCluster)
      ws <- as.data.frame(ws)
      ws <- data.frame(t(ws[1, ]),t(ws[2,]), row.names = 1:ncol(ws))
      colnames(ws) <- c("v", "e")
      
      plot(unclustered_points, 
           asp = 1, 
           xlim = c(0, 1), 
           ylim = c(0, 1),
           main = "FOREL 2")
      
      for (j in 1:nrow(ws)) {
        polygon(circle(ws[j, ], radius),
                border = j)
      }
      
    }
    
    poo <- map_dbl(ww, function(x) nrow(x))
    dd <- ww[[which.max(poo)]]
    
    
    clusters <- rbind(clusters,
                      cbind(dd, clNum = i))
    unclustered_points <- setdiff(unclustered_points, 
                                  dd)
    number_of_unclustered_points <- nrow(unclustered_points)
    
    {
      plot(unclustered_points, 
           asp = 1, 
           xlim = c(0, 1), 
           ylim = c(0, 1),
           main = "FOREL 2")
      polygon(circle(reevaluateCenterOfCluster(dd), radius),
              border = i)
      points(clusters[, 1:2], col = clusters$clNum, pch = 19)
    }
    
    
    if (number_of_unclustered_points == 0) break
  }
  return(clusters)
}


plotClustSearch <- function(unclustered_points, center, radius, i) {
  plot(unclustered_points, asp = 1, xlim = c(0, 1), ylim = c(0, 1))
  points(x = center[[1]],
         y = center[[2]],
         pch = 4)
  polygon(circle(center, radius),
          border = i)
}


circle <- function(center, radius){
  phi <- seq(0, 2*pi, by = 0.01)
  return(cbind(x = center[[1]] + radius*cos(phi),
               y = center[[2]] + radius*sin(phi)))
}


forel2Research <- function(sample_Scaled, radius){
  clustered_sample <- forel2(sample_Scaled, radius)
  clust_centers <- reevaluateCenters(clustered_sample, 
                                     max(clustered_sample$clNum), 
                                     clustered_sample[1:max(clustered_sample$clNum),1:2])
  quality1 <- clusterDensity(clustered_sample, 
                             clust_centers,
                             rad = T)
  
  quality2 <- clusterDensity(clustered_sample, 
                             clust_centers,
                             rad = F)
  plotClusters(clustered_sample,
               K = nrow(clust_centers),
               alg_option = "FOREL2",
               quality1,
               quality2)
  
  points(clust_centers,
         pch = 4)
  
  for(j in 1:nrow(clust_centers)) {
    polygon(circle(clust_centers[j, ], radius),
            border = j)
  }
  for (j in 1:6) {
    plotClusters(clustered_sample,
                 K = nrow(clust_centers),
                 alg_option = "FOREL2",
                 quality1,
                 quality2)
  }
  
  return(c(quality1, quality2, nrow(clust_centers)))
}


forelResearch <- function(sample_Scaled, radius, cent_meth = "minmedian"){
  plot(sample_Scaled, asp = 1, xlim = c(0, 1), ylim = c(0, 1), 
       main = paste("FOREL", cent_meth))
  clustered_sample <- forel(sample_Scaled, radius, cent_meth)
  clust_centers <- reevaluateCenters(clustered_sample, 
                                     max(clustered_sample$clNum), 
                                     clustered_sample[1:max(clustered_sample$clNum),1:2])
  
  quality1 <- clusterDensity(clustered_sample, 
                             clust_centers,
                             rad = T)
  
  quality2 <- clusterDensity(clustered_sample, 
                             clust_centers,
                             rad = F)
  plotClusters(clustered_sample,
               nrow(clust_centers),
               paste("FOREL", cent_meth),
               quality1,
               quality2)
  
  points(clust_centers,
         pch = 4)
  
  for(j in 1:nrow(clust_centers)) {
    polygon(circle(clust_centers[j, ], radius),
            border = j)
  }
  for (j in 1:6) {
    plotClusters(clustered_sample,
                 nrow(clust_centers),
                 paste("FOREL", cent_meth),
                 quality1,
                 quality2)
  }
  
  return(c(quality1, quality2, nrow(clust_centers)))
}
