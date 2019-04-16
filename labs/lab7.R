#масштабирование выборки
##стандартизация (z-score)

## require(dplyr)
## require(colorspace)

zscore <- data.frame(v = (sample$v - moments["mean","v"])/moments["deviation","v"], 
                     e = (sample$E - moments["mean","e"])/moments["deviation","e"])
zscore <- as.tbl(zscore)

##min-max normalization
sample_Scaled <- data.frame(v = (sample$v - min(sample$v))/vRange, 
                            e = (sample$E - min(sample$E))/eRange)
sample_Scaled <- as.tbl(sample_Scaled)

unclustered_points <- sample_Scaled

number_of_unclustered_points <- N

#### cent_meth: "minmedian", "maxmedian", "random"

comparTForel <- data.frame(radius = 1:10,
                           Rand_cd = 1,
                           Rand_innd = 1,
                           Rand_K = 1,
                           Minm_cd = 1,
                           Minm_innd = 1,
                           Minm_K = 1,
                           Maxm_cd = 1,
                           Maxm_innd = 1,
                           Maxm_K = 1,
                           Sec_cd = 1,
                           Sec_innd = 1,
                           Sec_K = 1)

cn <- 10
palette(sample(rainbow(20)))

#plot(1:(2*cn), col = 1:(2*cn))

{
  newdir <- "ExperRadius"
  dir.create(newdir)      
  swd <- getwd()          
  setwd(newdir)
  
  radius <- 0.1
  for (j in 1:10) {
    comparTForel[j, 1] <- radius
    
    {
      newdir <- paste0("random", radius)
      dir.create(newdir)      
      cwd <- getwd()          
      setwd(newdir)
      
      png(file="rand%03d.png")
      comparTForel[j, 2:4] <- forelResearch(sample_Scaled, radius, cent_meth ="random")
      
      dev.off()
      setwd(cwd)
    }
    
    {
      newdir <- paste0("minmedian", radius)
      dir.create(newdir)      
      cwd <- getwd()          
      setwd(newdir)
      
      png(file="minmedian%03d.png")
      comparTForel[j, 5:7] <- forelResearch(sample_Scaled, radius, cent_meth ="minmedian")
      
      dev.off()
      setwd(cwd)
    }
    
    {
      newdir <- paste0("maxmedian", radius)
      dir.create(newdir)      
      cwd <- getwd()          
      setwd(newdir)
      
      png(file="maxmedian%03d.png")
      comparTForel[j, 8:10] <- forelResearch(sample_Scaled, radius, cent_meth ="maxmedian")
      
      dev.off()
      setwd(cwd)
    }
    
    {
      newdir <- paste0("forelSec", radius)
      dir.create(newdir)      
      cwd <- getwd()          
      setwd(newdir)
      
      png(file="forelSec%03d.png")
      comparTForel[j, 11:13] <- forel2Research(sample_Scaled, radius)
      
      dev.off()
      setwd(cwd)
    }
    
    radius <- radius + 0.025
  }
  setwd(swd)
}


comparTForelRand <- data.frame(Rand_cd = 1:10,
                               Rand_innd = 1,
                               K = 1)

{
  newdir <- "ExperRand"
  dir.create(newdir)      
  swd <- getwd()          
  setwd(newdir)
  
  radius <- 0.15
  for (j in 1:10) {
    newdir <- paste0("random", j)
    dir.create(newdir)      
    cwd <- getwd()          
    setwd(newdir)
    
    png(file="rand%03d.png")
    comparTForelRand[j, ] <- forelResearch(sample_Scaled, radius, cent_meth ="random")
    
    dev.off()
    setwd(cwd)
  }
  
  setwd(swd)
}

{
  plot(comparTForel$radius, 
       y = comparTForel$Rand_cd, 
       type = "l", 
       col = 17,
       lwd = 2,
       main = "Зависимость СКР от R для FOREL алгоритмов",
       xlab = "Radius",
       ylab = "Средн. кластерный радиус")
  lines(comparTForel$radius, 
        y = comparTForel$Minm_cd, 
        col = 3,
        lwd = 2)
  lines(comparTForel$radius, 
        y = comparTForel$Maxm_cd, 
        col = 12,
        lwd = 2)
  lines(comparTForel$radius, 
        y = comparTForel$Sec_cd, 
        col = 8,
        lwd = 2)
  
  legend(x = "bottomright",
         inset = 0,
         legend = c("random", "minmedian", "maxmedian", "full"), 
         col = c(17, 3, 12, 8),
         bty = "n",
         lwd = 3,
         y.intersp = 0.7)
}

{
  plot(comparTForel$radius, 
       y = comparTForel$Rand_innd, 
       type = "l", 
       col = 17,
       lwd = 2,
       main = "Зависимость СВкР от R для FOREL алгоритмов",
       xlab = "Radius",
       ylab = "Средн. внутрикл. расстояния")
  lines(comparTForel$radius, 
        y = comparTForel$Minm_innd, 
        col = 3,
        lwd = 2)
  lines(comparTForel$radius, 
        y = comparTForel$Maxm_innd, 
        col = 12,
        lwd = 2)
  lines(comparTForel$radius, 
        y = comparTForel$Sec_innd, 
        col = 8,
        lwd = 2)
  
  legend(x = "bottomright",
         inset = 0,
         legend = c("random", "minmedian", "maxmedian", "full"), 
         col = c(17, 3, 12, 8),
         bty = "n",
         lwd = 3,
         y.intersp = 0.7)
}

{
  plot(comparTForel$radius, 
       y = comparTForel$Rand_K, 
       type = "l", 
       col = 17,
       lwd = 2,
       main = "Зависимость кол-ва кластеров от R для FOREL алгоритмов",
       xlab = "Radius",
       ylab = "Количество кластеров",
       yaxt = "n")
  axis(2, at = 1:19, 
       labels = 1:19, las = 1)
  lines(comparTForel$radius, 
        y = comparTForel$Minm_K, 
        col = 3,
        lwd = 2)
  lines(comparTForel$radius, 
        y = comparTForel$Maxm_K, 
        col = 12,
        lwd = 2)
  lines(comparTForel$radius, 
        y = comparTForel$Sec_K, 
        col = 8,
        lwd = 2)
  
  legend(x = "topright",
         inset = 0,
         legend = c("random", "minmedian", "maxmedian", "full"), 
         col = c(17, 3, 12, 8),
         bty = "n",
         lwd = 3,
         y.intersp = 0.7)
}
