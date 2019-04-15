#масштабирование выборки
##стандартизация (z-score)

## require(dplyr)

######################t(sample) - as.vector(moments["mean",], mode = "numeric")

zscore <- data.frame(v = (sample$v - moments["mean","v"])/moments["deviation","v"], 
                     e = (sample$E - moments["mean","e"])/moments["deviation","e"])
zscore <- as.tbl(zscore)

##min-max normalization
sample_Scaled <- data.frame(v = (sample$v - min(sample$v))/vRange, 
                            e = (sample$E - min(sample$E))/eRange)
sample_Scaled <- as.tbl(sample_Scaled)

# K - количество кластеров
# mykmeans(points, K, center_method, algoritm_option)
# center_method - способ выбора центров:
### "random" - случайные точки выборки
### "equidistant" - выбираются из отсортированной по v выборки с одинаковым шагом
#  algoritm_option - вариант алгоритма:
### "Lazy" - сначала формируются кластеры, потом производится пересчет центров
### "Long" - пересчет центров после каждой добавленной точки

for (K in 4:9){
  newdir <- paste0("Lazy_eq",K)
  dir.create(newdir)      
  cwd <- getwd()          
  setwd(newdir)
  mykmeans(sample_Scaled, K, "equidistant", "Lazy")
  setwd(cwd)
  
  newdir <- paste0("Long_eq",K)
  dir.create(newdir)      
  cwd <- getwd()          
  setwd(newdir)
  mykmeans(sample_Scaled, K, "equidistant", "Long")
  setwd(cwd)  
  
}

K <- 7
comparisonTable <- data.frame(Lazy_centdist = 1:10,
                              Lazy_innerdist = 1,
                              Long_centdist = 1,
                              Long_innerdist = 1)
for (i in 1:10) {
  newdir <- paste0("Rand",i)
  dir.create(newdir)      
  cwd <- getwd()          
  setwd(newdir)
  comparisonTable[i,] <- mykmeans_both(sample_Scaled, K, "random")
  setwd(cwd)
}

