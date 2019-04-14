#интервальные оценки для мат.ожидания
##надежность 
y <- 0.95
### распределение Стьюдента - симметричное, поэтому делим на два
ty <- -qt((1-y)/2, df = N-1)

deviation <- moments$v[8];
conf <- (deviation * ty)/sqrt(N)
### доверительный интервал для мат.ожидания v
vMeanConfInt <- c(moments$v[5] - conf, moments$v[5] + conf)

deviation <- moments$e[8]
conf <- (deviation * ty)/sqrt(N)
### доверительный интервал для мат.ожидания Е
eMeanConfInt <- c(moments$e[5] - conf, moments$e[5] + conf) 

#интервальные оценки для СКВО
vDevConfInt <- sqrt((N-1)*moments["var","v"]/qchisq(c((1+y)/2, (1-y)/2), df = N-1))
eDevConfInt <- sqrt((N-1)*moments["var","e"]/qchisq(c((1+y)/2, (1-y)/2), df = N-1))

#проверка простой гипотезы о нормальном распределении
a <- pnorm(vInterv[2:7], mean = moments["mean", "v"], sd = moments["deviation", "v"])
vHyp <- cbind(vIntSeq[, 1:4], pThLB = c(0, a), pThRB = c(a, 1))
vHyp <- cbind(vHyp, ThFreq = vHyp$pThRB - vHyp$pThLB)
vHyp <- cbind(vHyp, ThCounts = vHyp$ThFreq * N)
vChiSq <- sum( ((vHyp$Counts - vHyp$ThCounts)^2) / vHyp$ThCounts )
## a - уровень значимости, вероятность отклонить верную гипотезу
a <- 0.05
if (vChiSq > qchisq(1-a, df = numClass - 3)) 'Reject at a = 0.05' else 'Accept at a = 0.05'

a <- pnorm(eInterv[2:7], mean = moments["mean", "e"], sd = moments["deviation", "e"])
eHyp <- cbind(eIntSeq[, 1:4], pThLB = c(0, a), pThRB = c(a, 1))
eHyp <- cbind(eHyp, ThFreq = eHyp$pThRB - eHyp$pThLB)
eHyp <- cbind(eHyp, ThCounts = eHyp$ThFreq * N)
eChiSq <- sum( ((eHyp$Counts - eHyp$ThCounts)^2) / eHyp$ThCounts )
## a - уровень значимости, вероятность отклонить верную гипотезу
a <- 0.05
test <- qchisq(1-a, df = numClass - 3)
if (eChiSq > qchisq(1-a, df = numClass - 3)) 'Reject at a = 0.05' else 'Accept at a = 0.05'

