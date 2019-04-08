#корреляционная таблица
corTab <- table(cut(sample$v, vInterv, right = F, include.lowest = T),
                cut(sample$E, eInterv, right = F, include.lowest = T))

## обращение к элементам corTab[x, y], x - номер интервала для выборки v (от 1 до 7),
##                                     y - номер интервала для выборки E

#коэффициент корреляции
corMat <- as.matrix(corTab)

rb1 <- t(t(corMat * vIntSeq$Mids) * eIntSeq$Mids)
rb1 <- sum(rb1) - N*moments["mean", "v"]*moments["mean", "e"]
rb1 <- rb1/(N*moments["deviation", "v"]*moments["deviation", "e"])

rb2 <- t(t(corMat * vIntSeq$ui) * eIntSeq$ui)
rb2 <- sum(rb2) - N*moments["M1", "v"]*moments["M1", "e"]
rb2 <- rb2*vIntLength*eIntLength
rb2 <- rb2/(N*moments["deviation", "v"]*moments["deviation", "e"])

#доверительный интервал c надежностью a
a <- 0.95
z <- 0.5*log((1+rb2)/(1-rb2))
z <- z + c(-qnorm((1+a)/2)/sqrt(N-3), qnorm((1+a)/2)/sqrt(N-3))
rConfInt <- (exp(2*z) - 1)/(exp(2*z) + 1)

#проверка гипотезы H: r = 0
## a - уровень значимости, вероятность отклонить верную гипотезу
a <- 0.05
Tr <- rb2 * sqrt(numClass-2)/(1 - rb2^2) 
ty <- qt(1-a/2, df = numClass-2) #критическое значение
if (Tr > ty) 'Reject at a = 0.05' else 'Accept at a = 0.05'


