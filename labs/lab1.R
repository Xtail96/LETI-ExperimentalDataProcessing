install.packages("dplyr");
library("dplyr", lib.loc="~/R/R-3.5.2/library");
library("stats", lib.loc="~/R/R-3.5.2/library");
library(readr);

sample <- read_csv("sample_99.csv")
#размер выборки
N <- 99
#количество интервалов
numClass <- 1 + floor(log2(N))

#ранжированные тибблы
a <- arrange(sample, v)
b <- arrange(sample, E)

#ранжированные массивы
v <- a[[1]]
e <- b[[2]]

#вариационные ряды
vStat <- count(a, v)
eStat <- count(b, E)

#размах выборок
vRange <- diff(range(v))
eRange <- diff(range(e))

#длины интервалов
vIntLength <- vRange/numClass
eIntLength <- eRange/numClass

#границы интервалов
vInterv <- seq(from = min(v), to = max(v), by = vRange/numClass)
eInterv <- seq(from = min(e), to = max(e), by = eRange/numClass)

#интервальные ряды
vIntSeq <- table(cut(v, vInterv, right = F, include.lowest = T))
vIntSeq <- as.data.frame(vIntSeq)
vIntSeq <- cbind(vIntSeq, vInterv[1:7] + (vRange/(2*numClass)))
colnames(vIntSeq) <- c('Intervals', 'Freq', "Mids")
vIntSeq <- as.tbl(vIntSeq)

eIntSeq <- table(cut(e, eInterv, right = F, include.lowest = T))
eIntSeq <- as.data.frame(eIntSeq)
eIntSeq <- cbind(eIntSeq, eInterv[1:7] + (eRange/(2*numClass)))
colnames(eIntSeq) <- c('Intervals', 'Freq', "Mids")
eIntSeq <- as.tbl(eIntSeq)

#Эмпирические функции распределения
#Абсолютная ЭФР
## Это если определять F(x) = P(X < x) (так, как сам Середа определяет)
plot(stepfun(vIntSeq$Mids, cumsum(c(0, vIntSeq$Freq)), right = F), 
     verticals = F,
     lab = c(30, 15, 8),
     main = "Абс. ЭФР для v",
     ylab = "для F(x) = P(X < x)",
     xlab = "v", 
     col = "hotpink4",
     lwd = 2,
     cex.points = 1.5,
     pch = 60,
     xaxt = "n",
     yaxt = "n")
axis(1, at = vIntSeq$Mids, labels = round(vIntSeq$Mids, digits = 1))
axis(2, at = cumsum(c(0, vIntSeq$Freq)), 
     labels = cumsum(c(0, vIntSeq$Freq)), las = 1)

## Это если определять F(x) = P(X <= x) (а так определяют в большинстве источников)
plot(stepfun(vIntSeq$Mids, cumsum(c(0, vIntSeq$Freq)), right = T), 
     verticals = F,
     lab = c(30, 15, 8),
     main = "Абс. ЭФР для v",
     ylab = "для F(x) = P(X <= x)",
     xlab = "v", 
     col = "hotpink4",
     lwd = 2,
     xaxt = "n",
     yaxt = "n")
axis(1, at = vIntSeq$Mids, labels = round(vIntSeq$Mids, digits = 1))
axis(2, at = cumsum(c(0, vIntSeq$Freq)), 
     labels = cumsum(c(0, vIntSeq$Freq)), las = 1)

## Это F(x) = P(X < x)
plot(stepfun(eIntSeq$Mids, cumsum(c(0, eIntSeq$Freq)), right = F), 
     verticals = F, 
     lab = c(30, 15, 8),
     main = "Абс. ЭФР для E",
     ylab = "для F(x) = P(X < x)",
     xlab = "E", 
     col = "purple4",
     lwd = 2,
     cex.points = 1.5,
     pch = 60,
     xaxt = "n",
     yaxt = "n")
axis(1, at = eIntSeq$Mids, labels = round(eIntSeq$Mids, digits = 1))
axis(2, at = cumsum(c(0, eIntSeq$Freq)), 
     labels = cumsum(c(0, eIntSeq$Freq)), las = 1)

## Это F(x) = P(X <= x)
plot(stepfun(eIntSeq$Mids, cumsum(c(0, eIntSeq$Freq)), right = T), 
     verticals = F,
     lab = c(30, 15, 8),
     main = "Абс. ЭФР для E",
     ylab = "для F(x) = P(X <= x)",
     xlab = "E", 
     col = "purple4",
     lwd = 2,
     xaxt = "n",
     yaxt = "n")
axis(1, at = eIntSeq$Mids, labels = round(eIntSeq$Mids, digits = 1))
axis(2, at = cumsum(c(0, eIntSeq$Freq)), 
     labels = cumsum(c(0, eIntSeq$Freq)), las = 1)

#Относительная ЭФР
## Это F(x) = P(X < x) для v
plot(stepfun(vIntSeq$Mids, cumsum(c(0, vIntSeq$Freq))/N, right = F), 
     verticals = F, 
     lab = c(15, 5, 8),
     main = "Отн. ЭФР для v",
     ylab = "для F(x) = P(X < x)",
     xlab = "v", 
     col = "hotpink4",
     lwd = 2,
     cex.points = 1.5,
     pch = 60,
     xaxt = "n",
     yaxt = "n")
axis(1, at = vIntSeq$Mids, labels = round(vIntSeq$Mids, digits = 2))
axis(2, at = cumsum(c(0, vIntSeq$Freq))/N, 
     labels = round(cumsum(c(0, vIntSeq$Freq))/N, digits = 2), las = 1)

## Это F(x) = P(X <= x) для v
plot(stepfun(vIntSeq$Mids, cumsum(c(0, vIntSeq$Freq))/N, right = T), 
     verticals = F,
     lab = c(15, 5, 8),
     main = "Отн. ЭФР для v",
     ylab = "для F(x) = P(X <= x)",
     xlab = "v", 
     col = "hotpink4",
     lwd = 2,
     xaxt = "n",
     yaxt = "n")
axis(1, at = vIntSeq$Mids, labels = round(vIntSeq$Mids, digits = 2))
axis(2, at = cumsum(c(0, vIntSeq$Freq))/N, 
     labels = round(cumsum(c(0, vIntSeq$Freq))/N, digits = 2), las = 1)

## Это F(x) = P(X < x) для Е
plot(stepfun(eIntSeq$Mids, cumsum(c(0, eIntSeq$Freq))/N, right = F), 
     verticals = F, 
     lab = c(15, 5, 8),
     main = "Отн. ЭФР для E",
     ylab = "для F(x) = P(X < x)",
     xlab = "E", 
     col = "purple4",
     lwd = 2,
     cex.points = 1.5,
     pch = 60,
     xaxt = "n",
     yaxt = "n")
axis(1, at = eIntSeq$Mids, labels = round(eIntSeq$Mids, digits = 2))
axis(2, at = cumsum(c(0, eIntSeq$Freq))/N, 
     labels = round(cumsum(c(0, eIntSeq$Freq))/N, digits = 2), las = 1)

## Это F(x) = P(X <= x) для E
plot(stepfun(eIntSeq$Mids, cumsum(c(0, eIntSeq$Freq))/N, right = T), 
     verticals = F, 
     lab = c(15, 5, 8),
     main = "Отн. ЭФР для E",
     ylab = "для F(x) = P(X <= x)",
     xlab = "E", 
     col = "purple4",
     lwd = 2,
     xaxt = "n",
     yaxt = "n")
axis(1, at = eIntSeq$Mids, labels = round(eIntSeq$Mids, digits = 2))
axis(2, at = cumsum(c(0, eIntSeq$Freq))/N, 
     labels = round(cumsum(c(0, eIntSeq$Freq))/N, digits = 2), las = 1)


###vecdf <- ecdf(rep(vInterv, vIntSeq$Freq), right = T)
###eecdf <- ecdf(rep(eIntSeq$Mids, eIntSeq$Freq))

#Гистограммы
vhist <- hist(v, breaks = vInterv, right = F, include.lowest = T)
#vhist$density <- vhist$density * vIntLength

{
  plot(vhist, 
       col = "navajowhite3",
       main = "Гистограмма для v",
       ylab = "Абс. частота",
       xlab = "v",
       xaxt = "n",
       yaxt = "n")
  axis(1, at = c(vInterv, vIntSeq$Mids),
       labels = round( c(vInterv, vIntSeq$Mids), digits = 1 ) )
  axis(2, at = c(0, vhist$counts), las = 1,
       labels = c(0, vhist$counts))
}

{
  plot(vhist, 
       freq = F,
       col = "lightgreen",
       main = "Гистограмма для v",
       ylab = "",
       xlab = "v",
       xaxt = "n",
       yaxt = "n")
  mtext(text = "Плотность", side = 3, line = 0, 
        las = 1, adj = 1, padj=0, at = 345)
  axis(1, at = c(vInterv, vIntSeq$Mids),
       labels = round( c(vInterv, vIntSeq$Mids), digits = 1 ) )
  axis(2, at = c(0, vhist$density, 1), las = 1,
       labels = round( c(0, vhist$density, 1), digits = 4 ) )
}

ehist <- hist(e, breaks = eInterv, right = F, include.lowest = T)
#ehist$density <- ehist$density * eIntLength

{
  plot(ehist, 
       col = "moccasin",
       main = "Гистограмма для E",
       ylab = "Абс. частота",
       xlab = "E",
       xaxt = "n",
       yaxt = "n")
  axis(1, at = c(eInterv, eIntSeq$Mids),
       labels = round( c(eInterv, eIntSeq$Mids), digits = 1 ) )
  axis(2, at = c(0, ehist$counts), las = 1,
       labels = c(0, ehist$counts))
}

{
  plot(ehist, 
       freq = F,
       main = "Гистограмма для E",
       ylab = "",
       xlab = "E",
       col = "mediumspringgreen",
       xaxt = "n",
       yaxt = "n")
  mtext(text = "Плотность", side = 3, line = 0, 
        las = 1, adj = 1, padj=0, at = 80)
  axis(1, at = c(eInterv, eIntSeq$Mids),
       labels = round( c(eInterv, eIntSeq$Mids), digits = 1 ) )
  axis(2, at = c(0, ehist$density, 1), las = 1,
       labels = round( c(0, ehist$density, 1), digits = 4 ) )
}

#Полигоны
{
  plot(vhist$mids, vhist$counts,
       #  xlim = c(min(vhist$breaks), max(vhist$breaks)),
       ylim = c(0, max(vhist$counts)),
       type = "o", 
       xlab = "v", 
       ylab = "Частота", 
       main = "Частотный полигон для v", 
       pch = 19,
       col = "royalblue4",
       xaxt = "n",
       yaxt = "n")
  axis(1, at = vIntSeq$Mids,
       labels = round(vIntSeq$Mids, digits = 1 ) )
  axis(2, at = c(0, vhist$counts), las = 1,
       labels = c(0, vhist$counts))
  polygon(vhist$mids[c(1, 1:7, 7)], 
          c(0, vhist$counts, 0),
          border = "royalblue4",
          col = "pink")
}

{
  plot(vhist$mids, vhist$density,
       #     xlim = range(vhist$breaks),
       ylim = c(0, max(vhist$density)),
       type = "o", 
       xlab = "v", 
       ylab = "", 
       main = "Полигон плотности для v", 
       pch = 19,
       col = "palevioletred4",
       xaxt = "n",
       yaxt = "n")
  mtext(text = "Плотность", side = 3, line = 0, 
        las = 1, adj = 1, padj=0, at = 365)
  axis(1, at = vIntSeq$Mids,
       labels = round(vIntSeq$Mids, digits = 1 ) )
  axis(2, at = c(0, vhist$density, 1), las = 1,
       labels = round( c(0, vhist$density, 1), digits = 4 ) )
  polygon(vhist$mids[c(1, 1:7, 7)], 
          c(0, vhist$density, 0),
          border = "palevioletred4",
          col = "peachpuff")
}

{
  plot(ehist$mids, ehist$counts,
       #    xlim = c(min(ehist$breaks), max(ehist$breaks)),
       ylim = c(0, max(ehist$counts)),
       type = "o", 
       xlab = "E", 
       ylab = "Частота", 
       main = "Частотный полигон для E", 
       pch = 19,
       col = "tomato4",
       xaxt = "n",
       yaxt = "n")
  axis(1, at = eIntSeq$Mids,
       labels = round(eIntSeq$Mids, digits = 1 ) )
  axis(2, at = c(0, ehist$counts), las = 1,
       labels = c(0, ehist$counts))
  polygon(ehist$mids[c(1, 1:7, 7)], 
          c(0, ehist$counts, 0),
          border = "tomato4",
          col = "thistle2")
}

{
  plot(ehist$mids, ehist$density,
       #    xlim = range(ehist$breaks),
       ylim = c(0, max(ehist$density)),
       type = "o", 
       xlab = "E", 
       ylab = "", 
       main = "Полигон плотности для E", 
       pch = 19,
       col = "darkred",
       xaxt = "n",
       yaxt = "n")
  mtext(text = "Плотность", side = 3, line = 0, 
        las = 1, adj = 1, padj=0, at = 88)
  axis(1, at = eIntSeq$Mids,
       labels = round(eIntSeq$Mids, digits = 1 ) )
  axis(2, at = c(0, ehist$density, 1), las = 1,
       labels = round( c(0, ehist$density, 1), digits = 4 ) )
  polygon(ehist$mids[c(1, 1:7, 7)], 
          c(0, ehist$density, 0),
          border = "darkred",
          col = "slategray1")
}
