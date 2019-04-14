xv <- function(y) moments["mean", "v"] + rb1 * (moments["deviation", "v"]/moments["deviation", "e"])*(y - moments["mean", "e"])

{
  plot(sample)
  l <- legend("bottomright", 
              legend = c("y(x)", "x(y)"), 
              col = c("darkorchid4", "darkseagreen4"),
              lwd = 3,
              plot = T)
}

{
  plot(sample, 
       pch = 19, 
       cex = 0.9,
       xaxt = "n",
       yaxt = "n",
       ylab = "E",
       main = "Выборка и прямые среднеквадратичной регрессии")
  axis(1, at = vInterv,
       labels = round(vInterv, digits = 1 ) )
  axis(2, at = eInterv, las = 1,
       labels = round(eInterv, digits = 1 ) )
  curve(moments["mean", "e"] + 
          rb1 * (moments["deviation", "e"]/moments["deviation", "v"])*(x - moments["mean", "v"]),
        min(v), max(v),
        n = 10000,
        add = T,
        lwd = 2,
        col = "darkorchid4")
  lines(xv( seq(min(e), max(e), by = 0.01) ),  seq(min(e), max(e), by = 0.01),
        type = "l",
        lwd = 2,
        col = "darkseagreen4")
  
  legend(x = c(l$rect$left, l$rect$left + l$rect$w),
         y = c(l$rect$top, l$rect$top - l$rect$h),
         legend = c("y(x)", "x(y)"), 
         bty = "n",
         col = c("darkorchid4", "darkseagreen4"),
         lwd = 3,
         y.intersp = 0.5,
         title = "Прямые лин.регрессии",
         cex = 1.1,
         text.width = 20)
}

#остаточные дисперсии
resid_var <- c(moments["var", "e"]*(1 - rb2^2),
               moments["var", "v"]*(1 - rb2^2))

# групповые оценки для e (xi) и v (yi)
xi <- data.frame(Intervals = vIntSeq$Intervals, Mids = vIntSeq$Mids)
yi <- data.frame(Intervals = eIntSeq$Intervals, Mids = eIntSeq$Mids)

xi <- cbind(xi, yHits = rowSums(corMat)) #то же самое, что yHits = vIntSeq$Counts
yi <- cbind(yi, xHits = colSums(corMat))

xi <- cbind(xi, Avg_y_for_Xint = rowSums(t(t(corMat) * eIntSeq$Mids))/vIntSeq$Counts)
yi <- cbind(yi, Avg_x_for_Yint = colSums(corMat * vIntSeq$Mids)/eIntSeq$Counts)

xi <- cbind(xi, 
            deltaSq = rowSums(corMat * (rep.int(1, 7) %*% t(yi$Mids) - xi$Avg_y_for_Xint)^2)/xi$yHits)
#дельта квадрат горизонтальная
yi <- cbind(yi, 
            deltaSq = colSums(corMat * t((rep.int(1, 7) %*% t(xi$Mids) - yi$Avg_x_for_Yint)^2))/yi$xHits)

# внутригрупповые дисперсии
Dx_in <- sum(yi$xHits * yi$deltaSq)/N
Dy_in <- sum(xi$yHits * xi$deltaSq)/N

# межгрупповые дисперсии
Dx_out <- sum(yi$xHits * (yi$Avg_x_for_Yint - moments["mean", "v"])^2)/N
Dy_out <- sum(xi$yHits * (xi$Avg_y_for_Xint - moments["mean", "e"])^2)/N

# корреляционное отношение
#Dy_out/(Dy_out + Dy_in)
etaSq <- Dx_out/(Dx_out + Dx_in)
sqrt(Dx_out/(Dx_out + Dx_in)) > rb1


# апроксимация параболой
## для y ~ x
t <- solve(matrix(c(sum(v^4), sum(v^3), sum(v^2), 
                    sum(v^3), sum(v^2), sum(v), 
                    sum(v^2), sum(v), N), 
                  nrow = 3), 
           c(sum(e*v^2), 
             sum(e*v), 
             sum(e)) )

## для x ~ y

t <- c(t, 
       solve(matrix(c(sum(e^4), sum(e^3), sum(e^2), 
                      sum(e^3), sum(e^2), sum(e), 
                      sum(e^2), sum(e), N), 
                    nrow = 3), 
             c(sum(v*e^2), 
               sum(v*e), 
               sum(v)) )
)

{
  plot(sample, 
       pch = 19, 
       cex = 0.9,
       xaxt = "n",
       yaxt = "n",
       ylab = "E",
       main = "Выборка и параболы ср.-кв. регрессии")
  
  legend(x = c(l$rect$left, l$rect$left + l$rect$w),
         y = c(l$rect$top, l$rect$top - l$rect$h),
         inset = 0,
         legend = c("y(x)", "x(y)"), 
         col = c("turquoise4", "sienna4"),
         bty = "n",
         title = "Параболы",
         lwd = 3,
         y.intersp = 0.5,
         cex = 1.1,
         text.width = 20)
  
  axis(1, at = vInterv,
       labels = round(vInterv, digits = 1 ) )
  axis(2, at = eInterv, las = 1,
       labels = round(eInterv, digits = 1 ) )
  curve(t[1]*x^2 + t[2]*x + t[3], min(v), max(v),
        n = 10000,
        add = T,
        lwd = 2,
        col = "turquoise4")
  lines(sapply(seq(min(e), max(e), by = 0.01), function(x) t[4]*x^2 + t[5]*x + t[6]),  
        seq(min(e), max(e), by = 0.01),
        type = "l",
        lwd = 2,
        col = "sienna4")
}
