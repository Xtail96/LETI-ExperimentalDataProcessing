# vIntSeq
# eIntSeq

colnames(vIntSeq) <- c('Intervals', 'Counts', "Mids")
colnames(eIntSeq) <- c('Intervals', 'Counts', "Mids")

#добавим частоты
vIntSeq <- cbind(vIntSeq, Freq = vIntSeq$Counts/N)
eIntSeq <- cbind(eIntSeq, Freq = eIntSeq$Counts/N)

#условные варианты
vIntSeq <- cbind(vIntSeq, ui = (vIntSeq$Mids - vIntSeq$Mids[4])/vIntLength)
eIntSeq <- cbind(eIntSeq, ui = (eIntSeq$Mids - eIntSeq$Mids[4])/eIntLength)
vC <- vIntSeq$Mids[4]
eC <- eIntSeq$Mids[4]

# чисто для таблицы из отчета
#vIntSeq <- cbind(vIntSeq, step1 = (vIntSeq$ui * vIntSeq$Count))
#vIntSeq <- cbind(vIntSeq, step2 = (vIntSeq$ui * vIntSeq$step1))
#vIntSeq <- cbind(vIntSeq, step3 = (vIntSeq$ui * vIntSeq$step2))
#vIntSeq <- cbind(vIntSeq, step4 = (vIntSeq$ui * vIntSeq$step3))
# (x_i + 1)^4 * n_i
#vIntSeq <- cbind(vIntSeq, check_ = ((vIntSeq$ui + 1)^4 * vIntSeq$Count))


#слагаемые первого условного момента
vIntSeq <- cbind(vIntSeq, M1 = (vIntSeq$ui * vIntSeq$Freq))
eIntSeq <- cbind(eIntSeq, M1 = (eIntSeq$ui * eIntSeq$Freq))

#слагаемые второго условного момента
vIntSeq <- cbind(vIntSeq, M2 = (vIntSeq$ui * vIntSeq$M1))
eIntSeq <- cbind(eIntSeq, M2 = (eIntSeq$ui * eIntSeq$M1))

#слагаемые третьего условного момента
vIntSeq <- cbind(vIntSeq, M3 = (vIntSeq$ui * vIntSeq$M2))
eIntSeq <- cbind(eIntSeq, M3 = (eIntSeq$ui * eIntSeq$M2))

#слагаемые четвертого условного момента
vIntSeq <- cbind(vIntSeq, M4 = (vIntSeq$ui * vIntSeq$M3))
eIntSeq <- cbind(eIntSeq, M4 = (eIntSeq$ui * eIntSeq$M3))

# проверочный столбец
vIntSeq <- cbind(vIntSeq, check_ = ((vIntSeq$ui + 1)^4 * vIntSeq$Freq))
eIntSeq <- cbind(eIntSeq, check_ = ((eIntSeq$ui + 1)^4 * eIntSeq$Freq))

# чисто для таблицы из отчета
my_eIntSeq <- cbind(eIntSeq, step1 = (eIntSeq$ui * eIntSeq$Count))
my_eIntSeq <- cbind(my_eIntSeq, step2 = (my_eIntSeq$ui * my_eIntSeq$step1))
my_eIntSeq <- cbind(my_eIntSeq, step3 = (my_eIntSeq$ui * my_eIntSeq$step2))
my_eIntSeq <- cbind(my_eIntSeq, step4 = (my_eIntSeq$ui * my_eIntSeq$step3))
# (x_i + 1)^4 * n_i
my_eIntSeq <- cbind(my_eIntSeq, check_ = ((my_eIntSeq$ui + 1)^4 * my_eIntSeq$Count))

moments <- data.frame(
  v = colSums(vIntSeq[c("M1", "M2", "M3", "M4")]), 
  e = colSums(eIntSeq[c("M1", "M2", "M3", "M4")]))

###проверка
## vIntSeq$M4 + 4*vIntSeq$M3 + 6*vIntSeq$M2 + 4*vIntSeq$M1 + vIntSeq$Freq
## ((vIntSeq$ui + 1)^4) * vIntSeq$Freq
#
## sum(eIntSeq$M4 + 4*eIntSeq$M3 + 6*eIntSeq$M2 + 4*eIntSeq$M1 + eIntSeq$Freq)
# sum(((eIntSeq$ui + 1)^4) * eIntSeq$Freq)
###

#выборочное среднее
moments <- rbind(moments, mean = c(moments$v[1] * vIntLength + vC, 
                                   moments$e[1] * eIntLength + eC))

# дисперсия
moments <- rbind(moments, dispersion = c((moments$v[2] - moments$v[1]^2)*(vIntLength^2), 
                                  (moments$e[2] - moments$e[1]^2)*(eIntLength^2)))
#исправленная дисперсия (несмещенная)
moments["var",] <- moments["dispersion",] * (N/(N-1))
moments <- moments[-c(6),]

#несмещенное средн.кв. отлонение
moments <- rbind(moments, deviation = sqrt(moments["var",]))

#оценка асимметрии
moments <- rbind(moments,
                 asymmetry = (moments[3,] - 3*moments[2,]*moments[1,] + 2*(moments[1,]^3))*((c(vIntLength, eIntLength)/moments["deviation",])^3))

#оценка эксцесса
iod <- c(vIntLength, eIntLength)/moments["deviation",]
iod <- iod^4
m4 <- moments[4,] - 4*moments[3,]*moments[1,] + 6*moments[2,]*(moments[1,])^2 - 3*(moments[1,]^4)
moments <- rbind(moments, excess = m4*iod - 3)
