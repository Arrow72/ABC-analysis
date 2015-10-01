# Проведение попрепаратного ABC-анализа по отгрузке, доходу, кубам на складе.
library(ggplot2)
library(dplyr)
library(zoo)

# Получаем данные
setwd('N:/DEP_COMPROD/oucism/Федин/00) Проекты/ОУЦСМ.Текущие дела/20150915) Оптимизация ТЗ/R.ABC-analysis')
source('create_abc_mark_funcrion.r')
d <- read.csv(file = 'abc_data_ex1.csv', sep=';', header = TRUE, dec = ',')
d_cur <- d[d$PERIOD=='01.09.2015' & !d$MEDICINE_CODE==-1,]
d_cur <- d_cur[,c('MEDICINE_CODE','CURRENT_WABC_NAME','РУБЛИ_ОТГРУЗКА', 'ВД','КУБЫ_ТЗ')]

############################################################################################

a_sale <- create_abc_mark(d_cur[,c('MEDICINE_CODE','РУБЛИ_ОТГРУЗКА')], 'ОТГРУЗКА')
d_cur <- inner_join(d_cur,a_sale$dat)
names(d_cur)[names(d_cur)=="ABC_GROUP"] <- "ABC_GROUP_SALE"

a_vd <- create_abc_mark(d_cur[,c('MEDICINE_CODE','ВД')], 'ВД')
d_cur <- inner_join(d_cur,a_vd$dat)
names(d_cur)[names(d_cur)=="ABC_GROUP"] <- "ABC_GROUP_VD"

a_tz <- create_abc_mark(d_cur[,c('MEDICINE_CODE','КУБЫ_ТЗ')], 'КУБЫ_ТЗ')
a_tz$graph
d_cur <- inner_join(d_cur,a_tz$dat)
names(d_cur)[names(d_cur)=="ABC_GROUP"] <- "ABC_GROUP_TZ"

a_sale$graph
a_vd$graph
a_tz$graph
write.csv(d_cur, file = 'res.csv')
write.table(d_cur,"clipboard", sep="\t", dec = ',',row.names = FALSE) 
