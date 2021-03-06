# ���������� �������������� ABC-������� �� ��������, ������, ����� �� ������.
library(ggplot2)
library(dplyr)
library(zoo)

# �������� ������
setwd('N:/DEP_COMPROD/oucism/�����/00) �������/�����.������� ����/20150915) ����������� ��/R.ABC-analysis')
source('create_abc_mark_funcrion.r')
d <- read.csv(file = 'abc_data_ex1.csv', sep=';', header = TRUE, dec = ',')
d_cur <- d[d$PERIOD=='01.09.2015' & !d$MEDICINE_CODE==-1,]
d_cur <- d_cur[,c('MEDICINE_CODE','CURRENT_WABC_NAME','�����_��������', '��','����_��')]

############################################################################################

a_sale <- create_abc_mark(d_cur[,c('MEDICINE_CODE','�����_��������')], '��������')
d_cur <- inner_join(d_cur,a_sale$dat)
names(d_cur)[names(d_cur)=="ABC_GROUP"] <- "ABC_GROUP_SALE"

a_vd <- create_abc_mark(d_cur[,c('MEDICINE_CODE','��')], '��')
d_cur <- inner_join(d_cur,a_vd$dat)
names(d_cur)[names(d_cur)=="ABC_GROUP"] <- "ABC_GROUP_VD"

a_tz <- create_abc_mark(d_cur[,c('MEDICINE_CODE','����_��')], '����_��')
a_tz$graph
d_cur <- inner_join(d_cur,a_tz$dat)
names(d_cur)[names(d_cur)=="ABC_GROUP"] <- "ABC_GROUP_TZ"

a_sale$graph
a_vd$graph
a_tz$graph
write.csv(d_cur, file = 'res.csv')
write.table(d_cur,"clipboard", sep="\t", dec = ',',row.names = FALSE) 
