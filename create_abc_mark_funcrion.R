# Проведение попрепаратного ABC-анализа по отгрузке, доходу, кубам на складе.
library(ggplot2)
library(dplyr)
library(zoo)

create_abc_mark <- function(dat,txt_label)
{
  # Преобразуем вход
  tmp_colnames <- colnames(dat)
  colnames(dat) <- c('id','value')
  # Сдвинем данные вверх, чтобы избавиться от отрицательных значений
  # для классификации это не важно
  dat$value <- dat$value+ifelse(min(dat$value)<0, abs(min(dat$value)),0)
  
  # Сортируем по desc(value)
  dat <- arrange(dat,desc(value))
  
  # Создадим порядковый индекс
  dat$ind <- index(dat)
  
  # Считаем процент по строке, накопительный процент
  mx <- sum(dat$value)
  dat$val_percent <- dat$value/mx
  dat$val_cumpercent <- cumsum(dat$val_percent)
  
  ###################### РАЗБИЕНИЕ 1 ###################################
  # Уравнение линии
  my_line <- function(a,b,x) {return (a+b*x)}
  
  # Возьмем точки начала и конца отрезка
  df_1 <- data.frame(x=c(1,nrow(dat)), y=c(dat[1,'val_cumpercent'],dat[nrow(dat),'val_cumpercent']))  
  
  # Аналитическое построение опорной линии
  m_1 <- lm(y~x, data = df_1)
  a_1 <- as.numeric(m_1$coefficients[1])
  b_1 <- as.numeric(m_1$coefficients[2])
  
  # Проставим точки первой опорной линии
  line_1 <- data.frame(x=1:nrow(dat), y=NA)
  line_1$y <- my_line(a_1,b_1,line_1$x)
  
  # Посчитаем разницу между кумулятивной кривой и опорной линии
  diff_1 <- data.frame(x=line_1$x, y=line_1$y-dat$val_cumpercent)
  # plot(diff_1) # В этом графике идея как посчитать точку касания
  # Теперь, имея координаты точки касания и уравнение параллельной (опорной) прямой,
  # считаем свободный член для уравнения касательной...
  int_point_1 <- data.frame(
    x=diff_1[diff_1$y==min(diff_1$y),]$x, 
    y=dat[diff_1[diff_1$y==min(diff_1$y),]$x,]$val_cumpercent)
  tangent_a_1 <- int_point_1$y-b_1*int_point_1$x
  tangent_line_1 <- data.frame(
    x=dat$ind,
    y=my_line(tangent_a_1,b_1,dat$ind))
  
  
  ###################### РАЗБИЕНИЕ 2 ###################################
  # Возьмем точки начала и конца отрезка
  df_2 <- data.frame(x=c(int_point_1$x+1,nrow(dat)), y=c(dat[int_point_1$x+1,'val_cumpercent'],dat[nrow(dat),'val_cumpercent']))  
  
  # Аналитическое построение опорной линии
  m_2 <- lm(y~x, data = df_2)
  a_2 <- as.numeric(m_2$coefficients[1])
  b_2 <- as.numeric(m_2$coefficients[2])
  
  # Проставим точки первой опорной линии
  line_2 <- data.frame(x=df_2[1,'x']:df_2[2,'x'], y=NA)
  line_2$y <- my_line(a_2,b_2,line_2$x)
  
  # Посчитаем разницу между кумулятивной кривой и опорной линии
  diff_2 <- data.frame(x=line_2$x, y=line_2$y-dat[dat$ind>=df_2[1,'x'],]$val_cumpercent)
  
  # Теперь, имея координаты точки касания и уравнение параллельной (опорной) прямой,
  # считаем свободный член для уравнения касательной...
  int_point_2 <- data.frame(
    x=diff_2[diff_2$y==min(diff_2$y),]$x, 
    y=dat[diff_2[diff_2$y==min(diff_2$y),]$x,]$val_cumpercent)
  tangent_a_2 <- int_point_2$y-b_2*int_point_2$x
  tangent_line_2 <- data.frame(
    x=line_2$x,
    y=my_line(tangent_a_2,b_2,line_2$x))
  
  # Проставляем группы
  dat$abc_group <- "C"
  dat[dat$ind<=int_point_2$x,]$abc_group <- "B"
  dat[dat$ind<=int_point_1$x,]$abc_group <- "A"  
  
  ################################################################################################
  #                  ВЫВОД ДАННЫХ
  ################################################################################################
  # Собираем данные в кучу для графиков
  g_curve <- dat[,c('ind','val_cumpercent')]
  g_base_1 <- df_1
  g_tangent_1 <- tangent_line_1[tangent_line_1$y<1.2,]
  g_dotted_x_1 <- data.frame(x=rep(int_point_1$x,2), y=c(0, int_point_1$y))
  g_dotted_y_1 <- data.frame(x=c(0,int_point_1$x), y=rep(int_point_1$y,2))
  
  
  g_base_2 <- df_2
  g_tangent_2 <- tangent_line_2[tangent_line_2$y<1.2,]
  g_dotted_x_2 <- data.frame(x=rep(int_point_2$x,2), y=c(0, int_point_2$y))
  g_dotted_y_2 <- data.frame(x=c(0,int_point_2$x), y=rep(int_point_2$y,2))
  
  # Рисуем расчет
  p <- ggplot()+theme_bw()+ggtitle(paste('Расчет зон для',txt_label))+
    geom_line(aes(x=ind, y=val_cumpercent), data = g_curve, col='black')+ 
    ######################################################################################  
  geom_line(aes(x=x, y=y),           data = g_base_1,col='green')+
    geom_line(aes(x=x, y=y),           data = g_tangent_1,col='green')+
    geom_point(aes(x=x, y=y),          data = g_dotted_x_1[2,],col='darkgreen',size=3)+
    geom_line(aes(x=x, y=y),           data = g_dotted_x_1,col='darkgreen', linetype=2)+
    geom_line(aes(x=x, y=y),           data = g_dotted_y_1,col='darkgreen', linetype=2)+
    ######################################################################################  
  geom_line(aes(x=x, y=y),           data = g_base_2,col='blue')+
    geom_line(aes(x=x, y=y),           data = g_tangent_2,col='blue')+
    geom_point(aes(x=x, y=y),          data = g_dotted_x_2[2,],col='darkblue',size=3)+
    geom_line(aes(x=x, y=y),           data = g_dotted_x_2,col='darkblue', linetype=2)+
    geom_line(aes(x=x, y=y),           data = g_dotted_y_2,col='darkblue', linetype=2)
  
  ################################################################################################
  #                  Возврат функции
  ################################################################################################
  dat <- dat[,c('id','abc_group')]
  colnames(dat) <- c(tmp_colnames[1],'ABC_GROUP')
  return(list(dat=dat, graph=p))
}
