
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)

setwd("C:/Users/Aeint Thet Ngon/Documents/Portfolio/Crime DC")

files=list.files(pattern="*.csv")
names=paste("X", 2008:2017, sep="")

dc=NULL

for (i in 1:length(files)){
  assign(names[i], read.csv(files[i]))
  dc <- rbind(dc,read.csv(files[i]))
}

dc <- dc %>% mutate(DT=as.POSIXct(strptime(substring(REPORT_DAT, 1, 19), "%Y-%m-%dT%H:%M:%S"))) %>%
  mutate(year=strftime(DT, "%Y"), hour=strftime(DT, "%H"), wday=weekdays(DT))

X2016 <- X2016 %>% mutate(DT=as.POSIXct(strptime(substring(REPORT_DAT, 1, 19), "%Y-%m-%dT%H:%M:%S"))) %>%
  mutate(year=strftime(DT, "%Y"), hour=strftime(DT, "%H"), wday=weekdays(DT))

X2008 <- X2008 %>% mutate(DT=as.POSIXct(strptime(substring(REPORT_DAT, 1, 19), "%Y-%m-%dT%H:%M:%S"))) %>%
  mutate(year=strftime(DT, "%Y"), hour=strftime(DT, "%H"), wday=weekdays(DT))

X2017 <- X2017 %>% mutate(DT=as.POSIXct(strptime(substring(REPORT_DAT, 1, 19), "%Y-%m-%dT%H:%M:%S"))) %>%
  mutate(year=strftime(DT, "%Y"), hour=strftime(DT, "%H"), wday=weekdays(DT))

dc_l <- dc %>% group_by(year, OFFENSE) %>% summarise(n=n()) %>% filter(year != "2017")

gg <- ggplot(dc, aes(year))
gg + geom_bar(aes(fill = OFFENSE)) +
  scale_fill_manual(values=c("#860101", "#2D1818","#1F8802", "#0329FB", "#FB0303", "#8C03FB","#B18754", "#03EFFB", "#A4FB03"))

ggplot(data=dc_l, aes(x=year, y=n, group=OFFENSE, colour=OFFENSE)) +geom_line() +geom_point()+
  scale_colour_manual(values=c("#860101", "#2D1818","#1F8802", "#0329FB", "#FB0303", "#8C03FB","#B18754", "#03EFFB", "#A4FB03"))

dc1 <- dc %>% filter(year!= "2017") %>%group_by(year, hour) %>% summarise(n=n()) %>% arrange(year, hour) %>% transform(year=as.factor(year), hour=as.factor(hour))

dc_mat <- acast(dc1, year~hour, value.var="n")
plot_ly(x=colnames(dc_mat), y=rownames(dc_mat), z=dc_mat, type="heatmap")

plot_ly(x=colnames(chi_mat), y=rownames(chi_mat), z=chi_mat, type="heatmap")

X16 <- X2016 %>% group_by(OFFENSE, hour) %>% summarise(n=n())
X16_mat <- acast(X16, OFFENSE~hour, value.var="n")

plot_ly(x=colnames(X16_mat), y=rownames(X16_mat), z=X16_mat, type="heatmap")

X8 <- X2008 %>% group_by(OFFENSE, hour) %>% summarise(n=n())
X8_mat <- acast(X8, OFFENSE~hour, value.var="n")

plot_ly(x=colnames(X8_mat), y=rownames(X8_mat), z=X8_mat, type="heatmap")

#Getting some basic statistics #DC
summary(as.factor(strftime(dc$DT, "%m")))/330685

dc <- dc %>% mutate(DT=as.POSIXct(strptime(substring(REPORT_DAT, 1, 19), "%Y-%m-%dT%H:%M:%S"))) %>%
  mutate(year=strftime(DT, "%Y"), hour=strftime(DT, "%H"), wday=weekdays(DT), month=strftime(DT, "%m"))

dc_y <- dc %>% group_by(year, month) %>% summarise(n=n())
ggplot(data=dc_y, aes(x=year, y=n, group=month, colour=month)) +geom_line() +geom_point()

gg <- ggplot(dc, aes(year))
gg + geom_bar(aes(fill = month))

temp_june <- dc %>% mutate(m=as.integer(month)) %>% group_by(year) %>% summarise(n=sum(m<=6))

temp_homicide <- dc %>% filter(OFFENSE=="HOMICIDE")%>% group_by(year) %>% summarise(n=n())

temp_ward <- dc %>% filter(year!="2017") %>% group_by(year, WARD) %>% summarise(n=n()) %>% transform(year=as.factor(year),WARD=as.factor(WARD)) %>% na.omit()

temp_o <- dc %>% filter(year!="2017") %>% group_by(WARD, OFFENSE) %>% summarise(n=n())

xtabs(n~year+WARD, temp_ward)

xtabs(n~OFFENSE+WARD, temp_o)

ggplot(data=temp_ward, aes(x=year, y=n, group=WARD, colour=WARD)) +geom_line() +geom_point() +
  scale_color_brewer(palette="Dark2")

p <- ggplot(temp_ward, aes(x =WARD, y = year)) 
p+geom_point( aes(size=n),shape=21, colour="black", fill="skyblue")+
  theme(panel.background=element_blank(), panel.border = element_rect(colour = "blue", fill=NA, size=1))+
  scale_size_area(max_size=18)

p <- ggplot(temp_o, aes(x =WARD, y = OFFENSE)) 
p+geom_point( aes(size=n),shape=21, colour="black", fill="skyblue")+
  theme(panel.background=element_blank(), panel.border = element_rect(colour = "blue", fill=NA, size=1))+
  scale_size_area(max_size=18)

