#### Load Packages ####
library(agricolae)
library(dplyr)
library(ggbeeswarm)
library(ggh4x)
library(ggplot2)
library(patchwork)
library(tidyr)


#### Figure 1A ####

# Load data set
dat.Fig1A <- read.csv("data_Fig1A.csv", stringsAsFactors = T)

# Convert date
dat.Fig1A$Date <- as.Date(dat.Fig1A$Date, format="%Y-%m-%d")

# Create LT50 data set
dat.Fig1A.LT50 <- dat.Fig1A %>% 
  group_by(Sample, Cultivar, Trt, Date) %>% 
  dplyr::summarise(LT50 = mean(LTE)) %>% 
  ungroup()

# Subset each data set for primary treatments
dat.Fig1A.sub <- dat.Fig1A %>% 
  filter(Trt %in% c("constant", "field", "fluctuating"))
dat.Fig1A.LT50.sub <- dat.Fig1A.LT50 %>% 
  filter(Trt %in% c("constant", "field", "fluctuating"))


# Colors for figure
pal <- c("#CD534CFF", "#0073C2FF", "#EFC000FF") #constant, #field, #fluctuating
pal2= c("#1a476f", "#ffd200","#9c8847","#e37e00" , "#c10534")
pal3= c("#0073C2FF", "#EFC000FF","#8F7700FF","#A73030FF" , "#CD534CFF")

fig1a=ggplot()+
  labs(x="", y=expression("Cold hardiness (°C)"), color="")+
  geom_line(data=dat.Fig1A.LT50.sub, aes(x=Date, y=LT50, color=Trt), stat="summary")+
  geom_point(data=dat.Fig1A.LT50.sub, aes(x=Date, y=LT50, color=Trt), stat="summary")+
  geom_point(data=subset(dat.Fig1A.LT50, Trt == "con-flu"), aes(x=Date, y=LT50, group=Trt), color=pal2[4], stat="summary")+
  geom_point(data=subset(dat.Fig1A.LT50, Trt == "flu-con"), aes(x=Date, y=LT50, group=Trt), color=pal2[3], stat="summary")+
  geom_line(data=subset(dat.Fig1A.LT50, Trt == "con-flu"), aes(x=Date, y=LT50, group=Trt), linetype="dashed", color=pal2[4], stat="summary")+
  geom_line(data=subset(dat.Fig1A.LT50, Trt == "flu-con"), aes(x=Date, y=LT50, group=Trt), linetype="dashed", color=pal2[3], stat="summary")+
  scale_color_manual(values=pal2[c(5,1,2)])+
  geom_rect(aes(xmin=as.Date("2022-01-10"), xmax=as.Date("2022-01-28"),ymin=-29.7,ymax=-15 ),color="black", size=0.2, lty=3, fill="white",alpha=0)+
  scale_y_continuous(limits=c(-30,-5),breaks=c(-30,-25,-20,-15,-10,0), guide=guide_axis_truncated())+
  scale_x_date(date_labels= "%b", date_breaks = "1 month", limits=c(as.Date("2021-10-01"),as.Date("2022-02-01")), guide=guide_axis_truncated())+
  theme_bw(base_size=7)+
  theme(text= element_text(color="black", size=7),
        axis.text = element_text(size=7,color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size=0.2, color="black"),
        axis.ticks = element_line(size=0.2,color="black"),
        axis.ticks.length = unit(0.15, 'lines'),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(5, 'points'),
        legend.text = element_text(size=7, color="black"))

fig1a

#### Figure 1B ####

dat.Fig1B.BB <- read.csv("data_Fig1BC.BB.csv", stringsAsFactors = T)
dat.Fig1B.BB$Trt <- ordered(dat.Fig1B.BB$Trt, c("field", "fluctuating", "flu-con", "con-flu", "constant"))  

dat.Fig1B.LTE <- read.csv("data_Fig1BC.LTE.csv", stringsAsFactors = T)
dat.Fig1B.LTE$Trt <- ordered(dat.Fig1B.LTE$Trt, c("field", "fluctuating", "flu-con", "con-flu", "constant"))  

dat.Fig1B.BB50 <- dat.Fig1B.BB %>% 
  group_by(Cultivar, Trt) %>% 
  dplyr::summarize(BB50 = mean(BB_days)) %>% 
  ungroup()

dat.Fig1B.LT50 <- dat.Fig1B.LTE %>% 
  group_by(Sample_ID, Cultivar, Trt) %>% 
  dplyr::summarise(LT50 = mean(LTE)) %>% 
  ungroup()

dat.Fig1B.combine <- inner_join(dat.Fig1B.BB50, dat.Fig1B.LT50, by=c("Cultivar", "Trt"))
dat.Fig1B.combine2 <- dat.Fig1B.combine %>% 
  group_by(Trt) %>% 
  dplyr::summarize(BB50=mean(BB50), LT50=mean(LT50))

fig1b=ggplot()+
  geom_abline(slope=0, intercept=0, lty=3, size=0.2, alpha=1)+
  #geom_beeswarm(data=dat.Fig1B.BB, aes(x=Trt, y=BB_days, color=Trt), cex=1.25, priority="density", alpha=0.5, color="white", inherit.aes = F)+
  geom_beeswarm(data=dat.Fig1B.BB, aes(x=Trt, y=BB_days, color=Trt),cex=1, priority="density",size=1, alpha=0.4, inherit.aes = F)+
  geom_point(data=dat.Fig1B.combine2, aes(x=Trt, y=BB50), color="black", size=1.5, alpha=1, inherit.aes = F)+
  geom_beeswarm(data=dat.Fig1B.LTE, aes(x=Trt, y=LTE, color=Trt), cex=1, priority="density",size=1, alpha=0.4, inherit.aes = F)+
  geom_point(data=dat.Fig1B.combine2, aes(x=Trt, y=LT50), color="black", size=1.5, alpha=1, inherit.aes = F)+
  geom_segment(data=dat.Fig1B.combine2, aes(x=Trt, xend=Trt, y=LT50, yend=BB50),size=0.3, color="black", alpha=1)+
  scale_y_continuous(limits=c(-35,45),breaks=c(-35,-21,-7,0,7,21,35), guide=guide_axis_truncated(trunc_lower = c(-35,7),trunc_upper = c(-7,35)))+
  scale_x_discrete(guide=guide_axis(n.dodge=2))+
  scale_color_manual(values=pal2, labels=c("field","flu","flu-con","con-flu","con"))+
  scale_fill_manual(values=pal2, labels=c("field","flu","flu-con","con-flu","con"))+
  labs(x="Treatment", y="Cold hardiness (°C)                           Time to budbreak (days)     ", color="")+
  theme_bw(base_size=7)+
  theme(text= element_text(color="black", size=7),
        axis.text = element_text(size=7,color="black"),
        panel.spacing = unit(-.025, "lines"),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(size=0.2, color="black"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(size=0.2,color="black"),
        axis.ticks.length.y = unit(0.15, 'lines'),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(5, 'points'),
        legend.text = element_text(size=7, color="black"))

fig1b

#### Figure 1C ####

dat.Fig1C.BB <- read.csv("data_Fig1BC.BB.csv", stringsAsFactors = T)
dat.Fig1C.BB$Trt <- ordered(dat.Fig1C.BB$Trt, c("field", "fluctuating", "flu-con", "con-flu", "constant"))  
dat.Fig1C.BB$BB_days <- as.numeric(dat.Fig1C.BB$BB_days)

dat.Fig1C.LTE <- read.csv("data_Fig1BC.LTE.csv", stringsAsFactors = T)
dat.Fig1C.LTE$Trt <- ordered(dat.Fig1C.LTE$Trt, c("field", "fluctuating", "flu-con", "con-flu", "constant"))  

dat.Fig1C.BB50 <- dat.Fig1C.BB %>% 
  group_by(Cultivar, Trt) %>% 
  dplyr::summarize(BB50 = mean(BB_days)) %>% 
  ungroup()

dat.Fig1C.LT50 <- dat.Fig1C.LTE %>% 
  group_by(Sample_ID, Cultivar, Trt) %>% 
  dplyr::summarise(LT50 = mean(LTE)) %>% 
  ungroup()

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = sd(x[[col]], na.rm=TRUE) / sqrt(sum(!is.na(x[[col]]))))
  }
  data_sum <- ddply(data, groupnames, .fun=summary_func, varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}


dat.Fig1C.BB.se <- data_summary(dat.Fig1C.BB, varname="BB_days", groupnames= c("Cultivar", "Trt"))
dat.Fig1C.BB.se

dat.Fig1C.BB50.2 <- left_join(dat.Fig1C.BB50, dat.Fig1C.BB.se, by=c("Cultivar", "Trt")) %>% 
  dplyr::rename(se.BB = se)

dat.Fig1C.LTE.se <- data_summary(dat.Fig1C.LTE, varname="LTE", groupnames=c("Cultivar", "Trt"))
dat.Fig1C.LTE.se

dat.Fig1C.LT50.2 <- left_join(dat.Fig1C.LT50, dat.Fig1C.LTE.se, by=c("Cultivar", "Trt")) %>% 
  dplyr::rename(se.LT50 = se)

dat.Fig1C.combine <- full_join(dat.Fig1C.BB50.2, dat.Fig1C.LT50.2, by=c("Cultivar", "Trt"))


fig1c=ggplot(data=dat.Fig1C.combine, aes(x=LT50, y=BB50, color=Trt))+
  #stat_smooth(data=data.NEW, aes(x=LT50, y=BB50), method="lm", color="white", alpha=0.25, inherit.aes=F)+
  geom_smooth(data=dat.Fig1C.combine, aes(x=LT50, y=BB50),col="black", alpha=0.25, size=0, method="lm",fullrange=T, inherit.aes=F)+
  stat_smooth(data=dat.Fig1C.combine, aes(x=LT50, y=BB50), geom="line", method="lm", se=F, alpha=0.25,fullrange=T, inherit.aes=F)+
  #geom_label(aes(x=LT50, y=BB50, label=Cultivar))+
  geom_errorbar(aes(ymin=BB50-se.BB, ymax=BB50+se.BB), alpha=0.5, width=0)+
  geom_errorbarh(aes(xmin=LT50-se.LT50, xmax=LT50+se.LT50), alpha=0.5, height=0)+
  geom_point(shape=3, size=1.5, stroke=1)+
  geom_text(aes(x=LT50, y=BB50+0.6, label=Cultivar), color="black", size=1.8)+
  scale_y_continuous(limits=c(NA,30),breaks=c(14,21,28), guide=guide_axis_truncated())+
  scale_x_continuous(limits=c(-31,-14),breaks=c(-30,-25,-20,-15,-10), guide=guide_axis_truncated())+
  scale_color_manual(values=pal2, labels=c("field","flu","flu-con","con-flu","con"))+
  labs(x="Cold hardiness (°C)", 
       y="Time to budbreak (days)",
       color="")+
  theme_bw(base_size=7)+
  theme(text = element_text(color="black", size=7),
        axis.text = element_text(size=7,color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size=0.2, color="black"),
        axis.ticks = element_line(size=0.2,color="black"),
        axis.ticks.length = unit(0.15, 'lines'),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.direction = "horizontal",
        legend.key.size = unit(5, 'points'),
        legend.text = element_text(size=7, color="black"))

fig1c


#### Figure 2A ####

dat.Fig2A.LTE <- read.csv("data_Fig2A.csv", stringsAsFactors = T)

dat.Fig2A.LTE.2 <- pivot_longer(data = dat.Fig2A.LTE,
                                cols = starts_with("X"),
                                names_to = "Date",
                                names_prefix = "X",
                                values_to = "LTE",
                                values_drop_na = F)

dat.Fig2A.LTE.2 <- dat.Fig2A.LTE.2 %>% 
  mutate(Date = as.Date(Date, format="%y%m%d")) %>% 
  mutate(start = min(Date)) %>% # 2019-11-25
  mutate(days = as.numeric(Date-start))

dat.Fig2A.LT50 <- dat.Fig2A.LTE.2 %>% 
  group_by(Species, Temperature, Date, days) %>% 
  dplyr::summarize(LT50 = mean(LTE, na.rm=T)) %>% 
  na.omit() %>% 
  ungroup()


ggplot()+
  labs(y="Cold Hardiness (LTE, °C)", x="Days in Treatment", color="Treatment (°C)", fill="Treatment (°C)")+
  geom_smooth(data=dat.Fig2A.LTE.2, aes(x=days, y=-LTE, color=as.factor(Temperature)), span=.99, na.rm = T, se=F)+
  #geom_line(data=cdat4, aes(x=days, y=-LT50, color=as.factor(Temperature)), stat="summary", size=1.5, na.rm = T)+
  geom_point(data=dat.Fig2A.LT50, aes(x=days, y=-LT50, fill=as.factor(Temperature)), na.rm = T, color="black", shape=21, cex=1.5)+
  scale_y_continuous(limits=c(NA, NA), guide=guide_axis_truncated(trunc_upper=+Inf))+
  scale_x_continuous(limits=c(0, 20), guide=guide_axis_truncated())+
  scale_color_viridis_d()+
  scale_fill_viridis_d()+
  facet_wrap(Species~., ncol = 3)+
  theme_bw()+
  theme(aspect.ratio = 1,
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size=0.2, color="black"),
        axis.ticks = element_line(size=0.2,color="black"),
        axis.ticks.length = unit(0.4, 'lines'),
        panel.border = element_blank())

fig2a=ggplot()+
  labs(y="Cold hardiness (°C)", x="Time (days)", color="Treatment (°C)", fill="Treatment (°C)")+
  geom_line(data=dat.Fig2A.LT50, aes(x=days, y=-LT50, color=as.factor(Temperature)),lineend="round", stat="summary", size=0.7, na.rm = T)+
  geom_point(data=dat.Fig2A.LT50, aes(x=days, y=-LT50, color=as.factor(Temperature)), stat="summary", size=1.5, na.rm = T)+
  scale_y_continuous(limits=c(NA, NA),breaks=c(-28,-26,-24,-22), guide=guide_axis_truncated())+
  scale_x_continuous(limits=c(-0.5, 20.5), breaks=c(0,5,10,15,20), guide=guide_axis_truncated())+
  scale_color_manual(values=c("#1a476f", "#7b92a8", "#c10534", "#ffd200","#e37e00"), labels=c("-3","-3+5",11,4,7))+
  theme_bw(base_size=7)+
  theme(text= element_text(color="black", size=7),
        axis.text = element_text(size=7, color="black"),
        axis.text.x = element_text(size=7,angle = 0, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size=0.2, color="black"),
        axis.ticks = element_line(size=0.2,color="black"),
        axis.ticks.length = unit(0.15, 'lines'),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1, 'lines'))

fig2a


#### Figure 2B ####

dat.Fig2B.LTE <- read.csv("data_Fig2B.csv", stringsAsFactors = T)

dat.Fig2B.LTE.2 <- pivot_longer(data = dat.Fig2B.LTE,
                                cols = starts_with("X"),
                                names_to = "Date",
                                names_prefix = "X",
                                values_to = "LTE",
                                values_drop_na = F)

dat.Fig2B.LTE.2 <- dat.Fig2B.LTE.2 %>% 
  mutate(Date = as.Date(Date, format="%y%m%d")) %>% 
  mutate(start = min(Date)) %>% # 2020-10-20
  mutate(days = as.numeric(Date-start))

dat.Fig2B.LT50 <- dat.Fig2B.LTE.2 %>% 
  group_by(Species, Date, days) %>% 
  dplyr::summarize(LT50 = mean(LTE, na.rm=T))

dat.ladder.trt <- data.frame(moves_num = c(0,10,20,30,40),
                             moves_date = as.Date(c("2020-10-20",
                                                    "2020-10-30",
                                                    "2020-11-09",
                                                    "2020-11-19",
                                                    "20202-11-30")))
                             
# all LTE plots with LT50 highlighted
ggplot()+
  labs(y="Cold Hardiness (LTE, °C)", x="Days in Treatment")+
  geom_vline(data=dat.ladder.trt, aes(xintercept=moves_num), linetype="dashed", alpha=0.75)+
  geom_beeswarm(data=dat.Fig2B.LTE.2, aes(x=days, y=-LTE, color=Species), cex=1.5, priority="density", alpha=0.4)+
  geom_smooth(data=dat.Fig2B.LTE.2, aes(x=days, y=-LTE, color=Species), method="lm", se=F, span=0.99)+
  geom_point(data=dat.Fig2B.LT50, aes(x=days, y=-LT50, fill=Species), color="black", shape=21, cex=1.5, priority="density")+
  scale_x_continuous(limits=c(0,40), breaks=c(0,10,20,30,40))+
  facet_wrap(Species~., ncol = 4)+
  theme_bw()+
  theme(aspect.ratio = 1,
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# LT50 plots

dat.Fig2B.LT50AB=subset(dat.Fig2B.LT50, Species %in% "Abies balsamea")
dat.Fig2B.LT50CC=subset(dat.Fig2B.LT50, Species %in% "Cercis canadensis")

fig2b=ggplot()+
  labs(y=expression("Cold hardiness (°C)"), x="Time (days)")+
  geom_rect(aes(xmin=0, xmax=10, ymin=-35,ymax=-10), fill="steelblue4", alpha=0.1)+
  geom_rect(aes(xmin=10, xmax=20, ymin=-35,ymax=-10), fill="steelblue4", alpha=0.3)+
  geom_rect(aes(xmin=20, xmax=30, ymin=-35,ymax=-10), fill="steelblue4", alpha=0.6)+
  geom_rect(aes(xmin=30, xmax=40, ymin=-35,ymax=-10), fill="steelblue4", alpha=0.9)+
  geom_vline(aes(xintercept=c(10,20,30)), color="white", linetype="dashed", alpha=1, size=0.2)+
  geom_line(stat="smooth",data=dat.Fig2B.LT50, aes(x=days, y=-LT50, group=Species), color="white", method="lm", se=F, span=1, size=2, alpha=0.6,fullrange=T)+
  geom_smooth(data=dat.Fig2B.LT50, aes(x=days, y=-LT50, color=Species), method="lm", se=F,size=0.5, span=0.99,fullrange=T)+
  #geom_line(data=dat.Fig3.LT50, aes(x=days, y=-LT50, color=Species), stat="summary", se=F, span=0.99,fullrange=T)+
  geom_rect(aes(xmin=40, xmax=41, ymin=-35,ymax=-10), fill="white", alpha=1)+
  geom_rect(aes(xmin=-1, xmax=0, ymin=-35,ymax=-10), fill="white", alpha=1)+
  geom_point(data=dat.Fig2B.LT50AB, aes(x=days, y=-LT50), col="#E64B35FF", stat="summary", fun="mean", size=1)+
  geom_point(data=dat.Fig2B.LT50CC, aes(x=days, y=-LT50), col="#4DBBD5FF",stat="summary", fun="mean", size=1)+
  scale_y_continuous(limits=c(-35,-10), breaks=c(-10,-15, -20,-25, -30, -35), guide=guide_axis_truncated())+
  scale_x_continuous(limits=c(-1,41), breaks=c(0,10,20,30,40,50,60), guide=guide_axis_truncated())+
  scale_color_manual(values=c("#E64B35FF","#F39B7FFF","#8491B4FF",
                              "#4DBBD5FF","#7E6148FF","#B09C85FF",
                              "#91D1C2FF","#00A087FF",
                              "#3C5488FF","#CAB2D6",
                              "#FDBF6F"),
                     labels=c("AB","AR","AS","CC","CF","CM","FG","FM","LK","MG","PR"))+
  scale_fill_manual(values=c("#E64B35FF","#F39B7FFF","#8491B4FF",
                             "#4DBBD5FF","#7E6148FF","#B09C85FF",
                             "#91D1C2FF","#00A087FF","#DC0000FF",
                             "#3C5488FF","#CAB2D6","#6A3D9A",
                             "#FDBF6F","#FF7F00","#FFD300"),
                    labels=c("AB","AR","AS","CC","CF","CM","FG","FM","LK","MG","PR"))+
  #facet_wrap(Species~., ncol = 4)+
  theme_bw(base_size=7)+
  theme(text= element_text(color="black", size=7),
        axis.text = element_text(size=7,color="black"),
        axis.text.x = element_text(size=7,angle = 0, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size=0.2, color="black"),
        axis.ticks = element_line(size=0.2,color="black"),
        axis.ticks.length = unit(0.15, 'lines'),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1, 'lines'))

fig2b


#### Figure 3A ####

dat.Fig3A=read.csv("data_Fig3A.csv", stringsAsFactors = T)
dat.Fig3A$Date=as.POSIXct(dat.Fig3A$Date, format="%m/%d/%y %H:%M")
dat.Fig3A$TempC=(dat.Fig3A$TempF-32)/1.8

fig3a=ggplot()+
  geom_rect(aes(xmin=as.POSIXct("2019-08-28 01:00:00"), xmax=as.POSIXct("2020-07-01 00:00:00"),ymin=-1,ymax=10 ),color="black", size=0.1, lty=2, fill="cyan4",alpha=0.1)+
  geom_polygon(aes(x=c(as.POSIXct("2019-09-05 01:00:00"),as.POSIXct("2019-09-05 01:00:00"),as.POSIXct("2020-07-07 00:00:00"),as.POSIXct("2020-07-07 00:00:00")),y=c(13,-12,-12,3) ),color="black", size=0.1, lty=3, fill="steelblue4",alpha=0.2)+
  geom_line(data=dat.Fig3A, aes(y=TempC, x=Date),size=0.2)+
  ylab("Temperature (°C)") + xlab("") +
  scale_x_datetime(date_labels= "%b", date_breaks = "1 month", guide=guide_axis_truncated())+
  scale_y_continuous(limits=c(NA,NA), breaks=c(-15,0,15,30), guide=guide_axis_truncated())+
  theme_bw(base_size=7) +
  theme(text= element_text(color="black", size=7),
        legend.position = "bottom",
        legend.key.size = unit(0.2, 'lines'),
        strip.background = element_rect(color="#CCCCCC"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size=7, color="black"),
        axis.line = element_line(size=0.2, color="black"),
        axis.ticks = element_line(size=0.2,color="black"),
        axis.ticks.length = unit(0.15, 'lines'),
        axis.text.x = element_text(angle=45,vjust=0.5, hjust=0.5))

fig3a


#### Figure 3C ####

dat.Fig3C <- read.csv("data_Fig3BC.csv", stringsAsFactors = T)

fig3c=ggplot()+
  geom_smooth(data=dat.Fig3C, aes(y=-normLTE, x=normRate),method='loess',alpha=0.3,formula=y~x, span=0.8, se=T, size=0.3, col="black")+
  geom_point(data=dat.Fig3C, aes(y=-normLTE, x=normRate, fill=Species),shape=21,size=1,stroke=0.1, color="white")+
  ylab("Normalized cold hardiness") + xlab("Deacclimation potential") + labs(color = "", fill = "") +
  scale_x_continuous(limits=c(NA,1), breaks=c(0,0.5,1), guide=guide_axis_truncated())+
  scale_y_continuous(limits=c(NA,0), breaks=c(-1,-0.5,0), labels=c("1.0","0.5","0.0"), guide=guide_axis_truncated())+
  #scale_color_viridis_d()+
  #scale_fill_viridis_d()+
  scale_color_manual(values=c("#E64B35FF","#F39B7FFF","#8491B4FF",
                              "#4DBBD5FF","#7E6148FF","#B09C85FF",
                              "#91D1C2FF","#00A087FF","#DC0000FF",
                              "#3C5488FF","#CAB2D6","#6A3D9A",
                              "#FDBF6F","#FF7F00","#FFD300"),
                     labels=c("AB","AR","AS","CC","CF","CM","FG","FM","KL","LK","MG","PA","PR","PN","RC"))+
  scale_fill_manual(values=c("#E64B35FF","#F39B7FFF","#8491B4FF",
                             "#4DBBD5FF","#7E6148FF","#B09C85FF",
                             "#91D1C2FF","#00A087FF","#DC0000FF",
                             "#3C5488FF","#CAB2D6","#6A3D9A",
                             "#FDBF6F","#FF7F00","#FFD300"),
                    labels=c("AB","AR","AS","CC","CF","CM","FG","FM","KL","LK","MG","PA","PR","PN","RC"))+
  theme_bw(base_size=7) +
  theme(text= element_text(color="black", size=7),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.direction = "horizontal",
        legend.key.size = unit(5, 'points'),
        strip.background = element_rect(color="#CCCCCC"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size=7, color="black"),
        axis.line = element_line(size=0.2, color="black"),
        axis.ticks = element_line(size=0.2,color="black"),
        axis.ticks.length = unit(0.15, 'lines'),
        axis.text.x = element_text(angle=0,vjust=0, hjust=0.5))

fig3c


#### Figure 3B ####

dat.Fig3B=subset(dat.Fig3C, Species %in% c("Acer_rubrum","Cercis_canadensis","Cornus_mas","Metasequoia"))

fig3b=ggplot()+
  geom_smooth(data=dat.Fig3B, aes(y=LTE, x=Rate, fill=Species),method='loess',alpha=0.3,formula=y~x, span=0.8, se=T, size=0.3, col="black")+
  geom_point(data=dat.Fig3B, aes(y=LTE, x=Rate, fill=Species),shape=21,size=1.5,stroke=0.1,color="white")+
  ylab("Cold hardiness (°C)") + xlab(expression(italic(k[deacc]^"*")*" (°C "* day^-1*")")) + labs(color = "") +
  scale_x_continuous(limits=c(NA,NA), guide=guide_axis_truncated())+
  scale_y_continuous(limits=c(NA,0), breaks=c(-30,-25,-20,-15,-10,-5), guide=guide_axis_truncated())+
  #scale_color_viridis_d()+
  #scale_fill_viridis_d()+
  scale_color_manual(values=c("#F39B7FFF","#8491B4FF",
                              "#4DBBD5FF","#7E6148FF","#B09C85FF",
                              "#91D1C2FF","#00A087FF","#DC0000FF",
                              "#3C5488FF","#CAB2D6","#6A3D9A",
                              "#FDBF6F","#FF7F00","#FFD300"),
                     labels=c("AR","AS","CC","CF","CM","FG","FM","KL","LK","MG","PA","PR","PN","RC"))+
  scale_fill_manual(values=c("#F39B7FFF",
                             "#4DBBD5FF","#B09C85FF",
                             "#CAB2D6","#6A3D9A",
                             "#FDBF6F","#FF7F00","#FFD300"),
                    labels=c("AR","CC","CM","MG","PA","PR","PN","RC"))+
  theme_bw(base_size=7) +
  theme(text= element_text(color="black", size=7),
        legend.position = "none",
        strip.background = element_rect(colour="white", fill="white"),
        strip.text = element_text(color="black",size=7),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size=7, color="black"),
        axis.line = element_line(size=0.2, color="black"),
        axis.ticks = element_line(size=0.2,color="black"),
        axis.ticks.length = unit(0.15, 'lines'),
        axis.text.x = element_text(angle=45,vjust=0.5, hjust=0.5))+
  facet_wrap(.~Species, scales = "free")

fig3b


#### Summary statistics ####

# Prepare stats: 1B
print(LSD.test(lm(LTE ~ Trt, data = dat.Fig1B.LTE),"Trt")$groups)
print(LSD.test(lm(BB_days ~ Trt, data = dat.Fig1B.BB),"Trt")$groups)

# Prepare stats: 1C
summary(lm(BB50~LTE, data=dat.Fig1C.combine))

# Prepare stats: 2A
summary(lm(LT50~Temperature+Temperature:days, data=dat.Fig2A.LT50)) 

# Prepare stats: 2B
summary(lm(LT50~Species+Species:days, data=dat.Fig2B.LT50))


#### Box1 figure ####

box1a=ggplot()+
  labs(y=expression("Cold hardiness"), x="Time under forcing")+
  geom_polygon(aes(x=c(0,0,28),y=c(-30,-10,-30) ),color="black", size=0., lty=0, fill="#1a476f",alpha=0.4)+
  geom_polygon(aes(x=c(0,0,42,44,42,32,31.5,30,28.5,28),y=c(-30,-34,-34,-32,-30,-30,-30.7,-31.5,-30.7,-30) ),color="black", size=0., lty=0, fill="#55752f",alpha=0.4)+
  #geom_point(aes(x=30,y=-30), size=10, col="white")+
  scale_y_continuous(limits=c(-35,-10), breaks=c(-10,-15, -20,-25, -30, -35), labels=c(),  guide=guide_axis_truncated())+
  scale_x_continuous(limits=c(0,44), breaks=c(0,7,14,21,28,35,42),labels=c(), guide=guide_axis_truncated())+
  theme_bw(base_size=7)+
  theme(text= element_text(color="black", size=7),
        axis.text = element_text(size=7,color="black"),
        axis.text.x = element_text(size=7,angle = 0, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size=0.2, color="black"),
        axis.ticks = element_line(size=0.2,color="black"),
        axis.ticks.length = unit(0.15, 'lines'),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1, 'lines'))

box1b=ggplot()+
  labs(y=expression("Cold hardiness"), x="Time under forcing")+
  geom_polygon(aes(x=c(0,0,28),y=c(-55,-35,-55) ),color="black", size=0., lty=0, fill="#1a476f",alpha=0.4)+
  geom_polygon(aes(x=c(0,0,14),y=c(-30,-10,-30) ),color="black", size=0., lty=0, fill="#c10534",alpha=0.5)+
  #geom_point(aes(x=30,y=-30), size=10, col="white")+
  scale_y_continuous(limits=c(NA,NA), breaks=c(-10,-15, -20,-25, -30, -35, -40,-45,-50,-55), labels=c(),  guide=guide_axis_truncated(trunc_lower = c(-30,-55),trunc_upper = c(-10,-35)))+
  scale_x_continuous(limits=c(NA,44), breaks=c(0,7,14,21,28,35,42),labels=c(), guide=guide_axis_truncated())+
  theme_bw(base_size=7)+
  theme(text= element_text(color="black", size=7),
        axis.text = element_text(size=7,color="black"),
        axis.text.x = element_text(size=7,angle = 0, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size=0.2, color="black"),
        axis.ticks = element_line(size=0.2,color="black"),
        axis.ticks.length = unit(0.15, 'lines'),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1, 'lines'))

box1c=ggplot()+
  labs(y=expression("Cold hardiness"), x="Time under forcing")+
  geom_polygon(aes(x=c(0,0,28),y=c(-45,-25,-45) ),color="black", size=0., lty=0, fill="#1a476f",alpha=0.4)+
  geom_polygon(aes(x=c(0,0,14),y=c(-20,-10,-20) ),color="black", size=0., lty=0, fill="#ffd200",alpha=0.5)+
  #geom_point(aes(x=30,y=-30), size=10, col="white")+
  scale_y_continuous(limits=c(NA,NA), breaks=c(-10,-15, -20,-25, -30, -35, -40,-45), labels=c(),  guide=guide_axis_truncated(trunc_lower = c(-20,-45),trunc_upper = c(-10,-25)))+
  scale_x_continuous(limits=c(NA,44), breaks=c(0,7,14,21,28,35,42),labels=c(), guide=guide_axis_truncated())+
  theme_bw(base_size=7)+
  theme(text= element_text(color="black", size=7),
        axis.text = element_text(size=7,color="black"),
        axis.text.x = element_text(size=7,angle = 0, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size=0.2, color="black"),
        axis.ticks = element_line(size=0.2,color="black"),
        axis.ticks.length = unit(0.15, 'lines'),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1, 'lines'))

box1d=ggplot()+
  labs(y=expression("Cold hardiness"), x="Time under forcing")+
  geom_polygon(aes(x=c(0,0,14),y=c(-45,-25,-45) ),color="black", size=0., lty=0, fill="#c10534",alpha=0.5)+
  geom_polygon(aes(x=c(0,0,14),y=c(-20,-10,-20) ),color="black", size=0., lty=0, fill="#ffd200",alpha=0.5)+
  #geom_point(aes(x=30,y=-30), size=10, col="white")+
  scale_y_continuous(limits=c(NA,NA), breaks=c(-10,-15, -20,-25, -30, -35, -40,-45), labels=c(),  guide=guide_axis_truncated(trunc_lower = c(-20,-45),trunc_upper = c(-10,-25)))+
  scale_x_continuous(limits=c(NA,44), breaks=c(0,7,14,21,28,35,42),labels=c(), guide=guide_axis_truncated())+
  theme_bw(base_size=7)+
  theme(text= element_text(color="black", size=7),
        axis.text = element_text(size=7,color="black"),
        axis.text.x = element_text(size=7,angle = 0, hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size=0.2, color="black"),
        axis.ticks = element_line(size=0.2,color="black"),
        axis.ticks.length = unit(0.15, 'lines'),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(1, 'lines'))


#### Arranging figures together and exporting to *directory* ####

st=format(Sys.time(), "%Y-%m-%d_%I%M")

# Figure 1
ggsave(fig1a+plot_spacer()+fig1b+plot_spacer()+fig1c+plot_layout(widths=c(1,0.05,1,0.05,1)), #Export at 7.09 x 3
       filename = paste0("Fig1ABC_", st, ".pdf", sep=""),
       width = 7.09,
       height = 3)

# Figure 2
ggsave(fig2a/fig2b+plot_layout(heights=c(.7,1)), #Export at 3 x 5.1
       filename = paste0("Fig2AB_", st, ".pdf", sep=""),
       width = 3,
       height = 5.1)

# Figure 3
ggsave(fig3a+plot_spacer()+fig3b+plot_spacer()+fig3c+plot_layout(widths = c(1,0.01,0.8,0.01,0.8)), #Export at 7.09 x 3.2
       filename = paste0("Fig3ABC_", st, ".pdf", sep=""),
       width = 7.09,
       height = 3.2)

# Box 1
ggsave(box1a/box1b/box1c/box1d+plot_layout(heights=c(0.25,0.45,0.35,0.35)), # Export at 3.5 x 5.33
       filename = paste0("Box1_", st, ".pdf", sep=""),
       width = 3.5,
       height = 5.33)


#### End ####

