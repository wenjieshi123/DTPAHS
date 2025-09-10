#incidence of breast cancer
#setwd('D:\\English Papers for writing\\qianya\\breast')
rm(list = ls(all.names = TRUE))##清空环境
library(ggplot2)
library(ggsci)
library(cowplot)
library(dplyr)
library(tidyr)
library(ggpubr)
regions <- read.csv('regions.csv')
head(regions)
levels(regions$location)

regions$location <- factor(regions$location,
                             levels = c('Global',
                                        'Low SDI',
                                        'Low-middle SDI',
                                        'Middle SDI',
                                        'High-middle SDI',
                                        'High SDI',
                                        'High-income Asia Pacific',
                                        'Central Asia',
                                        "East Asia",
                                        "South Asia",
                                        "Southeast Asia",
                                        "Australasia",
                                        "Oceania",
                                        "Central Europe",
                                        "Eastern Europe",
                                        "Western Europe",
                                        "Andean Latin America",
                                        "Central Latin America",
                                        "Southern Latin America",
                                        "Tropical Latin America",
                                        "North Africa and Middle East",
                                        "High-income North America",
                                        "Caribbean",
                                        "Central Sub-Saharan Africa",
                                        "Eastern Sub-Saharan Africa",
                                        "Southern Sub-Saharan Africa",
                                        "Western Sub-Saharan Africa"))

locations <- levels(regions$location)
df2 = data.frame()
for(i in 1:27){
  dat = regions[regions$sex == 'Male' & regions$location == locations[i] & 
                  regions$age =='Age-standardized',]
  fit = lm(log(dat$val/1e5)~year,data = dat)
  eapc = 100*(exp(fit$coefficients[2])-1)
  lo = 100*(exp(confint(fit)[2,1])-1)
  up = 100*(exp(confint(fit)[2,2])-1)
  p = broom::tidy(fit)$p.value[2]
  df1 = data.frame(location = locations[i],
                  eapcs = round(eapc,2),
                  los =round(lo,2),
                  ups = round(up,2),
                  pvalue = round(p,3))
  df2  = rbind(df2,df1)
}
write.csv(df2,'region_aapc_male.csv')
df2 = data.frame()
for(i in 1:27){
  dat = regions[regions$sex == 'Female' & regions$location == locations[i] & 
                  regions$age =='Age-standardized',]
  fit = lm(log(dat$val/1e5)~year,data = dat)
  eapc = 100*(exp(fit$coefficients[2])-1)
  lo = 100*(exp(confint(fit)[2,1])-1)
  up = 100*(exp(confint(fit)[2,2])-1)
  p = broom::tidy(fit)$p.value[2]
  df1 = data.frame(location = locations[i],
                  eapcs = round(eapc,2),
                  los =round(lo,2),
                  ups = round(up,2),
                  pvalue = round(p,3))
  df2  = rbind(df2,df1)
}
write.csv(df2,'region_aapc_female.csv')
cases1 <- regions[regions$metric == 'Number' &
                   regions$sex == 'Female' & 
                   regions$location %in% c('Global',
                                           'Low SDI',
                                           'Low-middle SDI',
                                           'Middle SDI',
                                           'High-middle SDI',
                                           'High SDI'),]

ggplot(cases1,aes(year,val))+
  geom_bar(stat = 'identity',width = .5)+
  facet_wrap(~location,ncol=3)+
  xlab('Year')+ylab('Number of cases')+
  scale_y_continuous(expand = c(0,0),limits = c(0,2e6))+
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010,2015,2020),
                     expand = c(0.02,0.02))+
  theme_bw()+
  theme(strip.background = element_blank())

ASR1 <- regions[regions$metric == 'Rate' &
                    regions$sex == 'Female' & 
                  regions$age == 'Age-standardized'&
                    regions$location %in% c('Global',
                                            'Low SDI',
                                            'Low-middle SDI',
                                            'Middle SDI',
                                            'High-middle SDI',
                                            'High SDI'),]

a <- ggplot(ASR1,aes(year,val,fill = location))+
  geom_line()+
  geom_point(shape = 21,color = 'gray30',size = 2.6)+
  xlab('Year')+ylab('Age-standardized incidence rate (/100000)')+
  scale_y_continuous(expand = c(0,0),limits = c(min(ASR1$val-1),max(ASR1$val)+1))+
  scale_fill_lancet()+
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010,2015,2020),
                     expand = c(0.02,0.02))+
  theme_bw()+
  theme(strip.background = element_blank())

ASR2 <- regions[regions$metric == 'Rate' &
                  regions$sex == 'Male' & 
                  regions$age == 'Age-standardized'&
                  regions$location %in% c('Global',
                                          'Low SDI',
                                          'Low-middle SDI',
                                          'Middle SDI',
                                          'High-middle SDI',
                                          'High SDI'),]
b <- ggplot(ASR2, aes(year, val, fill = location)) +
  geom_line() +
  geom_point(shape = 21, color = 'gray30', size = 2.6) +
  xlab('Year') + 
  ylab('Age-standardized incidence rate (/100000)') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 95)) + ##可以自己修改
  scale_fill_lancet() +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2015, 2020),
                     expand = c(0.02, 0.02)) +
  theme_bw() +
  theme(strip.background = element_blank())

plot_grid(a,b,ncol = 2,labels = 'AUTO')

df2 <- read.csv('region_aapc_female.csv')
df2_2 <- df2[!df2$location %in% c('Low SDI',
                                  'Low-middle SDI',
                                  'Middle SDI',
                                  'High-middle SDI',
                                  'High SDI'),]

df2_2$location <- factor(df2_2$location,levels = df2_2$location[order(df2_2$eapcs,decreasing = T)])

a <- ggplot(df2_2,aes(location,eapcs))+
  geom_bar(stat = 'identity',width = 0.6)+
  geom_errorbar(aes(ymin = los,ymax = ups),width=.2)+
  labs(x = 'Regions',y = 'Average Annual Percentage \nChange (95% CI)')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 75,hjust = 1))

df2 <- read.csv('region_aapc_male.csv')
df2_2 <- df2[!df2$location %in% c('Low SDI',
                                  'Low-middle SDI',
                                  'Middle SDI',
                                  'High-middle SDI',
                                  'High SDI'),]

df2_2$location <- factor(df2_2$location,levels = df2_2$location[order(df2_2$eapcs,decreasing = T)])

b <- ggplot(df2_2,aes(location,eapcs))+
  geom_bar(stat = 'identity',width = 0.6)+
  geom_errorbar(aes(ymin = los,ymax = ups),width=.2)+
  labs(x = 'Regions',y = 'Average Annual Percentage \nChange (95% CI)')+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 75,hjust = 1))

plot_grid(a,b,ncol=1,labels = 'AUTO')


####画地图#######################################################################


countries <- read.csv('countries.csv')
countries2 <- countries[countries$location != 'Global'& countries$measure!='Deaths',]

country = unique(countries2$location)
#ages = unique(countries2$age)
df3 = data.frame()
for(i in 1:195){
  dat2 = countries2[countries2$location == country[i] & countries2$age=='Age-standardized',]
  #dat2 = countries2[dat$age=='Age-standardized',]
  dat2$val = dat2$val+0.001
  fit = lm(log(dat2$val/1e5)~year,data = dat2)
  eapc = 100*(exp(fit$coefficients[2])-1)
  lo = 100*(exp(confint(fit)[2,1])-1)
  up = 100*(exp(confint(fit)[2,2])-1)
  p = broom::tidy(fit)$p.value[2]
  df1 = data.frame(location = country[i],
                   eapcs = round(eapc,2),
                   los =round(lo,2),
                   ups = round(up,2),
                   pvalue = round(p,3))
  df3  = rbind(df3,df1)
  }


write.csv(df3,'country_aapc.csv')

countriesM <- subset(countries, sex == "Male")
country = unique(countriesM$location)
ages = unique(countriesM$age)
df3 = data.frame()
for(i in 1:195){
  #df22  =data.frame()
  dat = countriesM[countriesM$location == country[i] & countriesM$age=='Age-standardized',]
  dat2 = dat  
  dat2$val = dat2$val+0.001
    fit = lm(log(dat2$val/1e5)~year,data = dat2)
    eapc = 100*(exp(fit$coefficients[2])-1)
    lo = 100*(exp(confint(fit)[2,1])-1)
    up = 100*(exp(confint(fit)[2,2])-1)
    p = broom::tidy(fit)$p.value[2]
    df1 = data.frame(location = country[i],
                     age = 'ASR',
                     eapcs = round(eapc,2),
                     los =round(lo,2),
                     ups = round(up,2),
                     pvalue = round(p,3))
    df3  = rbind(df3,df1)
  }


write.csv(df3,'country_ASRaapc_male.csv')

cou_asr <- countries2[countries2$age == 'Age-standardized' &
                        countries2$metric == 'Rate' & 
                        countries2$year ==2021&
                        countries2$measure == 'Incidence',]

cou_asr$location <- as.character(cou_asr$location) 
cou_asr$location[cou_asr$location == 'United States'] = 'USA'
cou_asr$location[cou_asr$location == 'Russian Federation'] = 'Russia'
cou_asr$location[cou_asr$location == 'The Gambia'] = 'Gambia'
cou_asr$location[cou_asr$location == 'United Kingdom'] = 'UK'
cou_asr$location[cou_asr$location == 'The Bahamas'] = 'Bahamas'
cou_asr$location[cou_asr$location == 'Congo'] = 'Republic of Congo'
cou_asr$location[cou_asr$location == "Cote d'Ivoire"] = 'Ivory Coast'

worldData <- map_data('world')
total <- dplyr::full_join(worldData,cou_asr,by = c('region'='location'))
total$val_grp <- cut(total$val,breaks = c(8,15,30,40,50,75,100,150),
                     labels = c('8.0~15.0','15.1~30.0','30.1~40.0','40.1~50.0',
                                '50.1~75.0','75.1~100.0','100.1~150.0'),
                     include.lowest = T,right = T)


a <- ggplot()+ 
  geom_polygon(data=total, aes(x=long, y=lat, group = group,fill=val_grp),colour="black",size = .2) + 
  scale_fill_manual(values = c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#99000d'),
    na.value = "white" )+
  theme_void()+labs(x="", y="")+
  guides(fill = guide_legend(title='ASR(/10^5)'))+
  theme(legend.position = 'right')
#######
df33 <- read.csv('country_aapc.csv')

df33$aapc <- ifelse(df33$pvalue>0.05,0,df33$eapcs)
df33$location <- as.character(df33$location) 
df33$location[df33$location == 'United States'] = 'USA'
df33$location[df33$location == 'Russian Federation'] = 'Russia'
df33$location[df33$location == 'The Gambia'] = 'Gambia'
df33$location[df33$location == 'United Kingdom'] = 'UK'
df33$location[df33$location == 'The Bahamas'] = 'Bahamas'
df33$location[df33$location == 'Congo'] = 'Republic of Congo'
df33$location[df33$location == "Cote d'Ivoire"] = 'Ivory Coast'
worldData <- map_data('world')
total <- full_join(worldData,df33,by = c('region'='location'))
total$aapc_grp <- cut(total$aapc,breaks = c(-3,-2,-1,-0.00001,0.0001,1,2,3),
                     labels = c('-2.00~-1.01','-1.00~-0.01','0','0.01~1.00',
                                '1.01~2.00','2.01~3.00','3.01~5.00'),
                     include.lowest = T,right = T)

b <- ggplot()+ 
  geom_polygon(data=total, aes(x=long, y=lat, group = group,fill=aapc_grp),colour="black",size = .2) + 
  scale_fill_manual(values = c('#ffffcc','#c7e9b4','gray','#7fcdbb','#41b6c4','#2c7fb8','#253494'),
    na.value = "white" )+
  theme_void()+labs(x="", y="")+
  guides(fill = guide_legend(title='AAPC'))+
  theme(legend.position = 'right')


cou_asr <- countriesM[countriesM$age == 'Age-standardized' &
                        countriesM$metric == 'Rate' & 
                        countriesM$year ==2021&
                        countriesM$measure == 'Incidence',]

cou_asr$location <- as.character(cou_asr$location) 
cou_asr$location[cou_asr$location == 'United States'] = 'USA'
cou_asr$location[cou_asr$location == 'Russian Federation'] = 'Russia'
cou_asr$location[cou_asr$location == 'The Gambia'] = 'Gambia'
cou_asr$location[cou_asr$location == 'United Kingdom'] = 'UK'
cou_asr$location[cou_asr$location == 'The Bahamas'] = 'Bahamas'
cou_asr$location[cou_asr$location == 'Congo'] = 'Republic of Congo'
cou_asr$location[cou_asr$location == "Cote d'Ivoire"] = 'Ivory Coast'

worldData <- map_data('world')
total <- dplyr::full_join(worldData,cou_asr,by = c('region'='location'))
total$val_grp <- cut(total$val,breaks = c(8,15,30,40,50,75,100,150),
                     labels = c('8.0~15.0','15.1~30.0','30.1~40.0','40.1~50.0',
                                '50.1~75.0','75.1~100.0','100.1~150.0'),###根据你的需要调整范围
                     include.lowest = T,right = T)


cc <- ggplot()+ 
  geom_polygon(data=total, aes(x=long, y=lat, group = group,fill=val_grp),colour="black",size = .2) + 
  scale_fill_manual(values = c('#eff3ff','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#084594'),
    na.value = "white" )+
  theme_void()+labs(x="", y="")+
  guides(fill = guide_legend(title='ASR(/10^5)'))+
  theme(legend.position = 'right')


df33 <- read.csv('country_ASRaapc_male.csv')

df33$aapc <- ifelse(df33$pvalue>0.05,0,df33$eapcs)
df33$location <- as.character(df33$location) 
df33$location[df33$location == 'United States'] = 'USA'
df33$location[df33$location == 'Russian Federation'] = 'Russia'
df33$location[df33$location == 'The Gambia'] = 'Gambia'
df33$location[df33$location == 'United Kingdom'] = 'UK'
df33$location[df33$location == 'The Bahamas'] = 'Bahamas'
df33$location[df33$location == 'Congo'] = 'Republic of Congo'
df33$location[df33$location == "Cote d'Ivoire"] = 'Ivory Coast'
worldData <- map_data('world')
total <- full_join(worldData,df33,by = c('region'='location'))
total$aapc_grp <- cut(total$aapc,breaks = c(-6.0, -5.0, -4.0, -3.0, -2.0, -1.0, 0.0, 1.0),
                      labels = c('-6.00~-5.00', '-5.00~-4.00', '-4.00~-3.00', '-3.00~-2.00', 
					  '-2.00~-1.00', '-1.00~0.00', '0.01~1.00'),
                      include.lowest = T,right = T)

d <- ggplot()+ 
  geom_polygon(data=total, aes(x=long, y=lat, group = group,fill=aapc_grp),colour="black",size = .2) + 
  scale_fill_manual(values = c('#31a354','#e5f5e0','gray','#feebe2','#fbb4b9','#f768a1','#ae017e'),
    na.value = "white" )+
  theme_void()+labs(x="", y="")+
  guides(fill = guide_legend(title='AAPC'))+
  theme(legend.position = 'right')


plot_grid(a,cc,b,d,ncol = 2,labels = 'AUTO')

#######新图#######
sdi <- read.csv('SDIs.csv')###自己整理
sdi2 <- sdi[sdi$Year %in% 1990:2021 & sdi$Location %in% df3$location,]
sdi2$Location <- as.character(sdi2$Location)
sdi3 <- sdi[sdi$Year == 1990& sdi$Location %in% unique(df3$location),]
sdi3 <- sdi3[sdi3$Location.ID != 533,]

country <- unique(sdi2$Location)
df22 <- data.frame()

for (i in 1:length(country)) {
  dat <- sdi2[sdi2$Location == country[i], ]
  
  # 移除 Year 或 SDI.Index.Value 中的 NA 行
  dat <- dat[complete.cases(dat[, c("Year", "SDI.Index.Value")]), ]
  
  # 确保至少有 2 行数据才能拟合线性模型
  if (nrow(dat) >= 2) {
    fit <- lm(log(SDI.Index.Value) ~ Year, data = dat)
    conf <- confint(fit)
    eapc <- 100 * (exp(coef(fit)[2]) - 1)
    lo <- 100 * (exp(conf[2, 1]) - 1)
    up <- 100 * (exp(conf[2, 2]) - 1)
    p <- summary(fit)$coefficients[2, 4]
    
    df1 <- data.frame(
      location = country[i],
      aapc = round(eapc, 2),
      los2 = round(lo, 2),
      ups2 = round(up, 2),
      pvalue2 = round(p, 3)
    )
    df22 <- rbind(df22, df1)
  } else {
    message("⛔ 跳过：", country[i], " —— 有 NA 或数据不足（", nrow(dat), " 行）")
  }
}

#new figure
asr_sdi <- merge(df22,df3,by = 'location')
asr_sdi <- asr_sdi[asr_sdi$age == 'ASR',]
ggplot(asr_sdi, aes(x = aapc, y = eapcs)) +
   geom_point(size = 2.6, shape = 21, fill = 'gray30') +
   geom_smooth(method = "lm", se = TRUE, color = "blue") +
   theme_bw() +
   labs(
     x = "AAPC of SDI at national level",
     y = "AAPC of breast cancer incidence"
   ) +
   stat_cor(method = "pearson", label.x = 0, label.y = max(asr_sdi$eapcs))
 
 
# 步骤 1：构建 1990 和 2021 年 SDI 数据
sdi1990 <- sdi[sdi$Year == 1990 & sdi$Location %in% unique(df3$location), c("Location", "SDI.Index.Value")]
sdi2021 <- sdi[sdi$Year == 2021 & sdi$Location %in% unique(df3$location), c("Location", "SDI.Index.Value")]

colnames(sdi1990)[2] <- "sdi1990"
colnames(sdi2021)[2] <- "sdi2021"

nwdf <- merge(sdi1990, sdi2021, by = "Location")
nwdf$sdi9017 <- (nwdf$sdi2021 - nwdf$sdi1990) / nwdf$sdi2021
colnames(nwdf)[1] <- "location"  # 与 df3 和 df22 对齐

cou_asr2 <- countries2[countries2$age == 'Age-standardized' &
                         countries2$metric == 'Rate' & 
                         countries2$year ==1990&
                         countries2$measure == 'Incidence',]

inci<- data.frame(inci1990 = cou_asr2$val[cou_asr2$year == 1990],
                   inci2021 = cou_asr$val[cou_asr$year == 2021])
nwdf$sdi9017 <- (nwdf$sdi2021-nwdf$sdi1990)/nwdf$sdi2021
nwdf2 <- nwdf[1:195,]

nwdf3 <- cbind(nwdf2,inci)

ggplot(nwdf3,aes(sdi1990,inci1990))+
  geom_point(size = 2.6,shape = 21,fill = 'gray30')+
  geom_smooth(method = lm)+
  theme_bw()+
  labs(x = 'SDI in 1990',
       y = 'Breast cancer incidence in 1990')

ggplot(nwdf3,aes(sdi2021,inci2021))+
  geom_point(size = 2.6,shape = 21,fill = 'gray30')+
  geom_smooth(method = lm)+
  theme_bw()+
  labs(x = 'SDI in 2021',
       y = 'Breast cancer incidence in 2021')

df4 <- full_join(asr_sdi,nwdf3,by = 'location')
ggplot(df4,aes(inci1990,eapcs))+
  geom_point(size = 2.6,shape = 21,fill = 'gray30')+
  geom_smooth(method = lm)+
  theme_bw()+
  labs(x = 'Breast cancer incidence in 2021',
       y = 'AAPC of breast cancer incidence')





countries2 <- countries[countries$location == 'Global'&countries$measure == 'Incidence',]

country = unique(countries2$location)
ages = c('15-49 years','50-69 years','70+ years','Age-standardized')
df33 = data.frame()
for(i in 1){
  df22  =data.frame()
  dat = countries2[countries2$location == country[i],]
  for(j in 1:4){
    dat2 = countries2[dat$age==ages[j]&
                        dat$metric =='Rate',]
    dat2$val = dat2$val+0.001
    fit = lm(log(dat2$val/1e5)~year,data = dat2)
    eapc = 100*(exp(fit$coefficients[2])-1)
    lo = 100*(exp(confint(fit)[2,1])-1)
    up = 100*(exp(confint(fit)[2,2])-1)
    p = broom::tidy(fit)$p.value[2]
    df1 = data.frame(location = country[i],
                     age = ages[j],
                     eapcs = round(eapc,2),
                     los =round(lo,2),
                     ups = round(up,2),
                     pvalue = round(p,3))
    df22  = rbind(df22,df1)
  }
  df33 = rbind(df33,df22)
}



#new fig 4

countries <- read.csv('countries.csv')
ASRF <- countries[countries$measure == 'Incidence'&
                    countries$age == 'Age-standardized'&
                    countries$metric == 'Rate' & countries$year ==2021
                  &countries$location!='Global',]


ASRM <- countriesM[countriesM$measure == 'Incidence'&
                    countriesM$age == 'Age-standardized'&
                    countriesM$metric == 'Rate' & countriesM$year ==2021,]

ASRs <- full_join(ASRF,ASRM,by = 'location')

a <- ggplot(ASRs,aes(val.x,val.y))+
  geom_point(size = 2.6,shape = 21,fill = 'gray30')+
  geom_smooth(method = lm)+
  theme_bw()+
  labs(x = 'Female breast cancer incidence in 2021',
       y = 'Male breast cancer \nincidence in 2021')

countries <- read.csv('countries.csv')
ASRF <- countries[countries$measure == 'Incidence'&
                    countries$age == 'Age-standardized'&
                    countries$metric == 'Rate' & countries$year ==1990
                  &countries$location!='Global',]


ASRM <- countriesM[countriesM$measure == 'Incidence'&
                     countriesM$age == 'Age-standardized'&
                     countriesM$metric == 'Rate' & countriesM$year ==1990,]

ASRs <- full_join(ASRF,ASRM,by = 'location')

cc <- ggplot(ASRs,aes(val.x,val.y))+
  geom_point(size = 2.6,shape = 21,fill = 'gray30')+
  geom_smooth(method = lm)+
  theme_bw()+
  labs(x = 'Female breast cancer incidence in 1990',
       y = 'Male breast cancer \nincidence in 1990')


AAPCF <- read.csv('country_aapc.csv')
#AAPCF <- AAPCF[AAPCF$age =='Age-standardized',]
AAPCM <- read.csv('country_ASRaapc_male.csv')
AAPCs <- full_join(AAPCF,AAPCM,by = 'location')
cor.test(AAPCs$eapcs.x,AAPCs$eapcs.y)
b <- ggplot(AAPCs,aes(eapcs.x,eapcs.y))+
  geom_point(size = 2.6,shape = 21,fill = 'gray30')+
  geom_smooth(method = lm)+
  theme_bw()+
  labs(x = 'AAPC of female breast cancer incidence',
       y = 'AAPC of male breast \ncancer incidence')

plot_grid(cc,a,b,ncol = 1,labels = 'AUTO')

regionsM <- read.csv('regions.csv')###这里最好是自己去下载包含有年龄的数据
regionsM  <- subset(regionsM, sex == "Male")
regionsM <- regionsM[regionsM$location == 'Global' & regionsM$age %in% c('15-49 years','50-69 years','70+ years'),]
ages <- c('15-49 years','50-69 years','70+ years')

df22 = data.frame()
for(i in 1:3){
  dat2 = regionsM[regionsM$age == ages[i] & regionsM$metric =='Rate',]
  dat2$val = dat2$val+0.001
  fit = lm(log(dat2$val/1e5)~year,data = dat2)
  eapc = 100*(exp(fit$coefficients[2])-1)
  lo = 100*(exp(confint(fit)[2,1])-1)
  up = 100*(exp(confint(fit)[2,2])-1)
  p = broom::tidy(fit)$p.value[2]
  df1 = data.frame(
                   age = ages[i],
                   eapcs = round(eapc,2),
                   los =round(lo,2),
                   ups = round(up,2),
                   pvalue = round(p,3))
  df22  = rbind(df22,df1)
}


ASR_sdi <- sdi[sdi$Year %in% c(1990,2021) & sdi$Location %in% countries$location,]

df1 <- data.frame(sdi1990 = ASR_sdi$SDI.Index.Value[ASR_sdi$Year == 1990&ASR_sdi$Location!='Gloabl'],
                 sdi2021 = ASR_sdi$SDI.Index.Value[ASR_sdi$Year == 2021&ASR_sdi$Location!='Gloabl'],
                 locaton = ASR_sdi$Location[ASR_sdi$Location!='Global'])

df2 <- data.frame(asr1990 = countries$val[countries$year == 1990 & countries$location != 'Global'&
                                            countries$measure =='Incidence' & countries$age == 'Age-standardized'],
                  asr2021 = countries$val[countries$year == 2021 & countries$location != 'Global'&
                                            countries$measure =='Incidence' & countries$age == 'Age-standardized'],
location = countries$location[countries$location!='Global'])


asr2021 <- countries[countries$year == 2021 & countries$location != 'Global'&
                   countries$measure =='Incidence' & countries$age == 'Age-standardized',]

asr1990 <- countries[countries$year == 1990 & countries$location != 'Global'&
                       countries$measure =='Incidence' & countries$age == 'Age-standardized',]

asr <- full_join(asr1990,asr2021,by = 'location')
sdi1990 = sdi[sdi$Year == 1990&sdi$Location%in%asr$location,]
sdi2021 = sdi[sdi$Year == 2021&sdi$Location%in%asr$location,]
sdis <- full_join(sdi1990,sdi2021,by = 'Location')
sdis <- sdis[-c(27,197,198),]

sds_asr <- full_join(asr,sdis,by = c('location'='Location'))

cor.test(sds_asr$val.x,sds_asr$SDI.Index.Value.x)
cor.test(sds_asr$val.y,sds_asr$SDI.Index.Value.y)

sds_asr2 <- sds_asr[,c(2,8,17,22,25)]

sds_asr3 <- gather(sds_asr2,key = 'year',value = 'inci',-c(location,SDI.Index.Value.x,SDI.Index.Value.y))
sds_asr4 <- gather(sds_asr3,key = 'year2',value = 'sdi',-c(location,year,inci))
sds_asr4$year <- ifelse(sds_asr4$year == 'val.x',1990,2021)

a <- ggplot(sds_asr4,aes(sdi,inci,color = factor(year)))+
  geom_point(aes(color = factor(year)),size = 2.6)+
  scale_color_jama(name = 'Year')+
  geom_smooth(method = lm)+
  theme_bw()+
  labs(x = 'SDI',
       y = 'Female breast cancer ASR')+
  theme(legend.position = c(.15,.8))
  
countriesM <- read.csv('countries_male.csv')
asr2021M <- countriesM[countriesM$year == 2021 & countriesM$location != 'Global'&
                       countriesM$measure =='Incidence' & countriesM$age == 'Age-standardized',]

asr1990M <- countriesM[countriesM$year == 1990 & countriesM$location != 'Global'&
                       countriesM$measure =='Incidence' & countriesM$age == 'Age-standardized',]
asrM <- full_join(asr1990M,asr2021M,by = 'location')

sds_asrM <- full_join(asrM,sdis,by = c('location'='Location'))
cor.test(sds_asrM$val.x,sds_asrM$SDI.Index.Value.x)
cor.test(sds_asrM$val.y,sds_asrM$SDI.Index.Value.y)

sds_asrM2 <- sds_asrM[,c(2,8,17,22,25)]

sds_asrM3 <- gather(sds_asrM2,key = 'year',value = 'inci',-c(location,SDI.Index.Value.x,SDI.Index.Value.y))
sds_asrM4 <- gather(sds_asrM3,key = 'year2',value = 'sdi',-c(location,year,inci))
sds_asrM4$year <- ifelse(sds_asrM4$year == 'val.x',1990,2021)
b <- ggplot(sds_asrM4,aes(sdi,inci,color = factor(year)))+
  geom_point(aes(color = factor(year)),size = 2.6)+
  scale_color_jama(name = 'Year')+
  geom_smooth(method = lm)+
  theme_bw()+
  labs(x = 'SDI',
       y = 'Male breast cancer ASR')+
  theme(legend.position = c(.15,.8))

country_aapc <- read.csv('country_aapc.csv')
aapcF <- country_aapc[country_aapc$location!='Gloabl',]


sds_aapcf <- full_join(aapcF,sdis,by = c('location'='Location'))

cor.test(sds_aapcf$eapcs,sds_aapcf$SDI.Index.Value.x)
cor.test(sds_aapcf$eapcs,sds_aapcf$SDI.Index.Value.y)

sds_aapcf2 <- sds_aapcf[,c(1,3,9,12)]
sds_aapcf2$aapc_change <- (sds_aapcf2$SDI.Index.Value.y-sds_aapcf2$SDI.Index.Value.x)/sds_aapcf2$SDI.Index.Value.x

cc <- ggplot(sds_aapcf2,aes(aapc_change,eapcs))+
  geom_point(size = 2.6,fill = 'gray30',shape = 21)+
  #scale_color_jama(name = 'Year')+
  geom_smooth(method = lm)+
  theme_bw()+
  labs(x = 'SDI changes between 1990 and 2021',
       y = 'AAPC of female breast cancer \nASR between 1990 and 2021')
cor.test(sds_aapcf2$eapcs,sds_aapcf2$aapc_change)


country_aapcM <- read.csv('country_ASRaapc_male.csv')
aapcM <- country_aapcM[country_aapcM$location!='Gloabl',-1]


sds_aapcM <- full_join(aapcM,sdis,by = c('location'='Location'))

cor.test(sds_aapcM$eapcs,sds_aapcM$SDI.Index.Value.x)
cor.test(sds_aapcM$eapcs,sds_aapcM$SDI.Index.Value.y)

sds_aapcM2 <- sds_aapcM[,c(1,3,9,12)]
sds_aapcM2$aapc_change <- (sds_aapcM2$SDI.Index.Value.y-sds_aapcM2$SDI.Index.Value.x)/sds_aapcM2$SDI.Index.Value.x

d <- ggplot(sds_aapcM2,aes(aapc_change,eapcs))+
  geom_point(size = 2.6,fill = 'gray30',shape = 21)+
  #scale_color_jama(name = 'Year')+
  geom_smooth(method = lm)+
  theme_bw()+
  labs(x = 'SDI changes between 1990 and 2021',
       y = 'AAPC of male breast cancer \nASR between 1990 and 2021')

plot_grid(a,b,cc,d,ncol=2,labels = 'AUTO')

