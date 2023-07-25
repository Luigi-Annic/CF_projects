#### datasets ####

#setwd('A:\\880421_Prais_Mei-Zahav')
df<- read.table('db_Prais_all.csv', header = TRUE, sep= ',')


df$country2<-ifelse(df$country=="AL","Albania",
             ifelse(df$country=="AM","Armenia",
             ifelse(df$country=="AT","Austria",
             ifelse(df$country=="BE","Belgium",
             ifelse(df$country=="BG","Bulgaria",
             ifelse(df$country=="BY","Belarus",
             ifelse(df$country=="HR","Croatia",
             ifelse(df$country=="CY","Cyprus",
             ifelse(df$country=="CZ","Czech Republic",
             ifelse(df$country=="DK","Denmark",
             ifelse(df$country=="FR","France",
             ifelse(df$country=="GE","Georgia",
             ifelse(df$country=="DE","Germany",
             ifelse(df$country=="GR","Greece",
             ifelse(df$country=="HU","Hungary",
             ifelse(df$country=="IE","Ireland",
             ifelse(df$country=="IL","Israel",
             ifelse(df$country=="IT","Italy",
             ifelse(df$country=="LV","Latvia",
             ifelse(df$country=="LT","Lithuania",
             ifelse(df$country=="LU","Luxembourg",
             ifelse(df$country=="MK","North Macedonia",
             ifelse(df$country=="MD","Moldova",
             ifelse(df$country=="NL","Netherlands",
             ifelse(df$country=="NO","Norway",
             ifelse(df$country=="PT","Portugal",
             ifelse(df$country=="PL","Poland",
             ifelse(df$country=="RO","Romania",
             ifelse(df$country=="RU","Russian Federation",
             ifelse(df$country=="RS","Serbia",
             ifelse(df$country=="SK","Slovak Republic",
             ifelse(df$country=="SI","Slovenia",
             ifelse(df$country=="ES","Spain",
             ifelse(df$country=="CH","Switzerland",
             ifelse(df$country=="SE","Sweden",
             ifelse(df$country=="TR","Turkey",
             ifelse(df$country=="UA","Ukraine",
             ifelse(df$country=="GB","United Kingdom",NA))))))))))))))))))))))))))))))))))))))

df_all<- df # df_all comprende anche pz con NA per mrsa
rm(df)
df_mrsa<- subset(df_all, !is.na(df_all$MRSA))
#df_mrsa_infants<- subset(df_mrsa, df_mrsa$age_start<18)
#df_mrsa_adults<- subset(df_mrsa, df_mrsa$age_start>= 18)

df<- df_mrsa #df and df_mrsa are synonims from now.

df$ivDaysHosp_old<- df$ivDaysHosp
df$ivDaysTotal_old<- df$ivDaysTotal
df$HospDaysTotal_old<- df$HospDaysTotal

df$ivDaysHosp[df$ivDaysHosp== -1]<- NA
df$ivDaysTotal[df$ivDaysTotal==-1 | df$ivDaysTotal== -99]<- NA
df$HospDaysTotal[df$HospDaysTotal==-1 | df$HospDaysTotal== -99] <- NA


#setwd('A:\\880421_Prais_Mei-Zahav\\csv_mrsa\\All')

#### Table 1 ####

tab1_country<- addmargins(table(df_mrsa$country2, df_mrsa$MRSA))

country_names<- dimnames(tab1_country)[[1]]

country_prev<- c()
IC_country_inf<- c() 
IC_country_sup<- c()

for (x in (1: length(country_names)))
{
  w <- binom.test(tab1_country[x,2],tab1_country[x,3])$conf.int[1]
  IC_country_inf[x] <- w
  y <- binom.test(tab1_country[x,2],tab1_country[x,3])$conf.int[2]
  IC_country_sup[x] <- y
  z <- binom.test(tab1_country[x,2],tab1_country[x,3])$estimate
  country_prev[x] <- z
}

rm(w,y,z)

t_country<- cbind(country_prev,IC_country_inf,IC_country_sup)
dimnames(t_country)<- list(c(country_names),
                           c('estimate%', 'CI inf', 'CI sup'))
t_country<- format(round(t_country*100,1), nsmall=1, trim=T)

t_country_new<-paste(t_country[,1], " (",
                     t_country[,2], " - ",
                     t_country[,3], ")", sep="")

n_avail_data<- addmargins(table(df_mrsa$country2))

tab1<- cbind( n_avail_data,t_country_new)
dimnames(tab1)<- list(c(country_names), c('number of available data', 'prevalence% MRSA+ (95% CI)'))

# script per aggiungere a tab1 high income e low income
tab_income<- addmargins(table(df_mrsa$country_group, df_mrsa$MRSA, useNA = 'ifany'))
income_prev_inf<- c(binom.test(tab_income[1,2], tab_income[1,3])$conf.int[1],
                    binom.test(tab_income[2,2], tab_income[2,3])$conf.int[1])
income_prev_inf<- format(round(income_prev_inf*100,1),nsmall=1, trim= T)

income_prev_sup<- c(binom.test(tab_income[1,2], tab_income[1,3])$conf.int[2],
                    binom.test(tab_income[2,2], tab_income[2,3])$conf.int[2])
income_prev_sup<- format(round(income_prev_sup*100,1),nsmall=1, trim= T)

income_prev_exact<- c(binom.test(tab_income[1,2], tab_income[1,3])$estimate,
                      binom.test(tab_income[2,2], tab_income[2,3])$estimate)
income_prev_exact<- format(round(income_prev_exact*100,1),nsmall=1, trim= T)

income_prev<- paste(income_prev_exact, '(', income_prev_inf, ' - ', income_prev_sup, ')', sep='')

tab1<- rbind( tab1, cbind(tab_income[1:2,3], income_prev))

write.table(tab1, file= 'tab1.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)

#### Table 0 ####

addmargins(table(df_all$country2,is.na(df_all$MRSA)))
tab_miss<-round(prop.table(addmargins(table(df_all$country2,is.na(df_all$MRSA))),1)*2*100,2)
num_miss<-tab_miss[,2]


tab_miss_t<-addmargins(table(df_all$country2,is.na(df_all$MRSA), df_all$lung_t))
tab_miss_no_tran<-tab_miss_t[,,1]
tab_miss_tran<-tab_miss_t[,,2]

tab0<-cbind(tab_miss_no_tran[,3], paste(tab_miss_no_tran[,2], " (",
                                        format(round(prop.table(tab_miss_no_tran[,1:2],1)*100,1), nsmall=1, trim=T)[,2], "%)", sep=""),
            tab_miss_tran[,3], paste(tab_miss_tran[,2], " (",
                                     format(round(prop.table(tab_miss_tran[,1:2],1)*100,1), nsmall=1, trim=T)[,2], "%)", sep=""))
dimnames(tab0)[[2]]<- c('NTtot', 'NT NA_mrsa', 'TransTot', 'Trans NA_mrsa' )

write.table(tab0, file= 'tab0.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)

#### map Europe ####

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rgeos)

europe<- ne_countries(scale= 'medium',continent = 'Europe' ,returnclass = 'sf')
asia<- ne_countries(scale='medium', continent='Asia', returnclass = 'sf')
africa<- ne_countries(scale= 'medium', continent= 'Africa', returnclass = 'sf')
eurasiafrica<- rbind(europe, asia, africa)

country_prev<- round(country_prev*100, 2)
#prev_data<- cbind(country_names, country_prev)

#se voglio escludere le nazioni con pochi abitanti, usare il comando qui sotto:
prev_data<- cbind(country_names[n_avail_data>=10], country_prev[n_avail_data>=10])

map_europe_complete<-cbind(eurasiafrica$name, eurasiafrica$pop_est)

prev_data_map<- prev_data[-(length(prev_data[,1])),]

prev_data_map_corrected<- ifelse(prev_data_map[,1]=='Slovak Republic','Slovakia',
                          ifelse(prev_data_map[,1]=="Russian Federation","Russia",
                          ifelse(prev_data_map[,1]=="North Macedonia",'Macedonia',
                          ifelse(prev_data_map[,1]=="Czech Republic",'Czech Rep.',
                                 prev_data_map[,1]))))

prev_data_map<- cbind(prev_data_map, rep(0, length(prev_data_map[,1])),
                      prev_data_map_corrected)

map_europe_complete<- cbind(eurasiafrica$name, eurasiafrica$pop_est,
                            rep(NA,length(map_europe_complete[,1])))


for (z in c(1:dim(eurasiafrica)[1]))
{
  for (k in c(1: length(prev_data_map[,1])))
  {
    if (prev_data_map[k,4]==map_europe_complete[z,1])
    {
      map_europe_complete[z,3]<- prev_data_map[k,2]
      prev_data_map[k,3]<- 1
    }}}

prevalence<- as.numeric(map_europe_complete[,3])



MRSA_nations<- subset(eurasiafrica, 
                     (!is.na(prevalence) & eurasiafrica$sov_a3!= 'NOR'
                      & eurasiafrica$iso_a2!= 'GB'
                      & eurasiafrica$iso_a2!= 'DK'
                      & eurasiafrica$iso_a2!= 'GR'
                      & eurasiafrica$iso_a2!= 'FR'
                      & eurasiafrica$iso_a2!= 'RU'
                      & eurasiafrica$iso_a2!= 'IL'
                      & eurasiafrica$iso_a2!= 'HR'))

eurasiafrica_points<- st_centroid(MRSA_nations)
eurasiafrica_points<- cbind(MRSA_nations, st_coordinates(st_centroid(MRSA_nations$geometry)))



tiff(filename= 'MRSA_map_all.jpg', width= 1500, height= 1620)
ggplot(data= eurasiafrica) +
  geom_sf(color= 'black', aes(fill= prevalence)) +
  theme(legend.key.size = unit(3.5, 'lines'),
        legend.text = element_text(size= 20),
        legend.title = element_text(size= 20),
        plot.title = element_text(size=20))+
  labs(fill='Prevalence')+
  xlab('') + ylab('')+
  scale_fill_viridis_c(na.value = 'grey80', direction=-1, limits= c(0,22))+
  coord_sf(xlim= c(-25, 60), ylim= c(25, 70), expand= FALSE, label_axes = 'SW')+
  ggtitle('MRSA prevalence per country, whole cohort')+
  geom_text(data= eurasiafrica_points, aes(x=X, y=Y,label= iso_a2),size=5, color= 'black', fontface= 'bold')+
  annotate(geom= 'text',x=40, y=55, label= 'RU', color= 'black', size=5, fontface= 'bold')+
  annotate(geom= 'text',x=10, y=61, label= 'NO', color= 'black', size=5, fontface= 'bold')+
  annotate(geom='text', x=-1, y=52.5, label= 'GB', color= 'black', size=5,fontface= 'bold')+
  annotate(geom= 'text', x= 9, y= 56, label= 'DK', color= 'black', size= 5, fontface= 'bold')+
  annotate(geom= 'text', x= 17, y= 45.5, label= 'HR', color= 'black', size= 5, fontface= 'bold')+
  annotate(geom= 'text',x= 21.5, y= 39.5, label= 'GR', color= 'black', size= 5, fontface= 'bold')+
  annotate(geom= 'text',x= 3.5, y= 47, label= 'FR', color= 'black', size= 5, fontface= 'bold')+
  annotate(geom= 'text',x= 35, y= 31, label= 'IL', color= 'black', size= 5, fontface= 'bold')

dev.off()

#### table 2 ####


summary(df$bmi_z)

etaf<- c(df$age_start)
etaf[df$age_start<5]= 'a'; etaf[df$age_start>=5 & df$age_start<10]= 'b';
etaf[df$age_start>=10 & df$age_start<15]= 'c'
etaf[df$age_start>=15 & df$age_start<20]= 'd'; etaf[df$age_start>=20 & df$age_start<25]= 'e'
etaf[df$age_start>=25 & df$age_start<30]= 'f'; etaf[df$age_start>=30 & df$age_start<35]= 'g'
etaf[df$age_start>=35 & df$age_start<40]= 'h'; etaf[df$age_start>=40 & df$age_start<45]= 'i'
etaf[df$age_start>=45 & df$age_start<50]= 'j'; etaf[df$age_start>=50]= 'k'

#income_country<- ifelse(df$country_group=='1_low',1,0)
df$country_group01<- ifelse(df$country_group=='1_low',0,1)
df$gender<-ifelse(df$Gender==2,1,0)

age_diagnosis<- c(df$age_dia0)
age_diagnosis[df$age_dia0<1] = 'a'
age_diagnosis[df$age_dia0>=1 & df$age_dia0<=5] = 'b'
age_diagnosis[df$age_dia0>5] = 'c'


f_bmiz<- c(df$bmi_z)
f_bmiz[df$bmi_z>2]='a'
f_bmiz[df$bmi_z>= -2 & df$bmi_z<= 2]='b'
f_bmiz[df$bmi_z< -2]='c'

df$f2_bmiz<- ifelse(df$bmi_z<(-2),1,0)

f_fev1pp<- c(df$FEV1_pp)
f_fev1pp[df$FEV1_pp> 70]= 'a'
f_fev1pp[df$FEV1_pp>= 40 & df$FEV1_pp<= 70]= 'b'
f_fev1pp[df$FEV1_pp< 40]= 'c'

f_fvcpp<- c(df$FVC_pp)
f_fvcpp[df$FVC_pp>70]= 'a'
f_fvcpp[df$FVC_pp>=40 & df$FVC_pp<=70]= 'b'
f_fvcpp[df$FVC_pp<40]= 'c'


cfrd<- ifelse(df$Diabetes==0,0,1)
pneumothorax<- ifelse(df$pneumo== 0,0,1)

df$iva<-ifelse(is.na(df$iva),-1,df$iva)
df$lumaiva<-ifelse(is.na(df$lumaiva),-1,df$lumaiva)
df$tezaiva<-ifelse(is.na(df$tezaiva),-1,df$tezaiva)
df$elexatezaiva<-ifelse(is.na(df$elexatezaiva),-1,df$elexatezaiva)
df$cftrmodoth<-ifelse(is.na(df$cftrmodoth),-1,df$cftrmodoth)
df$cftrmod<-ifelse(df$iva==-1 & df$lumaiva==-1 & df$tezaiva==-1 & 
                     df$elexatezaiva==-1 & df$cftrmodoth==-1,NA,
                   ifelse(df$iva==1 | df$lumaiva==1 | df$tezaiva==1 | 
                     df$elexatezaiva==1 | df$cftrmodoth==1,1,0))

table(df$cftrmod, useNA="ifany")

pseudo_past2<- ifelse(df$pseudo_past==0, 0, 1) # chiedere!!
df$pseudo_4cat<- ifelse(df$pseudo_past==0 & df$pseudo==0,0,
                 ifelse(df$pseudo_past==0 & df$pseudo==1,1,
                 ifelse(df$pseudo_past==1 & df$pseudo==0,2,
                 ifelse(df$pseudo_past==1 & df$pseudo==1,3,NA))))

df$MRSA_PSA<-ifelse(df$MRSA==0 & df$pseudo==0,1, # MRSA no, pseudo no
                    ifelse(df$MRSA==0 & df$pseudo==1,2, # MRSA no, pseudo si
                           ifelse(df$MRSA==1 & df$pseudo==0,3, # MRSA si, pseudo no
                                  ifelse(df$MRSA==1 & df$pseudo==1,4,NA)))) # MRSA si, pseudo si
table(df$MRSA_PSA)
table(df$ivDaysTotal, useNA = 'always')

ivdaystotal2<- c(df$ivDaysTotal)
ivdaystotal2 <- ifelse(df$ivDaysTotal== 0, 0,
                       ifelse(df$ivDaysTotal> 0, 1, NA))
table(ivdaystotal2, useNA = 'always')


ivdayshosp2<- c(df$ivDaysHosp)

ivdayshosp2[df$ivDaysHosp==-1]<- NA
ivdayshosp2[df$ivDaysHosp==0]<- 0
ivdayshosp2[df$ivDaysHosp > 0]<- 1

table(ivdayshosp2, useNA = 'always')

hospdaystot2<- c(df$HospDaysTotal)
hospdaystot2 <- ifelse(df$HospDaysTotal== 0, 0,
                       ifelse(df$HospDaysTotal> 0, 1, NA))
table(hospdaystot2, useNA = 'always')


mat2_total<- cbind(etaf, df$gender, df$country_group ,age_diagnosis, df$class_geno,
                   df$enzymes, df$f2_bmiz, f_fev1pp, f_fvcpp,
                   pseudo_past2, df$pseudo, df$pseudo_4cat, df$burkho, df$staph,
                   df$malto, df$achro, df$myco,
                   ivdaystotal2, ivdayshosp2, hospdaystot2,
                   df$NaCl, df$rhDNase, df$Antibiotic, df$azitro,
                   df$steroid_inhaled, df$steroid_oral, df$cftrmod,
                   df$ABPA, cfrd, pneumothorax, df$hemo, df$lung_t)

l_mat2<- dim(mat2_total)[2] 

table2_nn<- c()
table2_pp<- c()
tab2_n_mrsa<- c()

for (x in (1:l_mat2))
{
  n<- as.numeric(table(mat2_total[,x]))  
  table2_nn<- c(table2_nn, n)
  mrsaN<- as.numeric(table(mat2_total[,x],df$MRSA)[,2])
  tab2_n_mrsa<- c(tab2_n_mrsa, mrsaN)
  p<- as.numeric(prop.table(table(mat2_total[,x], df$MRSA),1)[,2])
  p<- round(p*100,2)
  table2_pp<- c(table2_pp, p)
}

rm(x,p,n, mrsaN)

table2_pp<- format(table2_pp, nsmall=1, trim=T)
table2_pasted<- paste(tab2_n_mrsa,' (', table2_pp, '%)', sep='')
table2_descr<- cbind(table2_nn,table2_pasted)

dimnames(table2_descr)<- list(c('age 0-4', 'age 5-9', 'age 10-14',
                                'age 15-19', 'age 20-24', 'age 25-29',
                                'age 30-34', 'age 35-39', 'age 40-44',
                                'age 45-49', 'age 50+',
                                'Male', 'Female',
                                'Low income country', 'High income country',
                                'age at diagnosis < 1 y',
                                'age at diagnosis 1-5 y',
                                'age at diagnosis > 5 y',
                                'genotype: (mild)','genotype:(severe)',
                                'genotype: (unknown)',
                                'no pancreatic enzymes', 'yes pancreatic enzymes',
                                'BMI no underweight', 'BMI underweight',
                                'FEV1%>70', '40% <= FEV1% <= 70%', 'FEV1% < 40%',
                                'FVC >70', '40% <= FVC <= 70%', 'FVC < 40%',
                                'NO pseudopast', 'Pseudopast', 'NO Pseudo', 'Pseudo', 
                                'NO pseudopast no present', 'No Pseudopast yes present',
                                'Yes pseudopast no present', 'Pseudo past and present', 
                                'NO Burkh', 'Burkh',
                                'NO staph', 'staph', 'NO malto', 'Malto', 
                                'NO Achro', 'Achro', 'NO myco', 'myco',
                                'NO IVtotal', 'IVtotal', 'NO IVHosp', 'IVhosp',
                                'NO Hospital', 'Hospital',
                                'NO hyp sal', 'hyp sal',
                                'NO Dornase alfa', 'Dornase alfa',
                                'NO inhaled antibiotics', 'inhaled antibiotics',
                                'NO macrolides', 'macrolides',
                                'NO inhaled steroids', 'inhaled steroids',
                                'NO oral steroids', 'oral steroids',
                                'NO CFTRmod', 'CFTRmod',
                                'NO ABPA', 'ABPA', 'NO CFRD', 'CFRD',
                                'NO pneumothorax', 'pneumothorax',
                                'NO haemoptysis', 'haemoptysis',
                                'NO lung transplant', 'lung transplant'),
                              c('N patients', 'N (%) MRSA+'))


# odds-ratio e pvalue

OR_tab2<- c()
IC_tab2_inf<- c()
IC_tab2_sup<- c()
pval_tab2<- c()


for (g in (1:l_mat2))
{
  rlogz<- glm(df$MRSA~mat2_total[,g], binomial)
  c_OR_tab2<- as.numeric(exp(coef(rlogz)))
  c_OR_tab2<- round(c_OR_tab2,2)
  c_OR_tab2<- format(c_OR_tab2, nsmall=1, trim=T)
  c_OR_tab2[[1]]<- '(reference)'
  OR_tab2<- c(OR_tab2, c_OR_tab2)
  
  c_IC_tab2<- exp(confint(rlogz))
  c_IC_tab2<- round(c_IC_tab2,2)
  c_IC_tab2<- format(c_IC_tab2, nsmall=1, trim=T)
  c_IC_tab2_inf<- c_IC_tab2[,1]
  c_IC_tab2_inf[[1]]<- ''
  c_IC_tab2_inf<- as.character(c_IC_tab2_inf)
  IC_tab2_inf<- c(IC_tab2_inf, c_IC_tab2_inf)
  c_IC_tab2_sup<- c_IC_tab2[,2]
  c_IC_tab2_sup[[1]]<- ''
  c_IC_tab2_sup<- as.character(c_IC_tab2_sup)
  IC_tab2_sup<- c(IC_tab2_sup, c_IC_tab2_sup)
  
  c_pval_tab2<- summary(rlogz)$coef[,'Pr(>|z|)']
  c_pval_tab2<- round(c_pval_tab2,3)
  c_pval_tab2<- format(c_pval_tab2, nsmall =  3)
  c_pval_tab2<- ifelse(c_pval_tab2=='0.000','<0.001',c_pval_tab2)
  c_pval_tab2[[1]]<- ''
  c_pval_tab2<- as.character(c_pval_tab2)
  pval_tab2<- c(pval_tab2,c_pval_tab2)
  
}

ICOR_tab2<- paste(OR_tab2, ' (', IC_tab2_inf, ' - ', IC_tab2_sup, ')', sep='' )
ICOR_tab2<- ifelse(ICOR_tab2== "(reference) ( - )", '(reference)', ICOR_tab2)

tab2_total<- cbind(table2_descr, ICOR_tab2, pval_tab2)

tab2_total_ages<- tab2_total[1:11, 1:2]

write.table(tab2_total_ages, file= 'tab_prev_ages.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)

tab2_total<- tab2_total[12:76,]


write.table(tab2_total, file= 'tab2.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)

#### Table age, with Walter dummies ####

df$w1<-ifelse(df$age_start<5,0,1)
df$w2<-ifelse(df$w1==0|df$age_start<10,0,1)
df$w3<-ifelse(df$w2==0|df$age_start<15,0,1)
df$w4<-ifelse(df$w3==0|df$age_start<20,0,1)
df$w5<-ifelse(df$w4==0|df$age_start<25,0,1)
df$w6<-ifelse(df$w5==0|df$age_start<30,0,1)
df$w7<-ifelse(df$w6==0|df$age_start<35,0,1)
df$w8<-ifelse(df$w7==0|df$age_start<40,0,1)
df$w9<-ifelse(df$w8==0|df$age_start<45,0,1)
df$w10<-ifelse(df$w9==0|df$age_start<50,0,1)


rlogz<- glm(MRSA~w1+w2+w3+w4+w5+w6+w7+w8+w9+w10, data=df,
            family=binomial(link="identity"))
wvals<- (summary(rlogz)$coef)
west<- round(as.numeric(wvals[,1])*100,2)[-1]
west<- format(west, nsmall= 2, trim= T)
wpval<- round(as.numeric(wvals[,4]),3)[-1]
wpval<- format(wpval, nsmall= 3, trim= T)
wpval<- ifelse(wpval== '0.000', '<0.001', wpval)

wconf<- confint(rlogz, parm= 2:11)
wconf<- round(wconf*100, 2)
wconf<- format(wconf, nsmall= 2, trim= T)

wconf_inf<- wconf[,1]
wconf_sup<- wconf[,2]

wpaste<- paste(west, ' (', wconf_inf, ' - ', wconf_sup, ')', sep='')

wtab<- cbind(table2_descr[1:11,], rbind(c('',''), cbind(wpaste, wpval)))


write.table(wtab, file='table_age.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= FALSE, col.names= TRUE)

#### Graph age ####

tiff(filename= 'age_prev_MRSA.tiff', width= 1600, height= 800)


prevs<- as.numeric(table2_pp[1:11])

par(mar= c(7,7,4,2), mgp= c(5,2.4,0))
barplot(prevs, col= 'grey50', space=1,
        names.arg = c('0-4','5-9','10-14','15-19',
                      '20-24','25-29','30-34','35-39','40-44',
                      '45-49','50 +'),
        las=1, cex.axis=1.7, main= '',
        xlab= 'Age group', ylab= 'MRSA infection prevalence',
        cex.names= 1.7,ylim = c(0,10),
        cex.lab= 2.4, oma= c(4,4,4,4))

axis(2, at= c(1,3,5,7,9), las= 1, cex.axis= 1.7)

box()

dev.off()

#### Table 3 ####

#categorical variables

# In this table, class_geno == 'unknown' is considered as NA.
geno_cat<- ifelse(df$class_geno=='1_mild',0,
           ifelse(df$class_geno=='2_severe', 1, NA))
df$death_inverted<- ifelse(df$status==0,1,0)

mat3_categoric<- cbind(df$gender, df$country_group,geno_cat ,df$enzymes, df$f2_bmiz,
                       pseudo_past2, df$pseudo, df$burkho, df$staph,
                       df$malto, df$achro, df$myco,
                       df$NaCl, df$rhDNase, df$Antibiotic,df$azitro,
                       df$steroid_inhaled, df$steroid_oral, df$cftrmod,
                       df$ABPA, cfrd,  pneumothorax, df$hemo, df$lung_t,
                       df$death_inverted)
l_mat3_cat<- dim(mat3_categoric)[2]

N_dem_cat0<- c()
N_dem_cat1<- c()
tab3_nas0<- c()
tab3_nas1<- c()
prop_cat0<- c()
prop_cat1<- c()

OR_3_cat<- c()
ICOR_inf_3_cat<- c()
ICOR_sup_3_cat<- c()
pval_3_cat<- c()

for (n in (1:l_mat3_cat))
{
  n0<- table(mat3_categoric[,n], df$MRSA)[[2,1]] 
  n1<- table(mat3_categoric[,n], df$MRSA)[[2,2]]
  N_dem_cat0<- c(N_dem_cat0, n0)  
  N_dem_cat1<- c(N_dem_cat1, n1)
  n_nas0<- length((mat3_categoric[,n])[is.na(mat3_categoric[,n]) & df$MRSA==0 & !is.na(df$MRSA)])
  tab3_nas0<- c(tab3_nas0, n_nas0)
  n_nas1<- length((mat3_categoric[,n])[is.na(mat3_categoric[,n]) & df$MRSA==1 & !is.na(df$MRSA)])
  tab3_nas1<- c(tab3_nas1,n_nas1)
  p_cat0<- prop.table(table(mat3_categoric[,n], df$MRSA),2)[[2,1]]
  p_cat1<- prop.table(table(mat3_categoric[,n], df$MRSA),2)[[2,2]]
  prop_cat0<- c(prop_cat0, p_cat0)
  prop_cat1<- c(prop_cat1, p_cat1)
  
  rlog1<- glm(df$MRSA~mat3_categoric[,n], binomial)
  calc_OR<- exp(coef(rlog1))[[2]]
  calc_ICOR<- as.numeric(exp(confint(rlog1, parm=2)))
  calc_ICOR_inf<- calc_ICOR[[1]]
  calc_ICOR_sup<- calc_ICOR[[2]]
  calc_pval<- summary(rlog1)$coef[2,'Pr(>|z|)']
  OR_3_cat<- c(OR_3_cat,calc_OR)
  ICOR_inf_3_cat<- c(ICOR_inf_3_cat, calc_ICOR_inf)
  ICOR_sup_3_cat<- c(ICOR_sup_3_cat, calc_ICOR_sup)
  pval_3_cat<- c(pval_3_cat,calc_pval)
}

prop_cat0<- format(round(prop_cat0*100,1), nsmall=1, trim=T)
prop_cat1<- format(round(prop_cat1*100,1), nsmall=1, trim=T)

cat0<-paste(N_dem_cat0, " (", prop_cat0, "%)", sep="")
cat1<-paste(N_dem_cat1, " (", prop_cat1, "%)", sep="")

OR_3_cat<- format(round(OR_3_cat,1), nsmall=1, trim=T)
ICOR_inf_3_cat<- format(round(ICOR_inf_3_cat,1), nsmall=1, trim=T)
ICOR_sup_3_cat<- format(round(ICOR_sup_3_cat,1), nsmall=1, trim=T)
pval_3_cat<- format(round(pval_3_cat,3), nsmall=3, trim=T)
pval_3_cat<- ifelse(pval_3_cat=='0.000', '<0.001', pval_3_cat)
OR_IC_3_cat<- paste(OR_3_cat, ' (', ICOR_inf_3_cat, ' - ',
                    ICOR_sup_3_cat, ')', sep='')

tab3_categoric<- cbind(tab3_nas0, cat0, tab3_nas1, cat1, OR_IC_3_cat, pval_3_cat)
dimnames(tab3_categoric)<- list(c('gender: female', 'income country: high',
                                  'genotype: severe', 'pancreatic enzymes',
                                  'bmi underweight',
                                  'pseudomonas past', 'chronic pseudomonas',
                                  'burkholderia', 'staph' ,'stenotrophomonas',
                                  'achromobacter', 'nontub. myco',
                                  'NaCl', 'Dornase alpha', 'Inhaled antibiotics',
                                  'macrolides', 'inhaled steroids',
                                  'Oral steroids', 'CFTR modulators',
                                  'ABPA', 'CFRD', 'pneumothorax', 'haemoptysis',
                                  'lung transplant', 'death'),
                                c("NA MRSA-", 'MRSA-','NA MRSA+' ,'MRSA+','OR', 'pvalue'))

write.table(tab3_categoric, file= 'tab3_cat.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)

tab_p<- table(df$pseudo_4cat, df$MRSA)
p_cat_p<- prop.table(tab_p,2)
prop_cat0<- format(round(p_cat_p[,1]*100,1), nsmall=1, trim=T)
prop_cat1<- format(round(p_cat_p[,2]*100,1), nsmall=1, trim=T)
cat0<-paste(tab_p[,1], " (", prop_cat0, "%)", sep="")
cat1<-paste(tab_p[,1], " (", prop_cat1, "%)", sep="")

rlog1<- glm(df$MRSA~factor(df$pseudo_4cat), binomial)
OR_3_cat<- exp(coef(rlog1))
calc_ICOR<- exp(confint(rlog1))
ICOR_inf_3_cat<- calc_ICOR[,1]
ICOR_sup_3_cat<- calc_ICOR[,2]
pval_3_cat<- summary(rlog1)$coef[,'Pr(>|z|)']

OR_3_cat<- format(round(OR_3_cat,1), nsmall=1, trim=T)
ICOR_inf_3_cat<- format(round(ICOR_inf_3_cat,1), nsmall=1, trim=T)
ICOR_sup_3_cat<- format(round(ICOR_sup_3_cat,1), nsmall=1, trim=T)
pval_3_cat<- format(round(pval_3_cat,3), nsmall=3, trim=T)
pval_3_cat<- ifelse(pval_3_cat=='0.000', '<0.001', pval_3_cat)
OR_IC_3_cat<- paste(OR_3_cat, ' (', ICOR_inf_3_cat, ' - ',
                    ICOR_sup_3_cat, ')', sep='')

cbind(cat0, cat1, OR_IC_3_cat, pval_3_cat)

# numerical variables

mat3_numeric<-cbind(df$age_start,df$age_dia0, df$bmiECFSPR, df$bmi_z,
                    df$FEV1_pp, df$FVC_pp,
                    df$ivDaysTotal, df$ivDaysHosp, df$HospDaysTotal)
hist(df$age_start)
hist(df$age_dia0)
hist(df$bmi_z)
hist(df$FEV1_pp)
hist(df$FVC_pp)
hist(df$ivDaysTotal)
hist(df$ivDaysHosp)
hist(df$HospDaysTotal)
# ANNALISA: lascio l'ODDS ratio come misura di associazione, anche per le variabili numeriche,
# anche se la sua interpretazione non ? immediata. 
# Del resto la distribuzione di alcune variabili numeriche non solo ? asimmetrica
# ma ? fortemente sbilanciata verso lo 0 o comunque valori bassi.
# Ogni variabile richiederebbe un ragionamento a se. Si valuti bene se nell'articolo si vogliono lasciare le variabili in continuo
# o utilizzare invece una loro categorizzazione


qq0<- c()
qq1<-c()
tab3_numeric_nas0<- c()
tab3_numeric_nas1<- c()

OR_3_num<- c()
ICOR_inf_3_num<- c()
ICOR_sup_3_num<- c()
pval_3_num<- c()

for (n in (1:dim(mat3_numeric)[2]))
{
  t3_num_nas0<- length((mat3_numeric[,n])[is.na(mat3_numeric[,n]) & df$MRSA==0 & !is.na(df$MRSA)])
  tab3_numeric_nas0<- c(tab3_numeric_nas0, t3_num_nas0) 
  t3_num_nas1<- length((mat3_numeric[,n])[is.na(mat3_numeric[,n]) & df$MRSA==1 & !is.na(df$MRSA)])
  tab3_numeric_nas1<- c(tab3_numeric_nas1, t3_num_nas1) 
  q0<- as.numeric(tapply(mat3_numeric[,n], df$MRSA, quantile, na.rm=TRUE,
                         probs= c(.25,.5,.75))[[1]])
  qq0<- rbind(qq0,q0)
  q1<- as.numeric(tapply(mat3_numeric[,n], df$MRSA, quantile, na.rm=TRUE,
                         probs= c(.25,.5,.75))[[2]])
  qq1<- rbind(qq1,q1)
  
  rlog2<- glm(df$MRSA~mat3_numeric[,n], binomial)
  calc_OR_num<- exp(coef(rlog2))[[2]]
  calc_ICOR_num<- as.numeric(exp(confint(rlog2,parm=2)))
  calc_ICOR_inf_num<- calc_ICOR_num[[1]]
  calc_ICOR_sup_num<- calc_ICOR_num[[2]]
  calc_pval_num<- summary(rlog2)$coef[2,'Pr(>|z|)']
  OR_3_num<- c(OR_3_num, calc_OR_num)
  ICOR_inf_3_num<- c(ICOR_inf_3_num, calc_ICOR_inf_num)
  ICOR_sup_3_num<- c(ICOR_sup_3_num, calc_ICOR_sup_num)
  pval_3_num<- c(pval_3_num, calc_pval_num)
}
qq0<- format(round(qq0,1),nsmall=1, trim=T)
qq1<- format(round(qq1,1),nsmall=1, trim=T)

t3_ac0<-paste(qq0[,2], " (" ,qq0[,1], " - ", qq0[,3], ")", sep="")
t3_ac1<-paste(qq1[,2], ' (', qq1[,1], ' - ', qq1[,3], ')', sep='')


OR_3_num<- format(round(OR_3_num,1), nsmall=1, trim=T)
ICOR_inf_3_num<- format(round(ICOR_inf_3_num,1), nsmall=1, trim=T)
ICOR_sup_3_num<- format(round(ICOR_sup_3_num,1), nsmall=1, trim=T)
pval_3_num<- format(round(pval_3_num, 3), nsmall=3, trim=T)
pval_3_num<- ifelse(pval_3_num=='0.000','<0.001',pval_3_num)

OR_IC_3_num<- paste(OR_3_num, ' (',ICOR_inf_3_num, ' - ',ICOR_sup_3_num, ')', sep='')

tab3_numeric<- cbind(tab3_numeric_nas0,t3_ac0, tab3_numeric_nas1, t3_ac1,
                     OR_IC_3_num, pval_3_num)

dimnames(tab3_numeric)<- list(c('current age', 'age at diagnosis',
                                'BMI', 'BMI z-score', 'FEV1 % of predicted',
                                'FVC%', 'IvDaysTOT', 'IvDays hosp',
                                'hosp days'),
                              c('NA MRSA-', 'MRSA q2(q1-q3)',
                                'NA MRSA+','MRSA q2(q1-q3)',
                                'OR (95% CI)', 'pvalue'))

write.table(tab3_numeric, file= 'tab3_num_gen.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)

#### table3 fev, fevchange, fvc, fvcchange per age group #### 
df_age1 <- subset(df,df$age_start>=6 & df$age_start<12 )
df_age2 <- subset(df, df$age_start>=12 & df$age_start<18)
df_age3 <- subset(df, df$age_start>=18 & df$age_start<30)
df_age4<- subset(df,df$age_start>=30 & df$age_start<40)
df_age5<- subset(df, df$age_start>=40 & df$age_start<50 )
df_age6<- subset(df, df$age_start>=50)


qq0<- c()
qq1<-c()
tab3_numeric_nas0<- c()
tab3_numeric_nas1<- c()

OR_3_num<- c()
ICOR_inf_3_num<- c()
ICOR_sup_3_num<- c()
pval_3_num<- c()

tab_fev_plot<- c()
tab3_fev<- c()
tab3_fev_change<- c()
tab3_fvc<- c()
tab3_fvc_change<- c()


for (j in (1:6))
{
  if (j==1)
  {df_cons<- df_age1}
  if(j==2)
  {df_cons<- df_age2}
  if (j==3)
  {df_cons<- df_age3}
  if (j==4)
  {df_cons<- df_age4}
  if (j==5)
  {df_cons<- df_age5}
  if (j==6)
  {df_cons<- df_age6}
  
  mat3_numeric<- cbind(df_cons$FEV1_pp,
                       df_cons$fev1_pp_loss,
                       df_cons$FVC_pp,
                       df_cons$fvc_pp_loss)
  
  for (n in (1:dim(mat3_numeric)[2]))
  {
    t3_num_nas0<- length((mat3_numeric[,n])[is.na(mat3_numeric[,n]) & df_cons$MRSA==0 & !is.na(df_cons$MRSA)])
    tab3_numeric_nas0<- c(tab3_numeric_nas0, t3_num_nas0) 
    t3_num_nas1<- length((mat3_numeric[,n])[is.na(mat3_numeric[,n]) & df_cons$MRSA==1 & !is.na(df_cons$MRSA)])
    tab3_numeric_nas1<- c(tab3_numeric_nas1, t3_num_nas1) 
    q0<- as.numeric(tapply(mat3_numeric[,n], df_cons$MRSA, quantile, na.rm=TRUE,
                           probs= c(.25,.5,.75))[[1]])
    qq0<- rbind(qq0,q0)
    q1<- as.numeric(tapply(mat3_numeric[,n], df_cons$MRSA, quantile, na.rm=TRUE,
                           probs= c(.25,.5,.75))[[2]])
    qq1<- rbind(qq1,q1)
    
    
    rlog2<- glm(df_cons$MRSA~mat3_numeric[,n], binomial)
    calc_OR_num<- exp(coef(rlog2))[[2]]
    calc_ICOR_num<- as.numeric(exp(confint(rlog2,parm=2)))
    calc_ICOR_inf_num<- calc_ICOR_num[[1]]
    calc_ICOR_sup_num<- calc_ICOR_num[[2]]
    calc_pval_num<- summary(rlog2)$coef[2,'Pr(>|z|)']
    OR_3_num<- c(OR_3_num, calc_OR_num)
    ICOR_inf_3_num<- c(ICOR_inf_3_num, calc_ICOR_inf_num)
    ICOR_sup_3_num<- c(ICOR_sup_3_num, calc_ICOR_sup_num)
    pval_3_num<- c(pval_3_num, calc_pval_num)
  }
  qq0<- format(round(qq0,1),nsmall=1, trim=T)
  qq1<- format(round(qq1,1),nsmall=1, trim=T)
  
  t3_ac0<-paste(qq0[,2], " (" ,qq0[,1], " - ", qq0[,3], ")", sep="")
  t3_ac1<-paste(qq1[,2], ' (', qq1[,1], ' - ', qq1[,3], ')', sep='')
  
  
  OR_3_num<- format(round(OR_3_num,1), nsmall=1, trim=T)
  ICOR_inf_3_num<- format(round(ICOR_inf_3_num,1), nsmall=1, trim=T)
  ICOR_sup_3_num<- format(round(ICOR_sup_3_num,1), nsmall=1, trim=T)
  pval_3_num<- format(round(pval_3_num, 3), nsmall=3, trim=T)
  pval_3_num<- ifelse(pval_3_num=='0.000','<0.001',pval_3_num)
  
  OR_IC_3_num<- paste(OR_3_num, ' (',ICOR_inf_3_num, ' - ',ICOR_sup_3_num, ')', sep='')
  
  tab3_numeric<- cbind(tab3_numeric_nas0,t3_ac0, tab3_numeric_nas1, t3_ac1,
                       OR_IC_3_num, pval_3_num)
  
  dimnames(tab3_numeric)<- list(c('FEV', 'FEV change', 'FVC', 'FVC change'),
                                c('NA MRSA-', 'MRSA- q2(q1-q3)',
                                  'NA MRSA+','MRSA q2(q1-q3)',
                                  'OR (95% CI)', 'pvalue'))
  
  tab_fev_plot<-rbind(tab_fev_plot, as.numeric(as.character(c(qq0[1,2], qq0[1,1], qq0[1,3], qq1[1,2], qq1[1,1], qq1[1,3]))))
  tab3_fev<- rbind(tab3_fev, tab3_numeric[1,])
  tab3_fev_change<-rbind(tab3_fev_change, tab3_numeric[2,])
  tab3_fvc<- rbind(tab3_fvc, tab3_numeric[3,])
  tab3_fvc_change<- rbind(tab3_fvc_change, tab3_numeric[4,])
  
  if (j==1)
  {
    write.table(tab3_numeric, file= 'tab3_num_age1.csv',
                append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
                row.names= TRUE, col.names= TRUE)
  }
  
  if (j==2)
    
  {
    write.table(tab3_numeric, file= 'tab3_num_age2.csv',
                append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
                row.names= TRUE, col.names= TRUE)
  }
  
  if (j==3)
    
  {
    write.table(tab3_numeric, file= 'tab3_num_age3.csv',
                append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
                row.names= TRUE, col.names= TRUE)
  }
  
  if (j==4)
    
  {
    write.table(tab3_numeric, file= 'tab3_num_age4.csv',
                append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
                row.names= TRUE, col.names= TRUE)
  }
  
  if (j==5)
    
  {
    write.table(tab3_numeric, file= 'tab3_num_age5.csv',
                append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
                row.names= TRUE, col.names= TRUE)
  }
  
  if (j==6)
    
  {
    write.table(tab3_numeric, file= 'tab3_num_age6.csv',
                append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
                row.names= TRUE, col.names= TRUE)
  }
  
  qq0<- c()
  qq1<-c()
  tab3_numeric_nas0<- c()
  tab3_numeric_nas1<- c()
  
  OR_3_num<- c()
  ICOR_inf_3_num<- c()
  ICOR_sup_3_num<- c()
  pval_3_num<- c()
}

write.table(tab3_fev, file= 'tab3_FEV.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)

write.table(tab3_fev_change, file= 'tab3_FEV_change.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)

write.table(tab3_fvc, file= 'tab3_FVC.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)

write.table(tab3_fvc_change, file= 'tab3_FVC_change.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)


# plot on FEV1
plot(1:6,tab_fev_plot[,1], pch=16, cex=0.8, xaxt="n", ylim=c(30,110), xlim=c(1,6.2), las=1, 
     ylab=expression(paste(FEV[1], " percent of predicted", sep="")), xlab="Age groups (years)", col="black", cex.axis=0.9)
axis(1, (1:6)+0.1, c("6-11", "12-17", "18-29", "30-39","40-49", "50+"), cex.axis=0.9)
points(1:6,tab_fev_plot[,2], pch="-", col="black")
points(1:6,tab_fev_plot[,3], pch="-", col="black")
segments(1:6,tab_fev_plot[,2], 1:6, tab_fev_plot[,3], lwd=2, col="black")
points((1:6)+0.2,tab_fev_plot[,4], pch=16, cex=0.8, col="red")
points((1:6)+0.2,tab_fev_plot[,5], pch="-", col="red")
points((1:6)+0.2,tab_fev_plot[,6], pch="-", col="red")
segments((1:6)+0.2,tab_fev_plot[,5], (1:6)+0.2, tab_fev_plot[,6], col="red", lwd=2)
legend("bottomleft", inset=0.02, c("MRSA negative", "MRSA positive"), 
       col=c("black","red"), lwd=2, lty=1, bty="n", cex=0.9)


#############################################################################################
#####                     LONGITUTINAL ANALYSIS ON FEV1 DECLINE                         #####
#############################################################################################

library(rms)

hist(df$age_fev)
# ATTENZIONE: secondo me ? meglio limitare le analisi a 50
df50<-subset(df, age_start<=50)

# spline with 3 knots
mod_fev_rcs_int<-lm(FEV1_pp ~ rcs(age_fev,3)*MRSA, data=df50)
anova(mod_fev_rcs_int)
mod_fev_rcs<-lm(FEV1_pp ~ rcs(age_fev,3)+MRSA, data=df50)
anova(mod_fev_rcs)
summary(mod_fev_rcs, corr=F)
var_cov<-vcov(mod_fev_rcs)
#spline2_pred<-rcspline.eval(seq(6,50,1), knots=spline_knots3, inclx=T)[,2]
pred0<-predict(mod_fev_rcs, newdata = data.frame(age_fev=seq(6,50,1), MRSA=0), 
               type = "response")
pred1<-predict(mod_fev_rcs, newdata = data.frame(age_fev=seq(6,50,1), MRSA=1), 
               type = "response")

plot(6:50, pred0, type="l", col="black", lwd=2, ylim=c(40,100), 
     xlab="Age (years)", ylab=expression(paste("Mean ", FEV[1], " percent of predicted value", sep="")), las=1, cex.axis=0.9)
lines(6:50, pred1, col="red", lwd=2)
legend("bottomleft", inset=0.02, c("MRSA negative", "MRSA positive"), 
       col=c("black","red"), lwd=2, lty=1, bty="n", cex=0.9)

decline0<-pred0[2:45]-pred0[1:44]
decline1<-pred1[2:45]-pred1[1:44]
plot(7:50, decline0, type="l", col="black", lwd=2, las=1, ylim=c(-2.5,0.5),
     xlab="Age (years)", ylab=expression(paste("Mean ", FEV[1], " percent of predicted yearly decline", sep="")), cex.axis=0.9)
lines(7:50, decline1, type="l", col="red", lwd=2)
legend("bottomright", inset=0.02, c("MRSA negative", "MRSA positive"), 
       col=c("black","red"), lwd=2, lty=1, bty="n", cex=0.9)


#### linear regressions ####

rlin_s_diff<- c()
rlin_s_pvalue<-c()
rlin_m_diff<- c()
rlin_pvalue<- c()
for (n in c(1:3))
{
  if (n==1)
  { x = df$FEV1_pp }
  if (n==2)
  { x = df$ivDaysTotal }
  if (n==3)
  { x = df$HospDaysTotal }
  
  rlin_s<- lm(x~df$MRSA)
  rlin_s_coef<-format(round(coef(rlin_s)[2],1),nsmall=1, trim=T)
  rlin_s_CI<- format(round(confint(rlin_s)[2,],1),nsmall=1, trim=T)
  rlin_s_def<- paste(rlin_s_coef, ' (', rlin_s_CI[1], ' - ',
                     rlin_s_CI[2], ')', sep='')
  rlin_s_diff<- c(rlin_s_diff,rlin_s_def)

  rlin_s_pv<- format(round(summary(rlin_s)$coef[2,'Pr(>|t|)'],3),
                   nsmall = 3,trim = T)
  rlin_s_pv<- ifelse(rlin_s_pv== '0.000', '<0.001', rlin_pv)
  rlin_s_pvalue<- c(rlin_s_pvalue, rlin_s_pv)
    
  rlin_m<- lm(x~df$MRSA+df$age_start+df$gender+df$age_dia0+
                df$class_geno+df$bmi_z+ cfrd+ df$pseudo+ df$burkho+df$cftrmod)
  rlin_m_coef<- format(round(coef(rlin_m)[2],1),nsmall=1, trim=T)
  rlin_m_CI<- format(round(confint(rlin_m)[2,],1),nsmall=1, trim=T)
  rlin_m_def<- paste(rlin_m_coef, ' (', rlin_m_CI[1], ' - ',
                        rlin_m_CI[2], ')', sep='')
  rlin_m_diff<- c(rlin_m_diff, rlin_m_def)
  
  rlin_pv<- format(round(summary(rlin_m)$coef[2,'Pr(>|t|)'],3),
                       nsmall = 3,trim = T)
  rlin_pv<- ifelse(rlin_pv== '0.000', '<0.001', rlin_pv)
  rlin_pvalue<- c(rlin_pvalue, rlin_pv)
}

table_rlin<-cbind(rlin_s_diff, rlin_s_pvalue, rlin_m_diff, rlin_pvalue)
dimnames(table_rlin)[[1]]<- c('fev1pp', 'days on ivAnti', 'days hosp')

write.table(table_rlin, file= 'tab4_linear_regression.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)

#### logistic regressions ####

df$lung_death<-ifelse(df$lung_t==1 | df$death_inverted==1,1,0)
df$lung_death<-ifelse(is.na(df$lung_t),df$death_inverted,df$lung_death)

df$composite<-ifelse(df$pneumo==1 | df$hemo==1 | df$lung_t==1 | df$death_inverted==1,1,0)
df$composite<-ifelse(is.na(df$composite) & !is.na(df$lung_t),df$lung_t,df$composite)
df$composite<-ifelse(is.na(df$composite) & !is.na(df$pneumo),df$pneumo,df$composite)
df$composite<-ifelse(is.na(df$composite) & !is.na(df$hemo),df$hemo,df$composite)
df$composite<-ifelse(is.na(df$composite) & !is.na(df$death_inverted),df$death_inverted,df$composite)

df$pneumo_hemo<-ifelse(df$pneumo==1 | df$hemo==1,1,0)
df$pneumo_hemo<-ifelse(is.na(df$pneumo_hemo) & !is.na(df$pneumo),df$pneumo,df$pneumo_hemo)
df$pneumo_hemo<-ifelse(is.na(df$pneumo_hemo) & !is.na(df$hemo),df$hemo,df$pneumo_hemo)

rlog_s_diff<- c()
rlog_s_pvalue<- c()
rlog_m_diff<- c()
rlog_pvalue<- c()

for (n in c(1:3))
{
if (n==1)
{p= df$death_inverted}
if (n==2)
{p= df$composite}
if (n==3)
{p= df$pneumo_hemo}

rlog_s<- glm(p~df$MRSA, binomial)
rlog_s_OR<- format(round(exp(coef(rlog_s))['df$MRSA'],1), nsmall= 2, trim= T)
rlog_s_CI<- format(round(exp(confint(rlog_s))['df$MRSA',],1), nsmall=1, trim=T)
rlog_s_def<- paste(rlog_s_OR, ' (', rlog_s_CI[1], ' - ', rlog_s_CI[2], ')', sep='')
rlog_s_diff<- c(rlog_s_diff,rlog_s_def)

rlog_s_pv<- format(round(summary(rlog_s)$coef['df$MRSA','Pr(>|z|)'],3),
                 nsmall=3,trim=T)
rlog_s_pv<- ifelse(rlog_s_pv== '0.000', '<0.001', rlog_s_pv)
rlog_s_pvalue<- c(rlog_s_pvalue,rlog_s_pv)

rlog_m<- glm(p~df$MRSA+df$age_start+df$gender+df$age_dia0+df$class_geno
             +df$bmi_z+ cfrd+ df$pseudo+ df$burkho+df$cftrmod, binomial)
rlog_m_OR<- format(round(exp(coef(rlog_m))['df$MRSA'],1), nsmall= 2, trim= T)
rlog_m_CI<- format(round(exp(confint(rlog_m))['df$MRSA',],1), nsmall=1, trim=T)
rlog_m_def<- paste(rlog_m_OR, ' (', rlog_m_CI[1], ' - ', rlog_m_CI[2], ')', sep='')
rlog_m_diff<- c(rlog_m_diff,rlog_m_def)

rlog_pv<- format(round(summary(rlog_m)$coef['df$MRSA','Pr(>|z|)'],3),
                 nsmall=3,trim=T)
rlog_pv<- ifelse(rlog_pv== '0.000', '<0.001', rlog_pv)
rlog_pvalue<- c(rlog_pvalue,rlog_pv)
}

table_rlog<- cbind(rlog_s_diff, rlog_s_pvalue, rlog_m_diff, rlog_pvalue)
dimnames(table_rlog)[[1]]<- c('death','composite','pneumo_hemo')
write.table(table_rlog, file= 'tab4_logistic_regression.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)



#### Table 5 ####

#categorical variables

mat5_categoric<- cbind(df$gender, df$country_group01, geno_cat ,df$enzymes, df$f2_bmiz,
                       df$burkho, df$staph, df$malto, df$achro, df$myco,
                       df$NaCl, df$rhDNase, df$Antibiotic, df$azitro,
                       df$steroid_inhaled, df$steroid_oral, df$cftrmod,
                       df$ABPA, cfrd,  pneumothorax, df$hemo, df$lung_t,
                       df$death_inverted)
l_mat5_cat<- dim(mat5_categoric)[2]

N_dem_cat1<- c()
N_dem_cat2<- c()
N_dem_cat3<- c()
N_dem_cat4<- c()
tab5_nas1<- c()
tab5_nas2<- c()
tab5_nas3<- c()
tab5_nas4<- c()
prop_cat1<- c()
prop_cat2<- c()
prop_cat3<- c()
prop_cat4<- c()

OR_5_cat2<- c()
ICOR_inf_5_cat2<- c()
ICOR_sup_5_cat2<- c()
pval_5_cat2<- c()
OR_5_cat3<- c()
ICOR_inf_5_cat3<- c()
ICOR_sup_5_cat3<- c()
pval_5_cat3<- c()
OR_5_cat4<- c()
ICOR_inf_5_cat4<- c()
ICOR_sup_5_cat4<- c()
pval_5_cat4<- c()

for (n in (1:l_mat5_cat))
{
  n1<- table(mat5_categoric[,n], df$MRSA_PSA)[[2,1]] 
  n2<- table(mat5_categoric[,n], df$MRSA_PSA)[[2,2]]
  n3<- table(mat5_categoric[,n], df$MRSA_PSA)[[2,3]] 
  n4<- table(mat5_categoric[,n], df$MRSA_PSA)[[2,4]]
  N_dem_cat1<- c(N_dem_cat1, n1)  
  N_dem_cat2<- c(N_dem_cat2, n2)
  N_dem_cat3<- c(N_dem_cat3, n3)  
  N_dem_cat4<- c(N_dem_cat4, n4)
  n_nas1<- length((mat5_categoric[,n])[is.na(mat5_categoric[,n]) & df$MRSA_PSA==1 & !is.na(df$MRSA_PSA)])
  tab5_nas1<- c(tab5_nas1,n_nas1)
  n_nas2<- length((mat5_categoric[,n])[is.na(mat5_categoric[,n]) & df$MRSA_PSA==2 & !is.na(df$MRSA_PSA)])
  tab5_nas2<- c(tab5_nas2, n_nas2)
  n_nas3<- length((mat5_categoric[,n])[is.na(mat5_categoric[,n]) & df$MRSA_PSA==3 & !is.na(df$MRSA_PSA)])
  tab5_nas3<- c(tab5_nas3, n_nas3)
  n_nas4<- length((mat5_categoric[,n])[is.na(mat5_categoric[,n]) & df$MRSA_PSA==4 & !is.na(df$MRSA_PSA)])
  tab5_nas4<- c(tab5_nas4, n_nas4)
  p_cat1<- prop.table(table(mat5_categoric[,n], df$MRSA_PSA),2)[[2,1]]
  p_cat2<- prop.table(table(mat5_categoric[,n], df$MRSA_PSA),2)[[2,2]]
  p_cat3<- prop.table(table(mat5_categoric[,n], df$MRSA_PSA),2)[[2,3]]
  p_cat4<- prop.table(table(mat5_categoric[,n], df$MRSA_PSA),2)[[2,4]]
  prop_cat1<- c(prop_cat1, p_cat1)
  prop_cat2<- c(prop_cat2, p_cat2)
  prop_cat3<- c(prop_cat3, p_cat3)
  prop_cat4<- c(prop_cat4, p_cat4)
  
  rlog4<- glm(mat5_categoric[,n]~factor(df$MRSA_PSA), binomial)

  calc_ICOR<- exp(confint(rlog4, parm=2:4))
  
  calc_ICOR_inf<- calc_ICOR[1,1]
  calc_ICOR_sup<- calc_ICOR[1,2]
  calc_pval<- summary(rlog4)$coef[2,'Pr(>|z|)']
  calc_OR<- exp(coef(rlog4))[2]
  OR_5_cat2<- c(OR_5_cat2,calc_OR)
  ICOR_inf_5_cat2<- c(ICOR_inf_5_cat2, calc_ICOR_inf)
  ICOR_sup_5_cat2<- c(ICOR_sup_5_cat2, calc_ICOR_sup)
  pval_5_cat2<- c(pval_5_cat2,calc_pval)
  
  calc_ICOR_inf<- calc_ICOR[2,1]
  calc_ICOR_sup<- calc_ICOR[2,2]
  calc_pval<- summary(rlog4)$coef[3,'Pr(>|z|)']
  calc_OR<- exp(coef(rlog4))[3]
  OR_5_cat3<- c(OR_5_cat3,calc_OR)
  ICOR_inf_5_cat3<- c(ICOR_inf_5_cat3, calc_ICOR_inf)
  ICOR_sup_5_cat3<- c(ICOR_sup_5_cat3, calc_ICOR_sup)
  pval_5_cat3<- c(pval_5_cat3,calc_pval)
  
  calc_ICOR_inf<- calc_ICOR[3,1]
  calc_ICOR_sup<- calc_ICOR[3,2]
  calc_pval<- summary(rlog4)$coef[4,'Pr(>|z|)']
  calc_OR<- exp(coef(rlog4))[4] 
  OR_5_cat4<- c(OR_5_cat4,calc_OR)
  ICOR_inf_5_cat4<- c(ICOR_inf_5_cat4, calc_ICOR_inf)
  ICOR_sup_5_cat4<- c(ICOR_sup_5_cat4, calc_ICOR_sup)
  pval_5_cat4<- c(pval_5_cat4,calc_pval)
}

prop_cat1<- format(round(prop_cat1*100,1), nsmall=1, trim=T)
prop_cat2<- format(round(prop_cat2*100,1), nsmall=1, trim=T)
prop_cat3<- format(round(prop_cat3*100,1), nsmall=1, trim=T)
prop_cat4<- format(round(prop_cat4*100,1), nsmall=1, trim=T)

cat1<-paste(N_dem_cat1, " (", prop_cat1, "%)", sep="")
cat2<-paste(N_dem_cat2, " (", prop_cat2, "%)", sep="")
cat3<-paste(N_dem_cat3, " (", prop_cat3, "%)", sep="")
cat4<-paste(N_dem_cat4, " (", prop_cat4, "%)", sep="")

OR_5_cat2<- format(round(OR_5_cat2,1), nsmall=1, trim=T)
ICOR_inf_5_cat2<- format(round(ICOR_inf_5_cat2,1), nsmall=1, trim=T)
ICOR_sup_5_cat2<- format(round(ICOR_sup_5_cat2,1), nsmall=1, trim=T)
pval_5_cat2<- format(round(pval_5_cat2,3), nsmall=3, trim=T)
pval_5_cat2<- ifelse(pval_5_cat2=='0.000', '<0.001', pval_5_cat2)
OR_IC_5_cat2<- paste(OR_5_cat2, ' (', ICOR_inf_5_cat2, ' - ',
                    ICOR_sup_5_cat2, ')', sep='')
OR_5_cat3<- format(round(OR_5_cat3,1), nsmall=1, trim=T)
ICOR_inf_5_cat3<- format(round(ICOR_inf_5_cat3,1), nsmall=1, trim=T)
ICOR_sup_5_cat3<- format(round(ICOR_sup_5_cat3,1), nsmall=1, trim=T)
pval_5_cat3<- format(round(pval_5_cat3,3), nsmall=3, trim=T)
pval_5_cat3<- ifelse(pval_5_cat3=='0.000', '<0.001', pval_5_cat3)
OR_IC_5_cat3<- paste(OR_5_cat3, ' (', ICOR_inf_5_cat3, ' - ',
                    ICOR_sup_5_cat3, ')', sep='')
OR_5_cat4<- format(round(OR_5_cat4,1), nsmall=1, trim=T)
ICOR_inf_5_cat4<- format(round(ICOR_inf_5_cat4,1), nsmall=1, trim=T)
ICOR_sup_5_cat4<- format(round(ICOR_sup_5_cat4,1), nsmall=1, trim=T)
pval_5_cat4<- format(round(pval_5_cat4,3), nsmall=3, trim=T)
pval_5_cat4<- ifelse(pval_5_cat4=='0.000', '<0.001', pval_5_cat4)
OR_IC_5_cat4<- paste(OR_5_cat4, ' (', ICOR_inf_5_cat4, ' - ',
                    ICOR_sup_5_cat4, ')', sep='')

tab5_categoric<- cbind(tab5_nas1, cat1, tab5_nas2, cat2, tab5_nas3, cat3, tab5_nas4, cat4,
                       OR_IC_5_cat2, pval_5_cat2, OR_IC_5_cat3, pval_5_cat3, OR_IC_5_cat4, pval_5_cat4)
dimnames(tab5_categoric)<- list(c('gender: female', 'income country: high',
                                  'genotype: severe', 'pancreatic enzymes',
                                  'bmi underweight',
                                  'burkholderia', 'staph' ,'stenotrophomonas',
                                  'achromobacter', 'nontub. myco',
                                  'NaCl', 'Dornase alpha', 'Inhaled antibiotics',
                                  'macrolides', 'inhaled steroids',
                                  'Oral steroids', 'CFTR modulators',
                                  'ABPA', 'CFRD', 'pneumothorax', 'haemoptysis',
                                  'lung transplant', 'death'),
                                c('NA MRSA_PSA - -', 'MRSA_PSA - -',
                                  'NA MRSA_PSA - +', 'MRSA_PSA - +',
                                  'NA MRSA_PSA + -', 'MRSA_PSA + -',
                                  'NA MRSA_PSA + +', 'MRSA_PSA + +',
                                  'OR 2 vs 1','p-value 2 vs 1',
                                  'OR 3 vs 1','p-value 3 vs 1',
                                  'OR 4 vs 1','p-value 4 vs 1'))

write.table(tab5_categoric, file= 'tab5_cat.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)

# numerical variables

mat5_numeric<-cbind(df$age_start,df$age_dia0, df$bmiECFSPR, df$bmi_z,
                    df$FEV1_pp, df$FVC_pp,
                    df$ivDaysTotal, df$ivDaysHosp, df$HospDaysTotal)

qq1<-c()
qq2<-c()
qq3<-c()
qq4<-c()
tab5_numeric_nas1<- c()
tab5_numeric_nas2<- c()
tab5_numeric_nas3<- c()
tab5_numeric_nas4<- c()
rlin5_diff<- c()

for (n in (1:dim(mat5_numeric)[2]))
{
  t5_num_nas1<- length((mat5_numeric[,n])[is.na(mat5_numeric[,n]) & df$MRSA_PSA==1 & !is.na(df$MRSA_PSA)])
  tab5_numeric_nas1<- c(tab5_numeric_nas1, t5_num_nas1) 
  t5_num_nas2<- length((mat5_numeric[,n])[is.na(mat5_numeric[,n]) & df$MRSA_PSA==2 & !is.na(df$MRSA_PSA)])
  tab5_numeric_nas2<- c(tab5_numeric_nas2, t5_num_nas2) 
  t5_num_nas3<- length((mat5_numeric[,n])[is.na(mat5_numeric[,n]) & df$MRSA_PSA==3 & !is.na(df$MRSA_PSA)])
  tab5_numeric_nas3<- c(tab5_numeric_nas3, t5_num_nas3) 
  t5_num_nas4<- length((mat5_numeric[,n])[is.na(mat5_numeric[,n]) & df$MRSA_PSA==4 & !is.na(df$MRSA_PSA)])
  tab5_numeric_nas4<- c(tab5_numeric_nas4, t5_num_nas4) 
  q1<- as.numeric(tapply(mat5_numeric[,n], df$MRSA_PSA, quantile, na.rm=TRUE,
                         probs= c(.25,.5,.75))[[1]])
  qq1<- rbind(qq1,q1)
  q2<- as.numeric(tapply(mat5_numeric[,n], df$MRSA_PSA, quantile, na.rm=TRUE,
                         probs= c(.25,.5,.75))[[2]])
  qq2<- rbind(qq2,q2)
  q3<- as.numeric(tapply(mat5_numeric[,n], df$MRSA_PSA, quantile, na.rm=TRUE,
                         probs= c(.25,.5,.75))[[3]])
  qq3<- rbind(qq3,q3)
  q4<- as.numeric(tapply(mat5_numeric[,n], df$MRSA_PSA, quantile, na.rm=TRUE,
                         probs= c(.25,.5,.75))[[4]])
  qq4<- rbind(qq4,q4)
  
  rlin5<- lm(mat5_numeric[,n]~factor(df$MRSA_PSA))
  rlin5_CI<-confint(rlin5)
  
  rlin5_coef2<-format(round(coef(rlin5)[2],1),nsmall=1, trim=T)
  rlin5_CI2<- format(round(rlin5_CI[2,],1),nsmall=1, trim=T)
  rlin5_def2<- paste(rlin5_coef2, ' (', rlin5_CI2[1], ' - ',
                     rlin5_CI2[2], ')', sep='')
  rlin5_coef3<-format(round(coef(rlin5)[3],1),nsmall=1, trim=T)
  rlin5_CI3<- format(round(rlin5_CI[3,],1),nsmall=1, trim=T)
  rlin5_def3<- paste(rlin5_coef3, ' (', rlin5_CI3[1], ' - ',
                     rlin5_CI3[2], ')', sep='')
  rlin5_coef4<-format(round(coef(rlin5)[4],1),nsmall=1, trim=T)
  rlin5_CI4<- format(round(rlin5_CI[4,],1),nsmall=1, trim=T)
  rlin5_def4<- paste(rlin5_coef4, ' (', rlin5_CI4[1], ' - ',
                     rlin5_CI4[2], ')', sep='')
  
  calc_pval<- summary(rlin5)$coef[2:4,'Pr(>|t|)']
  pval_5_num<- format(round(calc_pval, 3), nsmall=3, trim=T)
  pval_5_num<- ifelse(pval_5_num=='0.000','<0.001',pval_5_num)
  
  rlin5_def<-cbind(rlin5_def2, pval_5_num[1], rlin5_def3, pval_5_num[2], rlin5_def4, pval_5_num[3])
  rlin5_diff<- rbind(rlin5_diff,rlin5_def)
  
}
qq1<- format(round(qq1,1),nsmall=1, trim=T)
qq2<- format(round(qq2,1),nsmall=1, trim=T)
qq3<- format(round(qq3,1),nsmall=1, trim=T)
qq4<- format(round(qq4,1),nsmall=1, trim=T)

t5_ac1<-paste(qq1[,2], " (" ,qq1[,1], " - ", qq1[,3], ")", sep="")
t5_ac2<-paste(qq2[,2], ' (', qq2[,1], ' - ', qq2[,3], ')', sep='')
t5_ac3<-paste(qq3[,2], " (" ,qq3[,1], " - ", qq3[,3], ")", sep="")
t5_ac4<-paste(qq4[,2], ' (', qq4[,1], ' - ', qq4[,3], ')', sep='')


tab5_numeric<- cbind(tab5_numeric_nas1, t5_ac1, tab5_numeric_nas2, t5_ac2, 
                     tab5_numeric_nas3, t5_ac3, tab5_numeric_nas4, t5_ac4, rlin5_diff)

dimnames(tab5_numeric)<- list(c('current age', 'age at diagnosis',
                                'BMI','BMI z-score', 'FEV1 % of predicted',
                                'FVC%', 'IvDaysTOT', 'IvDays hosp',
                                'hosp days'),
                              c('NA MRSA_PSA - -', 'MRSA_PSA - - q2(q1-q3)',
                                'NA MRSA_PSA - +', 'MRSA_PSA - + q2(q1-q3)',
                                'NA MRSA_PSA + -', 'MRSA_PSA + - q2(q1-q3)',
                                'NA MRSA_PSA + +', 'MRSA_PSA + + q2(q1-q3)',
                                'diff 2 vs 1','p-value 2 vs 1',
                                'diff 3 vs 1','p-value 3 vs 1',
                                'diff 4 vs 1','p-value 4 vs 1'))

write.table(tab5_numeric, file= 'tab5_num_gen.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)

#### table5 fev, fevchange, fvc, fvcchange per age group #### 

qq1<-c()
qq2<-c()
qq3<-c()
qq4<-c()
tab5_numeric_nas1<- c()
tab5_numeric_nas2<- c()
tab5_numeric_nas3<- c()
tab5_numeric_nas4<- c()


tab_fev_plot<- c()
tab5_fev<- c()
tab5_fev_change<- c()
tab5_fvc<- c()
tab5_fvc_change<- c()

rlin5_diff2<- c()

for (j in (1:6))
{
  if (j==1)
  {df_cons<- df_age1}
  if(j==2)
  {df_cons<- df_age2}
  if (j==3)
  {df_cons<- df_age3}
  if (j==4)
  {df_cons<- df_age4}
  if (j==5)
  {df_cons<- df_age5}
  if (j==6)
  {df_cons<- df_age6}
  
  mat5_numeric<- cbind(df_cons$FEV1_pp,
                       df_cons$fev1_pp_loss,
                       df_cons$FVC_pp,
                       df_cons$fvc_pp_loss)
  
  for (n in (1:dim(mat5_numeric)[2]))
  {
    t5_num_nas1<- length((mat5_numeric[,n])[is.na(mat5_numeric[,n]) & df_cons$MRSA_PSA==1 & !is.na(df_cons$MRSA_PSA)])
    tab5_numeric_nas1<- c(tab5_numeric_nas1, t5_num_nas1) 
    t5_num_nas2<- length((mat5_numeric[,n])[is.na(mat5_numeric[,n]) & df_cons$MRSA_PSA==2 & !is.na(df_cons$MRSA_PSA)])
    tab5_numeric_nas2<- c(tab5_numeric_nas2, t5_num_nas2) 
    t5_num_nas3<- length((mat5_numeric[,n])[is.na(mat5_numeric[,n]) & df_cons$MRSA_PSA==3 & !is.na(df_cons$MRSA_PSA)])
    tab5_numeric_nas3<- c(tab5_numeric_nas3, t5_num_nas3) 
    t5_num_nas4<- length((mat5_numeric[,n])[is.na(mat5_numeric[,n]) & df_cons$MRSA_PSA==4 & !is.na(df_cons$MRSA_PSA)])
    tab5_numeric_nas4<- c(tab5_numeric_nas4, t5_num_nas4) 
    q1<- as.numeric(tapply(mat5_numeric[,n], df_cons$MRSA_PSA, quantile, na.rm=TRUE,
                           probs= c(.25,.5,.75))[[1]])
    qq1<- rbind(qq1,q1)
    q2<- as.numeric(tapply(mat5_numeric[,n], df_cons$MRSA_PSA, quantile, na.rm=TRUE,
                           probs= c(.25,.5,.75))[[2]])
    qq2<- rbind(qq2,q2)
    q3<- as.numeric(tapply(mat5_numeric[,n], df_cons$MRSA_PSA, quantile, na.rm=TRUE,
                           probs= c(.25,.5,.75))[[3]])
    qq3<- rbind(qq3,q3)
    q4<- as.numeric(tapply(mat5_numeric[,n], df_cons$MRSA_PSA, quantile, na.rm=TRUE,
                           probs= c(.25,.5,.75))[[4]])
    qq4<- rbind(qq4,q4)
    
    rlin5_2<- lm(mat5_numeric[,n]~factor(df_cons$MRSA_PSA))
    rlin5_CI<-confint(rlin5_2)
    
    rlin5_coef2<-format(round(coef(rlin5_2)[2],1),nsmall=1, trim=T)
    rlin5_CI2<- format(round(rlin5_CI[2,],1),nsmall=1, trim=T)
    rlin5_def2<- paste(rlin5_coef2, ' (', rlin5_CI2[1], ' - ',
                       rlin5_CI2[2], ')', sep='')
    rlin5_coef3<-format(round(coef(rlin5_2)[3],1),nsmall=1, trim=T)
    rlin5_CI3<- format(round(rlin5_CI[3,],1),nsmall=1, trim=T)
    rlin5_def3<- paste(rlin5_coef3, ' (', rlin5_CI3[1], ' - ',
                       rlin5_CI3[2], ')', sep='')
    rlin5_coef4<-format(round(coef(rlin5_2)[4],1),nsmall=1, trim=T)
    rlin5_CI4<- format(round(rlin5_CI[4,],1),nsmall=1, trim=T)
    rlin5_def4<- paste(rlin5_coef4, ' (', rlin5_CI4[1], ' - ',
                       rlin5_CI4[2], ')', sep='')
    
    calc_pval<- summary(rlin5_2)$coef[2:4,'Pr(>|t|)']
    pval_5_num<- format(round(calc_pval, 3), nsmall=3, trim=T)
    pval_5_num<- ifelse(pval_5_num=='0.000','<0.001',pval_5_num)
    
    rlin5_def<-cbind(rlin5_def2, pval_5_num[1], rlin5_def3, pval_5_num[2], rlin5_def4, pval_5_num[3])
    rlin5_diff2<- rbind(rlin5_diff2,rlin5_def)
    
    
  }
  qq1<- format(round(qq1,1),nsmall=1, trim=T)
  qq2<- format(round(qq2,1),nsmall=1, trim=T)
  qq3<- format(round(qq3,1),nsmall=1, trim=T)
  qq4<- format(round(qq4,1),nsmall=1, trim=T)
  
  t5_ac1<-paste(qq1[,2], ' (', qq1[,1], ' - ', qq1[,3], ')', sep='')
  t5_ac2<-paste(qq2[,2], " (" ,qq2[,1], " - ", qq2[,3], ")", sep="")
  t5_ac3<-paste(qq3[,2], " (" ,qq3[,1], " - ", qq3[,3], ")", sep="")
  t5_ac4<-paste(qq4[,2], " (" ,qq4[,1], " - ", qq4[,3], ")", sep="")
  
  
  tab5_numeric<- cbind(tab5_numeric_nas1, t5_ac1, tab5_numeric_nas2, t5_ac2,
                       tab5_numeric_nas3, t5_ac3, tab5_numeric_nas4, t5_ac4, rlin5_diff2)
  
  dimnames(tab5_numeric)<- list(c('FEV', 'FEV change', 'FVC', 'FVC change'),
                                c('NA MRSA_PSA - -', 'MRSA_PSA - - q2(q1-q3)',
                                  'NA MRSA_PSA - +', 'MRSA_PSA - + q2(q1-q3)',
                                  'NA MRSA_PSA + -', 'MRSA_PSA + - q2(q1-q3)',
                                  'NA MRSA_PSA + +', 'MRSA_PSA + + q2(q1-q3)',
                                  'diff 2 vs 1','p-value 2 vs 1',
                                  'diff 3 vs 1','p-value 3 vs 1',
                                  'diff 4 vs 1','p-value 4 vs 1'))
  
  tab5_fev<- rbind(tab5_fev, tab5_numeric[1,])
  tab5_fev_change<-rbind(tab5_fev_change, tab5_numeric[2,])
  tab5_fvc<- rbind(tab5_fvc, tab5_numeric[3,])
  tab5_fvc_change<- rbind(tab5_fvc_change, tab5_numeric[4,])

  qq1<-c()
  qq2<-c()
  qq3<-c()
  qq4<-c()
  tab5_numeric_nas1<- c()
  tab5_numeric_nas2<- c()
  tab5_numeric_nas3<- c()
  tab5_numeric_nas4<- c()
  rlin5_diff2<-c()
  
}

tab_fev_fvc<-rbind(tab5_fev,tab5_fev_change,tab5_fvc,tab5_fvc_change)
write.table(tab_fev_fvc, file= 'tab5_FEV_FVC.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)


#############################################################################################
#####                     LONGITUTINAL ANALYSIS ON FEV1 DECLINE                         #####
#############################################################################################

library(rms)

hist(df$age_fev)
# ATTENZIONE: secondo me ? meglio limitare le analisi a 50
df50<-subset(df, age_start<=50)

# spline with 3 knots
mod_fev_rcs_int<-lm(FEV1_pp ~ rcs(age_fev,3)*MRSA_PSA, data=df50)
anova(mod_fev_rcs_int)
mod_fev_rcs<-lm(FEV1_pp ~ rcs(age_fev,3)+MRSA_PSA, data=df50)
anova(mod_fev_rcs)
summary(mod_fev_rcs, corr=F)
var_cov<-vcov(mod_fev_rcs)
#spline2_pred<-rcspline.eval(seq(6,50,1), knots=spline_knots3, inclx=T)[,2]
pred1<-predict(mod_fev_rcs_int, newdata = data.frame(age_fev=seq(6,50,1), MRSA_PSA=1), 
               type = "response")
pred2<-predict(mod_fev_rcs_int, newdata = data.frame(age_fev=seq(6,50,1), MRSA_PSA=2), 
               type = "response")
pred3<-predict(mod_fev_rcs_int, newdata = data.frame(age_fev=seq(6,50,1), MRSA_PSA=3), 
               type = "response")
pred4<-predict(mod_fev_rcs_int, newdata = data.frame(age_fev=seq(6,50,1), MRSA_PSA=4), 
               type = "response")

plot(6:50, pred1, type="l", col="black", lwd=2, ylim=c(20,100), 
     xlab="Age (years)", ylab=expression(paste("Mean ", FEV[1], " percent of predicted value", sep="")), las=1, cex.axis=0.8, cex.lab=0.9)
lines(6:50, pred2, col="blue", lwd=2)
lines(6:50, pred3, col="green", lwd=2)
lines(6:50, pred4, col="red", lwd=2)
legend("bottomleft", inset=0.02, c("MRSA neg PSA neg", "MRSA neg PSA pos","MRSA pos PSA neg", "MRSA pos PSA pos"), 
       col=c("black","blue","green","red"), lwd=2, lty=1, bty="n", cex=0.8)

decline1<-pred1[2:45]-pred1[1:44]
decline2<-pred2[2:45]-pred2[1:44]
decline3<-pred3[2:45]-pred3[1:44]
decline4<-pred4[2:45]-pred4[1:44]
plot(7:50, decline1, type="l", col="black", lwd=2, las=1, ylim=c(-3,0.5),
     xlab="Age (years)", ylab=expression(paste("Mean ", FEV[1], " percent of predicted yearly decline", sep="")), cex.axis=0.8, cex.lab=0.9)
lines(7:50, decline2, type="l", col="blue", lwd=2)
lines(7:50, decline3, type="l", col="green", lwd=2)
lines(7:50, decline4, type="l", col="red", lwd=2)
legend("bottomright", inset=0.02, c("MRSA neg PSA neg", "MRSA neg PSA pos","MRSA pos PSA neg", "MRSA pos PSA pos"), 
       col=c("black","blue","green","red"), lwd=2, lty=1, bty="n", cex=0.8)






##########################################################################################################
#### linear regressions ####

rlin_s_diff<- c()
rlin_m_diff<- c()
rlin_pvalue<- c()
for (n in c(1:3))
{
  if (n==1)
  { x = df$FEV1_pp }
  if (n==2)
  { x = df$ivDaysTotal }
  if (n==3)
  { x = df$HospDaysTotal }
  
  rlin_s<- lm(x~factor(df$MRSA_PSA))
  rlin_s_coef<-format(round(coef(rlin_s)[2:4],1),nsmall=1, trim=T)
  rlin_s_CI<- format(round(confint(rlin_s)[2:4,],1),nsmall=1, trim=T)
  rlin_s_def<- paste(rlin_s_coef, ' (', rlin_s_CI[,1], ' - ',
                     rlin_s_CI[,2], ')', sep='')
  rlin_s_diff<- rbind(rlin_s_diff,rlin_s_def)
  
  
  rlin_m<- lm(x~factor(df$MRSA_PSA)+df$age_start+df$gender+df$age_dia0+
                df$class_geno+df$bmi_z+ cfrd+ df$pseudo+ df$burkho+df$cftrmod)
  rlin_m_coef<- format(round(coef(rlin_m)[2:4],1),nsmall=1, trim=T)
  rlin_m_CI<- format(round(confint(rlin_m)[2:4,],1),nsmall=1, trim=T)
  rlin_m_def<- paste(rlin_m_coef, ' (', rlin_m_CI[,1], ' - ',
                     rlin_m_CI[,2], ')', sep='')
  rlin_m_diff<- rbind(rlin_m_diff, rlin_m_def)
  
  rlin_pv<- format(round(summary(rlin_m)$coef[2:4,'Pr(>|t|)'],3),
                   nsmall = 3,trim = T)
  rlin_pv<- ifelse(rlin_pv== '0.000', '<0.001', rlin_pv)
  rlin_pvalue<- rbind(rlin_pvalue, rlin_pv)
}

table_rlin<-cbind(rlin_s_diff, rlin_m_diff, rlin_pvalue)
dimnames(table_rlin)[[1]]<- c('fev1pp', 'days on ivAnti', 'days hosp')

write.table(table_rlin, file= 'tab6_linear_regression.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)

#### logistic regressions ####

rlog_s_diff<- c()
rlog_m_diff<- c()
rlog_pvalue<- c()

for (n in c(1:3))
{
  if (n==1)
  {p= df$lung_death}
  if (n==2)
  {p= df$composite}
  if (n==3)
  {p= df$pneumo_hemo}
  
  rlog_s<- glm(p~factor(df$MRSA_PSA), binomial)
  rlog_s_OR<- format(round(exp(coef(rlog_s))[2:4],1), nsmall= 2, trim= T)
  rlog_s_CI<- format(round(exp(confint(rlog_s))[2:4,],1), nsmall=1, trim=T)
  rlog_s_def<- paste(rlog_s_OR, ' (', rlog_s_CI[,1], ' - ', rlog_s_CI[,2], ')', sep='')
  rlog_s_diff<- rbind(rlog_s_diff,rlog_s_def)
  
  rlog_m<- glm(p~factor(df$MRSA_PSA)+df$age_start+df$gender+df$age_dia0+df$class_geno
               +df$bmi_z+ cfrd+ df$pseudo+ df$burkho+df$cftrmod, binomial)
  rlog_m_OR<- format(round(exp(coef(rlog_m))[2:4],1), nsmall= 2, trim= T)
  rlog_m_CI<- format(round(exp(confint(rlog_m))[2:4,],1), nsmall=1, trim=T)
  rlog_m_def<- paste(rlog_m_OR, ' (', rlog_m_CI[,1], ' - ', rlog_m_CI[,2], ')', sep='')
  rlog_m_diff<- rbind(rlog_m_diff,rlog_m_def)
  
  rlog_pv<- format(round(summary(rlog_m)$coef[2:4,'Pr(>|z|)'],3),
                   nsmall=3,trim=T)
  rlog_pv<- ifelse(rlog_pv== '0.000', '<0.001', rlog_pv)
  rlog_pvalue<- rbind(rlog_pvalue,rlog_pv)
}

table_rlog<- cbind(rlog_s_diff, rlog_m_diff, rlog_pvalue)
dimnames(table_rlog)[[1]]<- c('lung transplant_death','composite','pneumo_hemo')
write.table(table_rlog, file= 'tab6_logistic_regression.csv',
            append=FALSE, quote= FALSE, sep=',', eol='\n', na= 'NA', dec='.',
            row.names= TRUE, col.names= TRUE)

