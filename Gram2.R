# Dataset Preparation ####

# setto la wd direttamente dove butterò tutti i grafici
setwd('A:\\1001022_Gramegna_DiseaseBurdenNonCFTRm\\grafici')


df_all<- read.csv(file = "A:\\1001022_Gramegna_DiseaseBurdenNonCFTRm\\db_1001022.csv",
                  header = T, skipNul = T)

df2021a<- subset(df_all, df_all$year==2021)
#table(df2021$status, useNA = "ifany")

# pwCF in 2021
df2021<- df_all[df_all$year==2021 & (df_all$status==0 | df_all$status==1),]
 
 


#table(df2021$eligible, useNA = "ifany")


# pwCF seen in 2021 with genotype eligible for CFTR modulator
df_eligible<- df_all[df_all$year==2021 & df_all$eligible== 1 #& df_all$class_geno!="4_unknown"
                  & (df_all$status==0 | df_all$status==1),]



# pwCF seen in 2021 with genotype not eligible for CFTR modulator
df_prova<- df_all[df_all$year==2021 & df_all$eligible== 0 & df_all$class_geno!="4_unknown"
                 & (df_all$status==0 | df_all$status==1),]
#addmargins(table(df_prova$cftrmod_tot, df_prova$class_geno))

# in df abbiamo pazienti del 2021, non elegibili per modulators, non appartenenti alla classe genotipica unknown,
#  e visti nel 2021 
df<- df_all[df_all$year==2021 & df_all$eligible== 0 & df_all$class_geno!="4_unknown"
            & df_all$cftrmod_tot==0 & (df_all$status==0 | df_all$status==1),
            c("id","country","birth_yy","birth_mm","Gender","age_dia0",
                "mut1","mut2","genotype","mut1_class","mut2_class","class_geno",
              "mut1_mf_rf", "mut2_mf_rf", "class_geno_mf_rf",
              "year", "age_start","status","age","age_fev","FEV1_pp","bmi_z",
                "pseudo", "Diabetes","liver",
                "enzymes",
                "NaCl","rhDNase","Mannitol",
                "ivDaysTotal", "Antibiotic",
                "lung_t", "cftrmod_tot",
                "eligible")]

table(df$class_geno)

rm(df_all)

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
             ifelse(df$country=="GB","United Kingdom",
             ifelse(df$country=="FI", "Finland",
             ifelse(df$country=="IS","Island",NA))))))))))))))))))))))))))))))))))))))))


# dataset

# creo una copia di df, che chiamo dset. Ora modifico e dicotomizzo
# le variabili di mio interesse partendo da dset.
# I valori NA rimangono tali, non vengono modificati dagli ifelse

dset<- df 

df$age_class<- ifelse(dset$age<6, "0-5",
               ifelse(dset$age>=6 & dset$age < 12, "06-11",
               ifelse(dset$age>=12 & dset$age<18, "12-17",
               ifelse(dset$age>=18 & dset$age< 30, "18-29", "30+"))))

# Ricodifica class_geno: la prima categoria è minimal

df$class_geno<- ifelse(dset$class_geno== "1_respons", "2_respons",
                ifelse(dset$class_geno== "2_minimal", "1_minimal", "3_residua"))

# pseudomonas: 1 if chronic, 0 if absent or intermittent
df$pseudo<- ifelse(dset$pseudo==2,1,0)

# diabetes: 1 if present, 0 if absent
df$Diabetes<- ifelse(dset$Diabetes== 0, 0, 1)

# liver: 1 if present, 0 if absent
df$liver<- ifelse(dset$liver==0, 0, 1)

# ivdays on antibiotic: 1 if at least 1 day, 0 otherwise
df$ivDaysTotal<- ifelse(dset$ivDaysTotal>=1,1,0)

# status: 0 if alive, 1 if dead
df$status<- ifelse(dset$status==1,0,1)

# income index
df$gni<- ifelse(df$country %in% c("UA", "AM", "AL", "GE", "MD", "MK", "RS", "BY", "BG", "TR", "GR", "RU", "SK"), "1_low",
         ifelse(df$country %in% c("LV", "HR", "RO", "PT", "HU", "PL", "CY", "ES", "LT", "CZ", "SI", "IL", "IT"), "2_medium",
         ifelse(df$country %in% c("GB", "FR", "IS", "AT", "BE", "DE", "SE", "NL", "DK", "CH", "IE", "NO", "LU"), "3_high", "4_controlla!")))

# FEV1_pp: se lung_t==1 o se age<6 metto NA

df$FEV1_pp<- ifelse(df$age >= 6 & df$lung_t==0, dset$FEV1_pp, NA)

# Approfondimenti su responsive, eligible, elegibles che usano modulators ####

# Approfondimento sulla composizione dei responsive #
table(subset(df, df$class_geno== "2_respons")$class_geno_mf_rf, useNA = "ifany")
# 610 MF, 1928 RF, 402 unknown

# Approfondimento sugli eligible

addmargins(table(df_eligible$df, useNA = "ifany"))
# 20841 F508del heterozygote     20563 F508del homozygote    876 Not F508del 


# Analisi sui 45 non elegibili ma che usano modulators

df_prova$class_geno<- ifelse(df_prova$class_geno== "1_respons", "2_respons",
                      ifelse(df_prova$class_geno== "2_minimal", "1_minimal", "3_residua"))


df45<- subset(df_prova, df_prova$cftrmod_tot==1 & df_prova$class_geno != "2_respons")
df45[,c("class_geno","genotype")]

# tabelle con genotipo per i 36 minimal e i 9 residual che non sono elegibili ma assumono modulators
geno_minimal<- sort(table(subset(df45, df45$class_geno=="1_minimal")$genotype), decreasing = T)
geno_residua<- sort(table(subset(df45, df45$class_geno=="3_residua")$genotype), decreasing = T)

write.csv(geno_minimal, file = 'geno_minimal.csv',quote= FALSE, eol='\n', na= 'NA', 
          row.names= TRUE)
write.csv(geno_residua, file = 'geno_residua.csv',quote= FALSE, eol='\n', na= 'NA', 
          row.names= TRUE)




# Tabella iniziale #####

require(tableone)

myvars<- c("age","age_dia0", 
           "Gender", "lung_t", "status", "gni")

catvars<- c("country", "Gender", "lung_t", "status", "gni")

tab0<- CreateTableOne(data= df,
                      vars= myvars,
                      factorVars = catvars,
                      strata = "class_geno")

print_tab0<- print(tab0, showAllLevels = T, nonnormal = T, test = F)

write.csv(print_tab0, file = 'tab0.csv',quote= FALSE, eol='\n', na= 'NA', 
          row.names= TRUE)



# Tabelle e grafici per variabile ####

age_c<-  c("0-5", "06-11", "12-17","18-29", "30+")

## VARIABILI NUMERICHE

var_num<- c("FEV1_pp", "bmi_z")

array_num<- array(dim=c(5,3,length(var_num)))
sheet<- c()

for (i in 1:length(var_num)) {
  sheet<- c()
  for (j in 1:length(age_c)){
    dataset_y_c<- subset(df, age_class==age_c[j])
    res<- tapply(dataset_y_c[,var_num[i]], dataset_y_c$class_geno, median, na.rm=T)
    sheet<- rbind(sheet, res)
  }
  array_num[,,i]= sheet
} 

rownames(array_num)<- age_c
colnames(array_num)<- sort(unique(df$class_geno))

array_num<- round(array_num,2)

array_num



# grafici

# attenzione all'ordine, dipende da come lo abbiamo inserito in var_num
age_group_mean <- c(2.5,8.5,14.5,23.5,40)

for (i in var_num){
  if (i == "FEV1_pp") {
    tab<- array_num[,,1]
    main= "FEV1%"
    ylim= c(50,110)
    at2= seq(50,110,10)
    filename= "fev1.jpeg"
    lab_plot= c("6-11", "12-17","18-29", "30+")
    at_lab= c(8.5,14.5,23.5,40)
  }
  if (i == "bmi_z") {
    tab<- array_num[,,2]
    main= "BMI_z"
    ylim= c(-0.8,0.8)
    at2= seq(-1,1,0.2)
    filename= "bmi.jpeg"
    lab_plot= c("2-5", "6-11", "12-17","18-29", "30+")
    at_lab= c(2.5,8.5,14.5,23.5,40)
  }
  
  jpeg(filename = filename, width = 700, height = 700)
  
  plot(age_group_mean,tab[,1],
       ylim= ylim, type= "l", lty=1, las= 1, axes = F,
       xlab = "age class", ylab= "value", main= main, lwd=2)
  lines(age_group_mean ,tab[,2],type= "l", lty=2, lwd= 2 )
  lines(age_group_mean ,tab[,3],type= "l", lty=3, lwd=2)
  axis(1, at= at_lab, labels = lab_plot, cex.axis=1)
  axis(2, at= at2, las=1, cex.axis= 1)
  legend("topleft", legend = c("minimal","responsive", "residual"),
         lty= c(1,2,3), lwd=2, cex= 1, bty="n")
  
  box()
  
  dev.off()
}


# Tabella con anche q1,q3

sheet<- c()
array_complete<- array(dim=c(5,3,length(var_num)))

for (i in 1:length(var_num)) {
  sheet<- c()
  for (j in 1:length(age_c)){
    dataset_y_c<- subset(df, age_class==age_c[j])
    q1<- tapply(dataset_y_c[,var_num[i]], dataset_y_c$class_geno,
                 quantile, probs= c(0.25), na.rm=T)
    q2<-tapply(dataset_y_c[,var_num[i]], dataset_y_c$class_geno,
                quantile, probs= c(0.5), na.rm=T)
    q3<- tapply(dataset_y_c[,var_num[i]], dataset_y_c$class_geno,
                quantile, probs= c(0.75), na.rm=T)
    q1<- format(round(q1,2),trim = T)
    q2<- format(round(q2,2),trim=T)
    q3<- format(round(q3,2),trim=T)
    res<- paste(q2, " (", q1, " - ", q3, ")", sep= "")
    sheet<- rbind(sheet, res)
  }
  array_complete[,,i]= sheet
} 

rownames(array_complete)<- age_c
colnames(array_complete)<- sort(unique(df$class_geno))

array_complete

write.csv(array_complete, file = 'array_num_complete.csv',quote= FALSE, eol='\n', na= 'NA', 
          row.names= TRUE)

array_complete

## VARIABILI DICOTOMICHE

var_cat<- c("pseudo", "Diabetes", "liver", "enzymes","NaCl","rhDNase" ,"Antibiotic", "ivDaysTotal")
sheet<- c()
array_cat<- array(dim=c(5,3,length(var_cat)))

for (i in 1:length(var_cat)) {
  sheet<- c()
  for (j in 1:length(age_c)){
    dataset_y_c<- subset(df, age_class==age_c[j])
    res<- tapply(dataset_y_c[,var_cat[i]], dataset_y_c$class_geno, mean, na.rm=T)
    sheet<- rbind(sheet, res)
  }
  array_cat[,,i]= sheet
} 

array_cat<- round(array_cat*100,2)

rownames(array_cat)<- age_c
colnames(array_cat)<- sort(unique(df$class_geno))


array_cat


# Con anche numerosità

sheet<- c()
array_cat2<- array(dim=c(5,3,length(var_cat)))

for (i in 1:length(var_cat)) {
  sheet<- c()
  for (j in 1:length(age_c)){
    dataset_y_c<- subset(df, age_class==age_c[j])
    num<- tapply(dataset_y_c[,var_cat[i]], dataset_y_c$class_geno, sum, na.rm=T)
    perc<- tapply(dataset_y_c[,var_cat[i]], dataset_y_c$class_geno, mean, na.rm=T)
    perc<- round(perc*100,2)
    #tot<-  tapply(dataset_y_c[,var_cat[i]], dataset_y_c$class_geno, length)
    res<- paste(num, " (", perc, "%)", sep="")
    sheet<- rbind(sheet, res)
  }
  array_cat2[,,i]= sheet
} 

# ATTENZIONE: LENGTH TIENE DENTRO ANCHE I NA QUANDO CALCOLA, QUINDI LA FRAZIONE NELLA
# TABELLA CORRISPONDE ALLA PERCENTUALE SOLO SE NON CI SONO NA

array_cat2

write.csv(array_cat2, file = 'array_cat_complete.csv',quote= FALSE, eol='\n', na= 'NA', 
          row.names= TRUE)

# grafici

# attenzione all'ordine, dipende da come lo abbiamo inserito in var_cat
# Grafico a barre

for ( i in c(1:length(var_cat))){
  if (var_cat[i] == "pseudo") {
    main= "pseudomonas"
    ylim= c(0,20)
    at2= seq(4,20,4)
    filename= "pseudomonas_bar.jpeg"
  }
  if (var_cat[i] == "Diabetes") {
    main= "Diabetes"
    ylim= c(0,70)
    at2= seq(0,70,10)
    filename= "Diabetes_bar.jpeg"
  }
  if (var_cat[i] == "liver") {
    main= "Liver disease"
    ylim= c(0,40)
    at2= seq(0,40,10)
    filename= "liver_bar.jpeg"
  }
  if (var_cat[i] == "enzymes") {
    main= "Pancreatic enzymes"
    ylim= c(0,109)
    at2= seq(0,100,10)
    filename= "enzymes_bar.jpeg"
  }
  if (var_cat[i] == "NaCl") {
    main= "NaCl"
    ylim= c(0,70)
    at2= seq(0,100,10)
    filename= "NaCl_bar.jpeg"
  }
  if (var_cat[i] == "rhDNase") {
    main= "rhDNase"
    ylim= c(0,90)
    at2= seq(0,100,10)
    filename= "rhDNase_bar.jpeg"
  }
  if (var_cat[i] == "Antibiotic") {
    main= "Inhaled Antibiotic"
    ylim= c(0,70)
    at2= seq(0,70,10)
    filename= "Antibiotic_bar.jpeg"
  }
  if (var_cat[i] == "ivDaysTotal") {
    main= "ivDaysTotal"
    ylim= c(0,50)
    at2= seq(0,50,10)
    filename= "ivDaysTotal_bar.jpeg"
  }
  
  jpeg(filename = filename, width = 700, height = 700)
  
  tab<- array_cat[,,i]
  
  barplot(t(tab), ylim= ylim,  beside = T,axes = F,
       xlab = "age class", ylab= "value", main= main,
       names.arg = c("0-5", "6-11", "12-17","18-29", "30+"),
       col= c("gray10", "gray40", "gray85"))

  axis(2, at= at2, las=1, cex.axis= 1)
  legend("topleft", legend = c("minimal","responsive", "residual"),
         col= c("gray10", "gray40", "gray85"),lty= 1, lwd= 8, cex= 1, bty="n")
  
  box()
  
  dev.off()
}


# prova

#df_prova<- df[df$age_class== "12-17",]

#median(df_prova[df_prova$class_geno== "1_respons",]$bmi_z, na.rm = T)
#median(df_prova[df_prova$class_geno== "2_minimal",]$bmi_z, na.rm = T)
#median(df_prova[df_prova$class_geno== "3_residua",]$bmi_z, na.rm = T)

#prop.table(table(df_prova[df_prova$class_geno== "1_respons",]$pseudo))
#prop.table(table(df_prova[df_prova$class_geno== "2_minimal",]$pseudo))
#prop.table(table(df_prova[df_prova$class_geno== "3_residua",]$pseudo))

# COnfronti tra gruppi ####

# Attenzione: diabetes nei bimbi 0-5 non ha nessun malato, quindi non costruisce una tabella 2x2
#              e quindi se provi a fargli fare i test va in errore e blocca tutto il loop. 
#             Quindi uso l'if per trattare diversamente quel caso: do valore -999 in modo da
#             non lasciare lo spazio nella tabellina con tutti i pvalure, e faccio procedere

library(fifer)

options(scipen = 5)

fisher_tot<- c()
chisq_tot<- c()

for (i in var_cat){
  for (w in age_c) {
    data<- subset(df, df$age_class== w)
    print(i)
    print(w)
    print(table(data[,i], data$class_geno))
    if (i== "Diabetes" & w== "0-5") {
      fisher_one<- -999
      chisq_one<- c(-999, -999, -999)
      fisher_tot<- c(fisher_tot, fisher_one)
      chisq_tot<- rbind(chisq_tot, chisq_one)
    }
    
    else{
    fisher_one<- fisher.test(table(data[,i],data$class_geno))$p.value
    chisq_one<- chisq.post.hoc(table(data[,i],data$class_geno), #simulate.p.value=T,
                   test=c("fisher.test"), popsInRows=F, control="fdr", digits=3)
    print(fisher_one)
    print(chisq_one)
    
    fisher_tot<- c(fisher_tot, fisher_one)
    chisq_tot<- rbind(chisq_tot, chisq_one$adj.p)
    }
  }
}

fisher_tot
chisq_tot

chisq_bind<- cbind(fisher_tot,chisq_tot)
chisq_def<- format(round(chisq_bind, 3),trim = T)
colnames(chisq_def)<- c("Total", "min vs resp", "min vs resid", "resp vs resid")
rownames(chisq_def)<- rep(age_c,8)
chisq_def<- ifelse(chisq_def=="0.000", "<0.001", chisq_def)
chisq_def


write.csv(chisq_def, file = 'chisq_pvalues.csv',quote= FALSE, eol='\n', na= 'NA', 
          row.names= TRUE)

library(FSA)
krusk_tot<- c()
dunn_tot<- c()

for (i in var_num){
  for (w in age_c) {
    data<- subset(df, df$age_class== w)
    print(i)
    print(w)
    print(nrow(data))
    
    if (i == "FEV1_pp" & w == "0-5") {
      
      krusk_one<- -999
      dunn_one<- c(-999, -999, -999)
      krusk_tot<- c(krusk_tot, krusk_one)
      dunn_tot<- rbind(dunn_tot, dunn_one)
    }
    
    else {
      
    krusk_one<- kruskal.test(data[,i] ~ data$class_geno)$p.value
    dunn_one<- dunnTest(data[,i] ~ data$class_geno, method="bh")$res$P.adj
    print(krusk_one)
    print(dunn_one)
    
    krusk_tot<- c(krusk_tot, krusk_one)
    dunn_tot<- rbind(dunn_tot, dunn_one)
  }
  }
}

krusk_tot
dunn_tot

dunn_bind<- cbind(krusk_tot, dunn_tot)
dunn_def<- format(round(dunn_bind, 3),trim = T)
colnames(dunn_def)<- c("Total", "min vs resp", "min vs resid", "resp vs resid")
rownames(dunn_def)<- rep(age_c,2)
dunn_def<- ifelse(dunn_def=="0.000", "<0.001", dunn_def)
dunn_def

write.csv(dunn_def, file = 'dunn_pvalues.csv',quote= FALSE, eol='\n', na= 'NA', 
          row.names= TRUE)

# Mappe ####

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rgeos)

map_cf<- table(df2021$country)

europe<- ne_countries(scale= 'medium',continent = 'Europe' ,returnclass = 'sf')
asia<- ne_countries(scale='medium', continent='Asia', returnclass = 'sf')
africa<- ne_countries(scale= 'medium', continent= 'Africa', returnclass = 'sf')
eurasiafrica<- rbind(europe, asia, africa)


div<- 2

# div è la quantità per cui dividere le dimensioni di default (per avere immagine più leggera)
# per div=1 ovvero in default l'immagine pesa 7120 kb.
# All'aumentare di div l'immagine si fa più leggera ma perde qualità ( si nota specialmente per le etichette dei paesi)


for (w in 1:4) {
  if (w==1) {
    map_notelig<- table(df2021[df2021$eligible==0,]$country)
    tiff(filename= 'map_tot.jpg', width= 1500/div, height= 1620/div)
    limit_map<- c(0,100)
    map_title= "Total"
  } 
  if (w==2) {
    map_notelig<- table(df2021[df2021$eligible==0 & df2021$class_geno== "1_respons",]$country)
    tiff(filename= 'map_responsive.jpg', width= 1500/div, height= 1620/div)
    limit_map<- c(0,25)
    map_title= "Responsive"
  } 
  if (w==3) {
    map_notelig<- table(df2021[df2021$eligible==0 & df2021$class_geno== "2_minimal",]$country)
    tiff(filename= 'map_minimal.jpg', width= 1500/div, height= 1620/div)
    limit_map<- c(0,50)
    map_title= "Minimal Function"
  }
  if (w==4) {
    map_notelig<- table(df2021[df2021$eligible==0 & df2021$class_geno== "3_residua",]$country)
    tiff(filename= 'map_resid.jpg', width= 1500/div, height= 1620/div)
    limit_map<- c(0,10)
    map_title= "Residual function"
  }
  
  print(map_notelig)
  l<- c()
  for (z in (rownames(map_cf))) {
    j<-ifelse((z %in% rownames(map_notelig)),map_notelig[z],0)
    l<- c(l,j)
  }
  
  map_notelig1<- as.table(l)
  rownames(map_notelig1)<- rownames(map_cf)
  
  prop_notelig<- as.data.frame(round((map_notelig1/map_cf)*100,2))
  colnames(prop_notelig)<- c("iso_a2", "Freq")
  prop_notelig$control<- rep(0, nrow(prop_notelig))
  prop_notelig$ntot<- map_cf # numerosità di ogni nazione
  
  
  print(prop_notelig)
  
  p_notelig2<- prop_notelig[,-3]
  p_notelig2<- cbind(p_notelig2, l)
  p_notelig2$FFF<- paste(p_notelig2$l, "(", p_notelig2$Freq, "%)", sep = "")
  p_notelig2$country<- ifelse(prop_notelig$iso_a2=="AL","Albania",
             ifelse(prop_notelig$iso_a2=="AM","Armenia",
             ifelse(prop_notelig$iso_a2=="AT","Austria",
             ifelse(prop_notelig$iso_a2=="BE","Belgium",
             ifelse(prop_notelig$iso_a2=="BG","Bulgaria",
             ifelse(prop_notelig$iso_a2=="BY","Belarus",
             ifelse(prop_notelig$iso_a2=="HR","Croatia",
             ifelse(prop_notelig$iso_a2=="CY","Cyprus",
             ifelse(prop_notelig$iso_a2=="CZ","Czech Republic",
             ifelse(prop_notelig$iso_a2=="DK","Denmark",
             ifelse(prop_notelig$iso_a2=="FR","France",
             ifelse(prop_notelig$iso_a2=="GE","Georgia",
             ifelse(prop_notelig$iso_a2=="DE","Germany",
             ifelse(prop_notelig$iso_a2=="GR","Greece",
             ifelse(prop_notelig$iso_a2=="HU","Hungary",
             ifelse(prop_notelig$iso_a2=="IE","Ireland",
             ifelse(prop_notelig$iso_a2=="IL","Israel",
             ifelse(prop_notelig$iso_a2=="IT","Italy",
             ifelse(prop_notelig$iso_a2=="LV","Latvia",
             ifelse(prop_notelig$iso_a2=="LT","Lithuania",
             ifelse(prop_notelig$iso_a2=="LU","Luxembourg",
             ifelse(prop_notelig$iso_a2=="MK","North Macedonia",
             ifelse(prop_notelig$iso_a2=="MD","Moldova",
             ifelse(prop_notelig$iso_a2=="NL","Netherlands",
             ifelse(prop_notelig$iso_a2=="NO","Norway",
             ifelse(prop_notelig$iso_a2=="PT","Portugal",
             ifelse(prop_notelig$iso_a2=="PL","Poland",
             ifelse(prop_notelig$iso_a2=="RO","Romania",
             ifelse(prop_notelig$iso_a2=="RU","Russian Federation",
             ifelse(prop_notelig$iso_a2=="RS","Serbia",
             ifelse(prop_notelig$iso_a2=="SK","Slovak Republic",
             ifelse(prop_notelig$iso_a2=="SI","Slovenia",
             ifelse(prop_notelig$iso_a2=="ES","Spain",
             ifelse(prop_notelig$iso_a2=="CH","Switzerland",
             ifelse(prop_notelig$iso_a2=="SE","Sweden",
             ifelse(prop_notelig$iso_a2=="TR","Turkey",
             ifelse(prop_notelig$iso_a2=="UA","Ukraine",
             ifelse(prop_notelig$iso_a2=="GB","United Kingdom",
             ifelse(prop_notelig$iso_a2=="FI", "Finland",
             ifelse(prop_notelig$iso_a2=="IS","Island",NA))))))))))))))))))))))))))))))))))))))))
  
  write.csv(p_notelig2, file = paste("Table1_", map_title, ".csv", sep = ""),
            quote= FALSE, eol='\n', na= 'NA', row.names= F)
  
  map_europe_comp<- as.data.frame(cbind(eurasiafrica$name, eurasiafrica$iso_a2))
  dimnames(map_europe_comp)[[2]]<- c("name", "iso_a2")
  
  map_europe_comp$prop<- rep(NA, nrow(eurasiafrica))
  map_europe_comp$ntot<- rep(NA, nrow(eurasiafrica))
  
  for (z in c(1:nrow(eurasiafrica))) {
    for (k in c(1: nrow(prop_notelig))) {
      if (!is.na(map_europe_comp$iso_a2[z]) & prop_notelig$iso_a2[k]== map_europe_comp$iso_a2[z])
      {
        map_europe_comp$prop[z]<- prop_notelig$Freq[k]
        map_europe_comp$ntot[z]<- prop_notelig$ntot[k]
        prop_notelig$control[k]<- 1
      }
    }
  }
  
  
  nations<- subset(eurasiafrica, 
                   (!is.na(map_europe_comp$prop) & eurasiafrica$sov_a3!= 'NOR'
                    & eurasiafrica$iso_a2!= 'GB'
                    & eurasiafrica$iso_a2!= 'DK'
                    & eurasiafrica$iso_a2!= 'GR'
                    & eurasiafrica$iso_a2!= 'FR'
                    & eurasiafrica$iso_a2!= 'RU'
                    & eurasiafrica$iso_a2!= 'IL'
                    & eurasiafrica$iso_a2!= 'HR'))
  
  eurasiafrica_points<- st_centroid(nations)
  eurasiafrica_points<- cbind(nations, st_coordinates(st_centroid(nations$geometry)))

  # theme(panel.background = element_rect(fill = 'red'),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank()) è il comando per controllare il colore
  # del mare nella mappa e rimuovere la griglia

  print(ggplot(data= eurasiafrica) +
          geom_sf(color= 'black', aes(fill= map_europe_comp$prop)) +
          theme(legend.key.size = unit(3.5/div, 'lines'),
                legend.text = element_text(size= 20/div),
                legend.title = element_text(size= 20/div),
                plot.title = element_text(size=20/div))+
          labs(fill='Prevalence')+
          xlab('') + ylab('')+
          scale_fill_gradientn(colors=c("white", "lightblue", "turquoise", "blue", "darkblue") , na.value = 'gray80', limits= limit_map)+
          coord_sf(xlim= c(-25, 50), ylim= c(28, 70), expand= FALSE, label_axes = 'SW')+
          ggtitle(map_title)+
          geom_text(data= eurasiafrica_points, aes(x=X, y=Y,label= iso_a2),size=5/div, color= 'black', fontface= 'bold')+
          annotate(geom= 'text',x=40, y=55, label= 'RU', color= 'black', size=5/div, fontface= 'bold')+
          annotate(geom= 'text',x=10, y=61, label= 'NO', color= 'black', size=5/div, fontface= 'bold')+
          annotate(geom='text', x=-1, y=52.5, label= 'GB', color= 'black', size=5/div,fontface= 'bold')+
          annotate(geom= 'text', x= 9, y= 56, label= 'DK', color= 'black', size= 5/div, fontface= 'bold')+
          annotate(geom= 'text', x= 17, y= 45.5, label= 'HR', color= 'black', size= 5/div, fontface= 'bold')+
          annotate(geom= 'text',x= 21.5, y= 39.5, label= 'GR', color= 'black', size= 5/div, fontface= 'bold')+
          annotate(geom= 'text',x= 3.5, y= 47, label= 'FR', color= 'black', size= 5/div, fontface= 'bold')+
          annotate(geom= 'text',x= 35, y= 31, label= 'IL', color= 'black', size= 5/div, fontface= 'bold')
  )
  
  
  
  dev.off()
  
}


