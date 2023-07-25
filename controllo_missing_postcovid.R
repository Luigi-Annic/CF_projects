
df_all<- read.table(file = "A:\\1001022_Gramegna_DiseaseBurdenNonCFTRm\\db_1001022.csv",
                header = T, sep= ",")

df<- df_all[, c("id","country","birth_yy","birth_mm","Gender","age_dia0",
                "mut1","mut2","genotype","mut1_class","mut2_class","class_geno","year",
                "age_start","status","age","age_fev","FEV1_z","FEV1_pp",
                "bmiECFSPR","bmi_z",
                "pseudo",
                "Diabetes","liver",
                "enzymes",
                "NaCl","rhDNase","Mannitol",
                "ivDaysTotal", "steroid_inhaled",
                "lung_t",
                "eligible")]

data<- c()
df_2021<- df[df$year==2021,]
df_2019<- df[df$year==2019,]


# tabella missing totale

# FALSE: missing
# TRUE: missing

#for (i in 1:ncol(df)) {
#  print(colnames(df[i]))
#  for (y in (2019:2021)) {
#  if (y==2019) {data= df_2019}
#  if (y==2020) {data=df_2020}
#  if (y==2021) {data=df_2021}
    
#  tab<- round(prop.table(table(!is.na(data[,i])))*100,2)
#  print(y)
#  print(tab)
#  }
#}


# tabella missing per country

not_miss2019<- c()
not_miss2021<- c()


for (i in 1:ncol(df)) {
  print(colnames(df[i]))
  for (y in c(2019, 2021)) {
    if (y==2019) {data= df_2019}
    if (y==2021) {data= df_2021}
    
    tab<- round(prop.table(table(!is.na(data[,i]), data$country),2)*100,2)
    perc_not_miss<- tab[dimnames(tab)[[1]]== "TRUE",]
    
    if (y==2019) {not_miss2019<- cbind(not_miss2019,perc_not_miss)}
    if (y==2021) {not_miss2021<- cbind(not_miss2021,perc_not_miss)}
  }
}


colnames(not_miss2019)<- colnames(not_miss2021)<- colnames(df)

miss2019<- format(round(100-not_miss2019,2), trim = T, nsmall = 1)

miss2021<- format(round(100-not_miss2021,2), trim= T, nsmall= 1)

# eliminiamo l'Islanda dal 2021 perché non avevamo info nel 2019
miss2021<- miss2021[-(which(rownames(miss2021)== "IS")),]

View(miss2021)

rownames(miss2019)==rownames(miss2021)


# peggioramento è TRUE se aumenta il numerod i missing dal 2019 al 2021 CONTROLLARE TUTTO
peggioramento<- miss2019 < miss2021


miss2021b<- 100-not_miss2021
miss2021b<- miss2021b[-(which(rownames(miss2021b)== "IS")),]
miss2019b<- 100-not_miss2019
peggioramento_grave<- miss2021b-miss2019b > 5

View(peggioramento_grave)


write.csv(miss2019, file= 'A:\\1001022_Gramegna_DiseaseBurdenNonCFTRm\\Script_LA\\missing2019.csv',
           quote= FALSE, eol='\n', na= 'NA', 
          row.names= TRUE)

write.csv(miss2021, file= 'A:\\1001022_Gramegna_DiseaseBurdenNonCFTRm\\Script_LA\\missing2021.csv',
          quote= FALSE, eol='\n', na= 'NA', 
          row.names= TRUE)

write.csv(peggioramento, file= 'A:\\1001022_Gramegna_DiseaseBurdenNonCFTRm\\Script_LA\\peggioramento.csv',
          quote= FALSE, eol='\n', na= 'NA', 
          row.names= TRUE)

write.csv(peggioramento_grave, file= 'A:\\1001022_Gramegna_DiseaseBurdenNonCFTRm\\Script_LA\\peggioramento_me5.csv',
          quote= FALSE, eol='\n', na= 'NA', 
          row.names= TRUE)

#View(miss2019)
#View(miss2021[,"rhDNase"])

#round(prop.table(table(df_2021$steroid_inhaled, df_2021$country, useNA="ifany"),2)*100,2)

## Focus Pseudomonas Russia

#df_2021russia<- df_2021[df_2021$country== "RU"]
#table(df_2021russia$pseudo, useNA = "always")


