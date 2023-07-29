library(forestplot)

mci<- as.numeric(mult_confint_inf) # valori inferiori dell'intervallo di confidenza 
mcs<- as.numeric(mult_confint_sup) # valori superiori
mcp<- as.numeric(mult_OR)          # stime puntuali degli odds ratio

# fpnames<- rep('',23) 

fpnames<- c('Age, 5-9 years vs 0-4 years',
            'Age, 10-14 years vs 5-9 years',
            'Age, 15-19 years vs 10-15 years',
            'Age, 20-24 years vs 15-19 years',
            'Age, 25-29 years vs 20-24 years',
            'Age, 30-34 years vs 25-29 years',
            'Age, 35-39 years vs 30-34 years',
            'Age, 40-44 years vs 35-39 years',
            'Age, 45-49 years vs 40-44 years',
            'Age, 50+ years vs 45-49 years',
            'Gender, female vs male',
            'Age at diagnosis, 1-5 years vs <1 year',
            'Age at diagnosis, >5 years vs <1 year',
            'Genotype, severe vs mild',
            'Underweight vs normal and overweight',
            'FEV1 % of predicted, 40-70% vs >70%',
            'FEV1 % of predicted, <40% vs >70%',
            'Chronic Pseudomonas, yes vs no',
            'At least 1 day on IV antibiotics, yes vs no',
            'PPI, yes vs no',
            'Ivacaftor vs no modulator',
            'Luma/iva or Teza/iva vs no modulator',
            'Other CFTR modulator vs no modulator ')

forestplot(fpnames ,mean= mcp, lower= mci, upper= mcs,
           col= fpColors(box= 'blue', line= 'blue'),
           xlog= TRUE, boxsize= 0.2,
           txt_gp = fpTxtGp(label= gpar(cex=0.7), 
                            ticks= gpar(cex=0.75), xlab= gpar(cex=0.75)),
           xticks= c(0.25,0.5,1,2,3,7))

