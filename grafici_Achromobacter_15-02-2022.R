# grafico age ####

setwd('C:\\files\\Achromobacter_project\\grafici_2022')
tiff(filename= 'age2.tiff', width= 1600, height= 800)


prevs<- c(1.45, 3.24, 5.37, 7.06, 8.42,8.42,
          7.38,5.82,5.29,5.15,4.99)

par(mar= c(7,7,4,2), mgp= c(5,2.4,0))
barplot(prevs, col= 'grey50', space=1,
        names.arg = c('0-4','5-9','10-14','15-19',
                      '20-24','25-29','30-34','35-39','40-44',
                      '45-49','50 +'),
        las=1, cex.axis=2, main= '',
        xlab= 'Age group', ylab= 'Achromobacter infection prevalence',
        cex.names= 1.7,ylim = c(0,10),
        cex.lab= 2.4, oma= c(4,4,4,4))

axis(2, at= c(1,3,5,7,9), las= 1, cex.axis= 2)

box()

dev.off()
#axis(1, at= seq(0.5,24,2), labels = FALSE)


# 2 bacterium ####

NO_p<-  c(5.24,5.49,4.51,5.09,5.49,5.59,5.25)
YES_p<- c(5.80,2.62,6.52,8.40,7.53,4.32,7.87)

matrix(c(as.numeric(NO_p), as.numeric(YES_p)), nrow=7, ncol=2)->a
t(a)->a

setwd('C:\\files\\Achromobacter_project\\grafici_2022')
tiff(filename= 'bact2.tiff', width= 1600, height= 800)

par(mar= c(7,7,4,2), mgp= c(5,2.4,0))
barplot(a,main='',
        xlab= 'Bacterium',ylab= 'Achromobacter infection prevalence',
        beside = TRUE,ylim=c(0,10) ,
        names.arg =c('Pseudomonas \n aeruginosa', 'Burkholderia \n cepacia',
                     'Staphylococcus \n aureus','Stenotrophomonas \n maltophilia',
                     'Nontuberculous \n mycobacteria','Haemophilus \n influenzae','MRSA') ,
        cex.names = 1.7,col= c('grey80', 'black'),cex.axis = 2, las=1,
        cex.lab= 2.4)


axis(2, at= seq(1,10,2), las=1, cex.axis= 2)
legend(1,9.5, col= c('red', 'black'), legend = c('NO infection', 'Infection') ,
       fill= c('grey80', 'black'), cex= 2.4, bty= 'n',
       y.intersp = 1, x.intersp= 0.8)
box()

dev.off()
# se cambi colore alle categorie, ricorda di cambiarle anche in legend

#axis(1, at= seq(0.5,23,3), cex.axis=0.6, labels = FALSE)


# 3 treatment ####

NOtreat_p<- c(4.83,3.56,2.98,4.38,4.19,5.21,4.32)
YEStreat_p<-c(5.88,6.51,8.20,7.82,8.04,9.66,7.50)

matrix(c(as.numeric(NOtreat_p), as.numeric(YEStreat_p)), nrow=7, ncol=2)->b
t(b)->b

setwd('C:\\files\\Achromobacter_project\\grafici_2022')
tiff(filename= 'treat3.tiff', width= 1600, height= 800)

par(mar= c(7,7,4,2), mgp= c(5,2.4,0))

barplot(b,main='',xlab= 'Treatment',ylab= 'Achromobacter infection prevalence',
        beside = TRUE,ylim=c(0,10) , col = c('grey80', 'black'),
        cex.lab= 2.4,las=1,cex.names=1.7, cex.axis=2,
        names.arg =c('Hypertonic \n saline','Dornase \n alfa', 'Inhaled \n antibiotics',
                     'Macrolides', 'Inhaled \n steroids',
                     'Oral \n steroids','PPI'))
axis(2, at= seq(1,10,2), las=1, cex.axis= 2)

#axis(1, at= seq(0.5,29,3), cex.axis=0.6, labels = FALSE)

legend(1,9.5, col= c('red', 'black'), legend = c('NO Treatment', 'Treatment') ,
       fill= c('grey80', 'black'), cex= 2.4, bty= 'n', y.intersp = 1, x.intersp= 0.8)
box()
dev.off()




# tutti i batteri ####

achro_age_prev<-  c( 1.45, 3.24, 5.37, 7.06, 8.42, 8.42, 7.38, 5.82, 5.29, 5.15, 4.99)
pseudo_age_prev <- c( 5.65, 11.37 ,19.37, 29.58, 39.73, 48.36, 52.36, 56.70 ,55.92 ,51.03, 41.88)
burkho_age_prev <- c(0.26, 0.83, 2.03, 3.45, 4.63, 4.43, 5.05, 4.25, 4.98, 3.45, 2.45)
haemInf_age_prev<- c(25.71, 28.05, 16.32, 14.91, 11.72,  9.95,  9.53,  7.78,  7.54,  7.73,  7.33)
malto_age_prev <- c(4.87,  6.73,  9.70, 11.51, 10.80,  9.29,  8.61,  8.14,  7.33,  8.30,  9.64)
MRSA_age_prev<- c( 3.17, 4.29, 6.12, 6.86, 6.11, 5.88, 6.64, 5.13, 5.32, 4.81, 4.50)
nontub_age_prev<- c(0.31, 1.20, 3.43, 5.82, 6.76, 6.61, 5.61, 5.12, 4.78, 5.38, 5.91)
staph_age_prev <- c(25.39 ,37.71, 44.91 ,48.21 ,46.40 ,41.47, 39.09 ,32.13 ,33.33 ,29.22, 28.48)
#stapMRSA_age_prev<- c( 30.32, 43.08 ,50.53 ,54.10 ,51.50 ,46.19 ,44.60 ,36.88, 40.03 ,34.33, 34.00)

setwd('C:\\files\\Achromobacter_project\\grafici_2022')
tiff(filename= 'bacteria_together_2022-3.tiff', width= 1600, height= 1620)

par(mar= c(7,7,4,2), mgp= c(5,2.4,0)) # mar: quantità di margine bianco
# mgp: margine tra etichette, valori assi, asse   

plot(achro_age_prev, type= 'l', ylim= c(0,60), col= 'red', lwd=3,
     axes = FALSE, main= '',
     cex.main=1 , cex.lab= 2.4,
     xlab= 'Age group',
     ylab= 'Prevalence of bacterial infection')
lines(pseudo_age_prev, col= 'brown', lwd=3)
lines(burkho_age_prev, col= 'green', lwd=3)
lines(staph_age_prev, col= 'purple', lwd=3)
lines(malto_age_prev, col= 'violet', lwd=3)
lines(nontub_age_prev, col= 'orange', lwd=3)
lines(haemInf_age_prev, col= 'black', lwd=3)
lines(MRSA_age_prev, col= 'blue', lwd=3)
#lines(stapMRSA_age_prev, col= 'grey', lwd= 3)
axis(1, at= seq(1,11), labels = c('0-4','5-9','10-14','15-19',
                                  '20-24','25-29','30-34','35-39','40-44',
                                  '45-49','50 +'),
     cex.axis= 2)
axis(2, at= seq(0,60,5), las=1, cex.axis= 2)
legend(0.75,61, col= c('red','brown','green', 'purple','violet','orange',
                       'black','blue'), text.font=3,
       lty = c(1,1,1,1,1,1,1), lwd= c(3,3,3,3,3,3,3),
       legend= c('Achromobacter species', 'Pseudomonas aeruginosa', 'Burkholderia Cepacia Complex',
                 'Staphylococcus aureus', 'Stenotrophomonas maltophilia',
                 'Nontuberculous mycobacteria', 'Haemophilus influenzae', 
                 'MRSA'),cex=2, bty= 'n', ncol=2 )
box()

dev.off()
