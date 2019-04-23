


setwd("E:/Project/_New hope/Finished code")
#libraries
#install.packages("ggpubr")
library(kinship2)
library(pedigree)
#library(ggplot2)
#library(ggpubr)

#### 01 Load functions ####
source("Pedigree.R")
source("Genotype.R")
source("Heterozygosity.R")

##### 02 Simulating Pedigrees ####

# Random mating
set.seed(1);Ped1<-RandPed(nGens = 20,
              popSize = 11,
              maleRatio = 0.5,
              breedMale = 0.9,
              breedFemale = 0.9,
              breedingAge = 3,
              P.ages = c(1,2,3,4,5,6),
              P.death = c(0.4, 0.45, 0.5, 0.55, 0.6, 1),
              P.babies = c(0,0,0.5,0.5),
              nBabies = c(0,1,2,3))

print(Ped1)

# Kinship Mating
set.seed(1);Ped2<-KinPed(nGens = 20, 
             popSize = 11,
             maleRatio = 0.5,
             breedMale = 0.9,
             breedFemale = 0.9,
             breedingAge = 3,
             P.ages = c(1,2,3,4,5,6),
             P.death = c(0.4, 0.45, 0.5, 0.55, 0.6, 1),
             P.babies = c(0,0,0.5,0.5),
             nBabies = c(0,1,2,3))
print(Ped2)

Ped1$Dam[378]
Ped2$Dam[378]

Ped1$Sire[378]
Ped2$Sire[378]

##### 03 creating genotypes no inbreeding #####

# Random
Founder1<-FounderGenom(inBreeding = 0,
                       myPed = Ped1,
                       nAllel = 15,
                       nLoci = 10,
                       rSeed = 1010)

Geno1<-GenoSim(nLoci = 10,
               myPed = Ped1,
               Founder = Founder1)
print(Founder1)

print(Geno1)

Geno1[[378]]
Geno1[[190]]
Geno1[[175]]

# Kinship
Founder2<-FounderGenom(inBreeding = 0,
                       myPed = Ped2,
                       nAllel = 15,
                       nLoci = 10,
                       rSeed = 1010)

Geno2<-GenoSim(nLoci = 10,
               myPed = Ped2,
               Founder = Founder2)
Geno2[[378]]
Geno2[[190]]
Geno2[[22]]
##### 04 Creating Genotype with inbreeding ####
Founder1.3<-FounderGenom(inBreeding = 0.3,
                         myPed = Ped1,
                         nAllel = 15,
                         nLoci = 10,
                         rSeed = 1010)

Geno1.3<-GenoSim(nLoci = 10,
                 myPed = Ped1,
                 Founder = Founder1.3)

Founder1[[5]]
Founder1.3[[5]]
# Kinship
Founder2.3<-FounderGenom(inBreeding = 0.3,
                         myPed = Ped2,
                         nAllel = 15,
                         nLoci = 10,
                         rSeed = 1010)

Geno2.3<-GenoSim(nLoci = 10,
                 myPed = Ped2,
                 Founder = Founder2.3)

##### 05 Calculating Heterozygosity ####

#Random no inbreeding
(FH1<-FounderHet(myPed = Ped1,
                 nLoci = 10,
                 Founder = Founder1))

(LH1<-LivingHet(myPed = Ped1,
                nLoci = 10,
                Geno = Geno1))

## compared to .6-.7 based on study by Doyle et al. (2015)

#Kinship no inbreeding
(FH2<-FounderHet(myPed = Ped2,
                 nLoci = 10,
                 Founder = Founder2))

(LH2<-LivingHet(myPed = Ped2,
                nLoci = 10,
                Geno = Geno2))


#Random 0.3 inbreeding 
(FH1.3<-FounderHet(myPed = Ped1,
                 nLoci = 10,
                 Founder = Founder1.3))

(LH1.3<-LivingHet(myPed = Ped1,
                nLoci = 10,
                Geno = Geno1.3))


#Kinship 0.3 inbreeding
(FH2.3<-FounderHet(myPed = Ped2,
                 nLoci = 10,
                 Founder = Founder2.3))

(LH2.3<-LivingHet(myPed = Ped2,
                nLoci = 10,
                Geno = Geno2.3))

#Random no inbreeding
(Ped1hetF<- 1-(LH1/FH1))
#Random 0.3 inbreeding
(Ped1hetF.3<- 1-(LH1.3/FH1.3))

#Kinship no inbreeding
(Ped2hetF<- 1-(LH2/FH2))
#Kinship 0.3 inbreeding
(Ped2hetF.3<- 1-(LH2.3/FH2.3))

library(readxl)
GenoLrg <- read_excel("E:/Project/write up/results/Geno ANOVA.xlsx", 
                      sheet = "Large")
View(GenoLrg)
boxplot(GenoLrg$MeanF~GenoLrg$tretment,
        main="Large Population",
        names= c("0","0.3","0","0.3"),
        xlab = "Level of Founder Relatedness",
        ylab = "Mean Inbreeding",
        col = c("grey25","grey25","grey55","grey55"))

legend("topleft", inset=.02,
       c("Kinship","Random"), fill=c("grey25","grey55"), horiz=TRUE, cex=0.75)
##### 06 Next steps ####

# Optimise
# Generation by Generation Measument of Inbreeding
# Add Different Mating Systmes 

