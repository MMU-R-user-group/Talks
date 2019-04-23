##### 00 Genotype ####
#genotype simulation function definition
#simulation at the POPULATION level

#assume diploid, 2 sexes



##### 01) Genotyping Founders ####

#1.1) define function and arguments

FounderGenom<- function(inBreeding = NULL,
                        myPed = NULL,
                        nAllel = NULL,
                        nLoci = NULL,
                        rSeed = NULL){
#1.2) Creating ancestor
  #Matrix will represent genotype with 2 alleles per loci
  #2 rows for each allele and nLoci number of Loci
  #Randomply sample alleles for ancestor
ancGeno <- function (y) matrix(sample(x=nAllel, nLoci*2, replace = T), 
                                 nrow=2, ncol=nLoci)
nAncestor<-1
set.seed(rSeed)
  
  
Ancestor <- lapply( X = 1:nAncestor,
                      FUN = ancGeno)

#1.3) Creating founder  
  #Matrix for Genotype
  #Identify who are the founders
  #Make matrix for each founder

mymat <- matrix(data=rep(0,2*nLoci), nrow=2, ncol=nLoci)
FoundPed<-which(is.na(myPed$Sire)& is.na(myPed$Dam))
Founder <- rep(list(mymat),length(FoundPed))

#Filling in founder genotype
for(i in 1:length(FoundPed))  {
  for(j in 1:nLoci){
    for(k in 1:2){
      ifelse(runif(1,1,100) < inBreeding*100,#based on pre set level of inbreeding
             
             ifelse(runif(1,1,100) < 50,
                    Founder[[i]][[k,j]] <- Ancestor[[1]][[1,j]], #sample from founder
                    Founder[[i]][[k,j]] <- Ancestor[[1]][[2,j]]),
             
             Founder[[i]][[k,j]] <- sample(x= 1:nAllel, size = 1)  )#randomly sample alleles
      
    }
  }
}

return(Founder)
}


#### 02) Whole population genotype ####

#2.1) define function and arguments
GenoSim<- function (nLoci = NULL,
                    myPed = NULL,
                    Founder = NULL){
  
#2.2) Creating Matrix for genotypes  
  #Matrix for Genotype
  #Make matrix for each individual 
mymat <- matrix(data=rep(0,2*nLoci), nrow=2, ncol=nLoci)
Genotypes <- rep(list(mymat),max(myPed$Id))

  #Add previoulsy made founders to the population
for(i in 1:length(Founder)){
  Genotypes[[i]] <- Founder[[i]]
}

#2.3) Filling in alleles
  #sample 1 allele from each parent
for (i in which(myPed$Gen > min(myPed$Gen))){
  for (j in 1:nLoci) {
    Genotypes[[i]][1,j] <- Genotypes[[myPed$Sire[i]]][sample(1:2,1),j]
    
    Genotypes[[i]][2,j] <- Genotypes[[myPed$Dam[i]]][sample(1:2,1),j]
  }
}


return(Genotypes)


}
