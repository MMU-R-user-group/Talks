##### 00 Heterozygosity Calculator ####
# calculating heterozygosity function definition

#assume diploid, 2 sexes


#####01) Working out founder heterozygosity #####

#1.1) define function and arguments
FounderHet<- function (myPed = NULL,
                       nLoci = NULL,
                       Founder = NULL){


#1.2) hold heterozygosity count
  #create 1 by nLoci matrix
  #identify founders
  #makes a matrix for each founder
mymat <- matrix(data=rep(0,2*nLoci), nrow=1, ncol=nLoci)
TPed<-which(is.na(myPed$Sire)& is.na(myPed$Dam))
FHet <- rep(list(mymat),length(TPed))

  #count heterozygosity
  #if loci is heterozygotic count 1
for (i in 1:length(TPed)) {
  for (j in 1:nLoci) {
    if(Founder[[i]][1,j] != Founder[[i]][2,j]){
      FHet[[i]][,j]<-1
    }
    
  }
  
}


#1.3) calculating heterozygosity
  #vector as place holder for indevidual heterozygosity
IndFHet<-vector(mode = "integer", length = length(TPed))

  #from count avaerage heterozygosity for indeviduals 
for (i in 1:length(TPed)) {
  IndFHet[[i]] <- sum(FHet[[i]])/nLoci
} 

  #from indevidual averages average founder population heterozygosity
FoundHet<-sum(IndFHet)/length(TPed)

return(FoundHet)}


#####02) Working out living heterozygosity #####

#1.1) define function and arguments

LivingHet<- function(myPed = NULL,
                     nLoci = NULL,
                     Geno = NULL){

#1.2) hold heterozygosity count
  #create 1 by nLoci matrix
  #identify living population
  #makes a matrix for each founder
mymat <- matrix(data=rep(0,2*nLoci), nrow=1, ncol=nLoci)
TPed<-which(myPed$AgeAtDeath == "NA")
PHet <- rep(list(mymat),length(TPed))
test<-matrix(data = TPed, nrow = 1, ncol = length(TPed))

  #count heterozygosity
  #if loci is heterozygotic count 1
for (i in 1:length(TPed)) {
  k<-TPed[i]
  for (j in 1:nLoci) {
    if(Geno[[k]][1,j] != Geno[[k]][2,j]){
      PHet[[i]][,j]<-1
      }
  }
}

#1.3) calculating heterozygosity
  #vector as place holder for indevidual heterozygosity
IndPHet<-vector(mode = "integer", length = length(TPed))

  #from count avaerage heterozygosity for indeviduals 
for (i in 1:length(TPed)) {
  IndPHet[[i]] <- sum(PHet[[i]])/nLoci
} 
  #from indevidual averages average founder population heterozygosity
PopHet<-sum(IndPHet)/length(TPed)
return(PopHet)}



