##### 00 Pedigree ####
#pedigree simulation function definition
#simulation at the POPULATION level

#assume diploid, 2 sexes



##### 01) Pedigree simulation based on random mating ####

  #1.1) define function and arguments
RandPed <- function(nGens = NULL, # number of generation to loop for
                    popSize = NULL, # number of  founders
                    maleRatio = NULL, # sex ratio weighted to males
                    breedMale = NULL, # Chance of a male breeding
                    breedFemale = NULL, # Chance of female breeding
                    breedingAge = NULL, # age individual can start to be selected for breeding
                    P.ages = NULL, # Possible ages to die at
                    P.death = NULL, # Chance of individuals dying each generation
                    nBabies = NULL, # possible number of babies
                    P.babies = NULL) #Chance of pairs having each number of babies
{
  #1.2) Initialize output variables for founder population
  #creating and filling vectors
  
  Id <- 1:popSize
  
  Sex <- vector(mode = "character", length = popSize)
  
  ## work out if even or odd number of founders
  # if even works just on males ratio
  # if odd last individual gets sampled separately 
  ifelse(popSize %% 2 == 0,   
         Sex <- c(rep("male", popSize*maleRatio),  
                  rep("female", popSize*(1-maleRatio)) ),
         Sex <- c(rep("male", trunc(popSize*maleRatio,0)), 
                  rep("female", trunc(popSize*(1-maleRatio),0)),
                  ifelse(runif(1,0,1) <= maleRatio, "male", "female")))
  
  Gen <- vector(mode = "integer", length = popSize)
  Gen[1:popSize] <- rep(1, each = popSize)
  
  Age <- vector(mode="integer", length=popSize)
  Age[1:popSize] <- rep(breedingAge, each = popSize)
  
  Sire <- vector(mode = "integer", length = popSize)
  Sire[1:popSize] <- rep(NA, popSize)
  
  Dam <- vector(mode="integer", length=popSize)
  Dam[1:popSize] <- rep(NA, popSize)
  
  # 1.3) Creating next generation
  
  i=1
  # PICK POTENTIAL BREEDERS
  tempM <- which(Gen==i & Sex=="male")
  tempF <- which(Gen==i & Sex=="female")
  
  # PICK ACTUAL BREEDERS
  breederM <- runif(length(tempM),0,1) < breedMale
  breederF <- runif(length(tempF),0,1) < breedFemale
  
  # CREATE BREEDING PAIRS
  pairsF <- tempF[breederF]
  pairsM <- sample(x=tempM[breederM], size=length(pairsF), replace=T )
  
  # Make babies
  # for each breeding female sample the number of babies they will have 
  babiesF <- vector(length(pairsF), mode="numeric")
  for(i in 1:length(pairsF)){
    babiesF[i] <- sample(x=nBabies, size=1, prob=P.babies)
  }
  
  # 1.4) Append vector infor for next generation
  # sum(babiesF) is the size of next generation
  
  Gen <- append(x=Gen,values = rep((max(Gen)+1),sum(babiesF), after = popSize))
  
  Id <- append(x=Id, values = seq(popSize+1, (popSize+sum(babiesF))), after = popSize)
  
  Dam <-append(x=Dam, values = rep(pairsF, babiesF), after = popSize) 
  
  Sire <-append(x=Sire, values = rep(pairsM, babiesF), after = popSize)
  
  Age <- (Age + 1)
  
  Age <- append(x=Age, values = rep(+1 , sum(babiesF)), after =  popSize)
  
  popSize <-(max(Id))
  
  Sex <- append(x=Sex, values=ifelse(runif(sum(babiesF), 0,1) > maleRatio, 
                                     "male", "female"), after = max(which(Gen < max(Gen))))
  
  #Start to eliminate individuals from the population
  Dead<- vector(length(popSize), mode="numeric")
  for(i in Id){
    Dead[i] <- sample(x=P.ages, size=1, prob=P.death)#sampling IDs of individuals who will die
  }
  
  AgeAtDeath <- vector(mode="integer", length=popSize) 
  
  AgeAtDeath[1:popSize] <- ifelse(Age == Dead,Age,"NA" )#Noting age of death 
  
  
  #1.5) Creating full population
  
  #1.5.1 Use a while loop While Gen < nGen loop, 3.3 to 4.3
  
  
  while(max(Gen)<nGens) 
  {                           
    
    #PICK BREEDERS
    tempM <- which(Sex=="male" & AgeAtDeath == "NA" & Age >= breedingAge ) 
    tempF <- which(Sex=="female" & AgeAtDeath == "NA"& Age >= breedingAge )
    breederM <- runif(length(tempM),0,1) < breedMale
    breederF <- runif(length(tempF),0,1) < breedFemale
    
    #CREATE BREEDING PAIRS
    pairsF <- tempF[breederF]
    pairsM <- sample(x=tempM[breederM], size=length(pairsF), replace=T )
    
    #Make babies
    
    babiesF <- vector(length(pairsF), mode="numeric")
    for(i in 1:length(pairsF)){
      babiesF[i] <- sample(x=nBabies, size=1, prob=P.babies)
    }
    
    #Append new generation
    Gen <- append(x=Gen,values = rep((max(Gen)+1),sum(babiesF), after = popSize))
    
    Id <- append(x=Id, values = seq(popSize+1, (popSize+sum(babiesF))), after = popSize)
    
    Dam <-append(x=Dam, values = rep(pairsF, babiesF), after = popSize) 
    
    Sire <-append(x=Sire, values = rep(pairsM, babiesF), after = popSize)
    
    Age <- (Age + 1)
    Age <- append(x=Age, values = rep(+1 , sum(babiesF)), after =  popSize)
    
    length(Sire)
    popSize <-(max(Id))
    
    Sex <- append(x=Sex, values=ifelse(runif(sum(babiesF), 0,1) > maleRatio, 
                                       "male", "female"), after = max(which(Gen < max(Gen))))
    
    
    Dead<- vector(length(popSize), mode="numeric")
    for(i in Id){
      Dead[i] <- sample(x=P.ages, size=1, prob=P.death)#sampling IDs of individuals who will die
    }
    
    AgeAtDeath <- append(x=AgeAtDeath, value = rep( "NA", sum(babiesF)), after = max(which(Gen < max(Gen))))
    
    newDead<- which(Age==Dead & AgeAtDeath == "NA") #working out out of sampled IDs who is already dead
    maxAge<- which(Age == max(P.ages)& AgeAtDeath == "NA")#working out who has reached max age
    
    
    AgeAtDeath[newDead]<-Age[newDead]
    AgeAtDeath[maxAge]<-max(P.ages)
    
    myPed<- data.frame(Id, Sire, Dam, Sex, Gen, Age, AgeAtDeath)
    
    
    if (max(Gen) == (nGens +1) ){
      print(myPed)
      
    }
    
  }
  return((myPed)) 
}

  

##### 02) Pedigree simulation based on optoimall kinship mating ####
#2.1) define function and arguments
KinPed <- function(nGens = NULL, 
                   popSize = NULL, # n founders
                   maleRatio = NULL, 
                   #Chance of a male breeding
                   breedMale = NULL,
                   #Chance of female breeding
                   breedFemale = NULL,
                   #Chance of individuals dying each generation
                   breedingAge = NULL,
                   P.ages = NULL,
                   P.death = NULL,
                   P.babies = NULL,
                   nBabies = NULL)
{
  
    # 2.2) Initialize temp and output variables for founder population
  
  #call necessary packages
  library(kinship2)

  Id <- 1:popSize
  
  Sex <- vector(mode = "character", length = popSize)
  
  ## work out if even or odd number of founders
  # if even works just on males ratio
  # if odd last individual gets sampled separately 
  
  ifelse(popSize %% 2 == 0,
         Sex <- c(rep("male", popSize*maleRatio), 
                  rep("female", popSize*(1-maleRatio)) ),
         Sex <- c(rep("male", trunc(popSize*maleRatio,0)), 
                  rep("female", trunc(popSize*(1-maleRatio),0)),
                  ifelse(runif(1,0,1) <= maleRatio, "male", "female")))
  
  Gen <- vector(mode = "integer", length = popSize)
  Gen[1:popSize] <- rep(1, each = popSize)
  
  Age <- vector(mode="integer", length=popSize)
  Age[1:popSize] <- rep(breedingAge, each = popSize)
  
  Sire <- vector(mode = "integer", length = popSize)
  Sire[1:popSize] <- rep(NA, popSize)
  
  Dam <- vector(mode="integer", length=popSize)
  Dam[1:popSize] <- rep(NA, popSize)
  
  
  
  # 2.3) Creating next generation
  
  myPed<- data.frame(Id, Sire, Dam, Sex, Gen, Age)

  myKin <-kinship(id= myPed$Id, dadid= myPed$Sire, momid= myPed$Dam, sex= myPed$Sex)#calculated relatedness
  listKin<-as.list(myKin)#created kinship coefficient list
  
  tempM <- which(Sex=="male" &  Age >= breedingAge) 
  tempF <- which(Sex=="female" & Age >= breedingAge)
  
  #PICK ACTUAL BREEDERS
  breederM <- runif(length(tempM),0,1) < breedMale
  breederF <- runif(length(tempF),0,1) < breedFemale
  #CREATE BREEDING PAIRS
  pairsF <- tempF[breederF]  
  pairsM <- sample(x= which(listKin[tempM[breederM]] == min(myKin[pairsF,])),#pair individuals with the lowest reletedness score
                   size=length(pairsF), replace=T )
  
  
  babiesF <- vector(length(pairsF), mode="numeric")
  for(i in 1:length(pairsF)){
    babiesF[i] <- sample(x=nBabies, size=1, prob=P.babies)
  }
  
  # 2.4) Append vector infor for next generation
  # sum(nbabies) is the size of next generation
  
  Gen <- append(x=Gen,values = rep((max(Gen)+1),sum(babiesF), after = popSize))
  
  Id <- append(x=Id, values = seq(popSize+1, (popSize+sum(babiesF))), after = popSize)
  
  Dam <-append(x=Dam, values = rep(pairsF, babiesF), after = popSize) 
  
  Sire <-append(x=Sire, values = rep(pairsM, babiesF), after = popSize)
  
  Age <- (Age + 1)
  Age <- append(x=Age, values = rep(+1 , sum(babiesF)), after =  popSize)
  
  popSize <-(max(Id))
  
  Sex <- append(x=Sex, values=ifelse(runif(sum(babiesF), 0,1) > maleRatio, 
                                     "male", "female"), after = max(which(Gen < max(Gen))))
  
  #start eliminating individuals from the population
  Dead<- vector(length(popSize), mode="numeric")
  for(i in Id){
    Dead[i] <- sample(x=P.ages, size=1, prob=P.death)#sampling IDs of individuals who will die
  }
  AgeAtDeath <- vector(mode="integer", length=popSize) 
  
  AgeAtDeath[1:popSize] <- ifelse(Age == Dead,Age,"NA" )#noting age of death
  
  #2.5) Creating full population
  
  #2.5.1 Use a whileloop While Gen < nGen loop, 3.3 to 4.3
  
  
  while(max(Gen)<nGens) 
  {                           
    
    myPed<- data.frame(Id, Sire, Dam, Sex, Gen, Age, AgeAtDeath)
   
    myKin <-kinship(id= myPed$Id, dadid= myPed$Sire, momid= myPed$Dam, sex= myPed$Sex)#calculate relatedness
    listKin<-as.list(myKin)#created kinship coefficient list
    
    tempM <- which(Sex=="male" & AgeAtDeath == "NA" & Age >= breedingAge) 
    tempF <- which(Sex=="female" & AgeAtDeath == "NA"& Age >= breedingAge )
    breederM <- runif(length(tempM),0,1) < breedMale
    breederF <- runif(length(tempF),0,1) < breedFemale
    
    #CREATE BREEDING PAIRS
    pairsF <- tempF[breederF]  
    pairsM <- sample(x= which(listKin[tempM[breederM]] == min(myKin[pairsF,])),#pair individuals with the lowest reletedness score
                     size=length(pairsF), replace=T )
    
  
    babiesF <- vector(length(pairsF), mode="numeric")
    for(i in 1:length(pairsF)){
      babiesF[i] <- sample(x=nBabies, size=1, prob=P.babies)
    }
   
    
    Gen <- append(x=Gen,values = rep((max(Gen)+1),sum(babiesF), after = popSize))
    
    Id <- append(x=Id, values = seq(popSize+1, (popSize+sum(babiesF))), after = popSize)
    
    Dam <-append(x=Dam, values = rep(pairsF, babiesF), after = popSize) 
    
    Sire <-append(x=Sire, values = rep(pairsM, babiesF), after = popSize)
    
    Age <- (Age + 1)
    Age <- append(x=Age, values = rep(+1 , sum(babiesF)), after =  popSize)
    
    
    popSize <-(max(Id))
    
    Sex <- append(x=Sex, values=ifelse(runif(sum(babiesF), 0,1) > maleRatio, 
                                       "male", "female"), after = max(which(Gen < max(Gen))))
    
    
    Dead<- vector(length(popSize), mode="numeric")
    for(i in Id){
      Dead[i] <- sample(x=P.ages, size=1, prob=P.death)#Sampling IDs of individuals who will die
    }
    
    AgeAtDeath <- append(x=AgeAtDeath, value = rep( "NA", sum(babiesF)), after = max(which(Gen < max(Gen))))
    
    newDead<- which(Age==Dead & AgeAtDeath == "NA") #working out out of sampled IDs who is already dead
    maxAge<- which(Age == max(P.ages)& AgeAtDeath == "NA")#working out who has reached max age
    
    
    AgeAtDeath[newDead]<-Age[newDead]
    AgeAtDeath[maxAge]<-max(P.ages)
   
    myPed<- data.frame(Id, Sire, Dam, Sex, Gen, Age, AgeAtDeath)
    
    
    if (max(Gen) == (nGens +1) ){
      print(myPed)
      
    }
    
  }
  return((myPed)) 
}

