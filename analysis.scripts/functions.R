# This script contains functions for our analysis of fusions types

# gametes are named with four characters
# 1) X or Y to denote type of chromosome
# 2) E or S for sperm
# 3) 0 or 1 for male benefit and female benefit respectively
# 4) N, P, U for fusion to Non recombining region,
#    fusion to Par region, or Unfused

getInitialPop <-function(pop.size){
  sdr <- c("X","Y")
  gamete <- c("S","E")
  SA <- 0:1
  structure <- c("N","P","U")
  haplotypes <- c()
  for(i in 1:2){
    for(j in 1:2){
      for(k in 1:2){
        for(m in 1:3){
          haplotypes <- c(haplotypes,
                          paste(sdr[i],
                                gamete[j],
                                SA[k],
                                structure[m],
                                sep = ""))
        }
      }
    }
  }
  haplotypes <- haplotypes[-c(19:24)]
  eggs.names <- haplotypes[7:12]
  sperm.names <- haplotypes[c(1:6,13:18)]
  eggs <- rep(0, 6)
  names(eggs) <- eggs.names
  sperm <- rep(0, 12)
  names(sperm) <- sperm.names

  pop <- expand.grid(eggs,sperm, stringsAsFactors = F)
  pop$number <- rep(0, 72)
  colnames(pop)[1:2] <- c("egg","sperm")
  # this puts the max whole number of individuals into
  # each starting category
  eggs[3] <- floor(pop.size/4)
  eggs[6] <- floor(pop.size/4)
  sperm[3] <- floor(pop.size/8)
  sperm[6] <- floor(pop.size/8)
  sperm[9] <- floor(pop.size/8)
  sperm[12] <- floor(pop.size/8)

  # these are our leftovers
  leftover <- pop.size%%4

  if(leftover>0){
    if(sample(c(T,F), 1)){
      x <- sample(c(3,6), 1)
      eggs[x] <- eggs[x] + leftover
    }else{
      x <- sample(c(3,6,9,12), 1)
      sperm[x] <- sperm[x] + leftover
    }
  }
  gametes <-c(eggs,sperm)
  return(gametes)
}

getJuveniles <- function(pop.gam, pop.size){
  pop.mat <- matrix(0,6,12)
  colnames(pop.mat) <- names(pop.gam)[7:18]
  rownames(pop.mat) <- names(pop.gam)[1:6]

  # TODO fix this so that you always maintain population size - I think we fixed it twice
  frac.sperm <- pop.gam[7:18]/sum(pop.gam[7:18])
  frac.eggs <- pop.gam[1:6]/sum(pop.gam[1:6])
  vc <- sample(1:12, pop.size, replace=T, prob = frac.sperm)
  vr <- sample(1:6, pop.size, replace=T, prob = frac.eggs)
  vgenos <- paste(vc,"_",vr, sep = "")
  for(i in 1:ncol(pop.mat)){
    for(j in 1:nrow(pop.mat)){
      pop.mat[j, i] <- sum(vgenos == paste(i, "_", j, sep=""))
    }
  }
  ########
  #       TODO If we run well without this lets delete
  #
  # extra <- 0
  # for(i in 1:ncol(pop.mat)){
  #   for(j in 1:nrow(pop.mat)){
  #     pop.mat[j, i] <- floor(pop.size * frac.sperm[i] * frac.eggs[j])
  #     pos.extra <- as.vector(pop.size * frac.sperm[i] * frac.eggs[j] - pop.mat[j, i])
  #     if(pos.extra != 0){
  #       extra <- extra + pos.extra
  #     }
  #   }
  # }
  # extra <- round(extra)
  # if(extra != 0){
  #    # hits <- sample(1:72, size=extra, replace=T, prob = as.vector(pop.mat))
  #
  #    names <- as.vector(names(pop.gam))
  #    hits2 <- sample(names[1:6], size = extra, replace = T, prob = as.vector(pop.gam[1:6]))
  #    hits3 <- sample(names[7:18], size = extra, replace = T, prob = as.vector(pop.gam[7:18]))
  #    hits1 <- cbind(hits2,hits3)
  #
  #    temp.pop <- matrix(0, 6, 12)
  #    colnames(temp.pop) <- colnames(pop.mat)
  #    rownames(temp.pop) <- rownames(pop.mat)
  #
  #    for(i in 1:extra){
  #      x <- hits1[i,1]
  #      y <- hits1[i,2]
  #      temp.pop[x,y] <- temp.pop[x,y] + 1
  #    }
  #
  #   pop.mat <- pop.mat + temp.pop
  # }
  ########
  return(pop.mat)
}

popFit <- function(pop.juv, s, h){
  genotype.fit.mat  <- matrix(c(1+s, 1+h*s, 1, 
                                1/(1+s), 1/(1+h*s), 1),
                              3, 2, byrow = F)
  colnames(genotype.fit.mat) <- c("male", "female")
  rownames(genotype.fit.mat) <- c("00","01","11")
  pop.fit <- pop.juv
  for(i in 1:nrow(pop.fit)){
    #print(i)
    for(j in 1:ncol(pop.fit)){
      #print(j)
      a1 <- (substr(x = colnames(pop.fit)[j], start=3, stop=3))
      a2 <- (substr(x = rownames(pop.fit)[i], start=3, stop=3))
      geno <- paste(sort(c(a1,a2), decreasing=F), collapse="")
      if(j > 6){
        cur.fit.col <- 1
      }else{
        cur.fit.col <- 2
      }
      pop.fit[i, j] <- genotype.fit.mat[rownames(genotype.fit.mat) == geno, cur.fit.col]
    }
  }
  return(pop.fit)
}

perfSeln <- function(pop.juv, pop.fits){
  genos <- as.vector(pop.juv)
  geno.fits <- as.vector(pop.fits)
  genos.present <- which(genos!=0)
  fits.present <- geno.fits[which(genos!=0)]
  pop.genos <- vector(length=sum(pop.juv))
  pop.fits <- vector(length=sum(pop.juv))
  counter <- 1
  for(i in 1:length(genos.present)){
    gen.num <- genos[genos.present][i]
    pop.genos[counter: (counter + gen.num - 1)] <- rep(genos.present[i], gen.num)
    pop.fits[counter: (counter + gen.num - 1)] <- rep(fits.present[i], gen.num)
    counter <- counter + gen.num
  }
  # TODO
  # seperate out mothers and fathers surviving
  mothers <- pop.genos[pop.genos < 37]
  fathers <- pop.genos[!pop.genos < 37]
  mothers.fits <- pop.fits[1:length(mothers)]
  fathers.fits <- pop.fits[(length(mothers)+1):length(pop.fits)]
  surv.mothers <- sample(mothers, prob=mothers.fits, size=length(mothers), replace=T)
  surv.fathers <- sample(fathers, prob=fathers.fits, size=length(fathers), replace=T)

  pop.adu <- pop.juv
  pop.adu[,] <- 0
  x.mothers <- as.data.frame(table(surv.mothers), stringsAsFactors = F)
  x.fathers <- as.data.frame(table(surv.fathers), stringsAsFactors = F)
  pop.adu[as.numeric(x.mothers$surv.mothers)] <- x.mothers$Freq
  pop.adu[as.numeric(x.fathers$surv.fathers)] <- x.fathers$Freq
  return(pop.adu)
}



perfGameto <- function(pop.adu, r){
  recom.table.xx <- matrix(0,36,6)
  colnames(recom.table.xx) <- c("XE0N", "XE0P", "XE0U", "XE1N", "XE1P", "XE1U")
  recom.table.xx[1, ] <- c(1,0,0,0,0,0)
  recom.table.xx[2, ] <- c(.5,.5,0,0,0,0)
  recom.table.xx[3, ] <- c(.5,0,.5, 0,0,0)
  recom.table.xx[4, ] <- c(.5,0,0,.5,0,0)
  recom.table.xx[5, ] <- c((1-r)/2, r/2,	0,	r/2,	(1-r)/2,	0)
  recom.table.xx[6, ] <- c((1-r)/2,	0,	r/2,	r/2,	0,	(1-r)/2)
  recom.table.xx[7, ] <- c( 0.5,	0.5,	0,	0,	0,	0)
  recom.table.xx[8, ] <- c(0,	0.5,	0.5,	0,	0,	0)
  recom.table.xx[9, ] <- c(0,	0.5,	0.5,	0,	0,	0)
  recom.table.xx[10, ] <- c(r/2,	(1-r)/2,	0,	(1-r)/2,	r/2,	0)
  recom.table.xx[11, ] <- c(0,	0.5,	0,	0,	0.5,	0)
  recom.table.xx[12, ] <- c(0,	(1-r)/2,	r/2,	0,	r/2,	(1-r)/2)
  recom.table.xx[13, ] <- c(0.5,	0,	0.5,	0,	0,	0)
  recom.table.xx[14, ] <- c(0,	0.5,	0.5,	0,	0,	0)
  recom.table.xx[15, ] <- c(0,	0,	1,	0,	0,	0)
  recom.table.xx[16, ] <- c(r/2,	0,	(1-r)/2,	(1-r)/2,	0,	r/2)
  recom.table.xx[17, ] <- c(0,	r/2,	(1-r)/2,	0,	(1-r)/2,	r/2)
  recom.table.xx[18, ] <- c(0,	0,	0.5,	0,	0,	0.5)
  recom.table.xx[19, ] <- c(0.5,	0,	0,	0.5,	0,	0)
  recom.table.xx[20, ] <- c(r/2,	(1-r)/2,	0,	(1-r)/2,	r/2,	0)
  recom.table.xx[21, ] <- c(r/2,	0,	(1-r)/2,	(1-r)/2,	0,	r/2)
  recom.table.xx[22, ] <- c(0,	0,	0,	1,	0,	0)
  recom.table.xx[23, ] <- c(0,	0,	0,	0.5,	0.5,	0)
  recom.table.xx[24, ] <- c(0,	0,	0,	0.5,	0,	0.5)
  recom.table.xx[25, ] <- c((1-r)/2,	r/2,	0,	r/2,	(1-r)/2,	0)
  recom.table.xx[26, ] <- c(0,	0.5,	0,	0,	0.5,	0)
  recom.table.xx[27, ] <- c(0,	r/2,	(1-r)/2,	0,	(1-r)/2,	r/2)
  recom.table.xx[28, ] <- c(0,	0,	0,	0.5,	0.5,	0)
  recom.table.xx[29, ] <- c(0,	0,	0,	0,	1,	0)
  recom.table.xx[30, ] <- c(0,	0,	0,	0,	0.5,	0.5)
  recom.table.xx[31, ] <- c((1-r)/2,	0,	r/2,	r/2,	0,	(1-r)/2)
  recom.table.xx[32, ] <- c(0,	(1-r)/2,	r/2,	0,	r/2,	(1-r)/2)
  recom.table.xx[33, ] <- c(0,	0,	0.5,	0,	0,	0.5)
  recom.table.xx[34, ] <- c(0,	0,	0,	0.5,	0,	0.5)
  recom.table.xx[35, ] <- c(0,	0,	0,	0,	0.5,	0.5)
  recom.table.xx[36, ] <- c(0,	0,	0,	0,	0,	1)
  row.names(recom.table.xx) <- c("X0N X0N","X0N X0P","X0N X0U","X0N X1N","X0N X1P","X0N X1U",
                                 "X0P X0N","X0P X0P","X0P X0U","X0P X1N","X0P X1P","X0P X1U",
                                 "X0U X0N","X0U X0P","X0U X0U","X0U X1N","X0U X1P","X0U X1U",
                                 "X1N X0N","X1N X0P","X1N X0U","X1N X1N","X1N X1P","X1N X1U",
                                 "X1P X0N","X1P X0P","X1P X0U","X1P X1N","X1P X1P","X1P X1U",
                                 "X1U X0N","X1U X0P","X1U X0U","X1U X1N","X1U X1P","X1U X1U"
)


  # some of these genotypes are producing no gametes!!
  recom.table.xy <- matrix(0,36,12)
  colnames(recom.table.xy) <- c("XS0N", "XS0P", "XS0U", "XS1N", "XS1P", "XS1U","YS0N", "YS0P", "YS0U", "YS1N", "YS1P", "YS1U")
  recom.table.xy[1, ] <- c(0.5,	0,	0,	0,	0,	0,	0.5,	0,	0,	0,	0,	0)
  recom.table.xy[2, ] <- c(0,	0,	0,	0, 0,	0,	0,	0,	0,	0,	0,	0)
  recom.table.xy[3, ] <- c(0,	0,	0.5,	0,	0,	0,	0.5,	0,	0,	0,	0,	0)
  recom.table.xy[4, ] <- c(0.5*r,	0,	0,	0.5*(1-r),	0,	0,	0.5*(1-r),	0,	0,	0.5*r,	0,	0)
  recom.table.xy[5, ] <- c(0,	0,	0,	0, 0,	0,	0,	0,	0,	0,	0,	0)
  recom.table.xy[6, ] <- c(0,	0,	0.5*r,	0,	0,	0.5*(1-r),	0.5*(1-r),	0,	0,	0.5*r,	0,	0)
  recom.table.xy[7, ] <- c(0,	0,	0,	0, 0,	0,	0,	0,	0,	0,	0,	0)
  recom.table.xy[8, ] <- c(0,	0.5,	0,	0,	0,	0,	0,	0.5,	0,	0,	0,	0)
  recom.table.xy[9, ] <- c(0,	0.5,	0,	0,	0,	0,	0,	0,	0.5,	0,	0,	0)
  recom.table.xy[10, ] <- c(0,	0,	0,	0, 0,	0,	0,	0,	0,	0,	0,	0)
  recom.table.xy[11, ] <- c(0,	0.5*(1-r),	0,	0,	0.5*r,	0,	0,	0.5*r,	0,	0,	0.5*(1-r),	0)
  recom.table.xy[12, ] <- c(0,	0.5*(1-r),	0,	0,	0.5*r,	0,	0,	0,	0.5*r,	0,	0,	0.5*(1-r))
  recom.table.xy[13, ] <- c(0.5,	0,	0,	0,	0,	0,	0,	0,	0.5,	0,	0,	0)
  recom.table.xy[14, ] <- c(0,	0,	0.5,	0,	0,	0,	0,	0.5,	0,	0,	0,	0)
  recom.table.xy[15, ] <- c(0,	0,	0.5,	0,	0,	0,	0,	0,	0.5,	0,	0,	0)
  recom.table.xy[16, ] <- c(0.5*r,	0,	0,	0.5*(1-r),	0,	0,	0,	0,	0.5*(1-r),	0,	0,	0.5*r)
  recom.table.xy[17, ] <- c(0,	0,	0.5*(1-r),	0,	0,	0.5*r,	0,	0.5*r,	0,	0,	0.5*(1-r),	0)
  recom.table.xy[18, ] <- c(0,	0,	0.25,	0,	0,	0.25,	0,	0,	0.25,	0,	0,	0.25)
  recom.table.xy[19, ] <- c(0.5*(1-r),	0,	0,	0.5*r,	0,	0,	0.5*r,	0,	0,	0.5*(1-r),	0,	0)
  recom.table.xy[20, ] <- c(0,	0,	0,	0, 0,	0,	0,	0,	0,	0,	0,	0)
  recom.table.xy[21, ] <- c(0,	0,	0.5*(1-r),	0,	0,	0.5*r,	0.5*r,	0,	0,	0.5*(1-r),	0,	0)
  recom.table.xy[22, ] <- c(0,	0,	0,	0.5,	0,	0,	0,	0,	0,	0.5,	0,	0)
  recom.table.xy[23, ] <- c(0,	0,	0,	0, 0,	0,	0,	0,	0,	0,	0,	0)
  recom.table.xy[24, ] <- c(0,	0,	0,	0,	0,	0.5,	0,	0,	0,	0.5,	0,	0)
  recom.table.xy[25, ] <- c(0,	0,	0,	0, 0,	0,	0,	0,	0,	0,	0,	0)
  recom.table.xy[26, ] <- c(0,	0.5*r,	0,	0,	0.5*(1-r),	0,	0,	0.5*(1-r),	0,	0,	0.5*r,	0)
  recom.table.xy[27, ] <- c(0,	0.5*r,	0,	0,	0.5*(1-r),	0,	0,	0,	0.5*(1-r),	0,	0,	0.5*r)
  recom.table.xy[28, ] <- c(0,	0,	0,	0, 0,	0,	0,	0,	0,	0,	0,	0)
  recom.table.xy[29, ] <- c(0,	0,	0,	0,	0.5,	0,	0,	0,	0,	0,	0.5,	0)
  recom.table.xy[30, ] <- c(0,	0,	0,	0,	0.5,	0,	0,	0,	0,	0,	0,	0.5)
  recom.table.xy[31, ] <- c(0.5*(1-r),	0,	0,	0.5*r,	0,	0,	0,	0,	0.5*r,	0,	0,	0.5*(1-r))
  recom.table.xy[32, ] <- c(0,	0,	0.5*r,	0,	0,	0.5*(1-r),	0,	0.5*(1-r),	0,	0,	0.5*r,	0)
  recom.table.xy[33, ] <- c(0,	0,	0.25,	0,	0,	0.25,	0,	0,	0.25,	0,	0,	0.25)
  recom.table.xy[34, ] <- c(0,	0,	0,	0.5,	0,	0,	0,	0,	0,	0,	0,	0.5)
  recom.table.xy[35, ] <- c(0,	0,	0,	0,	0,	0.5,	0,	0,	0,	0,	0.5,	0)
  recom.table.xy[36, ] <- c(0,	0,	0,	0,	0,	0.5,	0,	0,	0,	0,	0,	0.5)
  row.names(recom.table.xy) <- c("Y0N X0N", "Y0N X0P", "Y0N X0U","Y0N X1N","Y0N X1P","Y0N X1U",
                                 "Y0P X0N","Y0P X0P","Y0P X0U","Y0P X1N","Y0P X1P","Y0P X1U",
                                 "Y0U X0N","Y0U X0P","Y0U X0U","Y0U X1N","Y0U X1P","Y0U X1U",
                                 "Y1N X0N","Y1N X0P","Y1N X0U","Y1N X1N","Y1N X1P","Y1N X1U",
                                 "Y1P X0N","Y1P X0P","Y1P X0U","Y1P X1N","Y1P X1P","Y1P X1U",
                                 "Y1U X0N","Y1U X0P","Y1U X0U","Y1U X1N","Y1U X1P","Y1U X1U"
  )
  eggs <- colSums(as.vector(pop.adu)[1:36] * recom.table.xx)
  sperm <- colSums(as.vector(pop.adu)[37:72] * recom.table.xy)
  # currently does nothing
  #results <- 4 * c(eggs, sperm)



  pop.recom <- c(eggs, sperm)
  return(pop.recom)
}

StochRound = function(pop.recom){

  for (i in 1:length(pop.recom))
       {
  ## extract the decimal portion
  x <- pop.recom[i]
    q = abs(x - trunc(x))

  ## draw a value 0 or 1 with probability
  ## based on how close we already are
  adj = sample(0:1, size = 1, prob = c(1 - q, q))

  ## make it negative if x is
  if(x < 0) adj = adj * -1

  ## return our new value
  pop.recom[i] <- trunc(x) + adj
  }
  return(pop.recom)
}

perfMutation <- function(pop.gam, num.mutes, model){
  if(num.mutes[i] > 0){

    # 1 = "XE0N"   2 = "XE0P"
    # 3 = "XE0U"   4 = "XE1N"
    # 5 = "XE1P"   6 = "XE1U"

    # 7 = "XS0N"   8 = "XS0P"
    # 9 = "XS0U"  10 = "XS1N"
    # 11 = "XS1P" 12 = "XS1U"

    # 13 = "YS0N" 14 = "YS0P"
    # 15 = "YS0U" 16 = "YS1N"
    # 17 = "YS1P" 18 = "YS1U"
    if(model == "auto.and.nonparX"){
      # fuse or fission between par and autosome
      # this code is specifically for fusion of X nonpar and an autosome
      for(j in 1:num.mutes[i]){

        # foo <- pop.gam[c(3, 6, 9, 12, 1, 4, 7, 10)]
        # foo <- replace(foo, foo <= 0, 0)

        hit <- sample(x = c(3, 6, 9, 12, 1, 4, 7, 10), 1,
                      prob = pop.gam[c(3, 6, 9, 12, 1, 4, 7, 10)])

        # # KW - This sample function will throw if all of the values in prob are 0
        # hit <- sample(x = c(3, 6, 9, 12), 1,
        #               prob = pop.gam[c(3, 6, 9, 12)])

        # KW - Changed this temporarily to try and avoid negative numbers of individuals

         #if (pop.gam[hit] > 0) {
        pop.gam[hit] <- pop.gam[hit] - 1

        # TODO fix this as a vector swap so hit 3 =1
        # hit 6 = 4 etc.
        if(hit==3)  pop.gam[1] <- pop.gam[1] + 1
        if(hit==6)  pop.gam[4] <- pop.gam[4] + 1
        if(hit==9)  pop.gam[7] <- pop.gam[7] + 1
        if(hit==12) pop.gam[10] <- pop.gam[10] + 1
        # mutation reverting
        if(hit==1)  pop.gam[3] <- pop.gam[3] + 1
        if(hit==4)  pop.gam[6] <- pop.gam[6] + 1
        if(hit==7)  pop.gam[9] <- pop.gam[9] + 1
        if(hit==10)  pop.gam[12] <- pop.gam[12] + 1
        # }


      }
    }
    if(model == "auto.and.parX"){
      # fuse or fission between par and autosome
      # this code is specifically for fusion of X par and an autosome
      for(j in 1:num.mutes[i]){
        hit <- sample(x = c(3, 6, 9, 12, 2, 5, 8, 11), 1,
                      prob = pop.gam[c(3, 6, 9, 12, 2, 5, 8, 11)])
        pop.gam[hit] <- pop.gam[hit] - 1

        if(hit==3)  pop.gam[2] <- pop.gam[2] + 1
        if(hit==6)  pop.gam[5] <- pop.gam[5] + 1
        if(hit==9)  pop.gam[8] <- pop.gam[8] + 1
        if(hit==12) pop.gam[11] <- pop.gam[11] + 1
        # mutation reverting
        if(hit==2)  pop.gam[3] <- pop.gam[3] + 1
        if(hit==5)  pop.gam[6] <- pop.gam[6] + 1
        if(hit==8)  pop.gam[9] <- pop.gam[9] + 1
        if(hit==11)  pop.gam[12] <- pop.gam[12] + 1
      }
    }
    if(model == "auto.and.nonparY"){
      # fuse or fission between non par and autosome
      # this code is specifically for fusion of Y non par and an autosome
      for(j in 1:num.mutes[i]){
        hit <- sample(x = c(15, 18, 13, 16), 1,
                      prob = pop.gam[c(15, 18, 13, 16)])
        pop.gam[hit] <- pop.gam[hit] - 1

        if(hit==15)  pop.gam[13] <- pop.gam[13] + 1
        if(hit==18)  pop.gam[16] <- pop.gam[16] + 1
        # mutation reverting
        if(hit==13)  pop.gam[15] <- pop.gam[15] + 1
        if(hit==16)  pop.gam[18] <- pop.gam[18] + 1
      }
    }
    if(model == "auto.and.parY"){
      # fuse or fission between par and autosome
      # this code is specifically for fusion of Y par and an autosome
      for(j in 1:num.mutes[i]){
        hit <- sample(x = c(15, 18, 14, 17), 1,
                      prob = pop.gam[c(15, 18, 14, 17)])
        pop.gam[hit] <- pop.gam[hit] - 1

        if(hit==15)  pop.gam[14] <- pop.gam[14] + 1
        if(hit==18)  pop.gam[17] <- pop.gam[17] + 1
        # mutation reverting
        if(hit==14)  pop.gam[15] <- pop.gam[15] + 1
        if(hit==17)  pop.gam[18] <- pop.gam[18] + 1
      }
    }
    if(model == "auto.and.allX"){
      # fuse or fission between both par and non-par X and autosome
      for(j in 1:num.mutes[i]){
        hit <- sample(x = c(3, 6, 9, 12, 1, 4, 7, 10, 2, 5, 8, 11), 1,
                      prob = pop.gam[c(3, 6, 9, 12, 1, 4, 7, 10, 2, 5, 8, 11)])
        pop.gam[hit] <- pop.gam[hit] - 1
        modes <- c("par","nonpar")
        mode <- sample(modes, size = 1)
        if(mode == "par"){

        # par fusion
        if(hit==3)  pop.gam[1] <- pop.gam[1] + 1
        if(hit==6)  pop.gam[4] <- pop.gam[4] + 1
        if(hit==9)  pop.gam[7] <- pop.gam[7] + 1
        if(hit==12) pop.gam[10] <- pop.gam[10] + 1
        }
        if(mode == "nonpar"){
        # nonpar fusion
        if(hit==3)  pop.gam[2] <- pop.gam[2] + 1
        if(hit==6)  pop.gam[5] <- pop.gam[5] + 1
        if(hit==9)  pop.gam[8] <- pop.gam[8] + 1
        if(hit==12) pop.gam[11] <- pop.gam[11] + 1
        }

    # mutation reverting
        if(hit==1)  pop.gam[3] <- pop.gam[3] + 1
        if(hit==4)  pop.gam[6] <- pop.gam[6] + 1
        if(hit==7)  pop.gam[9] <- pop.gam[9] + 1
        if(hit==10)  pop.gam[12] <- pop.gam[12] + 1

        if(hit==2)  pop.gam[3] <- pop.gam[3] + 1
        if(hit==5)  pop.gam[6] <- pop.gam[6] + 1
        if(hit==8)  pop.gam[9] <- pop.gam[9] + 1
        if(hit==11)  pop.gam[12] <- pop.gam[12] + 1
      }
    }
    if(model == "auto.and.allY"){
      # fuse or fission between par and non-par Y and autosome
      for(j in 1:num.mutes[i]){
        hit <- sample(x = c(15, 18, 14, 17, 13, 16), 1,
                      prob = pop.gam[c(15, 18, 14, 17, 13, 16)])
        pop.gam[hit] <- pop.gam[hit] - 1
        modes <- c("par","nonpar")
        mode <- sample(modes, size = 1)
        if(mode == "par"){

          # par fusion
          if(hit==15)  pop.gam[14] <- pop.gam[14] + 1
          if(hit==18)  pop.gam[17] <- pop.gam[17] + 1
        }
        if(mode == "nonpar"){
          # nonpar fusion
          if(hit==15)  pop.gam[13] <- pop.gam[13] + 1
          if(hit==18)  pop.gam[16] <- pop.gam[16] + 1
        }

        # mutation reverting
        if(hit==14)  pop.gam[15] <- pop.gam[15] + 1
        if(hit==17)  pop.gam[18] <- pop.gam[18] + 1

        if(hit==13)  pop.gam[15] <- pop.gam[15] + 1
        if(hit==16)  pop.gam[18] <- pop.gam[18] + 1
      }
    }
    if(model == "auto.and.allXY"){
      # fuse or fission between both par and non-par X and par and non-par Y and autosome
      for(j in 1:num.mutes[i]){
        hit <- sample(x = c(3, 6, 9, 12, 1, 4, 7, 10, 2, 5, 8, 11, 15, 18, 14, 17, 13, 16), 1,
                      prob = pop.gam[c(3, 6, 9, 12, 1, 4, 7, 10, 2, 5, 8, 11, 15, 18, 14, 17, 13, 16)])
        pop.gam[hit] <- pop.gam[hit] - 1
        modes <- c("par","nonpar")
        mode <- sample(modes, size = 1)
        if(mode == "par"){

          # par fusion
          if(hit==3)  pop.gam[1] <- pop.gam[1] + 1
          if(hit==6)  pop.gam[4] <- pop.gam[4] + 1
          if(hit==9)  pop.gam[7] <- pop.gam[7] + 1
          if(hit==12) pop.gam[10] <- pop.gam[10] + 1

          if(hit==15)  pop.gam[14] <- pop.gam[14] + 1
          if(hit==18)  pop.gam[17] <- pop.gam[17] + 1
        }
        if(mode == "nonpar"){
          # nonpar fusion
          if(hit==3)  pop.gam[2] <- pop.gam[2] + 1
          if(hit==6)  pop.gam[5] <- pop.gam[5] + 1
          if(hit==9)  pop.gam[8] <- pop.gam[8] + 1
          if(hit==12) pop.gam[11] <- pop.gam[11] + 1

          if(hit==15)  pop.gam[13] <- pop.gam[13] + 1
          if(hit==18)  pop.gam[16] <- pop.gam[16] + 1
        }

        # mutation reverting
        if(hit==1)  pop.gam[3] <- pop.gam[3] + 1
        if(hit==4)  pop.gam[6] <- pop.gam[6] + 1
        if(hit==7)  pop.gam[9] <- pop.gam[9] + 1
        if(hit==10)  pop.gam[12] <- pop.gam[12] + 1

        if(hit==2)  pop.gam[3] <- pop.gam[3] + 1
        if(hit==5)  pop.gam[6] <- pop.gam[6] + 1
        if(hit==8)  pop.gam[9] <- pop.gam[9] + 1
        if(hit==11)  pop.gam[12] <- pop.gam[12] + 1

        if(hit==14)  pop.gam[15] <- pop.gam[15] + 1
        if(hit==17)  pop.gam[18] <- pop.gam[18] + 1

        if(hit==13)  pop.gam[15] <- pop.gam[15] + 1
        if(hit==16)  pop.gam[18] <- pop.gam[18] + 1
      }
    }
   }
  return(pop.gam)
}


GetFreqs <- function(results, val){

  # Frequency of the 1 allele
  if(val == "1x"){
    simres <- rowSums(results[,c(4:6,10:12)])/rowSums(results[,1:12])
  }
  if(val == "1y"){
    simres <- rowSums(results[,16:18])/rowSums(results[,13:18])
  }
  # Frequency of fused chromosomes
  if(val == "unfusedx"){
    simres <- rowSums(results[,c(3,6,9,12)])/rowSums(results[,1:12])
  }
  if(val == "unfusedy"){
    simres <- rowSums(results[,c(15,18)])/rowSums(results[,13:18])
  }
  if(val == "fusedx"){
    simres <- rowSums(results[,c(1,2,4,5,7,8,10,11)])/rowSums(results[,1:12])
  }
  if(val == "fusedy"){
    simres <- rowSums(results[,c(13,14,16,17)])/rowSums(results[,13:18])
  }
  if(val == "pary"){
    simres <- rowSums(results[,c(14,17)])/rowSums(results[,13:18])
  }
  if(val == "npary"){
    simres <- rowSums(results[,c(13,16)])/rowSums(results[,13:18])
  }
  if(val == "parx"){
    simres <- rowSums(results[,c(2,5,8,11)])/rowSums(results[,1:12])
  }
  if(val == "nparx"){
    simres <- rowSums(results[,c(1,4,7,10)])/rowSums(results[,1:12])
  }

  return(simres)
}

