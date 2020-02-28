library(rpartitions)
library(ggplot2)
library(dunn.test)

########
#average delegate scenario

#random total number of members
set.seed(69)


#award delegates
award <- function(round2, ndelegates){
  t_voters <- sum(round2$Votes)
  d_out <- c()
  awarded_d <- c()
  for(groupi in 1:nrow(round2)){
    unrounded_delegate_num <- round2$Votes[groupi] * ndelegates/total_members
    #delegate_num <- round((round2$Votes[groupi] * ndelegates)/total_members, 0)
    d_out[groupi] <- unrounded_delegate_num
      
  }
  
  round2["Unrounded_Delegates"] <- d_out
  round2["Delegates"] <- floor(d_out + 0.5) #notice how i cant just use round(). I need R to do simple rounding, not to the nearest even number when at 0.5
  
  
  
  if(sum(round2$Delegates) > ndelegates){
    if(anyDuplicated(comparator(round2$Unrounded_Delegates)) != 0){
      comp_ar <- comparator(round2$Unrounded_Delegates)
      coin_flip = floor(runif(1, min = 1, max = 3))
      for(i in comp_ar){
        if(length(i) == 2) {
          round2$Delegates[i[coin_flip]] <- round2$Delegates[i[coin_flip]] - 1
          break}
      }
      
    }
    else{
    round2 <- removeDelegate(round2)}
  }
  else if(sum(round2$Delegates) < ndelegates){
    if(anyDuplicated(comparator(round2$Unrounded_Delegates)) != 0){
      comp_ar <- comparator(round2$Unrounded_Delegates)
      coin_flip = floor(runif(1, min = 1, max = 3))
      for(i in comp_ar){
        if(length(i) > 1) {
          round2$Delegates[i[coin_flip]] <- round2$Delegates[i[coin_flip]] + 1
          break}
      }
      
    }
    else{
    round2 <- addDelegate(round2)}
  }
  
  return(round2)

}


#these could probably be just one function
#This function removes a delegate from the group who had to the most rounding 
removeDelegate <- function(round2_df){
  
  sigma_array <- round_check(round2_df, addone = TRUE)
  above0.5 <- which(sigma_array >= 0.5)
  min_sig_i <- which.min(sigma_array[above0.5])
  round2_df$Delegates[[min_sig_i]] <- round2_df$Delegates[[min_sig_i]] - 1
  #round2_df$Delegates <- floor(round2_df$Unrounded_Delegates + 0.5)
  return(round2_df)
}
#this one does the opposite: adds a delegate to the group who did the least rounding 
addDelegate <- function(round2_df){
  
  sigma_array <- round_check(round2_df, addone = TRUE)
  
  min_sig_i <- which.max(sigma_array)
  round2_df$Delegates[[min_sig_i]] <- round2_df$Delegates[[min_sig_i]] + 1
  #round2_df$Delegates <- floor(round2_df$Unrounded_Delegates + 0.5)
  return(round2_df)
}

round_check <- function(df, addone= FALSE){
  sigma_array <- c()
  for(i in 1:length(df$Unrounded_Delegates)){
    if(df$Unrounded_Delegates[i] != 0 && trunc(df$Unrounded_Delegates[i]) == df$Unrounded_Delegates[i]){
      if(addone){
        sigma_array[i] <- 1
      }
      else{
      sigma_array[i] <- 0
      }
    }
    else if(df$Unrounded_Delegates[i] == 0){
      sigma_array[i] <- NaN
    }
    else{
      sigma_array[i] <- lessThanOne(df$Unrounded_Delegates[i])
    }
  }
  return(sigma_array)
}

comparator <- function(ar){
  
  if (anyDuplicated(ar, incomparables = c(0)) != 0){
    copy_ar <- list()
    for(x in 1:length(ar)){
      copy_ar[[x]] <- which(ar == ar[x])
    }
    return(copy_ar)
  }
  else{return(NULL)}
}

#Returns decimal 
lessThanOne <- function(num){
  if(num == 0){
    return(0)
  }
  return(num - as.integer(num))
}

assemble <- function(total_members = 100, candidates = LETTERS[1:5], ccd = 10){
  dvc <- ceiling(total_members * 0.15)
  
  algo_type <- floor(runif(1,1,3))
  algos <- c("best", "top_down")
  #getting viable candidate list of votes
  notfound <- TRUE
  while(notfound){
    p <- rand_partitions(total_members, length(candidates),1,zeros = TRUE, method = algos[algo_type])
    c <- 0
    for( i in p){
      if(i < 15 && i > 0){
        next()
      }
      c<- c +1
    }
    if(c == length(p)){
      notfound <- FALSE
    }
  }
  
  caucus <- data.frame(p)
  caucus[,2] <- candidates
  colnames(caucus) <- c("Votes", "Candidates")
  final_tally <- award(caucus, ccd)
  final_tally["Vote_per_Delegate"] <- final_tally$Votes/final_tally$Delegates
  return(final_tally)
}
#######
#Running the functions

iowa.sim <- list()
for(i in 1:1000){
  i_list <- list()
  
  total_members <- floor(runif(1, min = 90, max = 120))
  #total_members <- 100
  final_tally <- assemble(total_members = total_members, candidates = LETTERS[1:4])
  
  
  i_list[["Final_Round_DF"]] <- final_tally
  i_list[["VpD_Variance"]] <- var(final_tally$Vote_per_Delegate, na.rm = TRUE)
  i_list[["Vote_Variance"]] <- var(final_tally$Votes[which(final_tally$Votes != 0)])
  i_list[["Vote_SD"]] <- sd(final_tally$Votes)
  i_list[["Delegate_Var"]] <- var(final_tally$Delegates[which(final_tally$Delegates != 0)])
  i_list[["Unr_Delegate_Var"]] <- var(final_tally$Unrounded_Delegates[which(final_tally$Unrounded_Delegates != 0)])
  iowa.sim[[i]] <- i_list
  
}

Vote_var_ar <- c()
Delegate_var <- c()
U_Delegate_var <- c()
Vote_sd_ar <- c()
VpD_var_ar <- c()
CA <- c()
CD <- c()
CB <- c()
CC <- c()
#CE <- c()

Candidate_VpD <- matrix()
for(i in 1:length(iowa.sim)){
  Vote_var_ar[i] <- iowa.sim[[i]]$Vote_Variance
  Delegate_var[i] <- iowa.sim[[i]]$Delegate_Var
  U_Delegate_var[i] <- iowa.sim[[i]]$Unr_Delegate_Var
  Vote_sd_ar[i] <- iowa.sim[[i]]$Vote_SD
  VpD_var_ar[i] <- iowa.sim[[i]]$VpD_Variance
  
  
  CA[i]<- iowa.sim[[i]]$Final_Round_DF$Vote_per_Delegate[1]
  CD[i] <- iowa.sim[[i]]$Final_Round_DF$Vote_per_Delegate[4]
  CB[i] <- iowa.sim[[i]]$Final_Round_DF$Vote_per_Delegate[2]
  CC[i] <- iowa.sim[[i]]$Final_Round_DF$Vote_per_Delegate[3]
  #CE[i] <- iowa.sim[[i]]$Final_Round_DF$Vote_per_Delegate[5]
  
}
#colnames(Candidate_VpD) <- LETTERS[1:4]

Candidate_VpD$A <- CA
Candidate_VpD$B <- CB
Candidate_VpD$C <- CC
Candidate_VpD$D <- CD
#Candidate_VpD$E <- CE

attach(stack(Candidate_VpD))
boxplot(values ~ ind, xlab = "Candidates", ylab = "Votes per Delegate Earned")
aov <- (aov(values ~ ind))

krusk <- kruskal.test(values ~ ind)
krusk

dunn.test(values, ind)


plot(aov)


qplot(log10(Delegate_var))
qplot(log10(U_Delegate_var))
qplot(log10(VpD_var_ar))
qplot(log10(Vote_var_ar))

plot(log10(Vote_var_ar), log10(U_Delegate_var) , xlab = "Log10 Transformed Vote Variance", ylab = "Log10 Transformed Raw Awarded Delegate Variance")
plot(log10(Vote_var_ar), log10(Delegate_var), xlab = "Log10 Transformed Vote Variance", ylab = "Log10 Transformed Rounded Awarded Delegate Variance")
qplot(Delegate_var, U_Delegate_var)
qplot(log10(VpD_var_ar), log10(Vote_var_ar), xlab = "Vote per Delegate Variance (Log10)", ylab = "Vote Variance (Log10)")
qplot(VpD_var_ar, Vote_var_ar)

qplot(log10(Vote_var_ar), log10(Delegate_var))

par(mfrow = c(1,1))

