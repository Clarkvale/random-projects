

########
#average delegate scenario

#random total number of members
set.seed(69)

#total_members <- floor(runif(1, min = 100, max = 500))
total_members <- 100
#number of county convention delegates
ccd <- 10

#number of democratic candidates
ndc <- LETTERS[1:5]

#2 delegate viability cutoff
dvc <- ceiling(total_members * 0.15)

#partition function, 1st round
partition <- function(group_size, groups){
  groupout <- c()
  
  shrinking_t <- group_size
  
  group_vector <- c()
  r_group_order <- sample(groups, length(groups))
  count <- 1
  for(i in r_group_order){
    if(count == 1){
      out <- floor(rnorm(1,mean = 50))
      shrinking_t <- (shrinking_t - out)
    }
    else{
      out <- floor(runif(1, min = 0, max = shrinking_t))
      shrinking_t <- (shrinking_t - out)
    }
    count <- count + 1
    groupout[i] <- out
    
  
  
  }
  if(sum(groupout) != group_size){
    rand_candidate <- sample(groups, 1)
    groupout[rand_candidate] <- groupout[rand_candidate] + (group_size - sum(groupout))
  }
  return(groupout)
}

#partition function,2nd round, EXPECTS DATA.FRAME
partition2 <- function(mpars, cutoff){
  mobile <- mpars[mpars$Votes < cutoff,2]
  sum_mobile <- sum(mpars[mpars$Votes < cutoff,1])
  
  mpars[mpars$Votes < cutoff,1] <- 0 
  shrinking_mp <- sum_mobile
  viable <- mpars$Candidates[which(mpars$Votes != 0)]
  t_rearrange <- 0
  for(groups in sample(viable, length(viable))){
    add <- floor(runif(1,min = 0, max = shrinking_mp))
    shrinking_mp <- shrinking_mp - add
    t_rearrange <- t_rearrange + add
    mpars$Votes[which(mpars$Candidates == groups)] <- mpars$Votes[which(mpars$Candidates == groups)] + add
    
  }
  
  if(sum_mobile != t_rearrange){
    rand_candidate <- which(mpars$Candidates == sample(viable, 1))
    mpars$Votes[rand_candidate] <- mpars$Votes[rand_candidate] + (sum_mobile - t_rearrange)
  }
  return(mpars)  
} 


#award delegates
award <- function(round2, ndelegates){#Fix tie breaks and too many delegates
  t_voters <- sum(round2$Votes)
  d_out <- c()
  awarded_d <- c()
  for(groupi in 1:nrow(round2)){
    unrounded_delegate_num <- round2$Votes[groupi] * ndelegates/total_members
    #delegate_num <- round((round2$Votes[groupi] * ndelegates)/total_members, 0)
    d_out[groupi] <- unrounded_delegate_num
      
  }
  
  round2["Unrounded_Delegates"] <- d_out
  round2["Delegates"] <- round(d_out)
  
  
  
  if(sum(round2$Delegates) > ndelegates){
    if(anyDuplicated(comparator(round2$Unrounded_Delegates)) != 0){
      comp_ar <- comparator(round2$Unrounded_Delegates)
      coin_flip = floor(runif(1, min = 1, max = 2))
      for(i in comp_ar){
        if(length(i) == 2) {
          round2$Delegates[i[coin_flip]] - 1}
      }
      
    }
    round2 <- removeDelegate(round2)
  }
  else if(sum(sapply(round2$Unrounded_Delegates, FUN = round)) < ndelegates){
    round2 <- addDelegate(round2)
  }
  
  return(round2)

}


#these could probably be just one function
#This function removes a delegate from the group who had to the most rounding 
removeDelegate <- function(round2_df){
  sigma_array <- sapply(round2_df$Unrounded_Delegates, lessThanOne)
  sigma_array[which(sigma_array == 0)] <- NaN
  min_sig_i <- which.min(sigma_array)
  round2_df$Unrounded_Delegates[[min_sig_i]] <- round2_df$Unrounded_Delegates[[min_sig_i]] - 1
  round2_df$Delegates <- round(round2_df$Unrounded_Delegates)
  return(round2_df)
}
#this one does the opposite: adds a delegate to the group who did the least rounding 
addDelegate <- function(round2_df){
  sigma_array <- sapply(round2_df$Unrounded_Delegates, lessThanOne )
  min_sig_i <- which(sigma_array == max(sigma_array))
  round2_df$Unrounded_Delegates[[min_sig_i]] <- round2_df$Unrounded_Delegates[[min_sig_i]] - 1
  return(round2_df)
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





#######
#Running the functions



pars <- data.frame(matrix(partition(total_members, ndc)))
pars["Candidates"] <- ndc
colnames(pars) <- c("Votes", "Candidates")

pars2 <- partition2(pars, dvc)
final_tally <- award(pars2, ccd)
final_tally["Vote_per_Delegate"] <- final_tally$Votes/final_tally$Delegates


var(final_tally$Vote_per_Delegate, na.rm = TRUE)

#
