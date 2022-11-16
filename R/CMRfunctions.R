## Functions specific to CMR modeling process

# td: Trap Dependence, from RMARK chapter in the gentle introduction

create.td=function(ch,varname="td",begin.time=1)
  #
  # Arguments:
  # ch - capture history vector (0/1 values only)
  # varname - prefix for variable name
  # begin.time - time for first occasion
  #
  # Value:
  # returns a datframe with trap-dependent variables
  # named varnamet+1,...,varnamet+nocc-1
  # where t is begin.time and nocc is the
  # number of occasions
#
{
  # turn vector of capture history strings into a vector of characters
  char.vec=unlist(strsplit(ch,""))
  # test to make sure they only contain 0 or 1
  if(!all(char.vec %in% c(0,1)))
    stop("Function only valid for CJS model without missing values")
  else
  {
    # get number of occasions (nocc) and change it into a matrix of numbers
    nocc=nchar(ch[1])
    tdmat=matrix(as.numeric(char.vec),ncol=nocc,byrow=TRUE)
    # remove the last column which is not used
    tdmat=tdmat[,1:(nocc-1)]
    # turn it into a dataframe and assign the field (column) names
    tdmat=as.data.frame(tdmat)
    names(tdmat)=paste(varname,(begin.time+1):(begin.time+nocc-1),sep="")
    return(tdmat)
  }
}

# select model results by number from the model list produced by 
# the mark.wrapper function from RMark
# must use syntax: all.models=mark.wrapper(model.list,data=data.processed,ddl=data.ddl,threads=2)
select_model_results <- function(model_num){
  all.models[[paste0(model.list$Phi[model_num], '.', model.list$p[model_num])]]
}

# bootstrapping GOF functions ----

#function to get number of new releases for each group*occasion

Marked<-function(data=dipper,n.occasions=7,groups=Groups)
{
  group<-data[,2]
  marked<-matrix(nrow=length(Groups),ncol=n.occasions)
  for(g in 1:length(groups))
  {
    data_<-subset(data,group==groups[g])
    data_
    ch<-data_$ch
    for(i in 1:n.occasions)
    {
      ch1<-ch[(as.numeric(substr(ch,1,i)))==1]
      marked[g,i]<-length(ch1)
    }
  }
  return(marked)
}

#simulate CJS data for 1 group at a time

simul.cjs<-function(phi,p,marked, tsm = FALSE)
{
  n.occasions<-length(p)+1
  
  if(tsm == 0){
    Phi<-matrix(phi,n.occasions-1,nrow=sum(marked),byrow=T)
    P<-matrix(p,n.occasions-1,nrow=sum(marked),byrow=T)
  }
  
  if(tsm == 1){
    Phi <- matrix(0, ncol = n.occasions-1,nrow=sum(marked))
    for (i in 1:length(marked)){
      Phi[(sum(marked[1:i])-marked[i]+1):sum(marked[1:i]), i:(n.occasions-1)] <- matrix(rep(phi[c(i+n.occasions-2,seq(i, (n.occasions-2), length.out = n.occasions - i -1))], marked[i]),
                                                                                        ncol = n.occasions-i, byrow = TRUE)
    }
    
    P<-matrix(p,n.occasions-1,nrow=sum(marked),byrow=T)
  }
  
  
  
  #n.occasions<-dim(Phi)[2]+1
  CH<-matrix(0,ncol=n.occasions,nrow=sum(marked))
  #define a vector with marking occasion
  mark.occ<-rep(1:length(marked),marked[1:length(marked)])
  #fill in CH
  for (i in 1:sum(marked))
  {
    CH[i,mark.occ[i]]<-1
    if (mark.occ[i]==n.occasions) next
    for(t in (mark.occ[i]+1):n.occasions)
    {
      #survive?
      sur<-rbinom(1,1,Phi[i,t-1])
      if(sur==0) break #move to next
      #recaptured?
      rp<-rbinom(1,1,P[i,t-1])
      if(rp==1) CH[i,t]<-1
    } #t
  } #i
  return(CH)
}

###function to create capture history character strings

pasty<-function(x) 
{
  k<-ncol(x)
  n<-nrow(x)
  out<-array(dim=n)
  for (i in 1:n)
  {
    out[i]<-paste(x[i,],collapse="")
  }
  return(out)
}

