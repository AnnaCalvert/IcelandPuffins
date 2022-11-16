# Sat Mar 19 09:49:53 2022 ------------------------------

# attempt to set up a general bootstrapping procedure based solely on an existing model

### first - step up a dummy ch-array with known rates

### use Kery and Shaub approach

n.occs <- 10
n.groups <- 3

marked <- matrix(c(rep(50, n.occs - 1),
                   rep(40, n.occs - 1), 
                   rep(45, n.occs - 1)),
                 nrow = n.groups, byrow = TRUE)
phi <- matrix(c(rep(0.65, n.occs - 1),
                rep(0.6, n.occs - 1),
                rep(0.8, n.occs - 1)), 
              nrow = n.groups, byrow = TRUE)
p <- matrix(c(rep(0.5, n.occs - 1),
              rep(0.4, n.occs - 1),
              rep(0.6, n.occs - 1)), 
            nrow = n.groups, byrow = TRUE)

marked <- matrix(rep(50, n.occs - 1), nrow = n.groups, byrow = TRUE)
phi <- matrix(rep(0.8, n.occs - 1), nrow = n.groups, byrow = TRUE)
p <- matrix(rep(0.8, n.occs - 1), nrow = n.groups, byrow = TRUE)


simul.cjs<-function(phi,p,marked) {
  
  n.occasions <- dim(marked)[2] +1
  n.groups <- dim(marked)[1]
  
  if(n.groups == 1){ 
    Phi <- matrix(phi,n.occasions-1,nrow=sum(marked),byrow=TRUE)
    P <- matrix(p,n.occasions-1,nrow=sum(marked),byrow=TRUE)
  } else {
    Phi <- phi[rep(1:nrow(phi), times = rowSums(marked)), ]
    P <- p[rep(1:nrow(p), times = rowSums(marked)), ]
  }
  
  CH<-matrix(0,ncol=n.occasions,nrow=sum(marked))
  #define a vector with marking occasion
  mark.occ<-rep(rep(1:dim(marked)[2], n.groups), times = as.vector(t(marked)))
  
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


## this chunk for a model with group
mydata <- data.frame(ch = pasty(simul.cjs(phi,p,marked)), group = rep(1:n.groups, times = rowSums(marked)))
data.proc <- process.data(mydata,model="CJS", groups = "group") 
data.ddl <- make.design.data(data.proc)
mymodel <- mark(data.proc, data.ddl, 
                model.parameters = list(Phi = list(formula = ~time*group), p = list(formula = ~time*group)))

# add this in for tsm
data.ddl <- add.design.data(data.proc, data.ddl,
                            parameter="Phi", type="age", bins=c(0,1, 17),name="tsm",
                            right = FALSE, replace = TRUE)
mymodel <- mark(data.proc, data.ddl, 
                model.parameters = list(Phi = list(formula = ~tsm+time*group), p = list(formula = ~time*group)))


## this chunk for a model with just time
mydata <- data.frame(ch = pasty(simul.cjs(phi,p,marked)))
data.proc <- process.data(mydata,model="CJS") 
data.ddl <- make.design.data(data.proc)
mymodel <- mark(data.proc, data.ddl, 
                model.parameters = list(Phi = list(formula = ~time), p = list(formula = ~time)))


