# Wed Mar 23 16:32:48 2022 ------------------------------

# just playing around with removals on capture

library(RMark)

n.occs <- 10
n.groups <- 1


marked <- matrix(rep(500, n.occs - 1), nrow = n.groups, byrow = TRUE)
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


## this chunk for a model with just time
mydata <- data.frame(ch = pasty(simul.cjs(phi,p,marked)))

data.proc <- process.data(mydata,model="CJS") 

data.ddl <- make.design.data(data.proc)
mymodel <- mark(data.proc, data.ddl, 
                model.parameters = list(Phi = list(formula = ~time), p = list(formula = ~time)))

plot(1:9, mymodel$results$real[1:9,1],
     ylim = c(0.75,1),
     xlab = 'Time',
     ylab = expression(phi))

ratios <- c(0.01, 0.05, 0.1, 0.25, 0.5)

for(perc in ratios){
  
  data.proc$freq[] <- 1
  data.proc$freq[sample(1:length(data.proc$freq), size = floor(perc*length(data.proc$freq)), replace = FALSE)] <- -1
  
  data.ddl <- make.design.data(data.proc)
  mymodel <- mark(data.proc, data.ddl, 
                  model.parameters = list(Phi = list(formula = ~time), p = list(formula = ~time)))
  
  points(1:9, mymodel$results$real[1:9,1], add=TRUE, pch = which(ratios %in% perc)+1)
  }

legend("top", legend = c(0,ratios), pch = 1:6, ncol = 2)
abline(h = 0.8, lty = 3)

