# Entropy(S)
entropy <- function(ds, class.col=ncol(ds)) {
  n <- nrow(ds)
  
  if(n == 0) {
    return(0)
  } 

  f <- as.data.frame(ds)[class.col]
  l <- as.list(table(f)/n)
  s <- sum(sapply(l, function(p) {ifelse(p>0,-(p)*log(p,2),0)} ))
  
  return(s)
}

# Gain(S,A)
gain <- function(ds, attr.col, class.col=ncol(ds)) {
  n <- nrow(ds)

  if(n == 0) {
    return(0)
  } 

  e <- entropy(ds, class.col)
  f <- as.data.frame(ds)[attr.col]
  l <- as.list(table(f)/n)
  g <- sapply(names(l), 
              function(p,l,a,c) { l[[p]] * entropy(ds[ds[a]==p,],c) }, 
              l, attr.col, class.col)
  s <- sum(g)

  return(e - s)
}

# SplitInInformation(G,A)
split.in.info <- function(ds, attr.col) {
  n <- nrow(ds)
  
  if(n == 0) {
    return(0)
  }
  
  f <- as.data.frame(ds)[attr.col]
  l <- as.list(table(f)/n)
  s <- sum(sapply(l, function(p,n) { -(p) * log(p,2) }, n))

  
  return(s)
}

gain.ratio <- function(ds, attr.col, class.col=ncol(ds)) {
  ig  <- gain(ds, attr.col, class.col)
  sii <- split.in.info(ds, attr.col)
  return (ig/sii)
}