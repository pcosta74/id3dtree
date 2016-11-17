require(data.tree)

source('dataset.R')
source('tools.R')

id3.train <- function(form, data) {
  attr.cols <- all.vars(form)[-1]
  class.col <- all.vars(form)[1]
  class.lvl <- levels(as.factor(data[,class.col]))
  
  tree <- Node$new()
  tree <- build.tree(tree, data, attr.cols, class.col, class.lvl)
  
  # Extend with custom methods
  tree$isAttrNode <- function(self) { is.null(tree$split) }
    
  return(tree)
}

id3.predict <- function(tree, ds) {
  while(!tree$isLeaf) {
    tree$Climb(tree$children[1])
  }
}


build.tree <- function(tree, ds, attr.cols, class.col, class.lvl) {
  class.cnt <- table(ds[class.col], dnn=class.lvl)
  
  node     <- .find.best.split(ds, class.col, attr.cols, FUN=gain.ratio)
  n.name   <- names(node)
  new.node <- .add.tree.node(tree, n.name, class.cnt)

  values <- levels(as.factor(ds[,n.name]))
  new.attr.cols <- attr.cols[attr.cols != n.name]
  for(v in values) {
    new.ds <- ds[ds[n.name] == v,]
    new.val <- .add.node.val(new.node, v)
    
    if(length(as.list(unique(new.ds[class.col]))[[1]]) > 1) {
      build.tree(new.val, new.ds, new.attr.cols, class.col, class.lvl)
    } else {
      class.cnt <- table(new.ds[class.col], dnn=class.lvl)
      .add.tree.leaf(new.val, new.ds, class.col, class.cnt)
    }
  }

  return(tree)
}

.find.best.split <- function(ds, class.col, attr.cols,FUN=gain) {
  gains <- sapply(attr.cols, function(a,ds,cc) FUN(ds,a,cc), ds, class.col)
  names(gains) <- attr.cols
  return(which.max(gains))
}

.add.tree.node <- function(p.node, name, class.cnt) {
  # Create node label
  split <- paste(names(class.cnt), class.cnt, sep=":", collapse="/")
  label <- paste(name, split, sep="\n")
  
  # Create node
  if(p.node$isRoot) {
    node <- p.node$Set(name=name)
    SetEdgeStyle(node, arrowhead = "none")
  }
  else {
    node <- p.node$AddChild(name)
  }
  node$split <- split
  node$Set(isNode=TRUE)
  
  # Style node
  SetNodeStyle(node, shape = "box", style = "filled", fillcolor = "GreenYellow", label=label)
  
  return(node)
}

.add.tree.leaf <- function(p.node, ds, class.col, class.cnt) {
  # Create leaf label
  name  <- ds[1, class.col]
  label <- paste(name, class.cnt[name], sep="\n")
  
  # Create leaf
  leaf <- p.node$AddChild(name)
  
  # Style leaf
  SetNodeStyle(leaf, shape = "none", style="clear", label=label)
  
  return(leaf)
}

.add.node.val <- function(p.node, val) {
  # Create node
  node <- p.node$AddChild(val)
  node$Set(isValue=TRUE)
  
  # Style node
  SetNodeStyle(node, shape = "oval", style = "filled", fillcolor = "LightBlue")
  
  return(node)
}