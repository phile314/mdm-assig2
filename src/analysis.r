source("gm.r")

rhc.dat <- read.csv('../data/rhc-small.txt')

clique.table <- function(graphmodel){
  fmt <- function(set) paste0("{", paste(lapply(set, paste), collapse=", "), "}")
  cliques <- unique(lapply(graphmodel$cliques, sort))
  formatted <- lapply(cliques, fmt)
  clique.df <- as.data.frame(I(formatted))
  colnames(clique.df) <- "cliques"
  return(clique.df)
}

# Returns an igraph object from the list of its cliques
from.cliques <- function(cliques){
  mkFullGraph <- function(clique){
    g <- graph.full(length(clique))
    V(g)$name <- paste(clique)
    return(g)
  }
  graphs <- lapply(cliques, mkFullGraph)
  return(graph.union(graphs))
}

answer.c <- function() gm.search(table(rhc.dat), matrix(0, 10, 10), forward=TRUE,
                                 backward=TRUE, score="bic")

answer.e <- function() gm.search(table(rhc.dat), matrix(1, 10, 10) - diag(1, 10),
                                 forward=TRUE, backward=TRUE, score="bic")
