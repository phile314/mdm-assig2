source("cliques.r")

# gm.restart(nstart, prob, seed, counts, forward, backward, score)
# Learns an undirected graphical model starting from multiple random graphs.
#
# Arguments
#   nstart  : The number of restarts.
#   prob    : Probablity that an edge is created.
#   seed    : The seed for the random generator.
#   ...     : See documentation of gm.search
#
# Result
# A list containing the following named components:
#   best  : The best found model.
#   call  : The call to the function gm.restart that produced this result.
gm.restart <- function(nstart, prob, seed, counts, forward, backward, score) {
  set.seed(seed)
  best <- NULL
  for (i in (1:nstart)) {
    # generate random graph
    gr <- gm.randGraph(prob, length(dim(counts)))

    res <- gm.search(counts, gr, forward, backward, score)

    # is smaller score better?
    if(is.null(best) || best$score > res$score) {
      best <- res
    }
  }

  return(list(call = match.call(), best = best))
}

# gm.search(counts, graph.init, forward, backward, score)
# Learns an undirected graphical model from data.
#
# Arguments
#   counts : A contingency table representing the observed counts
#   grap.init : The adjacency matrix that represents the initial graph, from which the search starts
#   forward : A logical value that indicates whether adding edges during the search is allowed
#   backward : A logical value that indicates whether removing edges during the search is allowed
#   score : A string, either "aic" or "bic", that determines which score function is used
#
# Result
# A list containing the following named components:
#   model : A list containing the cliques of the final model. Each clique is a
#           vector containing the column numbers of the variables in the clique.
#   score : The AIC or BIC score of the final model.
#   trace : A data frame providing a trace of the search process. Row i of the data
#           frame provides relevant information on step i of the search.
#   call  : The call to the function gm.search that produced this result.
#
gm.search <- function(counts, graph.init, forward, backward, score) {
  stopifnot(forward || backward) # TODO is it allowed to call this function with both forward and backward set to FALSE?
  this.call <- call("gm.search", counts, graph.init, forward, backward, score)
  trace <- data.frame()
  score.f <- switch(score, aic = aic, bic = bic)
  score <- 0
  graph <- graph.init
  trainAndScore <- function(counts) {
    function(g){
      cliques <- find.cliques(c(), seq(nrow(g)), c(), g, c())
      model <- loglin(counts, cliques, fit = T)
      score <- score.f(model)
      return(list(fitted = model, score = score, cliques = cliques, graph = g))
    }
  }

  repeat{
    all.neighbors <- graph.neighbors(graph)
    added <- if (forward) all.neighbors$added else list()
    removed <- if (backward) all.neighbors$removed else list()
    neighbors <- c(added, removed)
    fitted.models <- lapply(neighbors, trainAndScore(counts))
    model <- best.model(fitted.models)
    counts <- model$fitted$fit
    graph <- model$graph

    if(model$score >= score) # if the score doesn't improve : local optimum
      break
    else
      score <- model$score
  }

  return(list(cliques = model$cliques,  # TODO should the name be model or cliques?
              score = model$score,
              trace = trace,
              call  = match.call))
}

# Function best.model(models)
# Returns the model with the lowest score from the given list
#
# Arguments
#   models : A non-empty list of models containing the numerical field score
#
# Result
#   The best model
best.model <- function(models){
  stopifnot(length(models) != 0)
  best <- models[[1]]
  for(model in models){
    if (model$score <= best$score)
      best <- model
  }
  return(best)
}

# Function: aic(M)
# Computes the AIC score for a fitted log-linear model
#
# Arguments
#   M : A log-linear model fitted using loglin with the fit parameter set to TRUE
#
# Result
# A number that represents the AIC score
aic <- function(M) M$lrt + 2 * (length(M$fit) - M$df)

# Function: bic(M)
# Computes the BIC score for a fitted log-linear model
#
# Arguments
#   M : A log-linear model fitted using loglin with the fit parameter set to TRUE
#
# Result
# A number that represents the BIC score
bic <- function(M) M$lrt + log(sum(length(M$fit))) * (length(M$fit) - M$df)

# graph.neighbors(graph)
# Computes the neighbors graphs of the given undirected graph
#
# Arguments
#   graph : An binary symmetric matrix representing the adjacency matrix of the graph
#
# Result
# A list containing two named fields
#   added : A list containing the graphs obtained by the given graph adding an edge
#   removed : A list containing the graphs obtained by the given graph removing an edge
graph.neighbors <- function(graph){
  n <- nrow(graph)
  added <- list()
  removed <- list()
  for(i in seq_len(n))
    for(j in seq_len(i - 1))
      if (graph[i, j]){
        r <- graph
        r[i, j] <- r[j, i] <- 0
        removed <- c(removed, list(r))
      } else {
        a <- graph
        a[i, j] <- a[j, i] <- 1
        added <- c(added, list(a))
      }
  return(list(added = added, removed = removed))
}

# gm.randGraph(prob, nNodes)
# Generates a random graph represented as adjacency matrix.
# No edges from a vertex back to itself are created.
#
# Arguments
#   prob   : Probablity that an edge is created.
#   nNodes : The number of nodes in the graph.
#
# Result
# The adjacency matrix.
gm.randGraph <- function(prob, nNodes) {
  m <- matrix(0,nNodes,nNodes)
  # TODO update graph
  # TODO explain why we are setting the diagonal to zero
  # (probably because it would mean that a node is independent
  #  from itself, which doesn't sound sensible)
  for (i in (1:nNodes)) {
    s <- sample((0:1), (i - 1), prob = c(1 - prob, prob), replace = TRUE)
    # fill lower half, set everything else to zero
    m[i,] <- c(s,rep(0, nNodes - i + 1))
    # fill upper half
    if (i > 1) {
      m[(1:(i-1)),i] <- s
    }
  }

  return(m)
}
