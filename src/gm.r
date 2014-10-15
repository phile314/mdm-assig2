
gm.restart <- function(nstart, prob, seed, obs, forward, backward, score) {
  set.seed(seed)
  best <- NULL
  for (i in (1:nstart)) {
    # generate random graph
    gr <- gm.randGraph(prob, length(dim(obs)))

    res <- gm.search(obs, gr, forward, backward, score)

    # is smaller score better?
    if(best == NULL || best$score > res$score) {
      best <- res
    }
  }

  # how do we return the "call" (requested by Ad)?

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

}

gm.randGraph <- function(prob, nNodes) {
  m <- matrix(0,nNodes,nNodes)
  # TODO update graph
  # TODO explain why we are setting the diagonal to zero
  # (probably because it would mean that a node is independent
  #  from itself, which doesn't sound sensible)
  for (i in (1:nNodes)) {
    s <- sample((0:1), (i - 1), c(prob - 1, prob))
    # fill upper half, set everything else to zero
    m[i,] <- c(s, rep(0, nNodes - i + 1)
  }
  for (i in (1:nNodes)) {
    m[i,] <- c(m[i,(1:i)], m
  }

  return(m)
}
