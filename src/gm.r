
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

gm.search <- function() {

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
