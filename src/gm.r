
gm.restart <- function(nstart, prob, seed, obs, forward, backward, score) {
  set.seed(seed)
  best <- NULL
  for (i in (1:nstart)) {
    # generate random graph
    gr <- gm.randGraph(prob)

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

gm.randGraph <- function(prob) {

}
