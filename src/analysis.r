library("parallel")
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

answer.f <- function() {
  return(list(complete = gm.search(table(rhc.dat), matrix(1, 10, 10) - diag(1, 10), forward=TRUE,
                                backward=TRUE, score="aic")
              , empty = gm.search (table(rhc.dat), matrix(0, 10, 10), forward=TRUE,
                                backward=TRUE, score="aic")))
}

# Computes the error rate for a number of combinations of nmin and minleaf parameters
search.params <- function(){
  ps_probs <- seq(0,1,0.25)
  ps_scores <- c("aic", "bic")

  ps_pars <- list()

  for(i in 1:length(ps_scores))
    for(j in 1:length(ps_probs))
      ps_pars[[length(ps_pars) + 1]] <- list(prob = ps_probs[j], score = ps_scores[i])

  ps_lbls <- rep("",length(ps_pars))

  e_ps <- eval_mthd(table(rhc.dat), ps_lbls, ps_pars, eval_with_pars)
  return(e_ps)
}

# Function: eval_to_df
#
# Arguments
#   eres : Result from the eval_mthd function.
#
#
# Result
#   A data frame representation of the result.
eval_to_df <- function(eres) {
  N <- length(eres$all)
  df <- data.frame(   scorefunction = rep(NA, N),
                      prob = rep(NA, N),
                      score = rep(NA, N))
  for(i in 1:(length(eres$all))) {
    pr <- eres$params[[i]]
    df[i, ] <- c(pr$score, pr$prob, eres$all[[i]]$model$model$best$score)
  }
  return(df)
}


# Function: eval_mthd
#
# Arguments
#   data : The data set on which to evaluate the parameters.
#   lbls : Vector containing the descriptions of the parameters.
#   vals : The parameter combinations to evaluate.
#   r    : The evaluation function.
#
#
# Result
#   A list containing:
#     all : A list containing the results of calling the `r` function.
eval_mthd <- function(data, lbls, vals, r) {
  f <- function(lbl, v) list(par_lbl = lbl, model =r(data, v))
  all <- (mcmapply(f, lbls, vals, SIMPLIFY = FALSE, mc.cores = detectCores()))
  return (list(all = all, lbls = lbls, params = vals))
}

# Function: eval_with_pars
#
# Arguments:
#   data: The dataset on which to evaluate the algorithm.
#   par : A list of the parameters to use.
#
# Result
# A list of
#   model : the fitted model.
#   score : the score of the fitted model.
#
# Evaluates the tree classification algorithm.
eval_with_pars <- function(data, par) {
  # just use the aic/bic score as result. We don't compare AIC with BIC models, so that is okay to do.
  model <- gm.restart(20, par$prob, 0, data, forward=TRUE, backward=TRUE, par$score)
  return(list(model=model, score=model$score))
}

