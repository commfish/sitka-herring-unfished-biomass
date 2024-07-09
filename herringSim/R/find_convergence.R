#' Find Series convergence
#'
#' This function decides when a series converges using two user-specified
#' criteria. Convergence is declared when the window root mean squared deviation
#' fall below some limiting value for n consecutive elements in the series.
#'
#' @param x A numeric vector
#' @param n A positive integer; defines both window size and how many window RMSD's
#' must fall below the limit for the convergence criteria to be considered satisfied
#' @param limit A positive numeric; the threshold value under which RMSD must fall
#' in order to satisfy the convergence criteria
#' @return The index of `x` after which the convergence criteria is met
#'
#' @export


find_convergence <- function(x, n = 100, limit = .05){

  # sanity check
  if(length(x) < n){
    warning("x less than n")
    return(FALSE)
  }

  # divide x in to n windows
  windows <- vector(mode = "list", length = length(x)-n)
  for(i in 1:length(windows)){
    windows[[i]] <- x[i:(i+n-1)]
    if(i+n >= length(x)) break
  }

  # root mean squared deviation utility function
  rmsd <- function(vec){
    return( sqrt( sum( (vec-mean(vec))^2 ) / (length(vec)-1) ) )
  }

  # compute RMSD's
  windowRmsd <- sapply(windows, FUN = rmsd)

  # find when convergence criteria is first met for n consecutive elements
  for(i in 1:length(windowRmsd)){
    if(i < n){
      next
    } else if(i >= n){
      if(all(windowRmsd[(i-n):i] / max(windowRmsd) < limit)){
        out <- i-n
        return(out)
      } else if(!all(windowRmsd[(i-n):i] / max(windowRmsd) < limit)){
        next
      }
    }
  }
  warning("convergence criteria not met: try adjusting n or limit")
  return(FALSE)
}

