myFunction=function(moveInfo,readings,positions,edges,probs) {
  # Calculate probabilities for a croc to be in each of the node
  crocProbs <- getCrocProbabilities(readings, probs)
  
  # Pick the most likely
  mostLikely <- which.max(crocProbs)
  
  # Rush towards that node
  step1 <- moveOneStep(positions[3], mostLikely, edges)
  if (step1 == mostLikely){
    step2 = 0
  }
  else{
    step2 <- moveOneStep(step1, mostLikely, edges)
  }
  
  moveInfo$moves=c(step1, step2)
  return(moveInfo)
}

getCrocProbabilities <- function(readings, probs) {
  likelihood <- numeric(40)
  
  for (i in 1:40) {
    salinity <- dnorm(readings[1], probs$salinity[i, 1], probs$salinity[i, 2])
    phosphate <- dnorm(readings[2], probs$phosphate[i, 1], probs$phosphate[i, 2])
    nitrogen <- dnorm(readings[3], probs$nitrogen[i, 1], probs$nitrogen[i, 2])
    
    likelihood[i] <- salinity * phosphate * nitrogen
  }
  
  likelihood / sum(likelihood)
}

# Calculates shortest path and returns the next node that should be taken.
#' @param currentNode The current node of the player.
#' @param goalNode The node that you want to reach.
#' @param edges The edges of the map
moveOneStep <- function(currentNode, goalNode, edges) {
  if (currentNode == goalNode) return(currentNode)
  
  visited <- rep(FALSE, 40)
  previous <- rep(NA_integer_, 40)
  queue <- currentNode
  visited[currentNode] <- TRUE
  
  while (length(queue) > 0) {
    node <- queue[1]
    queue <- queue[-1]
    
    options <- getOptions(node, edges)
    
    for (nextNode in options) {
      if (!visited[nextNode]) {
        visited[nextNode] <- TRUE
        previous[nextNode] <- node
        
        if (nextNode == goalNode) {
          step <- goalNode
          
          while (previous[step] != currentNode) {
            step <- previous[step]
          }
          
          return(step)
        }
        
        queue <- c(queue, nextNode)
      }
    }
  }
  
  return(NA_integer_)
}