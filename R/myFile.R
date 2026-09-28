createTransitionMatrix <- function(edges) {
  transitionMatrix <- matrix(0, nrow = 40, ncol = 40)
  # distribute all the possible transition probabilites for all the possible waterholes to all possibilities
  for (currentPosition in 1:40) {
    possibleNextPositions <- getOptions(currentPosition, edges)
    moveProbability <- 1 / length(possibleNextPositions)

    transitionMatrix[currentPosition, possibleNextPositions] <- moveProbability
  }

  return(transitionMatrix)
}

myFunction=function(moveInfo,readings,positions,edges,probs) {
  # The map does not change, so create and store its transition matrix once.
  if (is.null(moveInfo$mem$transitionMatrix)) {
    moveInfo$mem$transitionMatrix <- createTransitionMatrix(edges)
  }

  # Calculate probabilities for Croc's location from the current readings.
  readingProbs <- getCrocProbabilities(readings, probs, positions)

  # Start a fresh belief on the first turn of each game. The game sets status
  # to 0 for the first game and to 1 when memory is passed to a new game.
  newGame <- is.null(moveInfo$mem$belief) || moveInfo$mem$status %in% c(0, 1)

  if (newGame) {
    crocProbs <- readingProbs
    moveInfo$mem$status <- 2
  } else {
    # predict where croc could move given where he is with the transition matrix
    crocProbs <- as.numeric(
      moveInfo$mem$belief %*% moveInfo$mem$transitionMatrix
    )
    # apply the new sensor readings 
    crocProbs <- crocProbs * readingProbs
    crocProbs <- crocProbs / sum(crocProbs)
  }

  moveInfo$mem$belief <- crocProbs
  
  # Pick the most likely
  mostLikely <- which.max(crocProbs)
  
  # Rush towards that node
  step1 <- moveOneStep(positions[3], mostLikely, edges) 
  # ↑↑ no need to check if we already are on the right pos
  # it can return the current pos and we can search on the next move
  if (step1 == mostLikely){
    step2 = 0
  }
  else{
    step2 <- moveOneStep(step1, mostLikely, edges)
  }
  
  moveInfo$moves=c(step1, step2)
  return(moveInfo)
}

getCrocProbabilities <- function(readings, probs, positions) {
  likelihood <- numeric(40)

  # check for tourists, if they got eaten we know where they are
  if (!is.na(positions[1]) && positions[1] < 0) {
    likelihood[abs(positions[1])] <- 1
    return(likelihood)
  } else if (!is.na(positions[2]) && positions[2] < 0) {
    likelihood[abs(positions[2])] <- 1
    return(likelihood)
  }
  # report the sensor readings into a vector
  for (i in 1:40) {
  salinity <- dnorm(readings[1], probs$salinity[i, 1], probs$salinity[i, 2])
  phosphate <- dnorm(readings[2], probs$phosphate[i, 1], probs$phosphate[i, 2])
  nitrogen <- dnorm(readings[3], probs$nitrogen[i, 1], probs$nitrogen[i, 2])
  
  likelihood[i] <- salinity * phosphate * nitrogen
}

return(likelihood / sum(likelihood))

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
