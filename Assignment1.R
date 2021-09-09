
# Load the library
library("DeliveryMan")

# Read documentation
# ?runDeliveryMan
# ?testDM

myFunction <- function(trafficMatrix, carInfo, packageMatrix) {
  # What is our goal?
  if(carInfo$load == 0) {
    goal <- nextPickup(trafficMatrix, 
                                   carInfo, 
                                   packageMatrix)

  } else {
    goal <- packageMatrix[carInfo$load, c(3,4)]
  }
  # How do we get there?
  carInfo$nextMove <- nextMove(trafficMatrix,
                               carInfo,
                               packageMatrix, 
                               goal)
  return(carInfo)
}

# Find the nearest pickup location for an undelivered package
nextPickup <- function(trafficMatrix, carInfo, packageMatrix) {
  distanceVector = abs((packageMatrix[,1] - carInfo$x)) +
                    abs((packageMatrix[,2] - carInfo$y))
  distanceVector[packageMatrix[,5] != 0] = Inf
  return(packageMatrix[which.min(distanceVector), c(1,2)])
}

man_dist <- function(car_pos, goal_pos) {
  dis = abs((car_pos[[1]]-goal_pos[[1]])) + abs((car_pos[[2]]-goal_pos[[2]]))
  return(dis)
}

# Find the move to get to carInfo$mem$goal
nextMove <- function(trafficMatrix, carInfo, packageMatrix, goal) {

  road_con <- (mean(trafficMatrix$vroads) + mean(trafficMatrix$hroads))/2
  grad <- 1
  first_front <- list(x=carInfo$x, y=carInfo$y, f=0, h=man_dist(c(carInfo$x,carInfo$y), goal), p=c())
  frontier <- list(first_front)
  expand <- list()
  
  if (carInfo$x == goal[1] & carInfo$y == goal[2]){
    return(5)
  }
  
  
  while (length(frontier) != 0){
    
    path_vals = sapply(frontier, function(i) i[[3]]+i[[4]])
    best_index = rev(which(path_vals == min(path_vals)))[1] # reversed algorithm. Best so far.
          
    expand = frontier[[best_index]]
    frontier = frontier[-best_index] # Pop best frontier
    
    if (expand$x == goal[1] & expand$y == goal[2]) {
      return(expand$p[1])
    } else {
      if (ncol(trafficMatrix$vroads) >= expand$y){
        up <- list(x=expand$x, y=expand$y+1, f=trafficMatrix$vroads[expand$x,expand$y]+expand$f, 
                   h=man_dist(c(expand$x,expand$y+1), goal)*road_con*grad, p=append(expand$p, 8))
        frontier = append(frontier, list(up))
      }
      
      if (expand$y != 1) {
        down <- list(x=expand$x, y=expand$y-1, f=trafficMatrix$vroads[expand$x,expand$y-1]+expand$f, 
                     h=man_dist(c(expand$x,expand$y-1), goal)*road_con*grad, p=append(expand$p, 2))
        frontier = append(frontier, list(down))
      }
      
      if (expand$x != 1) {
        left <- list(x=expand$x-1, y=expand$y, f=trafficMatrix$hroads[expand$x-1,expand$y]+expand$f, 
                     h=man_dist(c(expand$x-1,expand$y), goal)*road_con*grad, p=append(expand$p, 4))
        frontier = append(frontier, list(left))
      }
      
      if (nrow(trafficMatrix$hroads) >= expand$x) {
        right <- list(x=expand$x+1, y=expand$y, f=trafficMatrix$hroads[expand$x,expand$y]+expand$f, 
                      h=man_dist(c(expand$x+1,expand$y), goal)*road_con*grad, p=append(expand$p, 6))
        frontier = append(frontier, list(right))
      }
    }
  }
}