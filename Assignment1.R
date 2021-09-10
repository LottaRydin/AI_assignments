
# Load the library
library("DeliveryMan")

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

# Function for calculating the manhattan distance
man_dist <- function(car_pos, goal_pos) {
  dis = abs((car_pos[[1]]-goal_pos[[1]])) + abs((car_pos[[2]]-goal_pos[[2]]))
  return(dis)
}

# Find the move to get to goal
nextMove <- function(trafficMatrix, carInfo, packageMatrix, goal) {

  road_con <- min(c(min(trafficMatrix$vroads), min(trafficMatrix$hroads))) #variable for storing traffic condition
  first_front <- list(x=carInfo$x, y=carInfo$y, f=0, h=man_dist(c(carInfo$x,carInfo$y), goal), p=c())
  frontier <- list(first_front)
  expand <- list()
  
  # If deliveryMan start on goal, it should stay for one move
  if (carInfo$x == goal[1] & carInfo$y == goal[2]){
    return(5)
  }
  
  while (length(frontier) != 0){
    # Find index for the best node in frontier, move to expand
    path_vals = sapply(frontier, function(i) i[[3]]+i[[4]])
    best_index = rev(which(path_vals == min(path_vals)))[1]
    expand = frontier[[best_index]]
    frontier = frontier[-best_index] # Pop best frontier
    
    # Return first move when the best path is found
    if (expand$x == goal[1] & expand$y == goal[2]) {
      return(expand$p[1])
      
    } else {
      # Find all neighboring nodes
      add_front = list()
      if (ncol(trafficMatrix$vroads) >= expand$y){
        up <- list(x=expand$x, y=expand$y+1, f=trafficMatrix$vroads[expand$x,expand$y]+expand$f, 
                   h=man_dist(c(expand$x,expand$y+1), goal)*road_con, p=append(expand$p, 8))
        add_front = append(add_front, list(up))
      }
      
      if (expand$y != 1) {
        down <- list(x=expand$x, y=expand$y-1, f=trafficMatrix$vroads[expand$x,expand$y-1]+expand$f, 
                     h=man_dist(c(expand$x,expand$y-1), goal)*road_con, p=append(expand$p, 2))
        add_front = append(add_front, list(down))
      }
      
      if (expand$x != 1) {
        left <- list(x=expand$x-1, y=expand$y, f=trafficMatrix$hroads[expand$x-1,expand$y]+expand$f, 
                     h=man_dist(c(expand$x-1,expand$y), goal)*road_con, p=append(expand$p, 4))
        add_front = append(add_front, list(left))
      }
      
      if (nrow(trafficMatrix$hroads) >= expand$x) {
        right <- list(x=expand$x+1, y=expand$y, f=trafficMatrix$hroads[expand$x,expand$y]+expand$f, 
                      h=man_dist(c(expand$x+1,expand$y), goal)*road_con, p=append(expand$p, 6))
        add_front = append(add_front, list(right))
      }
      # Check if node exists in frontier, replace it if current is better
      for (i in 1:length(add_front)){
        if (length(frontier) != 0){
          ex = FALSE
          for (j in 1:length(frontier)){
            if (add_front[[i]]$x == frontier[[j]]$x & add_front[[i]]$y == frontier[[j]]$y){
              ex = TRUE
              if ((add_front[[i]]$h + add_front[[i]]$f) < (frontier[[j]]$h + frontier[[j]]$f)){
                frontier[[j]] = add_front[[i]]
              }
            break
            }
          }
          if (!ex){
            frontier = append(frontier, list(add_front[[i]]))
          }
        } else {
          frontier = append(frontier, list(add_front[[i]]))
        }
      }
    }
  }
}