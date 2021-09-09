# File:         demo.r 
# Description:  Naive demo-solution given at classroom session
#               for Project 1, Artificial Intelligence 2019, UU
# Author:       Fredrik Nilsson

# Install the package
# install.packages("DeliveryMan_1.1.0.tar.gz", repos = NULL, type="source")

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
    #print(goal)
  } else {
    goal <- packageMatrix[carInfo$load, c(3,4)]
  }
 # print(cat("goal when born", goal))
  # How do we get there?
  carInfo$nextMove <- nextMove(trafficMatrix,
                               carInfo,
                               packageMatrix, 
                               goal)
  #print("------------------STEP----------------")
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
  #print("nextMove")
  road_con <- (mean(trafficMatrix$vroads) + mean(trafficMatrix$hroads))/2
  road_min <- min(c(min(trafficMatrix$vroads), min(trafficMatrix$hroads)))
  first_front <- list(x=carInfo$x, y=carInfo$y, f=0, h=man_dist(c(carInfo$x,carInfo$y), goal), p=c())
  frontier <- list(first_front)
  expand <- list()
  
  if (carInfo$x == goal[1] & carInfo$y == goal[2]){
    # print("same spot!!")
    return(5)
  }
  
  
  while (length(frontier) != 0){
    print(cat('lenght frontier: ',length(frontier)))
    # if (length(frontier)>1000){
    #   print(cat("carinfo: ", c(carInfo$x,carInfo$y)))
    #   print(cat("goal: ", goal))
    #   print(cat("expanded coordinats:", c(expand$x,expand$y)))
    # }
    # print("nextMove while")
    path_vals = sapply(frontier, function(i) i[[3]]+i[[4]])
    #best_index = which.min(path_vals) # find index of best frontier
    #best_index = rev(which(path_vals == min(path_vals)))[1]
    heu_vals = sapply(frontier, function(i) i[[4]])
    #print(heu_vals)
    #print(which.min(heu_vals))
    #print(path_vals)
    best_path_vals_indices = which(path_vals == min(path_vals))
    #print(best_path_vals)
    comp_heu_val = heu_vals[best_path_vals_indices]
    best_index_compr = which.min(heu_vals[which(path_vals == min(path_vals))])
    best_index = best_path_vals_indices[best_index_compr]
    
    expand = frontier[[best_index]]
    frontier = frontier[-best_index] # Pop best frontier
    #print("expand:")
    #print(expand)
    #print("frontier after sapply:")
    #print(frontier)
    
    if (expand$x == goal[1] & expand$y == goal[2]) {
      # print(cat("goal when return:", goal))
      # print(c(expand$x, expand$y))
      # print(cat("returned value:", expand$p[1]))
      return(expand$p[1])
    } else {
      # print("vroads:")
      # print(trafficMatrix$vroads)
      if (ncol(trafficMatrix$vroads) >= expand$y){
         #print("up")
        # print(cat("nrows vroads:", nrow(trafficMatrix$vroads)))
        # print(cat("x an y values for vroads", expand$x, expand$y))
        up <- list(x=expand$x, y=expand$y+1, f=trafficMatrix$vroads[expand$x,expand$y]+expand$f, 
                   h=man_dist(c(expand$x,expand$y+1), goal)*road_con, p=append(expand$p, 8))
        if (length(frontier) != 0) {
          added = 0
          for (i in 1:length(frontier)) {
            if (up$x == frontier[[i]]$x & up$y == frontier[[i]]$y) {
              if (up$f+up$h < frontier[[i]]$f+frontier[[i]]$h) {
                frontier[[i]] = up
                added = 1
               # print('replace up')
                break
              }
            } 
          } 
          if (added == 0) {
            frontier = append(frontier, list(up))
            #print('append up1')
          }
        } else {
          frontier = append(frontier, list(up))
          #print('append up2')
        }
        #frontier = append(frontier, list(up))
      }
      
      if (expand$y != 1) {
        #print("down")
        down <- list(x=expand$x, y=expand$y-1, f=trafficMatrix$vroads[expand$x,expand$y-1]+expand$f, 
                     h=man_dist(c(expand$x,expand$y-1), goal)*road_con, p=append(expand$p, 2))
        if (length(frontier) != 0) {
          added = 0
          for (i in 1:length(frontier)) {
            if (down$x == frontier[[i]]$x & down$y == frontier[[i]]$y) {
              if (down$f+down$h < frontier[[i]]$f+frontier[[i]]$h) {
                frontier[[i]] = down
                added = 1
                #print('replace down')
                break
              }
            }
          }
          if (added == 0) {
            frontier = append(frontier, list(down))
            #print('append down1')
          }
        } else {
          frontier = append(frontier, list(down))
          #print('append down2')
        }
          
        #frontier = append(frontier, list(down))
      }
      
      if (expand$x != 1) {
        #print("left")
        left <- list(x=expand$x-1, y=expand$y, f=trafficMatrix$hroads[expand$x-1,expand$y]+expand$f, 
                     h=man_dist(c(expand$x-1,expand$y), goal)*road_con, p=append(expand$p, 4))
        if (length(frontier) != 0) {
          added = 0
          for (i in 1:length(frontier)) {
            if (left$x == frontier[[i]]$x & left$y == frontier[[i]]$y) {
              if (left$f+left$h < frontier[[i]]$f+frontier[[i]]$h) {
                frontier[[i]] = left
                added = 1
                #print('replace left')
                break
              }
            }
          }
          if (added == 0) {
            frontier = append(frontier, list(left))
            #print('append left1')
          }
        } else {
          frontier = append(frontier, list(left))
          #print('append left2')
        }
        #frontier = append(frontier, list(left))
      }
      
      if (nrow(trafficMatrix$hroads) >= expand$x) {
        #print("right")
        right <- list(x=expand$x+1, y=expand$y, f=trafficMatrix$hroads[expand$x,expand$y]+expand$f, 
                      h=man_dist(c(expand$x+1,expand$y), goal)*road_con, p=append(expand$p, 6))
        if (length(frontier) != 0) {
          added = 0
          for (i in 1:length(frontier)) {
            if (right$x == frontier[[i]]$x & right$y == frontier[[i]]$y) {
              if (right$f+right$h < frontier[[i]]$f+frontier[[i]]$h) {
                frontier[[i]] = right
                added = 1
                #print('replace right')
                break
              }
            } 
          }
          if (added == 0) {
            frontier = append(frontier, list(up))
            #print('append right1')
          }
        } else {
          frontier = append(frontier, list(right))
          #print('append right2')
        }
        #frontier = append(frontier, list(right))
      }
      # print("frontier after added directions:")
      # print(frontier)
    }
  }
  #while fronties is not empty
  # move firt element i frontier to expand
  # check if expand is goal. 
  # if not: for each neighbour calculate cost of path and insert to frontier (i ordning)
  
  # if(carInfo$x < carInfo$mem$goal[1]) {
  #   return(6)
  # } else if (carInfo$x > carInfo$mem$goal[1]) {
  #   return(4)
  # } else if (carInfo$y < carInfo$mem$goal[2]) {
  #   return(8)
  # } else if (carInfo$y > carInfo$mem$goal[2]) {
  #   return(2)
  # } else {
  #   return(5)
  # }
}