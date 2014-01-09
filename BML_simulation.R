# Traffic Simulation Project #
# Traffic Simulation Code
# Ethan Yung
# Brenna Gomer

# as a side note on implementation efficiency: for/while loops were used, but only for the purposes of 
# initializing the traffic grid, and for running multiple steps. The main step function (taking one time step)
# is done as a vectorized (matrix) operation, thus our code runs fairly efficiently. 


### Initializing a Traffic Grid. ###

#represents the traffic grid as list of matrices.
gcd = function(u, v) {ifelse(u %% v != 0, gcd(v,(u%%v)), v)}
lcm = function(u, v) { abs(u*v)/gcd(u,v)}


random_cars=function(m, n, n_cars){
  cars=rep(c(1,2),each=floor(n_cars))
  empty=rep(0,times=(m*n-(2*n_cars)))
  random_placement = sample(c(cars,empty),size = m*n, replace = FALSE)
  return(random_placement)
}

make_car_list = function(m,n,p){
  # The output of make_car_list is a list consisting of 3 matrices. The first 2 will contain
  # rows of all the red/blue cars in the grid and list their (x,y) coordinates. 
  # The 3rd matrix will be a logical that indicates whether a given coordinate is occupied (TRUE) or not.
  n_cars = floor(m*n*p/2) # number of cars of each red and blue
  car_vector = random_cars(m,n,n_cars)
  if (all(car_vector == 0)){stop('no cars in grid.')}

  occupied = matrix(FALSE, nrow = m, ncol = n)
  red_x = numeric(n_cars) 
  red_y = numeric(n_cars)
  blue_x = numeric(n_cars)
  blue_y = numeric(n_cars)
  
  i = 1
  red_count = 1
  blue_count = 1
  
  while (i <= m * n){
    if (car_vector[i] == 1){
      x = (i-1) %% m + 1
      y = (i-1) %/% m + 1
      red_x[red_count] = x
      red_y[red_count] = y
      occupied[x, y] = TRUE
      red_count = red_count + 1
      
    }
    else if(car_vector[i] == 2){
      x = (i-1) %% m + 1
      y = (i-1) %/% m + 1
      blue_x[blue_count] = x 
      blue_y[blue_count] = y
      occupied[x, y] = TRUE
      blue_count = blue_count + 1
    }
    i = i + 1
  }
  reds = cbind(x = red_x, y = red_y)
  blues = cbind(x = blue_x, y = blue_y)
  return(list(reds = reds, blues = blues, occupied=occupied))
}


### Moving Cars, and Simulation ###
grid_index = function(x,m) {
  return((x - 1) %% m + 1)
}

move_cars2 = function(xy, other_xy, grid, color) { 
  # move_cars2 will move either all red or all blues cars in a given traffic grid.
  # Color should be 1 or 2, 
  #xy is the matrix of (x,y) of either blue or red car coordinates. xy_other is the other color.
  
  num_cars = nrow(xy)
  x = xy[,1]
  y = xy[,2]
  m = nrow(grid)
  n = ncol(grid)
  if (color == 1) {
    next_x = grid_index(xy[,1] + 1, m)
    next_xy = cbind(next_x, y)
    occupied = grid[next_xy] #occupied is mapped by index from next coordinates to the grid. It is a vector.
    x = ifelse(!occupied, next_x, x)
    
  } else if (color == 2) {
    next_y = grid_index(xy[,2] + 1, n)
    next_xy = cbind(x, next_y)
    occupied = grid[next_xy]
    y = ifelse(!occupied, next_y, y)
  }
  grid = matrix(FALSE, nrow = m, ncol = n)
  new_xy = cbind(x, y)
  grid[new_xy] = TRUE
  grid[other_xy] = TRUE
  return(list(new_xy, grid))
}

step = function(traffic_list){
  # Takes one time step, given a traffic_list representation of the grid.
  red_update = move_cars2(traffic_list$reds, 
                          traffic_list$blues,
                          traffic_list$occupied, 1)
  traffic_list$reds = red_update[[1]]
  traffic_list$occupied = red_update[[2]]  
  
  blue_update = move_cars2(traffic_list$blues, 
                           traffic_list$reds, 
                           traffic_list$occupied, 2)
  traffic_list$blues = blue_update[[1]]
  traffic_list$occupied = blue_update[[2]]
  
  return(traffic_list)
}


plot_traffic = function(traffic_list, n, sample_number, p, pch_size){
  #plots a graph of traffic.
  jpeg(paste('traffic',sample_number,'_',n,'_p',p*100,'.jpg', sep = ''))
  rows = nrow(traffic_list$occupied)
  cols = ncol(traffic_list$occupied)
  plot(x = traffic_list$blues[,'x'], y = traffic_list$blues[,'y'], col = 'blue', pch = 15, cex = pch_size,
       xlab = 'X', ylab = 'Y', 
       type = 'p', main = paste("Traffic Grid at Time Step ",n, " with Proportion ", p, sep = ''), 
       xlim = c(1,rows), ylim = c(1,cols))
  points(x = traffic_list$reds[,'x'], y = traffic_list$reds[,'y'], col = 'red', pch = 15, cex = pch_size)
  dev.off()
}

simulate = function(traffic_list, iterations, sample_number, p, summary=FALSE,pch_size=.7) {
  # Given a traffic_list representation of the grid at the initial state, simulates the traffic grid.
  # Iterations is the maximum number of time steps which the simulation is allowed to run.
  # If summary is TRUE, only the final state is plotted and saved as a JPEG in the current working directory.
  # Else if summary is FALSE, then the state at every time step is plotted and saved.
  
  i = 0
  names(i)="Number of Steps"
  
  if(!summary) {plot_traffic(traffic_list, i, pch_size)}
  
  freeflow_instances = 0
  freeflow_condition = lcm(nrow(traffic_list$occupied), ncol(traffic_list$occupied))
  # Free flow is defined as above, when traffic is able to cycle through the grid n times, for n
  # is the LCM of the number of columns and rows.
  
  while (i < iterations){
    next_traffic_list = step(traffic_list)
    
    allbluesmove=(!(any(traffic_list$blues[,2]==next_traffic_list$blues[,2]))) #true if all blue cars moved
    nobluesmove=all(traffic_list$blues[,2]==next_traffic_list$blues[,2]) #true if none moved
    
    allredsmove=(!(any(traffic_list$reds[,1]==next_traffic_list$reds[,1]))) #true if all blue cars moved
    noredsmove=all(traffic_list$reds[,1]==next_traffic_list$reds[,1]) #true if none moved
    
    if(allredsmove&allbluesmove)
    {
      freeflow_instances = freeflow_instances + 1
      
      if (freeflow_instances == freeflow_condition){
        plot_traffic(next_traffic_list, i, sample_number, p, 0.7)
        return(c(i, paste(freeflow_condition," Consecutive Instances of Free Flow Found")))
      }
    }
    else if(noredsmove&nobluesmove) {
      freeflow_instances = 0
      plot_traffic(next_traffic_list, i, sample_number, p, 0.7)
      return(c(i,"Traffic Jam"))
    }
    
    else {
      freeflow_instances = 0
    }
    i = i + 1 
    
    traffic_list = next_traffic_list
    if(!summary) {plot_traffic(traffic_list, i, pch_size)}
  }
  plot_traffic(next_traffic_list, i, sample_number, p, 0.7)
  return(c(i, "Mix of Free Flow and Traffic"))
}

sampling=function(total_samples,m,n,p,iterations=100,summary=TRUE,pch_size=.7) {
  simulation=function(m,n,p,sample_number,iterations=50,summary=TRUE,pch_size=.7){
  traffic_list=make_car_list(m,n,p)
  return(simulate(traffic_list, iterations, sample_number, p, summary,pch_size=.7))
}
  samples = list(total_samples)
  for (i in 1:total_samples){
    samples[[i]] = simulation(m,n,p,i,iterations)
  }
  save(samples, file = 'samples.RData') 
  #save the sample information in the working directory as well.
  return(samples)
}

### Running Statistics ###

statistics=function(total_samples,m,n,p,iterations){
  elements=unlist(sampling(total_samples,m,n,p,iterations))
  jamprop=sum(elements=="Traffic Jam")/total_samples
  names(jamprop)="Estimated Proportion of Traffic Jams"
  
  mixprop=sum(elements=="Mix of Free Flow and Traffic")/total_samples
  names(mixprop)="Estimated Proportion of Mixed Flow"  
  
  flowprop=1-jamprop-mixprop
  names(flowprop)="Estimated Proportion of Free Flowing Traffic"

return(list(jamprop,mixprop,flowprop))
}

varydensity=function(total_samples,low,high,m,n,iterations){
  for(i in seq(low,high,by=0.01)){
    statistics(total_samples,m,n,i,iterations)
  }
}

varygridsize=function(total_samples,m,least_n,most_n,p,iterations){
  for(i in seq(least_n,most_n)){statistics(total_samples,m,i,p,iterations)}
}

varysquare=function(total_samples,least_n,most_n,p,iterations){
  for(i in seq(least_n,most_n)){statistics(total_samples,i,i,p,iterations)}
}

size_test = function() {
stats_10x10_50 = statistics(20,10,10,0.5,3000)
stats_25x25_50 = statistics(20,25,25,0.5,3000)
stats_50x50_50 = statistics(20,50,50,0.5,3000)
stats_10x10_30 = statistics(20,10,10,0.3,3000)
stats_25x25_30 = statistics(20,25,25,0.3,3000)
stats_50x50_30 = statistics(20,50,50,0.3,3000)
save(stats_10x10_50, stats_25x25_50, stats_50x50_50, stats_10x10_30, stats_25x25_30, stats_50x50_30, file = 'stats.RData')
}

### Example code to run: 
#size_test()



