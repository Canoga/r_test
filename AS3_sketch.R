hill_function <- function(x){
  0.5*x^{3}+x^{2}-3*x+10
}

dhill_function <- function(x){
  1.5*x^{2}+2*x-3
}


x=seq(-3,3, 0.01)
plot(x, hill_function(x), type='l', col='dark green', main="Hiking with friends", xlab="Distance in Km", ylab="Altitude (x100m)")

dx <- 1/1000
x_1 <- 2.5
if (hill_function(x_1+dx) > hill_function(x_1)) {
  dx <- -1/1000
} 

# This is the starting point
point <- c(x_1, hill_function(x_1), dx)
point_list <- list(point)

##Version 2
for (i in 2:11) {
# First we need the df at the point, where we currently are
  df_new <- dhill_function(unlist(point_list[i-1])[1])
# Now we calculate the new step to use 
dx_new <- dx * df_new
# New x position is the previous one plus the new dx
  x_new <- unlist(point_list[i-1])[1] + dx_new
# New y position is the hill function at the new x
  y_new <- hill_function(x_new)
  # Drawing the point on the plot
  points(x_new, y_new, pch=10, col='red')
# Creating the point vector with the newly calculated data
  point <- c(x_new, y_new, dx_new, df_new)
# Appending the point vector to the list of points
  point_list <- append(point_list, list(point))
}
point_list
