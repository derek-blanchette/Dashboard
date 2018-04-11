upper.half.circle <- function(x,y,r,nsteps=100,  ...){  
  rs <- seq(0, pi, len=nsteps) 
  xc <- x+r*cos(rs) 
  yc <- y+r*sin(rs) 
  polygon(xc,yc,...) 
} 


upper.half.circle.rotated <- function(x,y,r,nsteps=100, score,...){  
  
  rs <- seq(0,pi,len=nsteps) 
  xc <- x+r*cos(rs) 
  yc <- y+r*sin(rs) 
  original <- matrix(c(xc, yc), byrow= TRUE, nrow=2, ncol = nsteps)
  
  angle = -score/100*pi
  
  rotate <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), byrow=TRUE, nrow = 2)
  
  rotate.matrix <- rotate %*% original
  
  new.xc <- rotate.matrix[1,]
  new.yc <- rotate.matrix[2,]
  
  
  polygon(new.xc, new.yc, ...) 
} 


lower.half.circle <- function(x,y,r,nsteps=100,...){ 
  rs <- seq(0,pi,len=nsteps) 
  xc <- x-r*cos(rs) 
  yc <- y-r*sin(rs) 
  polygon(xc,yc,...) 
} 



#Initialize plot
plot(1, type="n",axes=F,xlab="", ylab="",xlim=c(-50,50),ylim=c(-50,50), asp=1)

#Subtend a red arc- Outter arc
upper.half.circle(0,0,30,nsteps=100,col='red')

#Subtend arc that is 60% of 180 degrees
upper.half.circle.rotated(0,0,30,nsteps=100,col='white', score = 60)

#Subtend a green arc - Inner arc
upper.half.circle(0,0,20,nsteps=100,col='green')

#Subtend an arc that is 20% of 180 degrees
upper.half.circle.rotated(0,0,20,nsteps=100,col='white', score = 20)

#Fill an inner circle for aesthetics
upper.half.circle(0,0,10,nsteps=100,col='white')

#Clean up the plotting details
polygon(c(-50, 50, 50, -50, -50),c(0, 0, -50, -50, 0), col = "white", border = FALSE)

#Draw a base line
polygon(c(-30, 30),c(0, 0), col = "black")


