#
# WILD 595-4 Lab 02
# Band Recovery Model
#

br.model <- function ( pp ){
	S<-pp[1] # survival probability
	f<-pp[2] # recovery rate
	NPreleases <- c( 99,88,153,114 ) # number of releases by year
	NPrecov <- matrix( 0, 4, 4 ) # create a matrix with 4 rows and 4 columns
	NPrecov[1,] <- c(7,  4,  1,  0) # recoveries of birds banded in year 1
	NPrecov[2,] <- c(NA, 8,  5,  1) # recoveries of birds banded in year 2
	NPrecov[3,] <- c(NA, NA, 10, 4) # recoveries of birds banded in year 3
	NPrecov[4,] <- c(NA, NA, NA,16) # recoveries of birds banded in year 4
	
	CellProbs <- matrix( 0, 4, 4 )
	CellProbs[1,] <- c( f, S*f, S*S*f, S*S*S*f ) # recovery probability of birds banded in year 1
	CellProbs[2,] <- c( 0,  f,    S*f,   S*S*f ) # recovery probability of birds banded in year 2
	CellProbs[3,] <- c( 0,  0,      f,     S*f ) # recovery probability of birds banded in year 3
	CellProbs[4,] <- c( 0,  0,      0,       f ) # recovery probability of birds banded in year 4
	
	NeverSeen <- c( (NPreleases[1]-sum(NPrecov[1,], na.rm=T))*log(1-sum(CellProbs[1,])), # never recovered from year 1
					(NPreleases[2]-sum(NPrecov[2,], na.rm=T))*log(1-sum(CellProbs[2,])), # never recovered from year 2
					(NPreleases[3]-sum(NPrecov[3,], na.rm=T))*log(1-sum(CellProbs[3,])), # never recovered from year 3
					(NPreleases[4]-sum(NPrecov[4,], na.rm=T))*log(1-sum(CellProbs[4,])) )# never recovered from year 4
	
	logL <- sum( NPrecov*log(CellProbs), na.rm=T )+ sum(NeverSeen) # sum all of the data*log(probability) statements
	return( logL )
}

b<-c(0.5,0.05)
res<-optim( b, br.model, control=list(fnscale=-1), hessian=T )