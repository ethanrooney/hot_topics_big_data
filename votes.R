# Load CSV data 
dat.df = read.csv("HSall_votes.csv", colClasses=c("icpsr"="character"))
print("dat.df")

congressloop <- function(con, cham){
	con.df = subset(dat.df, congress==con, select = c(congress, chamber, rollnumber, icpsr, cast_code))
	con.df = subset(con.df, chamber==cham,)
	con.df$bool <- ifelse(con.df$cast_code > 0 & con.df$cast_code < 4 , 1,
					   ifelse(con.df$cast_code > 3 & con.df$cast_code <7, 0, -1))
	members <- unique(con.df$icpsr)
	nseats <- length(members)
	nvotes = length(unique(con.df$rollnumber))
	matto <- matrix(0,ncol=nseats,nrow=nseats)
	matpos <- matrix(0,ncol=nseats,nrow=nseats)
	rownames(matto) <- members
	rownames(matpos) <- members
	colnames(matto) <- members
	colnames(matpos) <- members
	for(i in 1:nvotes){
		vote.df = subset(con.df, rollnumber==i, select = c(rollnumber, icpsr, bool))
		voter = unique(vote.df$icpsr)
		length(vote.df$bool)
		for(j in voter){
			for(k in voter){
				if((vote.df$bool[[which(vote.df$icpsr == j)]])!=0 & (vote.df$bool[[which(vote.df$icpsr == k)]]) != 0){
					matpos[[j,k]] = matpos[[j,k]] + 1
					if((vote.df$bool[[which(vote.df$icpsr == j)]]) == vote.df$bool[[which(vote.df$icpsr == k)]]){
					matto[[j,k]]  <- matto[[j,k]]+1
					}
				}
			}
		}
	}
	matnorm  <-  matto/matpos
	return(matnorm)
}
ncon <- length(unique(dat.df$congress))
for( n in 1:ncon){
	edges  <- congressloop(n,"House")
	save(edges, file = paste("edge%d.Rdata",n))
	print(n/ncon)
}


