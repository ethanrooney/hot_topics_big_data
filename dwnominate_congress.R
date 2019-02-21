#!/usr/bin/Rscript
###################################################
###################################################
## Project Code ###################################
###################################################
###################################################
# Find the working directory prior to running any commands
if(!requireNamespace('gganimate')) install.packages('gganimate')
if(!requireNamespace('tweenr')) install.packages('tweenr')
workingdir <- getwd()
# load the csv into memory and then create a data frame from it.
filelocation = "HSall_members.csv"
mydata <- read.csv(file=filelocation, header=TRUE, sep=',')
# Makes a new directory if one doesn't already exist named "plots"
newdir <- "plots"
dir.create(file.path(workingdir, newdir), showWarnings = FALSE)
#this is going to create a file in 
newfile = paste(".", newdir, "congress%d.pdf", sep="//")  
pdf(newfile)
c = 0
year = 1789
repeat {
	year = year + c*2
	title = paste("congressional Polarization", year, sep=" ")
	c = c + 1
	con <- subset(mydata, congress==c)
	plot(x=con$nominate_dim1, xlab="Partision", xlim=c(-1,1), y = con$nominate_dim2, ylab="Reginal",ylim=c(-1,1), main="Congressional Polarization", main2="year", col= ifelse(con$party_code<=100, con$party_code, con$party_code/100 ))
	if (c > 114){
		break
	}
}
dev.off()


