###################################################
###################################################
## Project Code ###################################
###################################################
###################################################
# Launch R from the directory the data is stored, in my case that is ~/ethan/spring2019/project/
filelocation = "HSall_members.csv"
mydata <- read.csv(file=filelocation, header=TRUE, sep=',')
c = 0
repeat {
	c = c + 1
	con <- subset(mydata, congress==c)
	#head(con1)
	plot(x=con$nominate_dim1, y = con$nominate_dim2, main="Congresssional Polarization", col=con$party_code/20)
	Sys.sleep(.25)
	if (c > 114){
		break
	}
}
