#!/usr/bin/Rscript
###################################################
###################################################
## Project Code ###################################
###################################################
###################################################
# Launch R from the directory the data is stored, in my case that is ~/ethan/spring2019/project/
workingdir <- getwd()
newdir <- "pdfs"

filelocation = "HSall_members.csv"
mydata <- read.csv(file=filelocation, header=TRUE, sep=',')
dir.create(file.path(workingdir, newdir), showWarnings = FALSE)
pdf(".//pdfs//congress%d.pdf")
c = 0
year = 1789
sprintf("%f - %f", year , year+2*c )
repeat {
	year = year + c*2
	c = c + 1
	con <- subset(mydata, congress==c)
	plot(x=con$nominate_dim1, xlab="Partision", y = con$nominate_dim2, ylab="Reginal", main="Congressional Polarization", main2="year", col= ifelse(con$party_code<=100, con$party_code, con$party_code/100 ))
	if (c > 114){
		break
	}
}
dev.off()
