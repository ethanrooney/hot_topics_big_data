# Required Packages
if(!requireNamespace("plotrix")) install.packages("plotrix")
if(!requireNamespace("gifski")) install.packages("gifski")
if(!requireNamespace("gganimate")) install.packages("gganimate")
# First we need to import the data to R.
# Right now the data is all stored in a CSV file. It is in the same folder that I have this R file saved in.

# Load CSV data 
dat = read.csv("HSall_members.csv", header = TRUE)

# In case you want to see the top 5 lines of the CSV file (Across all columns): head(dat)

# Here I create a variable that will store all the congress numbers. 
# the cong.num name doesn't matter at all, I could name it anything, but R doesn't use periods so its safe
# To grab a specific column of data I use data_name$column_name
cong.num = dat$congress

 # Let's grab all the DWNominate_dim1 scores for the first congress.
DWN1.first = subset(dat$nominate_dim1, dat$congress < 2)
# If you look at the first entry of the data, it's NA, Let's set that to zero.
# We could remove or ignore that, but to make things easy for now, let's set it to zero. 
DWN1.first[1] = 0

# Now let's get the DWN_dim2 scores
DWN2.first = subset(dat$nominate_dim2, dat$congress < 2)
DWN2.first[1] = 0 # Again, set NA to zero. 

# Let's try plotting, I plot the DWN1 on the x and DWN2 on y. 
# I make the aspect ratio 1:1 so its a the x and y axes are spaced the same.
# Then I add color based on the Party Code column, but the numbers are very large, so I normalize it by 2500
plot(DWN1.first,DWN2.first,asp=1,col=dat$party_code/2500,xlab='DWN_dim1',ylab='DWN_dim2')
# There may be a better way to do add the x and y axis through the origin, but this is the way I found.
abline(v=0) # Plot y axis
abline(h=0) # Plot x axis

# I want to draw a unit circle, just to trace out the outer edges.
library(plotrix)
draw.circle(0,0,1,nv=100)
# Now, we could work on making the visual representation of this data better. (Like adding colors)
 

# Let's see if we can make many of these plots. And do it in a clean "Big Data' way.
# I think I need to make a data frame
library(ggplot2)
# par(mfrow = c(1,1)) # make sure the plots are on a 1,1 grid 

# Just a note, R automatically ignores data that can't be read. So the ' NA ' value that George Washington has 
# is easily ignored by R. 

# lets make all of these plots.
# I will make a plot list called plt. Then I will make a loop function that will make a plot of the 
# DWN scores for every different unique congress number.
plt <- list()
for (var in unique(dat$congress)) 
  local({
  tmp = dat[dat$congress==var,]
  pl_tmp <- ggplot( tmp, aes(x = tmp$nominate_dim1, y =tmp$nominate_dim2,color=factor(tmp$party_code)),asp=1) +
    geom_point()+
    annotate("path",x=cos(seq(0,2*pi,length.out=100)),y=sin(seq(0,2*pi,length.out=100)))+
    geom_hline(yintercept =0) + geom_vline(xintercept=0)+
    xlab("DWNominate Dim. 1") + ylab("DWNominate Dim. 2")+
    labs(title=paste(c('Congress Number ',var),collapse=" "),color="Party Code")
  plt[[var]] <<- pl_tmp
})
# Now we can access all the plots like this:
plt[[113]] # And just replace the number 50 with any congress number 1 - 113.

# Note: As I have it rightn now, the colors for the party codes are not unique across all congress numbers. 
# Meaning that some colors are repeated later for a different party. This is not ideal. I believe the way to
# fix this would be to make an array that had all the unique party codes given specific colors.

# and I can make a movie, but I will need to install some animation packages.
library(gganimate)
library(gifski)
# I can use the gganimate option of "transition_states"
congresstime <- ggplot( dat, aes(x = dat$nominate_dim1, y =dat$nominate_dim2,color=dat$party_code),asp=1) +
  geom_point()+
  annotate("path",x=cos(seq(0,2*pi,length.out=100)),y=sin(seq(0,2*pi,length.out=100)))+
  geom_hline(yintercept =0) + geom_vline(xintercept=0)+
  xlab("DWNominate Dim. 1") + ylab("DWNominate Dim. 2")+
  labs(title=paste('Congress Number', dat$congress),color="Party Code")#+
  #transition_states(dat$congress,transition_length = 2,state_length = 1)



anim_save("movie.gif",congresstime + transition_time(dat$congress))

# Now it shows a color bar for all the Party Code (uniquely colored), 
# but I don't think it has much meaning at the moment.
# Now I want to parameterize the groups somehow. Maybe some Network analysis things.
# Then I can make another movie of that parameter and play the two side by side. 

# Network Analysis Section:


# Can we view both animations? 





# Icing on the top kind of things:
# 1. use tweenr for smooth transitions between congressman
