votes = read.csv(file="HSall_votes.csv", head=TRUE)
member = read.csv(file="HSall_members.csv", head=TRUE)
member_con_1 = member[member$congress == 1, ]
member_con_1$rand_x = runif(n=96, min=-1, max=1)
member_con_1$rand_y = runif(n=96, min=-1, max=1)
