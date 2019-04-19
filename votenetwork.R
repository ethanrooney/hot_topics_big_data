library('igraph')
library('dplyr')
library('ggraph')
i = 80
pdf("votenetwork81_116.pdf", width = 7.5, height = 10)
while(i <80 ){
	i = i + 1
	k = 1
	edgefile = load(sprintf('edge%i.Rdata', i))
	members = read.csv('HSall_members.csv')
	head(members)
	edges[!is.finite(edges)] <- 0
	nodefile = subset(members, congress == i, select = c(icpsr, party_code, chamber, bioname))
	nodefile = subset(nodefile, chamber == "House" | chamber == "President" , select = c(icpsr, party_code, bioname))
	nodefile$color <- ifelse(nodefile$party_code == 100, "red", ifelse(nodefile$party_code == 200, "blue", ifelse(nodefile$party_code < 400, nodefile$party_code, round(nodefile$party_code/100))))
	parties = unique(nodefile$party_code)
	parties
	unique(nodefile$color)
	color = unique(c(nodefile[nodefile$party_code==parties[1], "color"],nodefile[nodefile$party_code==parties[2], "color"],nodefile[nodefile$party_code==parties[3], "color"],nodefile[nodefile$party_code==parties[4], "color"],nodefile[nodefile$party_code==parties[5], "color"]))
	color
	legendinfo = ('bottomright',
				  legend=c("Dem","Rep", "3rd Party"), 
				  col=c("red", "blue", "yellow"),
				  pch = 20,
				  cex = 2
	adjmatrix = edges/k
	linklevel = (mean(edges))
	adjmatrix[edges/k < linklevel/k] = 0
	conedge=graph_from_adjacency_matrix(adjmatrix, mode = c("max"), weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NA)

	head(nodefile)
	for(j in (V(conedge)$name)) {
		V(conedge)[j]$color		= nodefile[nodefile$icpsr == j, "color"]
		V(conedge)[j]$party		= nodefile[nodefile$icpsr == j, "party_code"]
		#V(conedge)[j]$bioname	= nodefile[nodefile$icpsr == j, "bioname"]
	}
	degree(conedge)
	V(conedge)$name = ""
	plot(conedge, vertex.size = 3, edge.color = "white", main = paste(i))
	legend('bottomright',
		   legend=c(parties), 
		   col=c(color),
		   pch = 20,
		   cex = 2)
	print(i)
}
dev.off()
