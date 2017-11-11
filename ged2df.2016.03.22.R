
options(stringsAsFactors = FALSE)

rm(list = ls())

setwd("/home/mcdevitt/_ds/_src")

library(igraph)
library(networkD3)
#library(data.tree)
#library(R6)
#library(network)

# ...	

ged <- readLines("james_mcdevitt_descendants.2016.03.16.ged")

ind <- grep("0 @I", ged)
inam <- ind+1
fam <- grep("FAM", ged)

ged[inam] <- gsub("1 NAME ", "", ged[inam])
ged[inam] <- gsub("/", "", ged[inam])

ged[ind] <- gsub("0 @I", "", ged[ind])
ged[ind] <- gsub("@ INDI", "", ged[ind])
ind_lst <- as.numeric(ged[ind])

ged_nodes <- data.frame(ind_lst, ged[inam])

nodes_srt <- ged_nodes[ order(ged_nodes[,1]), ]

fami_indx <- grep("0 @.+@ FAM", ged)
ged[fami_indx]
pere_indx <- fami_indx + 1
mere_indx <- fami_indx + 2	# usually works, but not always

fami <- gsub("0 @F", "", ged[fami_indx])
fami <- gsub("@ FAM", "", fami)
fami <- as.numeric(fami)

pere <- gsub("1 HUSB @I", "", ged[pere_indx])
pere <- gsub("@", "", pere)
pere <- as.numeric(pere)

mere <- gsub("1 WIFE @I", "", ged[mere_indx])
mere <- gsub("@", "", mere)
mere <- as.numeric(mere)

max_node <- max(nodes_srt[,1])
cpl <- data.frame(pere, mere, fami, max_node+fami, "couple")
cpl
cpl_srt <- cpl[ order(cpl[,3]), ]

max_node <- max(cpl_srt[,4])


# .... search for children within FAM
child_edge <- data.frame(from = integer(),
					  to = integer(),
					  weight = numeric(),
					  type = character(),
					  link_width = numeric()
					)
new_row <- child_edge

enfant <- rep(0, 10)
mere <- 0
pere <- 0
family <- 0

toutes_familles <- data.frame(family=0, pere=0, mere=0, enfant = I(1:10))

for (ii in 1:(length(fami_indx)-1))
{
#	print(ii)
	
	ifam <- fami_indx[ii] + 1
	jfam <- fami_indx[ii+1] - 1
	
	fam_unit <- ged[ifam:jfam]
	
	print(fam_unit)
	
	fami <- gsub("0 @F", "", ged[ifam-1])
	fami <- gsub("@ FAM", "", fami)
	fami <- as.integer(fami)
	
	pere_indx <- grep("HUSB", fam_unit)
	pere <- gsub("1 HUSB @I", "", fam_unit[pere_indx])
	pere <- gsub("@", "", pere)
	pere <- as.integer(pere)
	
	mere_indx <- grep("WIFE", fam_unit)
	mere <- gsub("1 WIFE @I", "", fam_unit[mere_indx])
	mere <- gsub("@", "", mere)
	mere <- as.integer(mere)
	
#	cpl <- data.frame(pere, mere, fami, max_node+fami, "couple")
	
	enfant_indx <- grep("CHIL", fam_unit)
	
	if (length(enfant_indx))
	{
		enfant <- gsub("1 CHIL @I", "", fam_unit[enfant_indx])
		enfant <- gsub("@", "", enfant)
		enfant <- as.integer(enfant)
		for (ienf in 1:length(enfant))
		{
#			cat(ii, cpl[ii,3], enfant[ienf], cpl[ii,3], "child\n")
	
			from <- as.integer(cpl[ii,4])
			to <- as.integer(enfant[ienf])
			weight <- 1
			type <- "child"
			link_width <- 100
			child_edge <- rbind(child_edge, data.frame(from, to, weight, type, link_width))
		}
	}
	une_famille <- c(fami, pere, mere, enfant)
	toutes_familles <- rbind(toutes_familles, une_famille)
}

# ... individuals in vertex frame
#v.frame <- as.data.frame(matrix(ncol = 6, nrow = max_node+1))
#names(v.frame) = c("ID", "Name", "generation",  "Nsize", "birth", "death")

# ... read .csv with generation and family branch info


v.frame.gen <- read.csv ("v.frame.csv")

n_row <- max_node + 1

v.frame <- data.frame(ID = integer(n_row),
					  Name = character(n_row),
					  generation = integer(n_row),
					  Nsize = numeric(n_row)
					  )

last <- as.integer(max_node)
v.frame[,1] <- 0L : last
#v.frame[,2] <- as.character(0:last)
#v.frame[,3] <- 0L
#v.frame[,4] <- 1

for (i in 1 : (dim(nodes_srt)[1]))
{
	v.frame[nodes_srt[i,1]+1, 1] <- as.integer(nodes_srt[i, 1])
	v.frame[nodes_srt[i,1]+1, 2] <-	as.character(nodes_srt[i, 2])
	v.frame[nodes_srt[i,1]+1, 3] <- as.integer(v.frame.gen[nodes_srt[i,1]+1, 4])
	v.frame[nodes_srt[i,1]+1, 4] <- 40
}

#head(v.frame, 20)
#tail(v.frame, 20)

# ... couples in vertex frame

for (i in 1 : (dim(cpl_srt)[1]))
{
	v.frame[cpl_srt[i,4]+1, 1] <- as.integer(cpl_srt[i, 4])
	v.frame[cpl_srt[i,4]+1, 2] <- as.character(paste0("m", cpl_srt[i, 3]))
	v.frame[cpl_srt[i,4]+1, 3] <- as.integer(v.frame.gen[cpl_srt[i,1]+1, 4])
	v.frame[cpl_srt[i,4]+1, 4] <- as.numeric(1)
}

#head(v.frame, 20)
#tail(v.frame, 20)

#write.csv (v.frame, file = "v.frame.csv")

# ... couples in edge frame

e.frame <- data.frame(from = integer(n_row),
					  to = integer(n_row),
					  weight = numeric(n_row),
					  type = character(n_row),
					  link_width = numeric(n_row)
					)

e_lst <- dim(cpl_srt)[1]
for (i in 1 : e_lst)
{
	e.frame[i, 1] <- as.integer(cpl_srt[i, 1])
	e.frame[i, 2] <- as.integer(cpl_srt[i, 2])
	e.frame[i, 3] <- 1
	e.frame[i, 4] <- "couple"
	e.frame[i, 5] <- 2
	
	indx <- e_lst
	e.frame[indx + i, 1] <- as.integer(cpl_srt[i, 1])
	e.frame[indx + i, 2] <- as.integer(cpl_srt[i, 4])
	e.frame[indx + i, 3] <- 1
	e.frame[indx + i, 4] <- "marriage"
	e.frame[indx + i, 5] <- 2
	
	indx <- 2 * e_lst
	e.frame[indx + i, 1] <- as.integer(cpl_srt[i, 2])
	e.frame[indx + i, 2] <- as.integer(cpl_srt[i, 4])
	e.frame[indx + i, 3] <- 1
	e.frame[indx + i, 4] <- "marriage"
	e.frame[indx + i, 5] <- 2
}

e.frame <- rbind (e.frame, child_edge)

e.frame <- subset(e.frame, !(is.na(e.frame["from"]) | is.na(e.frame["to"])))

#head(e.frame, 20)
#tail(e.frame, 20)

# ...	simple network

src <- e.frame[,1]
trgt <- e.frame[,2]
networkData <- data.frame(src, trgt)
simpleNetwork(networkData)

# ... forceNetwork ....

forceNetwork(Links = e.frame, Nodes = v.frame,
			 Source="from",
			 Target="to",
			 NodeID = "Name",
			 Group = "generation",
			 Value = "link_width",
			 linkWidth = 2,
			 linkColour = "#afafaf",
			 colourScale = "d3.scale.category10()",
			 fontSize = 40,
			 fontFamily = "cursive",
			 zoom = TRUE,
			 legend = TRUE,
			 Nodesize = "Nsize",
			 opacity = 1.0,
			 opacityNoHover = 0.5
#			 charge = -2000 
#			 width = 2000,
#			 height = 1000
			 )


# ... radial ....

