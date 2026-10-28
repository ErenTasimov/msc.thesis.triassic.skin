### Used in Master Thesis of Eren Tasimov

### Code for plotting the Supertree in Figure 22 of the thesis;
### It, together with most comments, was originally conceived by Dr. Emma M. Dunne,
### and since then it has been used and adapted for different supertree plots over time 




### Necessary packages
library(tidyverse)  
library(ape)       
library(phytools)   
library(paleotree)  
library(strap)    

### Supertree file
  tree <- read.nexus("data/supertree.nex")

  tree$tip.label # See the tip labels - in this case clade names
plot(tree, direction="rightwards", cex=0.56) # plot the tree 




########## Cleaning tree and age data




### ages dataset 
clade_ages <- read_csv("data/clade_ages.csv")

### Replacing spaces with "_", so it is compatible with tree
clade_ages$Clade <- gsub(" ", "_", clade_ages$Clade) 
clade_ages <- clade_ages %>% distinct(Clade, max_age, min_age, .keep_all = TRUE) # to remove any duplicates, in case there are any


### Checking tree and data agaist each other
intersect(clade_ages$Clade, tree$tip.label) # taxa in both tree and age data - ideally 19
setdiff(clade_ages$Clade, tree$tip.label) # clades in the data but not on the tree - ideally 0
setdiff(tree$tip.label, clade_ages$Clade) # clades on the tree, but not in the data - ideally 0


### Removing clades from tree without agedata (if there are any)
tree <- drop.tip(tree, tree$tip.label[!(tree$tip.label %in% clade_ages$Clade)])
setdiff(tree$tip.label, clade_ages$Clade) # ideally, should return "character(0)" 

### Removing clades from data that are not on tree (if there are any)
ages <- clade_ages[(clade_ages$Clade %in% tree$tip.label), ]
setdiff(ages$Clade, tree$tip.label) # ideally, should return "character(0)" 



### Connecting min and max ages to each clade (also works woth datasets that have
### more than one occurrence from a clade in a dataset)
single_ages <- ages %>% 
  group_by(Clade) %>% 
  summarise(FAD = max(max_age), LAD = min(min_age))

### "Clade" column needs to be converted to rownames, as it is required by the function 
test_timeData <- single_ages %>% column_to_rownames(var = "Clade")
class(test_timeData) # check that it is a data.frame




######### Timesclaing and generating the tree
ttree <- timePaleoPhy(
  tree = tree, # the supertree
  timeData = test_timeData, # final age information
  type = "basic" , # method for timescaling
  ntrees = 1, # nr of trees that will be output
  dateTreatment = "minMax", # this controls how the timeData is interpreted
  plot = TRUE
)

.

.

### now to plot the tree on a proper geological timescale
geoscalePhylo(
  ttree, # timescaled tree
  test_timeData, # age data
  direction = "rightwards",
  units = c("Period", "Epoch", "Age"),
  boxes = "Age",
  tick.scale = "myr",
  user.scale,
  cex.age = 0.6,
  cex.ts = 0.6,
  cex.tip = 0.6,
  width = 0.7,
  ts.col = TRUE,
  vers = "ICS2015", 
)

.

.

## using "equal" method
tree_l <- DatePhylo(tree, test_timeData, method = "equal", rlen = 1)

geoscalePhylo(tree = tree_l, boxes = "Age", cex.tip = 0.6)

# Plotting the tree with visible age/stratigraphical ranges
geoscalePhylo(tree = tree_l, ages = test_timeData, boxes = "Age", cex.tip = 0.6)

### IMPORTANT: SUBSEQUENTLY, THE TREE WAS EDITED AND ALTERED TO ITS FINAL FROMS USING THE SOFTWARE INKSCAPE (inkscape.org); 
### FOR EXAMPLE; IT WAS MIRRORED, SILHOUETTES WERE ADDED, FONT OF TAXA WAS CHANGED, TIME SCALE WAS ADAPTED TO MATCH MORE
### RECENT ICS VERSIONS ETC.