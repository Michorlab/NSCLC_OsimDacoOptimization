library(fishplot)

#provide a list of timepoints to plot
#You may need to add interpolated points to end up with the desired
#visualization. Example here was actually sampled at days 0 and 150
timepoints=c(0,25,50, 75,100)      

#provide a matrix with the fraction of each population
#present at each timepoint
frac.table = matrix(
  c(40, 10, 00, 00,
    55, 30, 10, 1,
    75, 40, 20, 15,
    98, 50, 30, 20,
    100, 55, 40, 30),
  ncol=length(timepoints))

#provide a vector listing each clone's parent
#(0 indicates no parent)
parents = c(0,1,1,2)

#create a fish object
fish = createFishObject(frac.table,parents,timepoints=timepoints,
                        col = c("springgreen4", "gold", "cornflowerblue", "coral"))

#calculate the layout of the drawing
fish = layoutClones(fish)

#draw the plot, using the splining method (recommended)
#and providing both timepoints to label and a plot title
png("fishPlot.pdf", width = 6, height = 4, units = "in", res = 600)
fishPlot(fish,shape="bezier", bg.type = "solid", bg.col = "white", border = 1)
dev.off()
