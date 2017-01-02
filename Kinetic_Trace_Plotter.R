# Plotting SF kinetic trace figures, highlighting different kinetic processes with colors.
# Author: @ Guang Yang
# Date:   10/27/2016


# Data are directly received from Kinetic Studio software and TgK Stopped-Flow.
# Files are in .txt format, usually separated by tab ('\t').
# First 15 rows are general info, and are omitted here.

# Type the filename with path.
FileName <- 'G:/Research/Stopped-flow data/03-05-2013_Fe-racPYBP/03-05-2013_Fe-racPYBP/2mM_20eq_+15C001.TXT'

# Define the processes being plotted out.
# Take a look at the Kinetic Studio DA traces before choosing intervals.
# INITIAL '0' IS NEEDED!!
times <- c(0, 2.2, 5.9, 65, 120)

# Pick the wavelengths: note here only 2 are allowed - more than 3 are usually not quite useful.
xnm <- c(550, 660)

# The result files are named as 'Batchname(x).tiff', and stored under the foler pointed by FilePath.
# File size:  3000 x 2500 pixels
# Resolution: 300 dpi
FilePath<-'G:/Research/Fe_paper/'
BatchName<-'FeracPYBP_20H2O2_15C'




# Please leave the code below, unless you're sure what you need to do.
#========================================================================================================================#

dat <- read.csv(FileName, sep='\t', skip=15, header=F)
wavelengths <- dat[-1, 1]
timescale <- dat[1, -1] / 1000
dat <- dat[-1, -1]
rownames(dat) <- wavelengths
colnames(dat) <- timescale

# The below plotting codes serves to plot kinetic traces at given wavelengths
# color-coding by reaction processes.

# Define colors.
cols1 <- c('darkblue','brown','darkred','darkgreen')
cols2 <- c('blue','purple','red','green')
cols <- rbind(cols1,cols2)
bgcols <- c('darkgrey','grey')

# Get Indeces of the nominated wavelengths.
# Note the actual wavelengths are not integers - we use the ones closest to the given integer.
xnmIdx <- sapply(xnm,function(x){which.min(abs(wavelengths-x))})

# Define the range of Y-axis.
XLIM <- c(0, round(max(as.numeric(names(dat)))))
YLIM <- c(0, max(dat[xnmIdx,]))

# Plot data into the above-named .tiff files.
# First layer plots data with emphasis of the i th process;
# Second layer plots the 2 different wavelengths with similar but distinct color sets.
sapply(1:(length(times)-1),function(i){
  hilightIdx <- which(timescale <= times[i+1] & timescale >= times[i])
  FileName <- paste0(FilePath, BatchName, '(', i, ').tiff')
  tiff(FileName, width = 250, height = 210, bg = 'transparent')
  par(mar = c(4, 4, 0.5, 0), bg = 'transparent')
  plot(as.numeric(timescale), as.numeric(dat[which.min(wavelengths - xnm[1]),]),
       type = 'n', xlab = 'Time (s)', ylab = 'Absorption', xlim = XLIM, ylim = YLIM,
       frame = F)
  sapply(seq_along(xnmIdx), function(x){
    points(as.numeric(timescale[hilightIdx]), as.numeric(dat[xnmIdx[x], hilightIdx]),
           type = 'l', lwd = 3, col = cols[x,i])
    points(as.numeric(timescale[which(timescale < times[i])]),  as.numeric(dat[xnmIdx[x], which(timescale < times[i])]),
           type = 'l', lwd = 3, col = bgcols[x])  
    points(as.numeric(timescale[which(timescale > times[i+1])]),  as.numeric(dat[xnmIdx[x], which(timescale > times[i+1])]),
           type = 'l', lwd = 3, col = bgcols[x])
  })
  dev.off()
})

