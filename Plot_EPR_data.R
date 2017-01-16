# Plot EPR data.
# Use 'setwd('C:/YourFolder/')' to set working directory.
# all .acs files will be read and plot, exporting as '.tiff' of the same name.

filenames <- dir()
sapply(filenames, function(filename) {
        dat <- read.csv(filename, sep = '\t', header = F)
        names(dat) <- c('Field', 'Intensity')
        output <- gsub('.asc', '.tiff', filename)
        if(file.exists(output)) {file.remove(output)}
        tiff(output, width = 480, height = 400, bg = 'transparent')
        par(mar = c(4, 4, 1, 1), bg = 'transparent')
        plot(dat$Intensity ~ dat$Field, type = 'l', lwd = 2,
             xlab = 'Field (G)', ylab = 'Intensity', frame = F)
        dev.off()
})


# Plot out a gallery of stacked EPR spectra.
# The directory must contain the .asc files of EPR spectra.
# The file names must contain a unique number of minutes.

#=====================================================================#
# Change parameter in the block if needed.
# First give the ones being plotted (number of minute)
times <- c(4, 10, 20, 30, 45, 60, 105)
# Then determine the field window (in Gauss).
XLIM <- c(1000, 6000)
# Select the color combination to be applied for spactral series.
# Replace blue and red with desired color name (legal ones).
library(grDevices)
pal <- colorRampPalette(c('red', 'blue'))
#=====================================================================#

datlist <- list()       # To store all data files.
filenames <- dir()      # Get all file names.
filenames <- filenames[grep('.asc', filenames)] # Extract .asc files.
# Extract files to be used and printed.
fileselected <- sapply(seq_along(times), function(i) {
        ptn <- paste0(times[i], 'min')
        filenames[grep(ptn, filenames)]
})
# Read in selected files
datlist <- sapply(fileselected, function(f) {
        read.csv(f, sep = '\t', header = F)
})
# Stratify datasets by adding a number to some of the data.
len <- length(times)
# Reversed order to make the earlier graph on top.

#======================================================================#
# Adjusting some datasets.

#======================================================================#
archive <- datlist
# Define layout.
d <- sapply(2:len, function(i) max(datlist[[2, i]]) - median(datlist[[2, i-1]]))

#======================================================================#
# Make adjustments for the stratified spectra.
# print d out, and paste below.
d = c(35000, 57000, 80000, 95000, 110000, 125000)
#======================================================================#
datlist = archive       # Restore datlist.

for(i in 1:(len-1)) {
        datlist[[2, i+1]] = datlist[[2, i+1]] - d[i]
}
# Create empty plot.
YLIM <- c(min(datlist[[2, len]]), max(datlist[[2, 1]]))
tiff(filename = 'Fe_meso_COE.tiff', width = 500, height = 700,
     bg = 'transparent')
par(mar = c(4, 4, 0.5, 0), bg = 'transparent')
plot(datlist[[1, 1]], datlist[[2, 1]], type = 'n', axes = F,
     xlim = XLIM, ylim = YLIM, xlab = 'Magnetic Field (G)', ylab = '')
Axis(x = datlist[[1, 1]], side = 1)
cols <- pal(len)
sapply(1:len, function(i) {
        points(x = datlist[[1, i]], y = datlist[[2, i]], type = 'l',
               col = cols[i], lwd = 2)
})
legs <- sapply(times, function(i) paste(i, 'min'))
legend(5000, 25000, legend = legs, text.col = cols)
dev.off()



# Plot EPR data into interactive Plotly plot.
# Convenient to read peak positions
.
library(plotly)
setwd('G:/Research/EPR/Fe-PYBP/Fe-mesPYBP_COE//')
filename <- 'Fe-mesPYBP_COE_105min.asc'
dat <- read.csv(filename, sep = '\t', header = F)
names(dat) <- c('Field', 'Intensity')
plot_ly(x = dat$Field, y = dat$Intensity, type = 'scatter', mode = 'lines',)

