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

# Plot EPR data into interactive Plotly plot.
# Convenient to read peak positions
.
library(plotly)
setwd('G:/Research/EPR/Fe-PYBP/Fe-mesPYBP_COE//')
filename <- 'Fe-mesPYBP_COE_105min.asc'
dat <- read.csv(filename, sep = '\t', header = F)
names(dat) <- c('Field', 'Intensity')
plot_ly(x = dat$Field, y = dat$Intensity, type = 'scatter', mode = 'lines',)
