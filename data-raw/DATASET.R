library(tidyverse)

# Example pictures for demos
rstats <- png::readPNG('data-raw/Rlogo.png')
hk <- jpeg::readJPEG('data-raw/hk.jpg')




# Game By body graphic (bg) -----------------------------------------------
# Body graphic post script
# Need to run so R can find Ghost Script on my computer
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.27/bin/gswin64c.exe")

# Trace the image and save the coordinates of polygons in an XML file
grImport::PostScriptTrace(file = 'data-raw/body-graphic.ps',
                          outfilename = 'data-raw/body-graphic.xml',
                          setflat = 0.2)

# Import the XML file as a grImport Picture
bg <- grImport::readPicture('data-raw/body-graphic.xml')

# Visualise each path separately
# I have converted the stroke around the screen surround to a path:
# so it appears as normal with all the other elements
grImport::picturePaths(bg, fill="white", freeScales = TRUE)

# Create a list containing each body graphic element path
# $coords = A tibble containing the xy coordinates
# $fill  = A hex code olour
bg <-
  lapply(bg@paths,
         function(b) list(coords = tibble(x=b@x, y=b@y), fill=b@rgb))

# Plot entire body
ggplot()+
  lapply(bg, function(b) annotate(geom = "polygon",
                                  x = b$coords$x,
                                  y = b$coords$y,
                                  fill = b$fill))+
  coord_equal()+
  theme_void()


# The screen is element 44 (for the current drawing!)
se <- 35

screen_w <- max(bg[[se]]$coords$x) - min(bg[[se]]$coords$x)
screen_h <- max(bg[[se]]$coords$y) - min(bg[[se]]$coords$y)
screen_b <- min(bg[[se]]$coords$y)
screen_l <- min(bg[[se]]$coords$x)

# Scale the Game Boy graphic so that the screen element is 0 to (1 + buffer) wide
# 2% buffer
scaler <- screen_w/1.02

# Scale all graphic paths
# Multiply all xy coordinates by scaler. Subtract the scaled xy coordinates of the screen bottom left corner
# Subtract half of the difference between screen dimensions and image dimenions to centre the image in x and y
scaled_paths <-
  lapply(bg,
         function(b) list(coords_s =
                            tibble::tibble(x = (b$coords$x - screen_l - (screen_w - scaler)/2) / scaler,
                                           y = ((b$coords$y - screen_b) / scaler) - ((screen_h/scaler) - 0.9)/2),
                          fill = b$fill))

# Return list of scaled ggplot2 polygons to annotate
gb_body <-
  lapply(scaled_paths,
       function(b) ggplot2::annotate(geom = "polygon",
                                     x = b$coords_s$x,
                                     y = b$coords_s$y,
                                     fill = b$fill))


usethis::use_data(rstats, hk, gb_body, overwrite = TRUE)
