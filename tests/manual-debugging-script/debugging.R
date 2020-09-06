library(tidyverse)
library(ggboy)

file = ggboy::hk
res=160
crop=TRUE
ncols=4
cols=c("#0f380f", "#306230", "#8bac0f", "#9bbc0f")


# Read in image file or array
  if(is.matrix(file) | is.array(file)){
    raw <- file
  } else {
    if(grepl('.png', tolower(file))) raw <- png::readPNG(file)
    if(grepl('.jpg|.jpeg', tolower(file))) raw <- jpeg::readJPEG(file)
  }

# If image has three channels (rgb) average them to get greyscale
  if(length(dim(raw)) == 3) raw <- rowMeans(raw, dims=2)

# Extract dimensions and centre pixel of raw image
  im_y <- dim(raw)[1]
  im_x <- dim(raw)[2]
  mid_y <- round(im_y/2)
  mid_x <- round(im_x/2)

# If crop (default) to original Game Boy screen aspect ratio (160 x 140)
  if(crop){
    # Set aspect to original Game Boy screen
    asp <- 144/160

    if(im_y/im_x < asp){
      # if image is landscape, subset to full height and height/asp width
      x_crop <- floor((im_y/asp)/2)
      raw <- raw[,(mid_x-x_crop):(mid_x+x_crop)]
      } else {
      # if image is portrait or square, subset to full width and width*asp height
      y_crop <- floor((im_x*asp)/2)
      raw <-raw[(mid_y-y_crop):(mid_y+y_crop),]
      }

    # Recompute x and y dimensions of cropped image
    im_y <- dim(raw)[1]
    im_x <- dim(raw)[2]

    # If res is passed as NULL, set res to the cropped max horizontal rsoultion
    if(is.null(res)){res <- im_x}

    # Compute height for the given res (maintaining the original Game Boy aspect ratio)
    height <- round(res * asp)

    } else {
    # Compute aspect of uncropped image
    asp <- im_y/im_x

    # If res is passed as NULL, set res to the un-cropped max horizontal rsoultion
    if(is.null(res)){res <- im_x}

    # Compute height for the given res (maintaining the original image aspect ratio)
    height <- round(res * asp)
  }




# Compute dataframe
  d <-
    expand.grid(y = im_y:1, x = 1:im_x) %>%
    dplyr::mutate(g = as.vector(raw)) %>%
    dplyr::mutate(xbin = cut(x, res, labels = FALSE, include.lowest = T),
                  ybin = cut(y, height, labels = FALSE, include.lowest = T)) %>%
    dplyr::group_by(xbin, ybin) %>%
    dplyr::summarise(g = mean(g), .groups = "drop") %>%
    dplyr::mutate(g = cut(g, ncols, labels = FALSE)) %>%
    dplyr::mutate(xn = (xbin - min(xbin))/(max(xbin) - min(xbin)),
                  yn = (ybin - min(ybin))/(max(xbin) - min(xbin)))

# Print for debugging
  print(
    list(image_dims = c(im_x, im_y),
         aspect = asp,
         computed_res_and_height = c(res, height),
         plot_xbin_range = range(d$xbin),
         plot_ybin_range = range(d$ybin)))

# Make plot
d %>%
  ggplot2::ggplot(ggplot2::aes(xn, yn))+
  ggplot2::geom_raster(ggplot2::aes(fill=g))+
  ggplot2::coord_equal()+
  ggplot2::theme(legend.position = "")+
  ggplot2::scale_fill_gradientn(colours = cols)
  # geom_vline(xintercept = c(1,160), col="red")+
  # geom_hline(yintercept = c(1,144), col="red")+
  # geom_hline(yintercept = 160, lty=2, col="red")


# last_plot()+
#   coord_equal(xlim=c(-5, 5), ylim=c(-5, 5))
#
# last_plot()+
#   coord_equal(xlim=c(-5, 5), ylim=c(140, 150))
