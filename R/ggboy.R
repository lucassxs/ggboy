#' Game Boy image simulator
#'
#' \code{ggboy()} attempts to simulate the way an image would look if displayed on the original (1989) Game Boy screen.
#' By default, images are cropped (centerally) to maintain the original Game Boy screen aspect ratio and pixel numbers (height/width = 144/160).
#'
#' @param file Path to image file or array returned by \code{png::readPNG()} or \code{read::JPEG{}}.
#' @param res Horizontal resolution of output (default = 160). Vertical resolution will be computed to match the output aspect ratio.
#' Set to NULL for original image horizontal resolution.
#' @param crop Should the image be cropped to match the original Game Boy screen aspect ratio (default = TRUE).
#' @param ncols The number of 'greenscale' colours to use (default = 4, the original Game Boy specification).
#' @param cols The colours used to create a fill gradient (passed to \code{ggplot2::scale_fill_gradientn()})
#' defaults to an approximation of the original Game Boy shades of green \code{c("#0f380f", "#306230", "#8bac0f", "#9bbc0f")}.
#' @param aggregator Function used to aggregate pixel intensities for final image.
#' This can be any function (can be anonymous) that takes a vector and returns a single value (default = mean).
#' @param graphic Should an approximation of the Game Boy body be included with the plot? (default = FALSE). The graphic can only be shown
#' if crop is also set to TRUE
#'
#' @return A ggplot2 plot
#' @export
ggboy <- function(file,
                  res = 160,
                  crop = TRUE,
                  ncols = 4,
                  cols = c("#0f380f", "#306230", "#8bac0f", "#9bbc0f"),
                  aggregator = mean,
                  graphic = FALSE){

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

  # If crop (default), crop to original Game Boy screen aspect ratio (160 x 144)
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

  # Compute dataframe for plot
  d <-
    expand.grid(y = im_y:1, x = 1:im_x) %>%
    dplyr::mutate(g = as.vector(raw)) %>%
    # Bin x into res bins and y in height bins
    dplyr::mutate(xbin = cut(x, res, labels = FALSE, include.lowest = T),
                  ybin = cut(y, height, labels = FALSE, include.lowest = T)) %>%
    # Aggregate the pixels for the new bins
    dplyr::group_by(xbin, ybin) %>%
    dplyr::summarise(g = aggregator(g), .groups = "drop") %>%
    # Bin the aggregated pixel intensities into ncols bins
    dplyr::mutate(g = cut(g, ncols, labels = FALSE)) %>%
    # Normalise the x and y coordinates so x is between 0 and 1
    # and y is between 0 and an upper value that keeps the aspect ratio
    dplyr::mutate(xn = (xbin - min(xbin))/(max(xbin) - min(xbin)),
                  yn = (ybin - min(ybin))/(max(xbin) - min(xbin)))

  # Make plot
  if(crop & graphic){
    d %>%
      ggplot2::ggplot(ggplot2::aes(xn, yn))+
      ggboy::gb_body +
      ggplot2::geom_raster(ggplot2::aes(fill=g))+
      ggplot2::coord_equal()+
      ggplot2::theme_void()+
      ggplot2::theme(legend.position = "")+
      ggplot2::scale_fill_gradientn(colours = cols)
  } else {
    d %>%
      ggplot2::ggplot(ggplot2::aes(xn, yn))+
      ggplot2::geom_raster(ggplot2::aes(fill=g))+
      ggplot2::coord_equal()+
      ggplot2::theme_void()+
      ggplot2::theme(legend.position = "")+
      ggplot2::scale_fill_gradientn(colours = cols)
  }
  }

