#' Game Boy image simulator
#'
#' Experimental - not used, not exported
#'
#' \code{ggboy_demo()} shows the original (in greyscale), the cropped (in greyscale) and the Game Boy version of the image
#'
#' @param file Path to image file or array returned by \code{png::readPNG()} or \code{read::JPEG{}}
#' @param res Horizontal resolution of output (default = 160). Vertical resolution will be computed to match the output aspect ratio.
#' Set to NULL for original image horizontal resolution.
#' @param crop Should the image be cropped to match the original Game Boy screen aspect ratio (default = TRUE)
#' @param ncols The number of 'greenscale' colours to use (default = 4, the original Game Boy specification)
#' @param cols The colours used in the fill scale (passed to \code{ggplot2::scale_fill_gradientn()}) defaults to an approximation of the original Game Boy shades of greem
#' @param aggregator Function used to aggregate pixel intensities for final image.
#' This can be any function (can be anonymous) that takes a vector and returns a single value (default = mean).
#'
#' @return A ggplot2 plot
ggboy_demo <- function(file,
                       res=160,
                       crop=TRUE,
                       ncols=4,
                       cols=c("#0f380f", "#306230", "#8bac0f", "#9bbc0f"),
                       aggregator = mean){

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


  # Create dataframe for original image
  original <-
    expand.grid(y = im_y:1, x = 1:im_x) %>%
    dplyr::mutate(g = as.vector(raw)) %>%
    dplyr::mutate(facet = "original")


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

  # Create dataframe for cropped image (or uncropped if cropped if crop = FALSE)
  cropped <-
    expand.grid(y = im_y:1, x = 1:im_x) %>%
    dplyr::mutate(g = as.vector(raw)) %>%
    dplyr::mutate(facet = "cropped")

  # Compute dataframe
  d <-
    expand.grid(y = im_y:1, x = 1:im_x) %>%
    dplyr::mutate(g = as.vector(raw)) %>%
    dplyr::mutate(xbin = cut(x, res, labels = FALSE, include.lowest = T),
                  ybin = cut(y, height, labels = FALSE, include.lowest = T)) %>%
    dplyr::group_by(xbin, ybin) %>%
    dplyr::summarise(g = aggregator(g), .groups = "drop") %>%
    dplyr::mutate(g = cut(g, ncols, labels = FALSE))

  # Create dataframe for final Game Boy image
  converted <-
    d %>%
    dplyr::select(y=ybin, x=xbin, g) %>%
    dplyr::mutate(facet="converted")


  # Plot all three images
  patchwork::wrap_plots(

    original %>%
      ggplot2::ggplot(ggplot2::aes(x, y))+
      ggplot2::geom_raster(ggplot2::aes(fill=g))+
      ggplot2::coord_equal()+
      ggplot2::theme_void()+
      ggplot2::theme(legend.position = "")+
      ggplot2::scale_fill_gradientn(colours = c("black", "white"))+
      ggplot2::labs(title = "Original",
                    subtitle = "Greyscale"),

    cropped %>%
      ggplot2::ggplot(ggplot2::aes(x, y))+
      ggplot2::geom_raster(ggplot2::aes(fill=g))+
      ggplot2::coord_equal()+
      ggplot2::theme_void()+
      ggplot2::theme(legend.position = "")+
      ggplot2::scale_fill_gradientn(colours = c("black", "white"))+
      ggplot2::labs(title = "Original cropped",
                    subtitle = "Greyscale"),

    converted %>%
      ggplot2::ggplot(ggplot2::aes(x, y))+
      ggplot2::geom_raster(ggplot2::aes(fill=g))+
      ggplot2::coord_equal()+
      ggplot2::theme_void()+
      ggplot2::theme(legend.position = "")+
      ggplot2::scale_fill_gradientn(colours = cols)+
      ggplot2::labs(title="Game Boy",
                    subtitle=paste0(round(res),"x",round(height)," - ", ncols, " shades"))
  )
}
