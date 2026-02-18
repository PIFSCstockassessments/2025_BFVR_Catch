draw_key_image <- function(data, params, size) {
  kt <- getOption("ggimage.keytype")
  if (is.null(kt) || length(kt) == 0) kt <- "image"
  
  if (kt == "image") {
    tryCatch({
      img <- magick::image_read("www/fishicon.png")
      
      grobs <- lapply(seq_along(data$colour), function(i) {
        colored_img <- ggimage:::color_image(img, data$colour[i], data$alpha[i])
        grid::rasterGrob(image = colored_img, width = unit(1, "npc"), height = unit(1, "npc"))
      })
      
      class(grobs) <- "gList"
      keyGrob <- ggplot2:::ggname("image_key", grid::gTree(children = grobs))
      
      return(keyGrob)
    }, error = function(e) {
      cat("Error in draw_key_image:", e$message, "\n")
      return(ggplot2::draw_key_point(data, params, size))
    })
  }
  
  ggplot2::draw_key_point(data, params, size)
}
