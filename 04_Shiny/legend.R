draw_key_image <- function(data, params, size) {
  kt <- getOption("ggimage.keytype")
  if (is.null(kt) || length(kt) == 0) kt <- "image"  # fallback
  
  if (kt == "image") {
    img <- magick::image_read("~/R/2025_BFVR_Catch-feat/04_Shiny/fishicon1.png")
    
    grobs <- lapply(seq_along(data$colour), function(i) {
      colored_img <- ggimage:::color_image(img, data$colour[i], data$alpha[i])
      grid::rasterGrob(image = colored_img, width = unit(1, "npc"), height = unit(1, "npc"))
    })
    
    class(grobs) <- "gList"
    keyGrob <- ggplot2:::ggname("image_key", grid::gTree(children = grobs))
    
    return(keyGrob)
  }
  
  ggplot2::draw_key_point(data, params, size)  # fallback
}
