
logo <- function() {
  ### Script to create logos -----------------------------------------------
    library(hexSticker)
    library(png)
    library(grid)

  ### leafpacs logo ---------------------
  # Image originally downloaded from flickr
  # https://flic.kr/p/cXg3uL
  # Biodiversity Heritage Library
  # n560_w1150
  # Flora von Deutschland, Österreich und der Schweiz..Gera,Zezschwitz,1903-. biodiversitylibrary.org/page/12306494
  # This work has been identified as being free of known restrictions under copyright law, including all related and neighboring rights (creativecommons.org/publicdomain/mark/1.0/).

  img <- readPNG(system.file("extdat/images", "branched-bur-reed.png", package = "leafpacs"))
  g <- rasterGrob(img, interpolate = TRUE)

  sticker(g,
          package = "LEAFPACS", p_color = "darkblue", p_x =1 , p_y = 1.1,
          p_size = 25, s_x = 1, s_y = 1, s_width = 3, s_height = 3,
          h_color = "darkgreen",  filename =   "man/figures/leafpacs_logo.png",
          white_around_sticker = T, l_x = 1, l_y = 0.8, spotlight = TRUE,
          p_family = "sans", h_fill = "#4e4098"
  )

  sticker(g,
          package = "LEAFPACS", p_color = "darkblue", p_x =1 , p_y = 1.1,
          p_size = 25, s_x = 1, s_y = 1, s_width = 3, s_height = 3,
          h_color = "darkgreen",  filename = "inst/extdat/images/leafpacs_logo.png",
          white_around_sticker = T, l_x = 1, l_y = 0.8, spotlight = TRUE,
          p_family = "sans", h_fill = "#4e4098"
  )



}
