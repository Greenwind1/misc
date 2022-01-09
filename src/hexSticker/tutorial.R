library(hexSticker)
library(ggplot2)

library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Gochi Hand", "gochi")
## Automatically use showtext to render text for future devices
showtext_auto()


s <- sticker(
  subplot = ~ plot(cars, cex=.5, cex.axis=.5, mgp=c(0,.3,0), xlab="", ylab=""),
  s_x = .8, s_y = .6, s_width = 1.4, s_height = 1.2,
  package = "hexSticker1",
  p_size = 20,
  filename = "fig/hexSticker1.jpg"
  )

plot(s)  # generic function


sticker(
  "fig/uni.jpg",
  s_x = 1., s_y = .75, s_width = .4, s_height = .35,
  package = "Maxwell",
  p_x = 1, p_y = 1.4, p_color = "#ff1493", 
  p_family = "gochi", p_size = 20, 
  h_size = 1.2, h_fill = "#9400d3", h_color = "#ff1493", 
  spotlight = FALSE,
  # l_x = 1, l_y = 0.5, l_width = 3, l_height = 3, l_alpha = 0.4, 
  url = "https://twitter.com/Maxwell_110", 
  u_x = 1.25, u_y = 0.08, u_color = "#ff1493", 
  u_family = "gochi", u_size = 3, u_angle = 30, 
  white_around_sticker = FALSE,
  filename = "fig/maxwell.png",
  asp = 1,
  dpi = 300
)
