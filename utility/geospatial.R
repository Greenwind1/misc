# Utility for geo-spatial analysis
# https://andrewpwheeler.com/2019/08/07/making-a-hexbin-map-in-ggplot/

arrow.df <- function(xb, yb, len) {
    s <- len
    arrow.x = c(0, 0.5, 1, 0.5, 0) - 0.5
    arrow.y = c(0, 1.7  , 0, 0.5, 0)
    adata <- data.frame(aX = xb + arrow.x * s, aY = yb + arrow.y * s)
    return(adata)
}


scale.df <- function(llx, lly, len, height) {
    box1 <- data.frame(
        x = c(llx, llx + len, llx + len, llx, llx),
        y = c(lly, lly, lly + height, lly + height, lly)
    )
    box2 <- data.frame(
        x = c(llx - len, llx, llx, llx - len, llx - len),
        y = c(lly, lly, lly + height, lly + height, lly)
    )
    return(list(box1, box2))
}


# Example

# x_cent <- 830000
# len_bar <- 3000
# offset_scaleNum <- 64300
# 
# arrow <- arrow_data(xb = x_cent, yb = 67300, len = 2500)
# 
# scale_bxs <- scale_data(
#     llx = x_cent,
#     lly = 65000,
#     len = len_bar,
#     height = 750
# )
# 
# label_data <- data.frame(
#     x = c(x_cent, x_cent - len_bar, x_cent, x_cent + len_bar, x_cent), 
#     y = c(72300, offset_scaleNum, offset_scaleNum, offset_scaleNum, 66500), 
#     lab = c("N", "0", "3", "6", "Kilometers")
# )
# 
# base_map <- ggplot() +
#     geom_polygon(
#         data = PhilBound,
#         size = 1.5,
#         color = 'black',
#         fill = 'darkgrey',
#         aes(x = long, y = lat)
#     ) +
#     geom_polygon(data = arrow, fill = 'black', aes(x = aX, y = aY)) +
#     geom_polygon(data = scale_bxs[[1]],
#                  fill = 'grey',
#                  color = 'black',
#                  aes(x = x,  y = y)) +
#     geom_polygon(data = scale_bxs[[2]],
#                  fill = 'white',
#                  color = 'black',
#                  aes(x = x, y = y)) +
#     geom_text(data = lab_data, size = 4, aes(x = x, y = y, label = lab)) +
#     coord_fixed() + theme_void()
