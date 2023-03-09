invisible(library(ggplot2))
invisible(library(extrafont))  # "Candara"

# Package management ----
# renv::snapshot()

# colors ----
crgb <- col2rgb(cc <- colors())
colnames(crgb) <- cc
# head(t(crgb))
# display_carto_all()
# RColorBrewer::display.brewer.all()
# ggsci::scale_color_npg()

col.tw <- rgb(219, 215, 210, maxColorValue = 255)   # TimberWolf
col.os <- rgb(65, 74, 76, maxColorValue = 255)      # Outer Space
col.r.p <- rgb(120, 81, 169, maxColorValue = 255)   # Royal Purple
col.m.b <- rgb(26, 72, 118, maxColorValue = 255)    # Midnight Blue
col.p.b <- rgb(28, 169, 201, maxColorValue = 255)   # Pacific Blue
col.c.b <- rgb(176, 183, 198, maxColorValue = 255)  # Cadet Blue
col.el <- rgb(206, 255, 29, maxColorValue = 255)    # Electric Lime
col.c.g <- rgb(0, 204, 153, maxColorValue = 255)    # Caribbean Green
col.mt <- rgb(255, 130, 67, maxColorValue = 255)    # Mango Tango
col.rm <- rgb(227, 37, 107, maxColorValue = 255)    # RazzMatazz
col.sl <- rgb(252, 40, 71, maxColorValue = 255)     # ScarLet

col.plos.yellow <- rgb(214, 225, 61, maxColorValue = 255)   # PLOS Yellow
col.plos.pink <- rgb(207, 0, 163, maxColorValue = 255)      # PLOS Pink

col.bmc.gray.p <- rgb(255, 253, 246, maxColorValue = 255)   # BMC Pale Gray
col.bmc.purple <- rgb(191, 18, 248, maxColorValue = 255)    # BMC Purple
col.bmc.navy <- rgb(26, 46, 79, maxColorValue = 255)        # BMC Navy Blue
col.bmc.blue <- rgb(1, 74, 129, maxColorValue = 255)        # BMC Blue
col.bmc.sky <- rgb(15, 153, 190, maxColorValue = 255)       # BMC Sky Blue
col.bmc.green.d <- rgb(0, 73, 64, maxColorValue = 255)      # BMC Deep Green
col.bmc.green.l <- rgb(0, 203, 170, maxColorValue = 255)    # BMC Light Green
col.bmc.pink <- rgb(239, 37, 95, maxColorValue = 255)       # BMC Pink

col.sage.purple <- rgb(122, 107, 130, maxColorValue = 255)  # SAGE Purple
col.sage.red <- rgb(175, 33, 38, maxColorValue = 255)       # SAGE Red
col.sage.gray <- rgb(238, 238, 238, maxColorValue = 255)    # SAGE Gray

# https://www.sciencedirect.com/journal/psychiatry-research
col.pr.pale.blue <- rgb(233, 243, 255, maxColorValue = 255) # Pale Blue
col.pr.gray <- rgb(245, 245, 245, maxColorValue = 255)      # Pale Gray
col.pr.black <- rgb(30, 20, 20, maxColorValue = 255)        # Dark Gray
col.pr.pink <- rgb(249, 175, 168, maxColorValue = 255)      # Pink (theme)
col.pr.orange <- rgb(255, 138, 68, maxColorValue = 255)     # Orange (theme)
col.pr.blue <- rgb(0, 114, 151, maxColorValue = 255)        # Blue (theme)

# fonts ----
# extrafont::fonttable()
font.base <- "Candara"
# font.base <- "Georgia"
# font.base <- "Times New Roman"
ggplot2::theme_set(theme_minimal(base_family = font.base))


# theme ----
font.face <- "italic"
ggplot2::theme_update(
    title = element_text(face = font.face, color = col.os, size = 9), 
    plot.subtitle = element_text(face = font.face, color = col.os, size = 7), 
    text = element_text(face = font.face, color = col.os, size = 10), 
    plot.caption = element_text(color = "gray77", size = 5), 
    axis.title = element_text(face = font.face, color = col.os), 
    axis.text = element_text(face = font.face, color = col.os), 
    panel.grid.major = element_line(size = 0.25), 
    panel.grid.minor = element_blank(), 
    legend.position = c(0.9, 0.9), 
    legend.text = element_text(size = 6), 
    legend.key.size = unit(0.04, "npc")
)


# dplyr ----
options(dplyr.summarise.inform = TRUE)


# ggplot2
png_to_grob <-function(png.file.name = "fig/twitter.png",
                       width = unit(1.20 * 3, "cm"),
                       height = unit(0.99 * 3, "cm"),
                       interpolate = FALSE) {
    # Usage: 
    # Add a code line below
    # + annotation_custom(
    #   grob = icon.grob, xmin = 1, xmax = 1, ymin = 1, ymax = 1
    # )
    # +
    # annotate(geom = "text", x = 1, y = 1, label = "@Maxwell_110", 
    #   alpha = 0.5, size = 4, family = "Candara", color = col.tw)
    
    icon.arr <- png::readPNG(png.file.name)
    icon.grob <- grid::rasterGrob(icon.arr, 
                                  width = width,
                                  height = height,
                                  interpolate = interpolate)
    return(icon.grob)
}

# plot(1:25, rep(1, 25), pch = c(1:25), cex = 2,)
# text(1:25, rep(1, 25), pos = 1, labels = c(1:25))
