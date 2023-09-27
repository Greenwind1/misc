invisible(pacman::p_load(ggplot2, extrafont, ggtext, showtext, stringr))
# extrafont::font_import()
# extrafont::loadfonts()

# Package management ----
# renv::snapshot()

# colors ----
crgb <- col2rgb(cc <- colors())
colnames(crgb) <- cc
# head(t(crgb))
# display_carto_all()
# RColorBrewer::display.brewer.all()
# ggsci::scale_color_npg()

col.tw <- "#DBD7D2"     # TimberWolf
col.c.b <- "#B0B7C6"    # Cadet Blue
col.os <- "#414A4C"     # Outer Space
col.r.p <- "#7851A9"    # Royal Purple
col.m.b <- "#1A4876"    # Midnight Blue
col.p.b <- "#1CA9C9"    # Pacific Blue
col.c.g <- "#00CC99"    # Caribbean Green
col.el <- "#CEFF1D"     # Electric Lime
col.mt <- "#FF8243"     # Mango Tango
col.rm <- "#E3256B"     # RazzMatazz
col.sl <- "#FC2847"     # ScarLet

col.plos.yellow <- "#D6E13D"    # PLOS Yellow
col.plos.pink <- "#CF00A3"      # PLOS Pink

col.bmc.gray.p <- "#FFFDF6"     # BMC Pale Gray
col.bmc.purple <- "#BF12F8"     # BMC Purple
col.bmc.navy <- "#1A2E4F"       # BMC Navy Blue
col.bmc.blue <- "#014A81"       # BMC Blue
col.bmc.sky <- "#0F99BE"        # BMC Sky Blue
col.bmc.green.d <- "#004940"    # BMC Deep Green
col.bmc.green.l <- "#00CBAA"    # BMC Light Green
col.bmc.pink <- "#EF255F"       # BMC Pink

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
# font.base <- "Candara"
# font.base <- "Georgia"
font.base <- "Times New Roman"
ggplot2::theme_set(theme_minimal(base_family = font.base))

# fonts for showtext ----
sysfonts::font_add("fa6-brands", "fonts/Font Awesome 6 Brands-Regular-400.otf")
sysfonts::font_add_google(name = "lusitana")
sysfonts::font_add_google(name = "cinzel")
font.base.showtext <- "cinzel"


# theme ----
# font.face <- "italic"
font.face <- "plain"
ggplot2::theme_update(
    title = element_text(face = font.face, color = col.os, size = 9), 
    plot.subtitle = element_text(face = font.face, color = col.os, size = 7), 
    text = element_text(face = font.face, color = col.os, size = 10), 
    plot.caption = element_text(color = "gray30", size = 12),
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


# Twitter image ----
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


# Twitter and GitHub fonts ----
x.glue  <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")  # Twitter
gh.glue <- str_glue("<span style='font-family:fa6-brands'>&#xf09b;</span>")  # GitHub

# caption_text  <- str_glue("{x.glue} @Maxwell_110 &bull; {gh.glue} Greenwind1")
# showtext_auto(enable = TRUE)  # necessary to show awesome icons
# 
# ggplot() + labs(caption = caption_text) +
#     theme(
#         plot.caption = element_markdown(
#             lineheight = 0.6, hjust = 0.5, halign = 0.5, margin = margin(t = 10, b = 10)
#         )
#     )

# sessioninfo::session_info(include_base = TRUE)
