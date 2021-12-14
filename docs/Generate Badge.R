require(hexSticker)
  imgurl <- "../../docs/media/PNGIX.com_companion-cube-png.png"
library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Nanum Gothic Coding", "nanum")
## Automatically use showtext to render text for future devices
showtext_auto()

sticker(imgurl, package="sentiment.ai",
        p_size=20, p_x=1, p_y=1.05,
        p_color = "#09001c", p_family = "nanum",
        s_x=1, s_y=1, s_height = .6, s_width = .6,
        h_color = "#3378c4", h_fill = "#ffffff",
        spotlight = FALSE, l_x=1, l_y=1, l_alpha = .2,
        filename="inst/figures/sentiment.ai.png")
