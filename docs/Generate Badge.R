require(hexSticker)
  imgurl <- "../../docs/media/sentimentai_alpha.png"
library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Nanum Gothic Coding", "nanum")
## Automatically use showtext to render text for future devices
showtext_auto()

outline_color = "#fdb5f9"
fill_color    = "#fff5fe"

sticker(imgurl, package="",
        p_size=20, p_x=1, p_y=1.05,
        p_color = "#09001c", p_family = "nanum",
        s_x=1, s_y=.9, s_height = 1, s_width = 1,
        h_color = outline_color, h_fill = fill_color,
        spotlight = FALSE, l_x=1, l_y=1, l_alpha = .2,
        filename="inst/figures/sentiment.ai.png")

sticker(imgurl, package="",
        p_size=20, p_x=1, p_y=1.05,
        p_color = "#09001c", p_family = "nanum",
        s_x=1, s_y=.9, s_height = 1, s_width = 1,
        h_color = outline_color, h_fill = fill_color,
        spotlight = FALSE, l_x=1, l_y=1, l_alpha = .2,
        filename="../../docs/media/sentimentai_alpha_hex.png")
