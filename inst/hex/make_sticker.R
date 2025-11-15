# ======================================================================
# likelyr hex sticker generator (serif lime-green ψ beneath curve)
# with correct centering (fix for thin left border)
# ======================================================================

if (!requireNamespace("hexSticker", quietly = TRUE)) install.packages("hexSticker")
if (!requireNamespace("showtext", quietly = TRUE)) install.packages("showtext")
if (!requireNamespace("magick", quietly = TRUE)) install.packages("magick")

library(hexSticker)
library(showtext)
library(ggplot2)
library(magick)

# Load fonts
font_add_google("Inter", "inter")      # main title
font_add_google("IBM Plex Serif", "plexserif") # serif psi
showtext_auto()

# Colors
bg_color   <- "#0A2342"  # navy
border_col <- "#1CA9C9"  # aqua
curve_col  <- "#A4DE02"  # lime green (curve)
psi_col    <- "#A4DE02"  # lime green (psi)

# ------------------------------------------------------------------------------
# Create likelihood curve panel, **with margin fix**
# ------------------------------------------------------------------------------
lik_curve <- ggplot(
  data.frame(x = seq(-2, 2, length.out = 200)),
  aes(x, y = dnorm(x, 0, 2))
) +
  geom_line(color = curve_col, linewidth = 1) +
  theme_void() +
  theme(plot.background = element_rect(fill = bg_color, color = NA))

# ------------------------------------------------------------------------------
# Sticker creation function
# ------------------------------------------------------------------------------
make_sticker <- function(filename) {

  # Step 1 — let hexSticker build the sticker
  sticker(
    subplot      = lik_curve,
    package      = "likelyr",
    p_size       = 34,
    p_color      = "white",
    p_family     = "inter",
    p_y          = 1.65,       # raise title
    s_x          = 1,
    s_y          = 0.9,        # raise curve
    s_width      = 1.05,
    s_height     = 1.25,
    h_size       = 1.6,
    h_color      = border_col,
    h_fill       = bg_color,
    dpi          = 600,
    filename     = filename
  )

  # Step 2 — add ψ symbol
  img <- magick::image_read(filename)

  img <- magick::image_annotate(
    img,
    text    = "ψ",
    size    = 180,
    gravity = "center",
    color   = psi_col,
    font    = "Times New Roman",
    weight  = 700
  )

  magick::image_write(img, path = filename)
}

# ------------------------------------------------------------------------------
# Output
# ------------------------------------------------------------------------------
dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

make_sticker("man/figures/logo.png")
make_sticker("man/figures/logo.svg")

message("Stickers written to man/figures/logo.png and logo.svg")
