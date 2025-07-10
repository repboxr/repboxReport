#' Generate a set of visually distinct colors
#'
#' Uses a Halton sequence to generate colors with distinct hues, suitable for
#' color-coding different categories.
#'
#' @param n The number of colors to generate.
#' @param h.range Range of hue (0-360).
#' @param s.range Range of saturation (0-1).
#' @param l.range Range of lightness (0-1).
#' @return A vector of hexadecimal color strings.
#' @export
rr_make_distinct_colors = function(n, h.range=c(15, 345), s.range=c(0.6, 0.9), l.range=c(0.7, 0.85)) {
  if (n == 0) return(character(0))
  if (n == 1) return(rr_hsl_to_rgb(h.range[1], s.range[2], l.range[2]))

  # Use Halton sequence for nicely spaced values
  halton_seq <- randtoolbox::halton(n, dim = 2)

  h <- h.range[1] + (h.range[2] - h.range[1]) * halton_seq[, 1]
  s <- s.range[1] + (s.range[2] - s.range[1]) * halton_seq[, 2]
  l <- l.range[1] + (l.range[2] - l.range[1]) * halton_seq[, 2]

  rr_hsl_to_rgb(h, s, l)
}

#' Convert HSL color values to RGB
#'
#' @param h Hue (0-360).
#' @param s Saturation (0-1).
#' @param l Lightness (0-1).
#' @return A hexadecimal color string.
#' @export
rr_hsl_to_rgb <- function(h, s, l) {
  h <- h / 360

  hue_to_rgb <- function(p, q, t) {
    t[t < 0] <- t[t < 0] + 1
    t[t > 1] <- t[t > 1] - 1

    ifelse(t < 1/6, p + (q - p) * 6 * t,
      ifelse(t < 1/2, q,
        ifelse(t < 2/3, p + (q - p) * (2/3 - t) * 6, p)
      )
    )
  }

  q <- ifelse(l < 0.5, l * (1 + s), l + s - l * s)
  p <- 2 * l - q

  r <- hue_to_rgb(p, q, h + 1/3)
  g <- hue_to_rgb(p, q, h)
  b <- hue_to_rgb(p, q, h - 1/3)

  grDevices::rgb(r, g, b)
}
