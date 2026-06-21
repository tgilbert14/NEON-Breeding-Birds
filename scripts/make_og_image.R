#----------------------------------------------------------------------
# make_og_image.R — draws docs/og-image.png (1200x630), the social card for
# the landing page. Self-contained base-R graphics in the "Field Guide" house
# palette (parchment + ink + rust + goldfinch), with a dawn-sky band and a faint
# scatter of birds in flight — deliberately distinct from the mammal app's navy
# card. Regenerate:
#   "C:\Program Files\R\R-4.3.1\bin\Rscript.exe" scripts/make_og_image.R
#----------------------------------------------------------------------
ROOT <- getwd()
out  <- file.path(ROOT, "docs", "og-image.png")
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)

parch <- "#f3e9d2"; paper <- "#fbf5e6"; ink <- "#2b2722"; ink2 <- "#5a4f3e"
rust  <- "#c1502e"; gold  <- "#e8a317"; green <- "#1a7f37"
dawn  <- grDevices::colorRampPalette(c("#f6c89a", "#efb07f", "#d99a96", "#b39bb0", "#8fb0c9"))(1200)

png(out, width = 1200, height = 630, res = 144)
op <- par(mar = c(0, 0, 0, 0), bg = parch); on.exit({ par(op); dev.off() })
plot.new(); plot.window(xlim = c(0, 1200), ylim = c(0, 630), xaxs = "i", yaxs = "i")

# parchment base + a faint ruled-paper texture (field-guide page)
rect(0, 0, 1200, 630, col = parch, border = NA)
for (yy in seq(40, 600, by = 26)) segments(0, yy, 1200, yy, col = grDevices::adjustcolor(ink, .025), lwd = 1)

# dawn-sky band across the top (the hero motif), as 1px vertical gradient rects
for (i in 1:1200) rect(i - 1, 486, i, 630, col = dawn[i], border = NA)
# soft feather of the band into the page
for (k in 0:30) rect(0, 486 - k, 1200, 487 - k, col = grDevices::adjustcolor(parch, k / 30 * 0.9), border = NA)

# a faint scatter of birds in flight (gull silhouettes) for texture
gull <- function(x, y, s, col, lwd = 2) {
  xs <- x + c(-s, -s * 0.45, 0, s * 0.45, s)
  ys <- y + c(0, s * 0.5, s * 0.16, s * 0.5, 0)
  lines(xs, ys, col = col, lwd = lwd)
}
set.seed(7)
for (k in 1:11) gull(runif(1, 90, 1120), runif(1, 90, 470), runif(1, 9, 20),
                     grDevices::adjustcolor(ink, runif(1, .05, .11)), lwd = 2)

# badge
text(70, 556, "NEON · BREEDING LANDBIRD POINT COUNTS · DP1.10003.001",
     col = "#7a3a22", cex = .9, font = 2, adj = 0)

# title
text(68, 470, "NEON Breeding Bird", col = ink, cex = 3.5, font = 2, adj = 0)
text(68, 394, "Explorer",           col = ink, cex = 3.5, font = 2, adj = 0)
# a small goldfinch in flight next to the wordmark
gull(452, 404, 22, gold, lwd = 4)

# subtitle
text(70, 322, "Who's singing where, 46 NEON sites from arctic tundra to Caribbean dry",
     col = ink2, cex = 1.12, adj = 0)
text(70, 292, "forest, on real breeding-season point-count data. Honest stats.",
     col = ink2, cex = 1.12, adj = 0)

# stat chips
chips <- list(c("46", "field sites"), c("555", "species"),
              c("29k", "point-counts"), c("real", "public data"))
x0 <- 70; gap <- 14; w <- 250; h <- 96; y1 <- 64
for (i in seq_along(chips)) {
  xl <- x0 + (i - 1) * (w + gap)
  rect(xl, y1, xl + w, y1 + h, col = paper, border = grDevices::adjustcolor(ink, .12))
  rect(xl, y1, xl + 6, y1 + h, col = rust, border = NA)                 # rust spine
  text(xl + 22, y1 + 62, chips[[i]][1], col = ink, cex = 1.95, font = 2, adj = 0)
  text(xl + 22, y1 + 28, chips[[i]][2], col = ink2, cex = .96, adj = 0)
}
cat("wrote", out, "\n")
