source("params.R")
# The block with the most leverage, and the reason is in BLOCKS.md: it pulls the entire framing
# family into the same gesture as everything else. Rule B2 already declares axis limits, bin edges,
# colour scales, smoothing windows, aspect ratio and which series are drawn to be fair game —
# declaring them here is what turns that permission into an affordance. The opposition sets
# `ylim = NULL`, recomputes, and puts the two figures side by side as THE SAME COMPUTATION WITH ONE
# VALUE CHANGED.
#
# Every parameter below is one of the deceptions in article 06, and each is one value to undo:
#   ylim            §The truncated axis   — 268.4 -> 266.2 is two identical bars, or a collapse
#   breaks          §Binning              — cut risk at 2.5 or at 3.0, both are "stratification"
#   errorbars       §Error bars           — four bars, one resting on n = 1
#   aspect          §Aspect ratio         — the least-noticed member of the family
#   palette/midpoint §Colour              — where a diverging scale is centred
#   smooth          §(trend)              — a window wide enough to erase what it smooths
P <- declare("report.figure", "Report", list(
  type      = list(default = "bar", oneof = c("bar", "line", "scatter", "hist")),
  x         = list(default = "jahr"),
  y         = list(default = "wert"),
  series    = list(default = NULL),                       # a grouping column: which series are drawn
  ylim      = list(default = NULL),
  xlim      = list(default = NULL),
  breaks    = list(default = NULL),                       # hist bin edges, or a count
  palette   = list(default = "steel", oneof = c("steel", "diverging", "grey")),
  midpoint  = list(default = NULL),                       # where a diverging scale is centred
  aspect    = list(default = NULL),                       # height/width; NULL = 520/720
  errorbars = list(default = FALSE),
  errorcol  = list(default = NULL),                       # column holding the +/- half-width
  smooth    = list(default = NULL),                       # moving-average window, in points
  title     = list(default = ""),
  scale     = list(default = 1),
  ylab      = list(default = NULL),
  xlab      = list(default = NULL)
))

d <- read.csv("data.csv", check.names = FALSE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
need <- function(col, what) {
  if (is.null(col)) return(invisible(NULL))
  if (!col %in% names(d)) stop(sprintf("report.figure: no column '%s' for %s; have: %s",
                                       col, what, paste(names(d), collapse = ", ")))
}
need(P$y, "y"); need(P$series, "series"); need(P$errorcol, "errorbars")
if (P$type != "hist") need(P$x, "x")

num <- function(v) if (is.null(v)) NULL else as.numeric(v)
v  <- as.numeric(d[[P$y]]) / P$scale
yl <- num(P$ylim); if (!is.null(yl)) yl <- yl / P$scale
xl <- num(P$xlim)
ylab <- if (is.null(P$ylab)) P$y else P$ylab
xlab <- if (is.null(P$xlab)) (if (P$type == "hist") P$y else P$x) else P$xlab

# The aspect ratio is a parameter because it is a deception (06 §Aspect ratio): the same series
# reads as a cliff or a plateau depending on it, and nobody looks at it.
h <- if (is.null(P$aspect)) 520 else round(720 * as.numeric(P$aspect))

# Colour. `diverging` needs a midpoint, and where that midpoint sits is the whole trick — a scale
# centred away from the data's own centre paints ordinary variation as two camps.
cols <- switch(P$palette,
  steel = rep("#4a6fa5", length(v)),
  grey  = rep("#8a8a8a", length(v)),
  diverging = {
    mid <- if (is.null(P$midpoint)) stats::median(v, na.rm = TRUE) else as.numeric(P$midpoint) / P$scale
    ifelse(v >= mid, "#c0563f", "#3f7fc0")
  })

# A moving average, and it is honest about its own effect: a window wide enough will erase anything.
smoothed <- NULL
if (!is.null(P$smooth) && as.integer(P$smooth) > 1L) {
  k <- as.integer(P$smooth)
  smoothed <- stats::filter(v, rep(1 / k, k), sides = 2)
  say_smooth <- sprintf("  smooth=%d points (%.0f%% of the series)", k, 100 * k / length(v))
} else say_smooth <- ""

png("figure.png", width = 720, height = h, res = 110)
par(mar = c(4.2, 5, if (nzchar(P$title)) 3.5 else 1.5, 1))

if (P$type == "hist") {
  br <- if (is.null(P$breaks)) "Sturges" else if (length(P$breaks) > 1) num(P$breaks) else as.integer(P$breaks)
  hist(v, breaks = br, col = cols[1], border = "white", main = P$title, xlab = xlab, ylab = "count")
} else if (P$type == "bar") {
  top <- if (is.null(yl)) c(0, max(v, na.rm = TRUE) * 1.15) else yl
  # barplot DROPS a label that does not fit, silently — a bar with no name, in a figure about
  # misleading figures. Shrink until they all fit, and turn them upright if that is not enough.
  labs <- as.character(d[[P$x]])
  # Deterministic, not clever: the width a label needs is roughly its character count times the
  # slot it gets. If the labels together want more room than the axis has, stand them upright.
  # Guessing with a fudge factor was tried and silently kept dropping one — which is the failure
  # this exists to prevent.
  vertical <- sum(nchar(labs)) * 7 > 640 || max(nchar(labs)) > 12
  cx <- if (vertical) 0.8 else min(1, 90 / max(sum(nchar(labs)), 1))
  if (vertical) par(mar = c(max(6, max(nchar(labs)) * 0.5), 5, if (nzchar(P$title)) 3.5 else 1.5, 1))
  bp <- barplot(v, names.arg = labs, ylim = top, xpd = FALSE, las = if (vertical) 2L else 0L,
                cex.names = cx, col = cols, border = NA, ylab = ylab,
                xlab = if (vertical) "" else xlab, main = P$title)
  # Error bars are drawn only when asked for, and their ABSENCE is the finding (06 §Error bars):
  # four bars with no whiskers, one of them resting on a single observation.
  if (isTRUE(P$errorbars) && !is.null(P$errorcol)) {
    e <- as.numeric(d[[P$errorcol]]) / P$scale
    arrows(bp, v - e, bp, v + e, angle = 90, code = 3, length = 0.05, col = "#333333", xpd = NA)
  }
  text(bp, v, sprintf("%.1f", v), pos = 3, xpd = NA, cex = 0.85)
} else {
  xv <- if (is.numeric(d[[P$x]])) d[[P$x]] else seq_along(v)
  grp <- if (is.null(P$series)) rep(1L, length(v)) else as.integer(factor(d[[P$series]]))
  plot(xv, v, type = if (P$type == "line") "b" else "p", pch = 19,
       ylim = yl, xlim = xl, xlab = xlab, ylab = ylab, main = P$title,
       col = if (is.null(P$series)) cols else grDevices::hcl.colors(max(grp), "Dark 3")[grp])
  if (!is.null(smoothed)) lines(xv, smoothed, col = "#c0563f", lwd = 2)
  if (isTRUE(P$errorbars) && !is.null(P$errorcol)) {
    e <- as.numeric(d[[P$errorcol]]) / P$scale
    arrows(xv, v - e, xv, v + e, angle = 90, code = 3, length = 0.05, col = "#333333")
  }
  if (!is.null(P$series)) legend("topright", legend = levels(factor(d[[P$series]])), bty = "n",
                                 col = grDevices::hcl.colors(max(grp), "Dark 3"), pch = 19)
}
invisible(dev.off())

# What was drawn, and — the part that matters for a Defensio — what was NOT.
message(sprintf("report.figure: figure.png %dx%d  type=%s  n=%d", 720, h, P$type, length(v)))
message(sprintf("  ylim=%s%s", if (is.null(P$ylim)) "NULL (full axis)" else paste(P$ylim, collapse = "-"),
                if (!is.null(P$ylim)) "  <- the axis is truncated" else ""))
if (!isTRUE(P$errorbars)) message("  errorbars=FALSE  <- uncertainty is not drawn")
if (nzchar(say_smooth)) message(say_smooth)
