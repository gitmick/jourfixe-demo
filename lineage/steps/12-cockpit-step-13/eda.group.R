source("params.R")
P <- declare("eda.group", "EDA", list(
  by        = list(default = "jahr"),
  value     = list(default = "betrag"),
  aggregate = list(default = "sum", oneof = c("sum", "mean", "median", "count"))
))

d <- read.csv("data.csv", check.names = FALSE, fileEncoding = "UTF-8")
f <- if (P$aggregate == "count") function(v) length(v) else match.fun(P$aggregate)
g <- aggregate(list(wert = d[[P$value]]), by = setNames(list(d[[P$by]]), P$by), FUN = f, na.rm = TRUE)
g <- g[order(g[[P$by]]), , drop = FALSE]

write.csv(g, "out.csv", row.names = FALSE, fileEncoding = "UTF-8")
fmt <- function(x) formatC(x, format = "fg", digits = 5, big.mark = "", drop0trailing = TRUE)
message(sprintf("eda.group: %s (%s of %s), %d groups", P$by, P$aggregate, P$value, nrow(g)))
for (i in seq_len(nrow(g))) message(sprintf("  %-32s %s", g[[P$by]][i], fmt(g$wert[i])))
if (nrow(g) == 2L) message(sprintf("           change: %+.2f%%", 100 * (g$wert[2] - g$wert[1]) / g$wert[1]))

o <- g[order(-g$wert), ]
open_png("plot.png", h = 190 + 30 * nrow(o), mar = c(4.5, 13, 3.6, 6))
bp <- barplot(rev(o$wert), horiz = TRUE, names.arg = shorten(as.character(rev(o[[P$by]])), 26),
              col = JAM_PAL[1], border = NA, cex.names = 0.8,
              xlim = c(0, max(o$wert, na.rm = TRUE) * 1.22),
              xlab = sprintf("%s(%s)", P$aggregate, P$value),
              main = sprintf("%s je %s", P$aggregate, P$by))
text(rev(o$wert), bp, paste0("  ", fmt(rev(o$wert))), pos = 4, xpd = NA, cex = 0.75, col = "#555555")
close_png()
message("  -> plot.png")
