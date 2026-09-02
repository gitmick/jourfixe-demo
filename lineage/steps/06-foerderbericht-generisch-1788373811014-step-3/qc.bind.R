source("params.R")
# Stack several tables into one. Columns are unioned; a table missing a column gets NA.
# The pipeline links each upstream out.csv in under the slot names listed in `files`.
P <- declare("qc.bind", "QC", list(
  files = list(default = c("a.csv", "b.csv"))
))

parts <- list()
for (f in P$files) {
  if (!file.exists(f)) stop(sprintf("qc.bind: no input '%s'", f))
  parts[[f]] <- read.csv(f, check.names = FALSE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
}
cols <- unique(unlist(lapply(parts, names)))
parts <- lapply(parts, function(p) { for (cl in setdiff(cols, names(p))) p[[cl]] <- NA; p[, cols, drop = FALSE] })
d <- do.call(rbind, parts)

write.csv(d, "out.csv", row.names = FALSE, fileEncoding = "UTF-8")
message(sprintf("qc.bind: %s -> %d rows x %d cols",
  paste(sprintf("%s(%d)", P$files, vapply(parts, nrow, 0L)), collapse = " + "), nrow(d), ncol(d)))
