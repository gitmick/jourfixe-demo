source("params.R")
# Generic row filter. The exclusion is a column, an operator and a value — not a free-text
# predicate — so the opposition changes ONE value rather than reading an expression and
# deciding what it does. `reason` is required: every exclusion here is defensible-sounding
# by construction, so the justification is recorded next to the rule in the team's own words.
P <- declare("qc.filter", "QC", list(
  column = list(default = NULL),
  op     = list(default = "lt", oneof = c("lt", "le", "gt", "ge", "eq", "ne", "in", "not_in")),
  value  = list(default = NULL),
  reason = list(required = TRUE)
))

d <- read.csv("data.csv", check.names = FALSE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
before <- nrow(d)

if (!is.null(P$column)) {
  if (!P$column %in% names(d)) stop(sprintf("qc.filter: no column '%s'; have: %s", P$column, paste(names(d), collapse = ", ")))
  # value = null heisst "keine Grenze". Der Sweep MUSS den ungefilterten Fall als Punkt
  # enthalten koennen — eine Kurve ohne den unbehandelten Nullpunkt zeigt nicht, wovon
  # abgewichen wurde.
  if (is.null(P$value)) {
    message(sprintf("qc.filter: %s ohne Grenze — keine Zeile entfernt  reason: %s", P$column, P$reason))
    write.csv(d, "out.csv", row.names = FALSE, fileEncoding = "UTF-8")
    quit(save = "no", status = 0)
  }
  v <- d[[P$column]]
  w <- if (is.numeric(v)) as.numeric(P$value) else as.character(P$value)
  keep <- switch(P$op,
    lt = v <  w[1], le = v <= w[1], gt = v >  w[1], ge = v >= w[1],
    eq = v == w[1], ne = v != w[1], `in` = v %in% w, not_in = !(v %in% w))
  keep[is.na(keep)] <- TRUE          # a missing value is not evidence for exclusion
  d <- d[keep, , drop = FALSE]
}

write.csv(d, "out.csv", row.names = FALSE, fileEncoding = "UTF-8")
message(sprintf("qc.filter: %s | %d -> %d rows (removed %d, %.2f%%)  reason: %s",
  if (is.null(P$column)) "no rule" else sprintf("%s %s %s", P$column, P$op, paste(P$value, collapse = ",")),
  before, nrow(d), before - nrow(d), 100 * (before - nrow(d)) / before, P$reason))
if (nrow(d) == 0L) stop("qc.filter: the rule removed every row")
