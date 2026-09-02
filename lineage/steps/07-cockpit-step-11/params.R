# Block prelude — parameters are a declared input, not code.
#
# Every block sources this, calls declare() once with its parameter table, and
# then reads values off `P`. The prelude:
#   1. reads params.json from the step's working directory,
#   2. rejects any key the block did not declare (a typo is an error, not a default),
#   3. fills declared defaults for keys the file omits,
#   4. writes params.used.json — the resolved set, as a declared OUTPUT, so the
#      values are readable in the lineage graph without resolving an input hash.
#
# No package dependencies: uses jsonlite when the image has it, otherwise a
# small reader for the flat JSON these files are.

.have_jsonlite <- requireNamespace("jsonlite", quietly = TRUE)

.json_tokens <- function(s) {
  out <- character(0); i <- 1L; n <- nchar(s)
  while (i <= n) {
    ch <- substr(s, i, i)
    if (grepl("[[:space:]]", ch)) { i <- i + 1L; next }
    if (ch %in% c("{", "}", "[", "]", ":", ",")) { out <- c(out, ch); i <- i + 1L; next }
    if (ch == '"') {
      j <- i + 1L; buf <- ""
      while (j <= n) {
        c2 <- substr(s, j, j)
        if (c2 == "\\") { buf <- paste0(buf, substr(s, j + 1L, j + 1L)); j <- j + 2L; next }
        if (c2 == '"') break
        buf <- paste0(buf, c2); j <- j + 1L
      }
      out <- c(out, paste0("\001", buf)); i <- j + 1L; next
    }
    j <- i
    while (j <= n && !grepl('[][{}:,[:space:]]', substr(s, j, j))) j <- j + 1L
    out <- c(out, paste0("\002", substr(s, i, j - 1L))); i <- j
  }
  out
}

.json_value <- function(st) {
  t <- st$tok[st$i]; st$i <- st$i + 1L
  if (t == "{") {
    res <- list()
    if (st$tok[st$i] == "}") { st$i <- st$i + 1L; return(res) }
    repeat {
      k <- substring(st$tok[st$i], 2L); st$i <- st$i + 2L   # key, then ':'
      res[[k]] <- .json_value(st)
      if (st$tok[st$i] == ",") { st$i <- st$i + 1L; next }
      st$i <- st$i + 1L; break                              # '}'
    }
    return(res)
  }
  if (t == "[") {
    res <- list()
    if (st$tok[st$i] == "]") { st$i <- st$i + 1L; return(unlist(res)) }
    repeat {
      res[[length(res) + 1L]] <- .json_value(st)
      if (st$tok[st$i] == ",") { st$i <- st$i + 1L; next }
      st$i <- st$i + 1L; break                              # ']'
    }
    return(unlist(res))
  }
  if (substr(t, 1L, 1L) == "\001") return(substring(t, 2L))
  lit <- substring(t, 2L)
  if (lit %in% c("null", "NA", "NaN")) return(NULL)
  if (lit == "true")  return(TRUE)
  if (lit == "false") return(FALSE)
  as.numeric(lit)
}

read_json <- function(path) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  if (.have_jsonlite) return(jsonlite::fromJSON(txt, simplifyVector = TRUE))
  st <- new.env(); st$tok <- .json_tokens(txt); st$i <- 1L
  .json_value(st)
}

.json_scalar <- function(v) {
  if (is.null(v) || length(v) == 0L) return("null")
  if (length(v) == 1L && is.na(v)) return("null")
  if (length(v) > 1L) return(paste0("[", paste(vapply(v, .json_scalar, ""), collapse = ", "), "]"))
  if (is.logical(v)) return(if (v) "true" else "false")
  if (is.numeric(v)) return(format(v, scientific = FALSE, trim = TRUE))
  paste0('"', gsub('"', '\\\\"', v), '"')
}

write_json <- function(x, path) {
  body <- vapply(names(x), function(k) paste0('  "', k, '": ', .json_scalar(x[[k]])), "")
  writeLines(c("{", paste(body, collapse = ",\n"), "}"), path)
}

# declare(block, stage, params) -> the resolved parameter list.
#   params: named list of list(type=, default=, required=, oneof=)
declare <- function(block, stage, params, file = "params.json") {
  stages <- c("QC", "EDA", "Model", "Model Performance", "Report")
  if (!stage %in% stages) stop(sprintf("declare: stage '%s' is not one of: %s", stage, paste(stages, collapse = ", ")))

  given <- if (file.exists(file)) read_json(file) else list()
  given$stage <- NULL; given$block <- NULL       # carried alongside, not parameters

  unknown <- setdiff(names(given), names(params))
  if (length(unknown)) {
    stop(sprintf("%s: undeclared parameter(s): %s\n  declared: %s",
                 block, paste(unknown, collapse = ", "), paste(names(params), collapse = ", ")))
  }

  P <- list()
  for (k in names(params)) {
    spec <- params[[k]]
    has  <- k %in% names(given)
    if (!has && isTRUE(spec$required)) stop(sprintf("%s: required parameter '%s' not set", block, k))
    v <- if (has) given[[k]] else spec$default
    if (!is.null(v) && !is.null(spec$oneof) && !all(v %in% spec$oneof)) {
      stop(sprintf("%s: %s = %s; allowed: %s", block, k, paste(v, collapse = ","), paste(spec$oneof, collapse = ", ")))
    }
    P[[k]] <- v
  }

  used <- P
  used$block <- block
  used$stage <- stage
  write_json(used, "params.used.json")
  message(sprintf("[%s / %s] %s", block, stage,
                  paste(vapply(names(P), function(k) paste0(k, "=", .json_scalar(P[[k]])), ""), collapse = "  ")))
  P
}

# --- Plot-Helfer -------------------------------------------------------------
# Jeder EDA-Block schreibt eine plot.png neben seiner CSV: die Ansicht gehoert zum
# Hinschauen. report.figure bleibt der Report-Block fuer die veroeffentlichte Abbildung.
JAM_PAL <- c("#4a6fa5", "#b3701a", "#1b7f6f", "#7a3b4a", "#5c5470", "#8a8f3c",
             "#a4515f", "#3c6e71", "#9c6644", "#4f6d7a")

open_png <- function(file = "plot.png", w = 960, h = 640, res = 110, mar = c(4.5, 5, 3.6, 1.5)) {
  png(file, width = w, height = h, res = res)
  par(mar = mar, mgp = c(2.9, 0.7, 0), las = 1, bty = "n",
      col.axis = "#444444", col.lab = "#444444", col.main = "#111111",
      cex.main = 1.05, font.main = 1)
}
close_png <- function() invisible(dev.off())
pal <- function(n) rep(JAM_PAL, length.out = n)
shorten <- function(x, n = 22) ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "…"), x)
