source("params.R")
P <- declare("qc.read", "QC", list(
  file       = list(required = TRUE),
  delimiter  = list(default = "auto", oneof = c("auto", ",", ";", "\t", "|")),
  decimal    = list(default = "auto", oneof = c("auto", ".", ",")),
  encoding   = list(default = "UTF-8"),
  na_strings = list(default = c("", "NA")),
  skip       = list(default = 0),
  add_column = list(default = NULL),   # name of a constant column to add (e.g. "jahr")
  add_value  = list(default = NULL),
  columns    = list(default = NULL),   # nur diese Spalten behalten (Reihenfolge wie angegeben)
  on_ragged  = list(default = "error", oneof = c("error", "skip", "fill"))
))

con <- file(P$file, encoding = P$encoding); on.exit(close(con), add = TRUE)
head_lines <- readLines(con, n = P$skip + 5L, warn = FALSE)
probe <- head_lines[P$skip + 1L]
probe <- sub("^﻿", "", probe)

delim <- P$delimiter
if (delim == "auto") {
  counts <- vapply(c(";", ",", "\t", "|"), function(d) lengths(regmatches(probe, gregexpr(d, probe, fixed = TRUE))), 0L)
  delim <- names(counts)[which.max(counts)]
  if (max(counts) == 0L) delim <- ","
}

enc <- if (identical(P$encoding, "UTF-8")) "UTF-8-BOM" else P$encoding
# Zerlegbarkeit zuerst, Inhalt danach. read.table(fill = TRUE) fuellt kaputte Zeilen still auf
# und macht aus einem Parse-Fehler plausible Daten: CORDIS project.csv liefert so 35 674 statt
# 35 389 Zeilen, 193 doppelte ids und 20 leere — und nichts warnt. Deshalb ist die Behandlung
# hier ein deklarierter Parameter mit "error" als Vorgabe.
nf <- count.fields(P$file, sep = delim, quote = "\"", comment.char = "", skip = P$skip)
soll <- nf[1]
schlecht <- which(nf != soll)                            # die Kopfzeile definiert soll, steht also nie drin
if (length(schlecht)) {
  if (P$on_ragged == "error")
    stop(sprintf(paste("qc.read: %d von %d Zeilen haben nicht %d Felder (erste: Zeile %d).",
                       "Setze on_ragged auf 'skip' (Zeilen verwerfen, Anzahl wird berichtet)",
                       "oder 'fill' (auffuellen — erzeugt Phantomzeilen).", sep = "\n  "),
                 length(schlecht), length(nf) - 1L, soll, schlecht[1]))
  message(sprintf("qc.read: ACHTUNG %d von %d Zeilen sind nicht zerlegbar (%.2f%%), Behandlung: %s",
                  length(schlecht), length(nf) - 1L, 100 * length(schlecht) / (length(nf) - 1L), P$on_ragged))
}
# Einlesen. Bei sauberen Dateien read.table; sind Saetze kaputt, wird ueber scan() Feld fuer Feld
# gelesen und anhand der Satzlaengen aus count.fields zerlegt — count.fields kennt die Satzgrenzen
# (es respektiert Anfuehrungszeichen mit Zeilenumbruechen darin) und sagt genau, welcher Satz nicht
# passt. Nur so ist "verwerfen" ehrlich: es trifft die 284 kaputten Saetze und keinen anderen.
if (!length(schlecht)) {
  d <- read.table(P$file, sep = delim, header = TRUE, quote = "\"", comment.char = "",
                  colClasses = "character", check.names = FALSE, skip = P$skip,
                  fileEncoding = enc, na.strings = P$na_strings)
} else if (P$on_ragged == "fill") {
  d <- read.table(P$file, sep = delim, header = TRUE, quote = "\"", comment.char = "",
                  colClasses = "character", check.names = FALSE, skip = P$skip,
                  fileEncoding = enc, na.strings = P$na_strings, fill = TRUE)
  message(sprintf("           aufgefuellt: %d Zeilen im Ergebnis (Rohsaetze: %d) — die Differenz sind Phantomzeilen",
                  nrow(d), length(nf) - 1L))
} else {
  flat <- scan(P$file, what = "", sep = delim, quote = "\"", quiet = TRUE,
               skip = P$skip, fileEncoding = enc, comment.char = "", blank.lines.skip = FALSE)
  ende <- cumsum(nf); start <- ende - nf + 1L
  gut <- which(nf == soll); gut <- gut[gut > 1L]          # ohne die Kopfzeile
  kopf <- flat[start[1]:ende[1]]
  M <- matrix(NA_character_, nrow = length(gut), ncol = soll)
  for (i in seq_along(gut)) M[i, ] <- flat[start[gut[i]]:ende[gut[i]]]
  d <- as.data.frame(M, stringsAsFactors = FALSE)
  names(d) <- kopf
  for (nm in names(d)) d[[nm]][d[[nm]] %in% P$na_strings] <- NA
  message(sprintf("           %d Saetze verworfen, %d bleiben", length(schlecht), nrow(d)))
}

# Dezimaltrennzeichen. Die alte Regel — "welches Zeichen macht mehr Spalten numerisch" — ist
# wertlos: unter "," parst "145585005.96" auch, naemlich als 14558500596. Sie hat genau das getan,
# bei 69 476 Werten in CORDIS organization.csv, Faktor 100, ohne ein Wort.
#
# Stattdessen wird nach BELEGEN gesucht, nicht nach Parsebarkeit:
#   ",\d{1,3}$" am Wertende  -> Komma ist Dezimalzeichen (deutsch)
#   "\.\d{1,2}$" oder "\.\d{4,}$" -> Punkt ist Dezimalzeichen (englisch); ".\d{3}" allein waere
#                                    auch ein Tausenderpunkt und beweist nichts
# Findet sich fuer beides ein Beleg oder fuer keines, wird nicht geraten.
# Ein Beleg ist ein Wert, der ALS GANZES eine Zahl in der jeweiligen Schreibweise ist — sonst
# zaehlt "Smith, Inc." als Beleg fuer das Komma. Beide Muster verlangen eine Nachkommastelle:
# "1.234" allein beweist nichts, das ist deutsch Tausender und englisch Dezimal zugleich.
BELEG <- c("," = "^-?[0-9]{1,3}([.][0-9]{3})*,[0-9]+$|^-?[0-9]+,[0-9]+$",
           "." = "^-?[0-9]{1,3}(,[0-9]{3})*[.][0-9]+$|^-?[0-9]+[.][0-9]+$")
belege <- function(x, dd) { x <- trimws(x); x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) 0L else sum(grepl(BELEG[[dd]], x)) }

# JE SPALTE, nicht je Datei. CORDIS organization.csv fuehrt ecContribution und netEcContribution
# mit Punkt und totalCost mit Komma — in derselben Zeile. Ein Trennzeichen fuer die ganze Datei
# waere hier zwangslaeufig fuer eine der drei Geldspalten falsch, um Faktor 100.
dec_of <- character(0)
for (nm in names(d)) {
  if (P$decimal != "auto") { dec_of[nm] <- P$decimal; next }
  bk <- belege(d[[nm]], ","); bp <- belege(d[[nm]], ".")
  if (bk > 0 && bp > 0)
    stop(sprintf(paste("qc.read: Spalte '%s' ist uneindeutig — %d Werte sprechen fuer ',' und %d fuer '.'.",
                       "Setze decimal explizit.", sep = "\n  "), nm, bk, bp))
  dec_of[nm] <- if (bk > 0) "," else if (bp > 0) "." else "."
}
gemischt <- unique(dec_of[vapply(names(d), function(nm) belege(d[[nm]], dec_of[nm]) > 0, TRUE)])
if (P$decimal == "auto")
  message(sprintf("qc.read: Dezimaltrennzeichen je Spalte%s", if (length(gemischt) > 1)
    sprintf(" — GEMISCHT in einer Datei: %s", paste(sprintf("%s -> '%s'",
      names(dec_of)[belege_hit <- vapply(names(d), function(nm) belege(d[[nm]], dec_of[nm]) > 0, TRUE)],
      dec_of[belege_hit]), collapse = ", ")) else sprintf(" ('%s')", gemischt[1])))

to_num <- function(v, dd) {
  x <- trimws(v)
  if (dd == ",") x <- gsub(",", ".", gsub(".", "", x, fixed = TRUE), fixed = TRUE)
  else           x <- gsub(",", "", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}
for (nm in names(d)) {
  num <- to_num(d[[nm]], dec_of[nm])
  ok <- !is.na(d[[nm]]) & nzchar(trimws(d[[nm]]))
  if (sum(ok) > 0 && all(!is.na(num[ok]))) d[[nm]] <- num
}
if (!is.null(P$columns)) {
  fehlt <- setdiff(P$columns, names(d))
  if (length(fehlt)) stop(sprintf("qc.read: Spalte(n) nicht vorhanden: %s; vorhanden: %s",
                                  paste(fehlt, collapse = ", "), paste(names(d), collapse = ", ")))
  d <- d[, P$columns, drop = FALSE]
}
if (!is.null(P$add_column)) d[[P$add_column]] <- P$add_value

storage_of <- function(v) if (is.numeric(v)) "numeric" else "character"
guess_type <- function(v) {
  if (is.numeric(v)) {
    u <- unique(v[!is.na(v)])
    if (length(u) <= 12 && all(u == round(u))) return("ordinal")
    return("continuous")
  }
  if (length(unique(v[!is.na(v)])) <= max(50, 0.05 * length(v))) "categorical" else "id"
}
sch <- data.frame(
  name     = names(d),
  storage  = vapply(d, storage_of, ""),
  type     = vapply(d, guess_type, ""),
  n        = nrow(d),
  missing  = vapply(d, function(v) sum(is.na(v) | (is.character(v) & !nzchar(trimws(ifelse(is.na(v), "", v))))), 0L),
  distinct = vapply(d, function(v) length(unique(v[!is.na(v)])), 0L),
  example  = vapply(d, function(v) { x <- v[!is.na(v)]; if (length(x)) as.character(x[1]) else "" }, ""),
  stringsAsFactors = FALSE, row.names = NULL
)

write.csv(d, "out.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(sch, "schema.csv", row.names = FALSE, fileEncoding = "UTF-8")
message(sprintf("qc.read: %s  delim='%s' dec='%s'  %d rows x %d cols", P$file, delim,
                paste(unique(dec_of[names(d)]), collapse = "/"), nrow(d), ncol(d)))
message(paste0("  ", paste(sprintf("%s[%s]", sch$name, sch$type), collapse = " ")))
