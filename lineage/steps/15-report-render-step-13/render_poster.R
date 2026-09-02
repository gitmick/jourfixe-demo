#!/usr/bin/env Rscript
# render_poster.R — das Jam-Poster aus report.json rendern.
#
# Der EINE notwendige Unterschied zur QM-Methode, und der Grund fuer diese Datei: ein leerer Slot
# haelt den Lauf nicht an, sondern wird als sichtbare Luecke gezeichnet. Ein Arbeitsdokument muss
# sich rendern lassen, BEVOR es fertig ist — sonst sieht ein Team sein Poster zum ersten Mal, wenn
# es nichts mehr aendern kann. Die Luecke nennt dabei ihren eigenen Namen, damit sie ein Auftrag
# ist und keine Panne ("hier fehlt eda-uebersicht.png").
#
# Der zweite Unterschied ist Weglassen: kein reference_doc, keine QM-Kopfzeile, keine Regelliste.
# Ein Poster fuer einen Science Jam soll nicht wie eine SOP aussehen.
#
# Die Struktur steht NICHT hier, sondern in report.json. Diese Datei erfindet keine Sektion und
# ordnet keine um — sonst gaebe es zwei Quellen fuer den Aufbau, und die Vorlage waere nur noch
# ein Vorschlag.
#
# Ausgaben, gemessen am Image (2026-09-02, qms-docx 0.3.0):
#   docx  rmarkdown + pandoc                                        geht
#   pdf   LibreOffice --convert-to pdf, eigenes Profilverzeichnis    geht
#   html  pandoc, self-contained                                     geht, wenn pandoc es kann
#   png   NICHTS im Image kann ein PDF rastern: kein pdftoppm, kein ImageMagick, kein gs, kein
#         pdftools; LibreOffice hat fuer Writer keinen PNG-Exportfilter und kann PDF nicht einmal
#         oeffnen. Der Versuch steht trotzdem unten — er kostet nichts und faengt an zu
#         funktionieren, sobald poppler-utils oder das R-Paket pdftools im Image liegt.

suppressWarnings(suppressMessages(library(jsonlite)))
Sys.setenv(HOME = getwd())   # headless LibreOffice und pandoc brauchen ein beschreibbares HOME

# Verlinkte Eingaben landen FLACH im Workdir; report.json adressiert sie mit Unterpfad.
here <- function(path) if (file.exists(path)) path else basename(path)
read_text <- function(path) {
    p <- here(path)
    if (!file.exists(p)) return(NA_character_)
    paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

r <- fromJSON("report.json", simplifyVector = FALSE)
if (is.null(r$sections) || !length(r$sections)) stop("report.json has no sections")

# `document` ist bei der QM-Methode Pflicht. Fuer ein Poster ist es das nicht: ein Team, das noch
# keinen Titel hat, soll trotzdem rendern koennen.
d  <- if (!is.null(r$document)) r$document else list()
md <- if (!is.null(r$metadata)) r$metadata else list()
pick <- function(...) { for (v in list(...)) if (!is.null(v) && nzchar(trimws(as.character(v)))) return(as.character(v)); "" }
doc_id  <- pick(d$id, "JAM-POSTER")
title   <- pick(d$title, md$title, "Poster without a title")
authors <- if (!is.null(md$authors) && length(md$authors)) paste(unlist(md$authors), collapse = ", ") else ""
team    <- pick(md$team, d$created_by)
klasse  <- pick(d$class, "As-If Science Jam")

gaps <- character()   # was fehlt — am Ende als Zusammenfassung ins Log

# Eine sichtbare Luecke anstelle eines fehlenden Bildes. Gezeichnet, nicht mitgeliefert: das
# png-Device ist im Image da (capabilities()["png"] == TRUE), eine Bilddatei zum Einbetten waere
# ein weiterer Helfer, der veralten kann.
placeholder <- function(want) {
    out <- paste0("__leer-", gsub("[^A-Za-z0-9]+", "-", want), ".png")
    png(out, width = 1600, height = 520, res = 150)
    op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op), add = TRUE)
    plot.new(); plot.window(c(0, 1), c(0, 1))
    rect(0.005, 0.06, 0.995, 0.94, border = "#9aa0a6", lty = 2, lwd = 2, col = "#f4f5f7")
    text(0.5, 0.60, "slot still empty", cex = 1.7, col = "#5f6368")
    text(0.5, 0.34, want, cex = 1.0, col = "#80868b", family = "mono")
    dev.off()
    out
}

hashes <- function(level) paste(rep("#", if (is.null(level)) 1 else level), collapse = "")

body <- c()
n <- 0
for (sec in r$sections) {
    body <- c(body, paste(hashes(sec$level), sec$heading), "")
    for (blk in sec$blocks) {
        if (identical(blk$type, "narrative")) {
            txt <- read_text(blk$file)
            if (is.na(txt) || !nzchar(trimws(txt))) {
                # Der Text fehlt. Das ist die haeufigste Luecke und die billigste zu schliessen,
                # also wird sie benannt statt verschwiegen.
                gaps <- c(gaps, paste0("text: ", blk$file))
                body <- c(body, sprintf("*— the text is still missing here (%s) —*", basename(blk$file)), "")
            } else {
                body <- c(body, txt, "")
            }
        } else if (identical(blk$type, "image")) {
            f <- here(blk$file)
            if (!file.exists(f)) {
                gaps <- c(gaps, paste0("image: ", blk$file))
                f <- placeholder(basename(blk$file))
            }
            n <- n + 1
            body <- c(body,
                      sprintf('```{r img%d, echo=FALSE, out.width="100%%"}', n),
                      sprintf('knitr::include_graphics("%s")', f), "```", "")
        } else if (identical(blk$type, "table")) {
            f <- here(blk$file)
            n <- n + 1
            if (!file.exists(f)) {
                gaps <- c(gaps, paste0("table: ", blk$file))
                body <- c(body, sprintf("*— the table is still missing here (%s) —*", basename(blk$file)), "")
            } else {
                body <- c(body, sprintf("```{r tbl%d, echo=FALSE}", n),
                          sprintf('knitr::kable(jsonlite::fromJSON("%s"))', f), "```", "")
            }
        } else if (identical(blk$type, "rchunk")) {
            n <- n + 1
            body <- c(body, sprintf("```{r raw%d, echo=FALSE}", n), read_text(blk$file), "```", "")
        }
    }
}

fm <- c("---",
        sprintf('title: "%s"', gsub('"', "'", title)),
        if (nzchar(team))    sprintf('subtitle: "%s"',  gsub('"', "'", paste(klasse, "·", team))) else sprintf('subtitle: "%s"', klasse),
        if (nzchar(authors)) sprintf('author: "%s"',    gsub('"', "'", authors)) else NULL,
        "---", "")

rmd <- paste0(doc_id, ".Rmd")
writeLines(c(fm, body), rmd, useBytes = TRUE)
cat("assembled:", rmd, "from", length(r$sections), "sections\n")

docx <- sub("\\.Rmd$", ".docx", rmd)
rmarkdown::render(rmd, output_format = rmarkdown::word_document(), quiet = TRUE)
if (!file.exists(docx)) stop("pandoc produced no .docx")
cat("docx:", docx, file.size(docx), "B\n")

# --- PDF: LibreOffice, mit eigenem Profilverzeichnis (HOME ist im Container nicht beschreibbar) --
pdf_out <- sub("\\.docx$", ".pdf", docx)
lo <- Sys.which("libreoffice"); if (!nzchar(lo)) lo <- Sys.which("soffice")
if (nzchar(lo)) {
    Sys.setenv(LD_LIBRARY_PATH = paste("/usr/lib/libreoffice/program", Sys.getenv("LD_LIBRARY_PATH"), sep = ":"))
    system2(lo, c(paste0("-env:UserInstallation=file://", file.path(tempdir(), "lo-profile")),
                  "--headless", "--norestore", "--convert-to", "pdf", "--outdir", ".", shQuote(docx)))
}
if (file.exists(pdf_out)) cat("pdf:", pdf_out, file.size(pdf_out), "B\n") else
    cat("pdf: FAILED (LibreOffice produced nothing)\n")

# --- HTML: dasselbe Rmd, self-contained, damit die Grafiken mitreisen ---------------------------
html <- sub("\\.Rmd$", ".html", rmd)
ok <- tryCatch({
    rmarkdown::render(rmd, output_format = rmarkdown::html_document(self_contained = TRUE, theme = "flatly"),
                      quiet = TRUE); TRUE
}, error = function(e) { cat("html: skipped —", conditionMessage(e), "\n"); FALSE })
if (ok && file.exists(html)) cat("html:", html, file.size(html), "B\n")

# --- PNG: der Versuch, in der Reihenfolge der Wahrscheinlichkeit ---------------------------------
# Heute schlaegt jeder Zweig fehl (gemessen). Der Code bleibt, weil das Bild dann ohne Aenderung
# hier entsteht, sobald das Image poppler-utils oder pdftools bekommt.
png_made <- character()
if (file.exists(pdf_out)) {
    if (requireNamespace("pdftools", quietly = TRUE)) {
        png_made <- tryCatch(pdftools::pdf_convert(pdf_out, format = "png", dpi = 150,
                        filenames = sprintf("%s-seite-%%d.png", doc_id)), error = function(e) character())
    } else if (nzchar(Sys.which("pdftoppm"))) {
        system2("pdftoppm", c("-png", "-r", "150", shQuote(pdf_out), shQuote(paste0(doc_id, "-seite"))))
        png_made <- list.files(pattern = paste0("^", doc_id, "-seite.*\\.png$"))
    } else if (nzchar(Sys.which("convert"))) {
        system2("convert", c("-density", "150", shQuote(pdf_out), shQuote(paste0(doc_id, "-seite.png"))))
        png_made <- list.files(pattern = paste0("^", doc_id, "-seite.*\\.png$"))
    }
}
if (length(png_made)) cat("png:", paste(png_made, collapse = " "), "\n") else
    cat("png: not possible — this image cannot rasterise a PDF (no pdftools/pdftoppm/convert, and\n",
        "     LibreOffice has no PNG export filter for Writer and no PDF import).\n",
        "     One line in the image (poppler-utils) would fix it; until then pdf and html ARE the picture.\n")

# Aufraeumen: die Platzhalter sind in docx UND html schon eingebettet, und Rscript legt beim
# Zeichnen ungefragt ein Rplots.pdf an. Beides wuerde sonst als "Ausgabe des Steps" im Cockpit
# stehen und so aussehen, als haette der Lauf es gemeint.
unlink(c(list.files(pattern = "^__leer-.*\\.png$"), "Rplots.pdf"))

# --- Was noch fehlt ------------------------------------------------------------------------------
if (length(gaps)) {
    cat("\nGaps in the poster (", length(gaps), "):\n", sep = "")
    for (g in gaps) cat("  -", g, "\n")
    cat("The poster rendered anyway — that is exactly what this method is for.\n")
} else {
    cat("\nNo gaps: every slot is filled and every text written.\n")
}
