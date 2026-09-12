# rec_fill_workbook.R
# PROPOSED -- Claude, UNSIGNED. Andy stamps or corrects.
#
# Fills the 60 "Rec - <player> <QB> v <DEF>" sheets in NE_v_SEA_4.xlsx from
# the 60 matching "Rec - ..." workbooks in the Receiving folder.
#
# It does NOT open the workbook with openxlsx. It unzips the xlsx, rewrites
# ONLY the 60 sheet XML parts that belong to those sheets, and rezips. Every
# other part -- all 105 original sheets, styles, sharedStrings, workbook.xml,
# defined names -- is copied through untouched. The C21:M21 and O1:R4
# formulas are regenerated verbatim, identical strings to what is in the file
# now, so nothing downstream of them changes.
#
# Needs: openxlsx, zip (openxlsx depends on zip, so it is already installed).

REC_HDR <- c("ind", "players", "tgt_shr", "rte_shr", "pbp_xtd_shr", "part_xtd_shr", "ypa",
             "pbp_xypa", "part_xypa", "acc_rate", "fastr_cp", "pbp_cp", "part_cp")
REC_BLOCKS <- c(1, 5, 9, 13, 17)
REC_LENS   <- c("blitz", "depth", "less", "pa", "pressure")

REC_PLAYERS <- c("R.Stevenson" = "Rhamondre Stevenson", "C.Kiner"    = "Corey Kiner",
                 "R.Gilliam"   = "Reggie Gilliam",      "H.Henry"    = "Hunter Henry",
                 "E.Raridon"   = "Eli Raridon",         "A.Brown"    = "AJ Brown",
                 "R.Doubs"     = "Romeo Doubs",         "M.Hollins"  = "Mack Hollins",
                 "D.Douglas"   = "Demario Douglas",     "K.Williams" = "Kyle Williams")
REC_QBS  <- c("NE" = "NEMaye-2025", "TEN" = "TENTannehill-2019", "DAL" = "DALPrescott-2025")
REC_DEFS <- c("SEA25" = "SEA2025", "SEA24" = "SEA2024")

# sheet name -> source file name, by the mapping tables above
rec_src_file <- function(sheet) {
  p <- strsplit(sub("^Rec - ", "", sheet), " ", fixed = TRUE)[[1]]
  if (length(p) != 4 || p[3] != "v") stop("cannot parse sheet name: ", sheet)
  if (is.na(REC_PLAYERS[p[1]])) stop("no player mapping for '", p[1], "' in sheet: ", sheet)
  if (is.na(REC_QBS[p[2]]))     stop("no QB mapping for '", p[2], "' in sheet: ", sheet)
  if (is.na(REC_DEFS[p[4]]))    stop("no DEF mapping for '", p[4], "' in sheet: ", sheet)
  paste0("Rec - ", REC_PLAYERS[[p[1]]], " - ", REC_QBS[[p[2]]], " vs ", REC_DEFS[[p[4]]], ".xlsx")
}

# read one source workbook -> list of 5 lenses, each a 2 x 12 numeric matrix
# (row 1 = In, row 2 = Out; cols = players .. part_cp). Blocks are read one
# at a time by explicit row number so an empty spacer row cannot shift them.
rec_read_src <- function(path) {
  out <- vector("list", length(REC_BLOCKS))
  for (i in seq_along(REC_BLOCKS)) {
    b <- REC_BLOCKS[i]
    h <- openxlsx::read.xlsx(path, sheet = 1, rows = b, cols = 1:13, colNames = FALSE)
    if (ncol(h) != 13 || as.character(h[1, 1]) != "ind" || as.character(h[1, 13]) != "part_cp")
      stop("header row ", b, " is not the 13-column rec header in: ", basename(path))
    d <- openxlsx::read.xlsx(path, sheet = 1, rows = c(b + 1, b + 2), cols = 1:13, colNames = FALSE)
    if (nrow(d) != 2 || ncol(d) != 13)
      stop("rows ", b + 1, "-", b + 2, " are not 2 x 13 in: ", basename(path))
    if (!identical(as.character(d[[1]]), c("In", "Out")))
      stop("rows ", b + 1, "-", b + 2, " are not In/Out in: ", basename(path),
           " -- got: ", paste(d[[1]], collapse = "/"))
    m <- suppressWarnings(matrix(as.numeric(as.matrix(d[, 2:13])), nrow = 2))
    out[[i]] <- m
  }
  names(out) <- REC_LENS
  out
}

# A=1 .. M=13, O=15 .. R=18
rec_col <- function(i) { s <- ""; while (i > 0) { r <- (i - 1) %% 26; s <- paste0(LETTERS[r + 1], s); i <- (i - 1) %/% 26 }; s }
rec_istr <- function(ref, v) paste0('<c r="', ref, '" t="inlineStr"><is><t>', v, '</t></is></c>')
rec_num  <- function(ref, v) if (is.na(v) || is.nan(v)) "" else paste0('<c r="', ref, '"><v>', formatC(v, digits = 15, format = "g"), '</v></c>')
rec_form <- function(ref, f) paste0('<c r="', ref, '"><f>', gsub(">", "&gt;", f, fixed = TRUE), '</f></c>')

# build one complete sheet XML: headers, In/Out values, and the same
# C21:M21 / O1:R4 formulas the sheet already carries
rec_sheet_xml <- function(vals) {
  rows <- setNames(rep("", 21), as.character(1:21))
  add <- function(r, s) rows[[as.character(r)]] <<- paste0(rows[[as.character(r)]], s)
  for (i in seq_along(REC_BLOCKS)) {
    b <- REC_BLOCKS[i]
    for (j in 1:13) add(b, rec_istr(paste0(rec_col(j), b), REC_HDR[j]))
    for (k in 1:2) {
      r <- b + k
      add(r, rec_istr(paste0("A", r), c("In", "Out")[k]))
      for (j in 1:12) add(r, rec_num(paste0(rec_col(j + 1), r), vals[[i]][k, j]))
    }
  }
  for (i in 1:4) add(1, rec_istr(paste0(rec_col(14 + i), 1), c("TGT", "XTD", "YPA", "ACC")[i]))
  agg <- list(O = c("C", "D"), P = c("E", "F"), Q = c("G", "I"), R = c("J", "M"))
  for (cl in names(agg)) {
    for (k in 1:2) {
      rng <- paste(sprintf("%s%d:%s%d", agg[[cl]][1], REC_BLOCKS + k, agg[[cl]][2], REC_BLOCKS + k), collapse = ",")
      add(k + 1, rec_form(paste0(cl, k + 1), paste0("AVERAGE(", rng, ")")))
    }
    add(4, rec_form(paste0(cl, "4"), paste0(cl, "2/", cl, "3")))
  }
  for (j in 3:13) {
    cl <- rec_col(j)
    add(21, rec_form(paste0(cl, "21"),
                     paste(sprintf("(%s%d>%s%d)", cl, REC_BLOCKS + 1, cl, REC_BLOCKS + 2), collapse = "+")))
  }
  body <- paste0(vapply(which(nzchar(rows)), function(r) paste0('<row r="', r, '">', rows[[r]], '</row>'), ""), collapse = "")
  paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\r\n',
         '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
         '<dimension ref="A1:R21"/><sheetViews><sheetView workbookViewId="0"/></sheetViews>',
         '<sheetFormatPr defaultRowHeight="14.5"/>',
         '<cols><col min="1" max="1" width="10.88671875" customWidth="1"/></cols>',
         '<sheetData>', body, '</sheetData>',
         '<pageMargins left="0.7" right="0.7" top="0.75" bottom="0.75" header="0.3" footer="0.3"/></worksheet>')
}

rec_fill <- function(wb_in, wb_out, src_dir) {
  if (!file.exists(wb_in)) stop("workbook not found: ", wb_in)
  if (!dir.exists(src_dir)) stop("source folder not found: ", src_dir)
  if (normalizePath(wb_in, mustWork = TRUE) == normalizePath(wb_out, mustWork = FALSE))
    stop("wb_out must differ from wb_in -- the input is never overwritten")
  
  tmp <- file.path(tempdir(), paste0("recfill_", as.integer(Sys.time())))
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(wb_in, exdir = tmp)
  
  wbx  <- paste(readLines(file.path(tmp, "xl", "workbook.xml"), warn = FALSE), collapse = "")
  rels <- paste(readLines(file.path(tmp, "xl", "_rels", "workbook.xml.rels"), warn = FALSE), collapse = "")
  ent  <- regmatches(wbx, gregexpr("<sheet [^>]*/>", wbx))[[1]]
  nm   <- sub('.*name="([^"]*)".*', "\\1", ent)
  rid  <- sub('.*r:id="([^"]*)".*', "\\1", ent)
  rel  <- regmatches(rels, gregexpr("<Relationship [^>]*/>", rels))[[1]]
  rmap <- setNames(sub('.*Target="([^"]*)".*', "\\1", rel), sub('.*Id="([^"]*)".*', "\\1", rel))
  
  targets <- nm[grepl("^Rec - .+ (NE|TEN|DAL) v SEA2[45]$", nm)]
  cat("sheets to fill: ", length(targets), "\n", sep = "")
  if (!length(targets)) stop("no 'Rec - <player> <QB> v <DEF>' sheets found in ", wb_in)
  
  # every source file must exist before a single byte is written
  srcs <- vapply(targets, rec_src_file, "")
  miss <- targets[!file.exists(file.path(src_dir, srcs))]
  if (length(miss))
    stop(length(miss), " source files missing, nothing written. First few: ",
         paste(head(srcs[!file.exists(file.path(src_dir, srcs))], 3), collapse = " | "))
  
  blanks <- character(0)
  for (i in seq_along(targets)) {
    sh   <- targets[i]
    vals <- rec_read_src(file.path(src_dir, srcs[i]))
    nb   <- sum(vapply(vals, function(m) sum(is.na(m) | is.nan(m)), 0L))
    if (nb > 0) blanks <- c(blanks, sprintf("%s (%d)", sh, nb))
    part <- file.path(tmp, "xl", rmap[[rid[match(sh, nm)]]])
    if (!file.exists(part)) stop("sheet part not found for: ", sh)
    writeLines(rec_sheet_xml(vals), part, sep = "")
    cat("[", i, "/", length(targets), "] ", sh, "  <- ", srcs[i], "\n", sep = "")
  }
  
  files <- list.files(tmp, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  files <- c("[Content_Types].xml", setdiff(files, "[Content_Types].xml"))
  if (file.exists(wb_out)) file.remove(wb_out)
  zip::zip(zipfile = wb_out, files = files, root = tmp, mode = "cherry-pick")
  
  cat("\nwrote ", wb_out, "\n", sep = "")
  cat("filled ", length(targets), " sheets; every other part copied through untouched\n", sep = "")
  if (length(blanks)) cat("blank cells (NaN in source, left empty): ", paste(blanks, collapse = ", "), "\n", sep = "")
  invisible(targets)
}

# usage (nothing above fires):
rec_fill(wb_in  = "C:/Users/vflre/Downloads/outcomes/NE_v_SEA_4.xlsx",
        wb_out = "C:/Users/vflre/Downloads/outcomes/NE_v_SEA_5.xlsx",
        src_dir = "C:/Users/vflre/Downloads/outcomes/Receiving")
#
# check one sheet against the sweep before trusting the other 59:
#   openxlsx::read.xlsx("C:/Users/vflre/Downloads/NE_v_SEA_4_filled.xlsx",
#                       sheet = "Rec - R.Stevenson NE v SEA25", rows = 1:3, cols = 1:13, colNames = FALSE)


