#' Create numeric cell formats for googlesheets4 API
#'
#' @param type one of "PERCENT", "CURRENCY", or "NUMBER"
#' @param pattern a regex pattern to apply to the number format. See API documentation: https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets/cells#cellformat
#'
#' @examples
#' percent_cell_format  <- number_format(type = "PERCENT", pattern = "#0%")
#' currency_cell_format <- number_format(type = "CURRENCY", sprintf('%s#,##0', "$"))
#' number_cell_format   <- number_format(type = "NUMBER", '#,##0')
#' parity_cell_format   <- number_format(type = "CURRENCY", sprintf('%s#,##0.00', "$"))
#' @export number_format
number_format <- function(type, pattern) {
  googlesheets4:::CellData(                  # this is the top level value outside the JSON in the documentation
    userEnteredFormat = googlesheets4:::new( # userEnteredFormat is the name within CellData
      "CellFormat",                          # This is the name within    "userEnteredFormat": { object (CellFormat) }
      numberFormat = list(                   # Go to CellFormat -- { "numberFormat": { object (NumberFormat) }}
        type = type,                         # Go to NumberFormat     --  {"type": enum ( NumberFormatType ), ...
        pattern = pattern                    # ...                          "pattern": string }
      )
    )
  )
}

#' Create text cell formats for googlesheets4 API
#'
#' @examples
#' wrap_cell_format     <- text_format(strategy = "WRAP", valign = "TOP")
#' @param strategy one of "WRAP" (default) or another wrapping strategy. See API documentation.
#' @param pattern a regex pattern to apply to the number format. See API documentation: https://developers.google.com/sheets/api/reference/rest/v4/spreadsheets/cells#cellformat
#' @export text_format
text_format <- function(strategy = "WRAP", valign = "TOP") {
  googlesheets4:::CellData(
    userEnteredFormat = googlesheets4:::new(
      "CellFormat",
      wrapStrategy = strategy,
      verticalAlignment = valign
    )
  )
}


# If googlesheets4 develops support for resizing columns, the ReadMe should have this:
# Column a = 125
# Column b = 252 px
# Column c = 435
# Row 9 - merge A:c and center

# Another way to do this would be to use a custom class
# This is how chatgpt suggested it, but I found the functions above
# to me more succinct and useable. The class approach is probably more robust.
#
# Define a custom class for the percent cell format
# CellFormat <- R6::R6Class(
#   "CellFormat",
#   public = list(
#     numberFormat = NULL,
#     userEnteredFormat = NULL,
#
#     initialize = function(type, pattern) {
#       self$numberFormat <- list(
#         type = type,
#         pattern = pattern
#       )
#
#       self$userEnteredFormat <- list(
#         numberFormat = self$numberFormat
#       )
#     }
#   )
# )

#' extensions for openxlsx
#' #' #export textStyle
#' textStyle <- createStyle(
#'   halign = "left",
#'   valign = "top",
#'   wrapText = TRUE,
#' )

# percentStyle <- createStyle(
#   fontName = NULL,
#   fontSize = NULL,
#   fontColour = NULL,
#   numFmt = openxlsx_getOp("numFmt", "GENERAL"),
#   border = NULL,
#   borderColour = openxlsx_getOp("borderColour", "black"),
#   borderStyle = openxlsx_getOp("borderStyle", "thin"),
#   bgFill = NULL,
#   fgFill = NULL,
#   halign = NULL,
#   valign = NULL,
#   textDecoration = NULL,
#   wrapText = FALSE,
#   textRotation = NULL,
#   indent = NULL,
#   locked = NULL,
#   hidden = NULL
# )
#
# currencyStyle <- createStyle(
#   fontName = NULL,
#   fontSize = NULL,
#   fontColour = NULL,
#   numFmt = openxlsx_getOp("numFmt", "GENERAL"),
#   border = NULL,
#   borderColour = openxlsx_getOp("borderColour", "black"),
#   borderStyle = openxlsx_getOp("borderStyle", "thin"),
#   bgFill = NULL,
#   fgFill = NULL,
#   halign = NULL,
#   valign = NULL,
#   textDecoration = NULL,
#   wrapText = FALSE,
#   textRotation = NULL,
#   indent = NULL,
#   locked = NULL,
#   hidden = NULL
# )
#
# numberStyle <- createStyle(
#   fontName = NULL,
#   fontSize = NULL,
#   fontColour = NULL,
#   numFmt = openxlsx_getOp("numFmt", "GENERAL"),
#   border = NULL,
#   borderColour = openxlsx_getOp("borderColour", "black"),
#   borderStyle = openxlsx_getOp("borderStyle", "thin"),
#   bgFill = NULL,
#   fgFill = NULL,
#   halign = NULL,
#   valign = NULL,
#   textDecoration = NULL,
#   wrapText = FALSE,
#   textRotation = NULL,
#   indent = NULL,
#   locked = NULL,
#   hidden = NULL
# )
# Not working
# parityStyle <- createStyle(
#   numFmt = openxlsx_getOp("numFmt",  "CURRENCY")
# )

# align_cell_readme <- cell_alignment()


# a function to find the cell indices of specific disaggregates
get_cells <- function(data, target_string, column_indexes) {

  # Get the indices where the specified column contains the target string
  indices <- which(grepl(target_string, data[["Disaggregate Name"]]))

  # Convert indices to A1-formatted cell references
  cells <- expand.grid(column_indexes, indices + 1) %>%
    unite("cells", Var1, Var2, sep="")

  return(cells$cells)
}

# use the cells gathered using the formula above and the predefined formats
format_cells <- function(data, sheet, ss) {

  number_cells <- get_cells(data, "Climate adaptation/climate risk management"
                            ,  c("E", "F"))
  map(number_cells, ~ range_flood(ss = ss
                                  , sheet = sheet
                                  , range = .x
                                  , cell = number_cell_format))
  # format the currency cells
  currency_cells <- get_cells(data, "Value", c("E", "F"))
  map(currency_cells, ~ range_flood(ss = ss, sheet = sheet
                                    , range = .x
                                    , cell = currency_cell_format))
  # format the percent cells
  range_flood(ss = ss, sheet = sheet, range = "G:G", cell = percent_cell_format)
}
