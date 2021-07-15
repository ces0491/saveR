#' Title
#'
#' @param coeff.table
#' @param se.table
#' @param pvalue.table
#' @param plot.actual.vs.fitted
#' @param save.name
#' @param output.directory
#'
#' @return
#' @export
#'
#' @examples
save_panel_model_output <- function(coeff.table, se.table, pvalue.table, plot.actual.vs.fitted, save.name, output.directory = NULL) {

  if (is.null(output.directory)) {
    my.folder <- paste0("PanelModels_", format(Sys.Date(), "%Y%m%d"))
    output.directory <- file.path("./inst/extdata", my.folder)
  }

  dir.create(output.directory, showWarnings = FALSE)

  pdf.name <- paste(save.name, "_", format(Sys.Date(), "%Y%m%d"),".pdf",sep = "")
  my.pdf.path <- file.path(output.directory, pdf.name)
  pdf(file = my.pdf.path, width = 12, height = 8, paper = "a4r")

  #summ stats with categorical variables converted to factor
  table.coefs <- create_table_grob(coeff.table, title = "Coefficients")
  table.se <- create_table_grob(se.table, title = "Standard Errors")

  gridExtra::grid.arrange(table.coefs, table.se, ncol = 2)

  #significant variables ols vs fgls
  table.pvalue <- create_table_grob(pvalue.table, title = "Significant variables - p values",
                                    footnote = paste("significance at 0.05 level. NA's indicate not significant", sep = " "))
  gridExtra::grid.arrange(table.pvalue) #grid.arrange opens a new page after each grob without having to specify grid.newpage

  #predicted values vs actual charts
  gridExtra::grid.arrange(grid::grid.text(strwrap("Actual vs Predicted Values"),
                                          gp = grid::gpar(fontsize = 30)))

  for (n in 1:nrow(plot.actual.vs.fitted)) {
    grid::grid.draw(plot.actual.vs.fitted$plot[[n]])
  }

}

