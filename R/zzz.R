.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)

  # 1. Zjistíme, jestli běžíme v "Checku" (pomocí systémové proměnné)
  # nebo v PDF zařízení bez fontů
  is_check <- Sys.getenv("_R_CHECK_RUNNING_") != "" ||
    (!interactive() && is.null(grDevices::dev.list()))

  if (exists(".uwb_vals", envir = ns)) {

    font_ok <- tryCatch({
      grDevices::pdf(NULL)
      on.exit(grDevices::dev.off())
      graphics::strwidth("test", family = ns$.uwb_vals$font)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)

    # 3. Pokud test selhal nebo jsme v Checku, prostě jen přepíšeme hodnotu
    if (!font_ok || is_check) {
      # U prostředí nepotřebujeme unlockBinding!
      # Stačí rovnou zapsat novou hodnotu do "slotu" font.
      ns$.uwb_vals$font <- "sans"
    }

    # 4. Wire the public exported names to the internal objects so that
    #    reference semantics are preserved: user changes to uwb_vals propagate
    #    to the internal .uwb_vals used by the plot/theme functions.
    unlockBinding("uwb_vals", ns)
    assign("uwb_vals", ns$.uwb_vals, envir = ns)
    lockBinding("uwb_vals", ns)

    unlockBinding("uwb_scales", ns)
    assign("uwb_scales", ns$.uwb_scales, envir = ns)
    lockBinding("uwb_scales", ns)
  }
}


# Nazvy promennych v tibble, ktere vytvareji funkce
# R je mylne povazuje za gobalni promenne
utils::globalVariables(c("n", "nsize", "nsize_raw", "nsize_listwise",
                         "y", "ymin", "ymax", "ypos",
                         "yvar", "yvar_n", "yvar_shadow", "yvar_total",
                         "yvar_show",
                         "x", "xmin", "xmax",
                         "xvar", "xvar_df", "x_nsize",
                         "next_x",
                         "zvar", "zvar_df", "z_nsize",
                         "vvar", "vvar_nr",
                         "lab", "labvar", "labvar_single", "labvar_full",
                         "labvar_out", "labvar_show", "labvar_total",
                         "label", "label_wrap",
                         "name", "fak", "pos",
                         "ID", "id_faks", "fak",
                         "total_rows",
                         "group", "group_id",
                         "jitter_offset",
                         "var_origin",
                         "value", "value_num",
                         "parse_number",
                         "pos_single",
                         "next_node",
                         "codebook"
                         ))
