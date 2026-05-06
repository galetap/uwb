.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)

  # 1. Zjistíme, jestli běžíme v "Checku" nebo v PDF zařízení,
  # kde Franklin Gothic skoro určitě nebude fungovat
  is_check <- !interactive() || is.null(grDevices::dev.list())

  # 2. Pokud existují konstanty, zkusíme otestovat font
  if (exists(".uwb_vals", envir = ns)) {

    # Trik: Zkusíme vytvořit testovací grafické zařízení v paměti.
    # Pokud Franklin selže, zachytíme chybu (tryCatch).
    font_ok <- tryCatch({
      # Zkusíme zjistit šířku textu s tvým fontem
      # Pokud font neexistuje, R hodí warning nebo error
      grDevices::pdf(NULL) # Virtuální PDF zařízení
      on.exit(grDevices::dev.off())
      graphics::strwidth("test", family = ns$.uwb_vals$font)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)

    # 3. Pokud test selhal nebo jsme v Checku, přepneme na "sans"
    if (!font_ok || is_check) {
      new_vals <- ns$.uwb_vals
      new_vals$font <- "sans"

      unlockBinding(".uwb_vals", ns)
      assign(".uwb_vals", new_vals, envir = ns)
      lockBinding(".uwb_vals", ns)
    }
  }
}

# Nazvy promennych v tibble, ktere vytvareji funkce
# R je mylne povazuje za gobalni promenne
utils::globalVariables(c("n", "yvar", "yvar_n", "xvar", "labvar", "nsize",
                         "vvar", "vvar_nr", "labvar_full"))
