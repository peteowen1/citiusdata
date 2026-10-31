# Synthetic check for citius_display_name() (citiusdata#55). Run:
#   Rscript scripts/test_display_names.R     (stops on the first failure)
source(here::here("citiusdata", "scripts", "_display_names.R"))
chk <- function(got, want) stopifnot(identical(got, want))

# the examples from the issue
chk(citius_display_name("Edwin KURGAT"), "Edwin Kurgat")
chk(citius_display_name("Scott BEATTIE"), "Scott Beattie")
# already title case: untouched
chk(citius_display_name("Ky Robinson"), "Ky Robinson")
# multi-part surname with the capitals in the middle: kept in place, not reordered
chk(citius_display_name("Wissy Franck HOYE YENDA Moukoula"), "Wissy Franck Hoye Yenda Moukoula")
chk(citius_display_name("Md Nazul Putera FAEIZ Firdaous"), "Md Nazul Putera Faeiz Firdaous")
chk(citius_display_name("Claude Emmanuel ITOUNGUE Bogognie"), "Claude Emmanuel Itougue Bogognie")
# surname first, hyphen, apostrophe, accents
chk(citius_display_name("SMITH-JONES Anna"), "Smith-Jones Anna")
chk(citius_display_name("Sean O'BRIEN"), "Sean O'Brien")
chk(citius_display_name("Ayça ÖZKAN"), "Ayça Özkan")
# mixed case and initials are left alone
chk(citius_display_name("Sam McLEOD"), "Sam McLEOD")
chk(citius_display_name("J. SMITH"), "J. Smith")
# idempotent
x <- c("Edwin KURGAT", "Ky Robinson", "Wissy Franck HOYE YENDA Moukoula")
chk(citius_display_name(citius_display_name(x)), citius_display_name(x))
# NA, empty and length are preserved
chk(citius_display_name(c(NA, "", "A B")), c(NA, "", "A B"))
chk(citius_display_name(character(0)), character(0))
# the two feeds now agree on one person
chk(citius_display_name("Edwin KURGAT"), citius_display_name("Edwin Kurgat"))
cat("citius_display_name: all checks passed\n")
