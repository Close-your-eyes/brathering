install.packages("stringi")
library(stringi)

# \\s → standard whitespace
# U+00A0 → NBSP (very common)
# U+2009 → thin space
# U+202F → narrow NBSP
# U+2007 → figure space
# \u00A0 → NBSP (very common)
# \u2009 → thin space
# \u202F → narrow NBSP
# \u2007 → figure space

paste0("a b")
paste0("a\u00A0b")
paste0("a\u2009b")
paste0("a\u202Fb")
paste0("a\u2007b")

x <- " 105067 "
stri_replace_all_charclass(x, "\\p{WHITE_SPACE}", "")

gsub("[\\s\\u00A0\\u2009\\u202F\\u2007]+", "", x, perl = TRUE)

gsub("^[\\s\\u00A0\\u2009\\u202F]+|[\\s\\u00A0\\u2009\\u202F]+$", "", x, perl = TRUE)

trimws(x)
stri_trim_both(x)
?stri_trim_both


