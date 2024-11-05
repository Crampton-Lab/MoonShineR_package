.onAttach <- function(libname, pkgname) {
  message("MoonShineR v1.1 is tested on these dependency package versions:
- suncalc (<= 0.5.1),
- magrittr (<= 2.0.3),
- lubridate (<= 1.9.3),
- REdaS (<= 0.9.4),
- dplyr (<= 1.1.4),
- ggplot2 (<= 3.5.1)
If MoonShineR is not working, try downgrading these packages to the specified version"
  )
}
