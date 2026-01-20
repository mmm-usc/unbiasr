test_that("Plot CAI across range for four groups", {
  skip_on_cran()
  library(lavaan)
  hs2 <- cbind(HolzingerSwineford1939, g = rep(LETTERS[1:4], c(70, 70, 70, 91)))
  hs_mod <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(hs_mod, data = hs2, group = "g")
  p1 <- PartInv(fit, propsel = .1, plot_contour = FALSE, show_mi_result = TRUE,
                labels = paste0("G", 1:4), reference = "G4")
  plot_CAI_across_range(x = p1, by = 0.01, plot_only_g = c("G2", "G1"))        
})