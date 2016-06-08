
z <- get_zillow_data("SingleFamilyResidence", loc_type = "Zip")

png("performance_chart.png", width = 20, height = 15, units = "in", res = 300)
  performance_chart(z$returns, from = "2001-01-01", to = "2015-01-01")
dev.off()
