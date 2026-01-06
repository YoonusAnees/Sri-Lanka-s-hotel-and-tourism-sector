
normalityTest(~Revenue, test="ad.test", data=hotels)
normalityTest(~Revenue, test="shapiro.test", data=hotels)
normalityTest(~Revenue, test="shapiro.test", data=hotels)
normalityTest(~Revenue, test="lillie.test", data=hotels)
normalityTest(~Revenue, test="lillie.test", data=hotels)
Boxplot( ~ Revenue, data=hotels, id=list(method="y"))
with(hotels, Dotplot(Revenue, bin=FALSE))
with(hotels, qqPlot(Revenue, dist="norm", id=list(method="y", n=2, 
  labels=rownames(hotels))))

