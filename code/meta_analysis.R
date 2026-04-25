# PrEP Initiation in AGYW (SSA) - V2 (World Class)
# Author: Gemini CLI

library(metafor)

data <- read.csv("../data/raw_studies.csv")
trials <- data

es <- escalc(measure="PLO", xi=event_count, ni=n, data=trials)
res <- rma(yi, vi, data=es, method="REML", test="knha")
l1o <- leave1out(res)
res_sub <- rma(yi, vi, data=es, mods = ~ intervention_type - 1, method="REML", test="knha")

png("../paper/forest_plot.png", width=1000, height=500, res=100)
par(mar=c(5,4,2,2))
forest(res, transf=transf.ilogit, slab=trials$study_id, header=TRUE, 
       main="PrEP Initiation in AGYW (SSA) (Random-Effects)",
       ilab=cbind(trials$intervention_type, trials$event_count, trials$n),
       ilab.xpos=c(-2, -1, -0.5), cex=0.9, xlim=c(-3.5, 2.5), refline=transf.ilogit(res$b),
       xlab="Proportion")
text(c(-2, -1, -0.5), res$k+2, c("Intervention", "Events", "N"), font=2, cex=0.9)
dev.off()

png("../paper/funnel_plot.png", width=600, height=600, res=100)
funnel(res, main="Funnel Plot (Logit Scale)")
dev.off()

sink("../paper/analysis_summary.txt")
cat("=== PrEP Initiation in AGYW (SSA) Summary (Logit Transformed) ===\n")
print(summary(res))
cat("\n\n=== Back-transformed Pooled Estimate (Proportion) ===\n")
cat(sprintf("Pooled Proportion: %.3f (95%% CI: %.3f - %.3f)\n", 
            transf.ilogit(res$b), transf.ilogit(res$ci.lb), transf.ilogit(res$ci.ub)))
cat("\n=== Subgroup Analysis by Intervention Type ===\n")
print(res_sub)
cat("\n=== Leave-One-Out Sensitivity Analysis ===\n")
print(l1o)
sink()
