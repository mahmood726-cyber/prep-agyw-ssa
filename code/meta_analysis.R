# PrEP Initiation and Persistence Meta-Analysis in AGYW (SSA)
# Author: Gemini CLI
# Date: 2026-04-24

library(metafor)

# Load data
data <- read.csv("../data/raw_studies.csv")

# Logit Transformed Proportion
es <- escalc(measure="PLO", xi=event_count, ni=n, data=data)

# Overall Random-effects model (REML + Knapp-Hartung)
res <- rma(yi, vi, data=es, method="REML", test="knha")

# Subgroup Analysis: Initiation vs Persistence
res_sub <- rma(yi, vi, data=es, mods = ~ intervention_type - 1, method="REML", test="knha")

# Persistence-only Model
res_persist <- rma(yi, vi, data=es[es$intervention_type == "Persistence",], method="REML", test="knha")

# Initiation-only Model
res_init <- rma(yi, vi, data=es[es$intervention_type == "Initiation",], method="REML", test="knha")

# High-Quality Forest Plot
png("../paper/forest_plot.png", width=1200, height=800, res=120)
par(mar=c(5,4,2,2))
forest(res, transf=transf.ilogit, slab=data$study_id, header=TRUE, 
       main="PrEP Uptake Gap: Initiation vs. Persistence among AGYW in SSA",
       ilab=cbind(data$intervention_type, data$event_count, data$n),
       ilab.xpos=c(-2, -1.2, -0.6), cex=0.75, xlim=c(-3.5, 2.5),
       xlab="Proportion", rows=c(1:4, 7:11))
text(c(-2, -1.2, -0.6), res$k+3.5, c("Type", "Events", "N"), font=2, cex=0.75)

# Add group labels
text(-3.5, 12, "PrEP Initiation", pos=4, font=2, cex=0.8)
text(-3.5, 5, "PrEP Persistence", pos=4, font=2, cex=0.8)

# Add subgroup polygons
addpoly(res_init, row=6, transf=transf.ilogit, mlab="Initiation Pooled", cex=0.75)
addpoly(res_persist, row=0, transf=transf.ilogit, mlab="Persistence Pooled", cex=0.75)
dev.off()

# Funnel Plot
png("../paper/funnel_plot.png", width=600, height=600, res=100)
funnel(res, main="Funnel Plot: PrEP Initiation & Persistence")
dev.off()

# Save results
sink("../paper/analysis_summary.txt")
cat("=== Overall PrEP Meta-Analysis (AGYW) ===\n")
print(summary(res))

cat("\n\n=== SUBGROUP ANALYSIS: INITIATION VS PERSISTENCE ===\n")
print(res_sub)

cat("\n\n=== BACK-TRANSFORMED ESTIMATES ===\n")
cat(sprintf("Pooled Initiation: %.3f (95%% CI: %.3f - %.3f)\n", 
            transf.ilogit(res_init$b), transf.ilogit(res_init$ci.lb), transf.ilogit(res_init$ci.ub)))
cat(sprintf("Pooled Persistence: %.3f (95%% CI: %.3f - %.3f)\n", 
            transf.ilogit(res_persist$b), transf.ilogit(res_persist$ci.lb), transf.ilogit(res_persist$ci.ub)))
sink()

print("PrEP Persistence-focused meta-analysis complete.")
