

subDir <- paste0('forest_', mh_tool)
ifelse(!dir.exists(file.path(dir.fig, subDir)), dir.create(file.path(dir.fig, subDir)), 'Dir already exists.')
f <- paste0(dir.fig, subDir, '/', sub_ind_i, '_', 
            postfix_simplify, 
            postfix_subgroup, '_', today, 
            '.png'); print(f)
png(filename = f, 
    # width = 3000, height = 3000, units = "px", pointsize = 22,
    width = width_forestPlot, height = height_cm/1.5, units = "cm", res = 100)


meta::forest(ma_smd_data,
             # layout = "RevMan5",
             # sortvar = TE,
             prediction = F, ## prediction interval for the treatment effect in a future study
             col.predict="gray",
             addpred=TRUE, 
             print.tau2 = T,
             
             ##' specify which variables should be displayed on the left side of the forest plot. 
             leftcols = leftcols_show, 
             # leftlabs = c(),
             # lab.e = "Post-intervention",
             # lab.c = "Pre-intervention",
             
             # xlim = 's', # “s” to produce symmetric forest plots
             
             # print.byvar = FALSE, ## whether the name of the grouping variable should be printed in front of the group labels.
             # label.left = "loss",
             # label.right = "gain",
             digits=2,
             digits.sd = 2,
             digits.tau2 = 2,
             colgap = "0.2cm",
             # colgap.forest = "1cm",
             colgap.forest.left = '1.8cm',
             col.by = "black",
             col.square = "black",
             col.inside = "black",
             col.square.lines = "black",
             shade=TRUE,
             test.effect.subgroup.random = test.effect.subgroup.random_para,
             pooled.totals=T,
             overall = T,
             overall.hetstat = T)

# forest(ma_smd_data, 
#        addpred=TRUE, 
#        header=TRUE, 
#        # xlim=c(-1,1), 
#        # slab = study_label, 
#        digits.sd = 2,
#        digits.tau2 = 2,
#        shade=TRUE)

# Add a title to the entire plot
grid::grid.text(label = sub_ind_i, x = 0, y = .98, just = 'left', gp=gpar(cex=1, fontface='bold'))

dev.off()