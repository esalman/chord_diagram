library(circlize)
library(png)
library(ComplexHeatmap)
library(rmatio)
library(latex2exp)

outpath = '../results/no_single_dom_states_3/'
cdmi_csv = paste(outpath, 'cdmi_reg_0.099612.csv', sep='')
domain_png = '../results/domains/'

#load data
dat = read.csv(cdmi_csv, header=TRUE)
cdmi = dat[,FALSE]
cdmi$from = paste(dat$domPair1_1, '-', dat$domPair1_2)
cdmi$to = paste(dat$domPair2_1, '-', dat$domPair2_2)
cdmi$value = sign(dat$beta) * -10 * log10(dat$padj)
# cdmi$rank = order(cdmi$value, decreasing = FALSE)

png(paste(outpath, 'chord.png', sep=''), width=8, height=8, units = 'in', res=300)

gap.degree = 5
# circos.par(gap.degree = gap.degree)
circos.par(track.height = .2)

col_fun_links = colorRamp2(c(-max(abs(cdmi$value)), -14, -13, 0, 13, 14, max(abs(cdmi$value))), 
                            c("Blue", "Cyan", "LightCyan", "White", "LightYellow", "Yellow", "Red"), space = 'LAB')

# Make the circular plot
chordDiagram(cdmi, 
             annotationTrack = 'grid',
             annotationTrackHeight = .02,
             preAllocateTracks = 2,
             transparency = .2,
             col = col_fun_links(cdmi$value),
             scale = FALSE,
             link.largest.ontop = TRUE)

# label track
idx = 2
for(si in get.all.sector.index()) {
  xlim = get.cell.meta.data("xlim", sector.index = si, track.index = idx)
  ylim = get.cell.meta.data("ylim", sector.index = si, track.index = idx)
  circos.text(mean(xlim), ylim[1], si, facing = "bending.outside", adj = c(0.5, 1),
              niceFacing = TRUE, cex = .6, col = "black", sector.index = si, track.index = idx)
}

# brain track
width = '1.5cm'
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  # if( CELL_META$xrange < 10 ) {
  #   return()
  # }
  image1 = as.raster(readPNG(paste(domain_png, CELL_META$sector.index, '.png', sep='')))
  circos.raster(image1, CELL_META$xcenter, CELL_META$ycenter-.85, width = width,
                facing = "inside", niceFacing = TRUE)
}, track.index = 1, bg.border = NA, bg.col = 'white')

# # centroids track
# centroids = read.mat(paste(outpath, 'centroids.mat', sep = ''))
# cdmi_pairs =  c('SC - SC', 'SC - SM', 'SC - VIS', 'SC - ATTN', 'SC - FRN', 'SC - DMN', 
#   'SM - SM', 'SM - VIS', 'SM - ATTN', 'SM - FRN', 'SM - DMN', 'VIS - VIS', 'VIS - ATTN', 
#   'VIS - FRN', 'VIS - DMN', 'ATTN - ATTN', 'ATTN - FRN', 'ATTN - DMN', 'FRN - FRN', 
#   'FRN - DMN', 'DMN - DMN')
# col_fun_heatmap = colorRamp2(c(-.2, -.1, 0, .1, .2), c("cyan", "blue", "white", "red", "yellow"), space = 'RGB')
# 
# circos.track(ylim = c(0, 10), panel.fun = function(x, y) {
#   if( CELL_META$xrange < 10 ) {
#     return()
#   }
#   i = match(CELL_META$sector.index, cdmi_pairs)
#   t1 = centroids$centroidImgs[[1]][i][[1]][1,,]
#   t2 = centroids$centroidImgs[[1]][i][[1]][2,,]
#   t3 = centroids$centroidImgs[[1]][i][[1]][3,,]
#   r = dim(t1)[1]
#   tt = cbind(t1, rep(0, r), t2, rep(0, r), t3)
#   print(dim(tt))
#   
#   col_mat = col_fun_heatmap(tt)
#   nr = nrow(tt)
#   nc = ncol(tt)
#   for(i in 1:nr) {
#     circos.rect(1:nc - 1, rep(nr - i, nc), 
#                 1:nc, rep(nr - i + 1, nc), 
#                 border = col_mat[i, ], col = col_mat[i, ])
#   }
# }, track.index = 3, bg.border = NA)

circos.clear()

pushViewport(viewport(x = 0.2, y = 0.125, just="left"))
lgd_links = Legend(at = c(-round(max(abs(cdmi$value)), 2), -round(-10*log10(.05), 2), 0, round(-10*log10(.05), 2), round(max(abs(cdmi$value)), 2)), 
                   col_fun = col_fun_links, title_position = "topcenter", title = TeX("\\mathit{-sign($\\beta$)*10log_{10}(p_{adj})}"),
                   direction = "horizontal")
grid.draw(lgd_links)

dev.off()

