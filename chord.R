library(circlize)
library(png)
library(ComplexHeatmap)
library(rmatio)
library(latex2exp)

outpath = './'
cdmi_csv = paste(outpath, 'data_.csv', sep='')
domain_png = './domains/'

#load data
cdmi = read.csv(cdmi_csv, header=TRUE)

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
  circos.raster(image1, CELL_META$xcenter, CELL_META$ycenter-.45, width = width,
                facing = "inside", niceFacing = TRUE)
}, track.index = 1, bg.border = NA, bg.col = 'white')

circos.clear()

pushViewport(viewport(x = 0.2, y = 0.125, just="left"))
lgd_links = Legend(at = c(-round(max(abs(cdmi$value)), 2), -round(-10*log10(.05), 2), 0, round(-10*log10(.05), 2), round(max(abs(cdmi$value)), 2)), 
                   col_fun = col_fun_links, title_position = "topcenter", title = TeX("\\mathit{-sign($\\beta$)*10log_{10}(p_{adj})}"),
                   direction = "horizontal")
grid.draw(lgd_links)

dev.off()

