# #' Plot multiple ggplot objects in one pane
# #' @source ggedit
#
#
# vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y)
#
# multiggplot <- function(obj,plot.layout=NULL){
#     if(!is.null(obj$UpdatedPlots)) obj=obj$UpdatedPlots
#     if(is.null(plot.layout)){
#         plot.layout=1:length(obj)
#         numPlots = length(obj)
#         cols=min(numPlots,2)
#         plotCols = cols
#         plotRows = ceiling(numPlots/plotCols)
#         grid::grid.newpage()
#         grid::pushViewport(grid::viewport(layout = grid::grid.layout(plotRows,plotCols)))
#         for (i in 1:numPlots) {
#             curRow = ceiling(i/plotCols)
#             curCol = (i-1) %% plotCols + 1
#             print(obj[[i]], vp = vplayout(curRow, curCol))
#         }
#     }else{
#         numPlots = length(obj)
#         plotRows=max(unlist(lapply(plot.layout,'[',1)))
#         plotCols=max(unlist(lapply(plot.layout,'[',2)))
#         grid::grid.newpage()
#         grid::pushViewport(grid::viewport(layout = grid::grid.layout(plotRows,plotCols)))
#         for (i in 1:numPlots) {
#             print(obj[[i]], vp = vplayout(plot.layout[[i]]$rows, plot.layout[[i]]$cols))
#         }
#     }
# }
