makeFootnote <- function#Adds footnotes to a plot
###Creates a footnote in the right corner of the plot. If MyScript defined add this to the footnote, else takes the actual working dir, or whatever is defined as parameter footnoteText.
###FunctionGroup:
###Tools
(footnoteText= paste(Sys.getenv("USER"),getwd(),format(Sys.time(), "%d %b %Y %T"),sep=" ; "),
###Is the string of the footnote
size= .4,
###Defines the footnote size
color= grey(.5)
###Defines the footnote color
) {
   require(grid)
   pushViewport(viewport())
   if (exists("MyScript")) {
     footnoteText=paste(Sys.getenv("USER"),MyScript,format(Sys.time(), "%d %b %Y %T"),sep=" ; ")
   }
   grid.text(label= footnoteText,x = unit(1,"npc") - unit(2, "mm"),y= unit(0.5, "mm"),just=c("right", "bottom"),gp=gpar(cex= size, col=color))
   popViewport()
### No return, but updates the actual viewport (the actual plot)
}
