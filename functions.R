#' Function to turn data frame into capture history matrix
#' @param data A data.frame with at least the following named columns:
#' \code{occasion}, numeric day or time index;
#' \code{post}, a factor vector of post (trap) names,
#' and \code{group}. It is the pairing of
#' \code{group} and \code{occasion} that forms the individual
#' "call id". Optional to include a column of bearings (in radians)
#' and/or distances. 
get.capt.hist <- function(data){
    occasion <- data$occasion
    post <- data$post
    group <- data$group
    cantor <- 1/2 * (occasion + group)* (occasion + group + 1) + group
    tmp <- data.frame(array = rep(1,nrow(data)), ID = cantor,
                      occasion = occasion, trap = as.numeric(data$post))
    if("bearing" %in% names(data)) {tmp$bearing <- data$bearing}
    if("distance" %in% names(data)) {tmp$dist <- data$distance}
    tmp <- tmp[order(tmp$ID),]
    capt.hist <- create.capt(tmp,n.traps = length(table(tmp$trap)))
    capt.hist
}
#' Function to plot mask along with trap locations in a 'tidy' presentable manner
#' @param mask mask object from \code{create.mask}
#' @param traps a matrix of trap locations used to create the \link{mask}
show.mask <- function(mask = NULL,traps = NULL){
    df <- data.frame(x = mask[,1], y = mask[,2])
    tb <- data.frame(x = traps$x, y = traps$y)
    g <- ggplot(df,aes(x, y)) + 
        geom_point(col = "grey") + xlab("x-axis") + ylab("y-axis") +
        theme(panel.background = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))
    g +  geom_point(data = tb,aes(x = tb$x,y = tb$y), col = "red", size = 3)
}

#' Function to plot Von Mises distribution of bearing measurement error
#' from model.
#' @param fit ascr model
show.dvm <- function(theta = NULL, kappa = NULL){
    val <- CircStats::dvm(theta = theta,mu = 0, kappa = kappa)
    plot(theta,val, type="l",xlim = c(-pi/2,pi/2),ylim = range(0,max(val)),
         main = "", axes = FALSE, xlab = "bearings (rad)", ylab = "")
    axis(1, at = c(-pi/2,0,pi/2), labels = c(expression(-pi/2),0,expression(pi/2)))
    axis(2)
}

#' Function to plotGamma distridution of distance measurtment error
#' from model.
#' @param fit ascr model
#' @param d distance of call/animal at which to plot
show.distgam <- function(x = NULL, shape = NULL,d = NULL){
    val <- dgamma(x = x, shape = shape,
                  scale = d/shape)
    plot(x,val, type="l",ylim = range(val), main = "", axes = FALSE, xlab = "distance (m)", ylab = "")
    axis(1)
    axis(2)    
}
    
#' Function to show capture history data, can also be used alongside model fit information
#' to show estimated locations
#' @param traps a matrix of trap locations
#' @param capt.hist a list of capture history information, an object returned by \link{get.capt.hist}
#' @param xlim x-axis limits of plot, by default this is based on trap locations
#' @param ylim y-axis limits of plot, by default this is based on trap locations
#' @param id call number to display, by default this is 1
#' @param show.axes logical, whhether to include plot axes
#' @examples
#' \dontrun{
#' detections <- shiny_example_detections
#' traps <- shiny_example_traps
#' detections <- shiny_example_detections
#' show.data(traps, capt.hist)}
show.data <- function(traps, capt.hist, xlim = NULL,ylim = NULL, id = 1,show.axes = FALSE){
    if(is.null(xlim)){
        xrang = range(traps[,1])
        diff = diff(xrang)
        xlim =  c(xrang[1] - diff,xrang[2] + diff)
    }
    if(is.null(ylim)){
        yrang = range(traps[,2])
        diff = diff(yrang)
        ylim =  c(yrang[1] - diff,yrang[2] + diff)
    }
    traps <- cbind(traps$x,traps$y)
    capt <- capt.hist$bincapt[id,]
    plot.new()
    plot.window(xlim = xlim, ylim = ylim, asp = 1)
    box()
    if (show.axes){
            axis(1)
            axis(2)
        }
    points(traps, col = 1, pch = 4, lwd = 2)
    points(traps[capt == 1, , drop = FALSE], col = 1, cex = 2, lwd = 2)
    if("bearing"%in%names(capt.hist) & !("dist"%in%names(capt.hist))){
        bearing = capt.hist$bearing[id, ]
        arrow.length = 0.05*min(c(diff(range(xlim)), diff(range(ylim))))
        bearing.capt = bearing[capt == 1]
        trappos = traps[which(capt == 1), , drop = FALSE]
        sinb = sin(bearing.capt)*arrow.length
        cosb = cos(bearing.capt)*arrow.length
        arrows(trappos[, 1], trappos[, 2], trappos[, 1] + sinb, trappos[, 2] + cosb,
               length = 0.1, col = 1, lwd = 2)
    }
    if("dist"%in%names(capt.hist) & !("bearing"%in%names(capt.hist))){
        distance = capt.hist$dist[id, ]
        dist.capt = distance[ capt == 1]
        trappos = traps[which(capt == 1), , drop = FALSE]
        for (i in 1:nrow(trappos)){
            centre = trappos[i, ]
            radius = dist.capt[i]
            circles(as.numeric(centre), radius, col = 1, lwd = 2)
        }
    }
    if("dist"%in%names(capt.hist) & "bearing"%in%names(capt.hist)){
        bearing = capt.hist$bearing[id, ]
        distance = capt.hist$dist[id, ]
        arrow.length = distance[ capt == 1]
        bearing.capt = bearing[capt == 1]
        trappos = traps[which(capt == 1), , drop = FALSE]
        sinb = sin(bearing.capt)*arrow.length
        cosb = cos(bearing.capt)*arrow.length
        arrows(trappos[, 1], trappos[, 2], trappos[, 1] + sinb, trappos[, 2] + cosb,
               length = 0.1, col = 1, lwd = 2)
    }
}

