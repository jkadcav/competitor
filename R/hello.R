library(events)

#' Retrieve competitor finish position
#'
#' @param eventId Database ID of the event
#' @param compEventId Event Competitor ID
#' @keywords competitors
#' @export
#' @examples
#' event_finish_positions( 1175769, 10962625 )
finish_position<-function( eventId, compEventId ) {
  event<-events::retrieve_event(eventId)
  a<-paste('event$event_competitors$`',compEventId,'`$finish_position',sep="")
  finishPosition<-eval(parse(text=a))

  return (finishPosition)
}

#' Retrieve competitor scratched property
#'
#' @param eventId Database ID of the event
#' @param compEventId Event Competitor ID
#' @keywords competitors
#' @export
#' @examples
#' scratched( 1175769, 10962625 )
scratched<-function( eventId, compEventId ) {
  event<-events::retrieve_event(eventId)
  a<-paste('event$event_competitors$`',compEventId,'`$scratched',sep="")
  scratched<-eval(parse(text=a))
  return (scratched)
}

#' Retrieve competitor scratched property
#'
#' @param eventId Database ID of the event
#' @keywords competitors
#' @export
#' @examples
#' scratchings( 1175769 )
scratchings<-function( eventId ) {
  event<-events::retrieve_event(eventId)
  rns<-names(eval(parse(text=paste('event$event_competitors',sep=""))))
  rn<-length(rns)
  eventid<-rep(eventId,rn)
  a<-mapply(scratched,eventid,rns)
  scr<-length(a[a==TRUE])
  return(scr)
}
