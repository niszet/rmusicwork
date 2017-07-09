
make_note_frame <- function(smf){
  #smf$tracks
  #item == 9 get type
  #find item == 8 with type calculate length of time
  stopifnot(class(smf)=="rsmf")

  tmp <-  data.frame(stringsAsFactors = FALSE)
  k <- 0
  for (i in 1:length(smf$tracks)){
    k <- k+1
    tmp <- rbind(tmp, smf$tracks[[k]])
  }

  tmp <-  tmp %>% filter(item %in% c(8,9))

  notes <-  as.data.frame(list("ch"=NULL,"type"=NULL, "val"=NULL, "start_time"=NULL, "end_time"=NULL))
  k <- 0
  for (i in 1:nrow(tmp)){
    k <- k+1
    rr <- tmp[k,]
    if( rr$item == 9){
      if( rr$val != 0){
        # note on
        note <- list("ch"=rr$ch,"type"=rr$type, "val"=rr$val, "start_time"=rr$abs_time, "end_time"=NULL)
        # find note off time
        m <- k
        for( j in k:nrow(tmp)){
          tmpx <-  tmp[m,]
          if( (tmpx$item == 8 || (tmpx$item==9 && tmpx$val==0)) && tmpx$type==note$type){
            # note off found
            note$end_time <- tmpx$abs_time
            break
          }
          m <- m+1
        }
        notes <- rbind(notes, as.data.frame(note))
      }
    }
  }
  return(notes)
}



