#' rsmf to note_frame
#'
#' @name read_smf
#' @param smf rsmf object
#' @export
#'

make_note_frame <- function(smf){
  #smf$tracks
  #item == 9 get type
  #find item == 8 with type calculate length of time
  stopifnot(class(smf)=="rsmf")

  df <-  data.frame(stringsAsFactors = FALSE)
  k <- 0
  for (i in 1:length(smf$tracks)){
    k <- k+1
    df <- rbind(df, smf$tracks[[k]]$data)
  }

  df <- df %>% filter(between(item, 128, 159))

  notes <-  as.data.frame(list("ch"=NA,"height"=NA, "val"=NA_character_,
                               "start_time"=NA, "end_time"=NA, "nn"=NA), stringsAsFactors = FALSE)
  k <- 0
  for (i in 1:nrow(df)){
    k <- k+1
    rr <- df[k,]
    rr$ch <- rr$item %% 16 # extract ch
    rr$item <- (rr$item - rr$ch)/16 # convert item to deciaml
    if( rr$item == 9){
      if( rr$val != 0){
        # note on
        note <- list("ch"=rr$ch,"height"=rr$type, "val"=as.character(rr$val),
                     "start_time"=rr$abs_time, "end_time"=NULL, "nn"=rr$type %% 12)
        # find note off time
        m <- k
        for( j in k:nrow(df)){
          tmpx <- df[m,]
          tmpx$ch <- tmpx$ch %% 16
          tmpx$item <- (tmpx$item - tmpx$ch)/16

          if( (tmpx$item == 8 || (tmpx$item==9 && tmpx$val==0)) && tmpx$type ==note$height){
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
  notes <- na.omit(notes)
  notes$val <-  as.integer(notes$val)
  return(notes)
}



make_nf2 <- function(nf){
  # itme = 9 note on
  nf2 <- nf %>% select(-end_time) %>% mutate(item=144) %>% rename(time=start_time)
  # itme = 8 note off
  nf3 <- nf %>% select(-start_time, -val) %>% mutate(item=128, val=0) %>%
    rename(time=end_time)
  ch <- nf3 %>% select(ch) %>% distinct()
  nf4 <- data.frame()
  for (i in ch$ch){
    nf4 <- rbind(nf4, rbind(nf2, nf3) %>% filter(ch==i) %>% arrange(ch, time) %>%
      mutate(dtime=ifelse(is.na(time-lag(time)), time, time-lag(time))) %>%
        mutate(ch=ch+1)
    )
  }
  nf4
}
