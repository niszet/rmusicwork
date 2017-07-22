#' SMF write funcition
#'
#' @name write_smf
#' @param file output file path
#' @param rsmf output data
#' @export
#'
write_smf <- function(file, rsmf){
  con <- file(file, "wb")
  on.exit(close(con))

  #rsmf_head <- get_header(rsmf)
  #wr_state <- write_header(con,rsmf_head)
  # ntracks <- length(rsmf$tracks)
  ntracks <- as.integer(subset(rsmf$header,item=="track")$val)

  write_header(con, as.integer(subset(rsmf$header,item=="track")$val),
              as.integer(subset(rsmf$header,item=="timeunit")$val))

  # rsmf_tracks <- get_tracks(rsmf)

  # if( ntracks > 1){
  #  smf_format <- 1
  #}else{
  #  smf_format <- 0
  #}
  smf_format <- as.integer(subset(rsmf$header,item=="format")$val)

  nf <- make_nf2(make_note_frame(rsmf)) # %>% filter(ch==i-1)

  # writing tracks
  for (i in 1:ntracks){
    nf2 <- nf %>% filter(ch==i-1)
    write_track(con, nf2, i-1)
  }
}

#' @name write_header
#' @param con connection object
#' @param ntracks number of tracks
#' @param tunit timeunits
#'
write_header <- function(con, ntracks, tunit){
  writeChar("MThd", con, eos=NULL)
  writeBin(as.raw(c(0,0,0,6)), con, endian = "big")
  writeBin(as.raw(c(0,1)), con, endian = "big")
  writeBin(as.raw(c(0,ntracks)), con, endian = "big")
  writeBin(int_to_raws(tunit), con, endian = "big")
}

write_track <- function(con, data, ch){
  writeChar("MTrk", con, eos=NULL)

  dn <- 0

  if (nrow(data)!=0){
    dn <- data %>% mutate(dn=floor(log(dtime,128))+1) %>%
      select(dn) %>% sum()
  }

  if(ch!=0){
    tsize <- int_to_raws(nrow(data)*3+dn+3+2)
  }else{
    tsize <- int_to_raws(22+3+4+2)
  }

  for (i in 1:(4-length(tsize))){
    tsize <- c(as.raw(0), tsize)
  }

  writeBin(tsize, con, endian="big")

  if(ch==0){
    # tempo
    writeBin(as.raw(c(0)), con, endian = "big")
    writeBin(as.raw(c(255,81,3,int_to_raws(500000))), con, endian = "big")
    # smpte offset
    writeBin(as.raw(c(0)), con, endian = "big")
    writeBin(as.raw(c(255,84,5,0)), con, endian = "big")
    # beat
    writeBin(as.raw(c(0)), con, endian = "big")
    writeBin(as.raw(c(255,84,5,4,4,24,8)), con, endian = "big")
    # coard
    writeBin(as.raw(c(0)), con, endian = "big")
    writeBin(as.raw(c(255,84,5,0,0)), con, endian = "big")
  }

  if(nrow(data)!=0){
    write_notes(con, data)
  }
  if (ch==0){
    write_dtime(con, 737)
  }else{
    write_dtime(con, 737-515)
  }
  writeBin(as.raw(c(255,47,00)),con, endian="big")

}

write_notes <- function(con, data){
  for (i in 1:nrow(data)){
    write_note(con, data[i,5], data[i,1], data[i,2], data[i,3], data[i,4], data[i,6])
  }
}


write_dtime <- function(con, val){
  if(is.na(val)){
    return(NA)
  }
  val_a <-  val
  keta <- floor(log(val, 128))+1
  v <- c()
  if( keta > 1){
    for (i in keta:1){
      if( i!=1 ){
        # add 1***_**** (add 128 as integer)
        v[[keta-i+1]] <- as.raw(floor(val/(128**(i-1)))+128)
        val <- val-floor(val/128**(i-1))*(128**(i-1))
      }else{
        v[[keta-i+1]] <- as.raw(floor(val/(128**(i-1))))
        val <- val-floor(val/128**(i-1))*(128**(i-1))
      }
    }
  }else{
    v <- as.raw(val)
  }
  writeBin(v, con, endian = "little")
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

write_note <- function(con, item, ch, height, vol, time, dtime){
  if(!is.na(dtime)){
    write_dtime(con, dtime)
  }
  lin <- c(as.raw(item), int_to_raws(height), int_to_raws(vol))
  writeBin(lin, con, endian = "big")
}

int_to_raws <- function(val, endian="big"){
  if (is.na(val)){
    return()
  }
  val_a <-  val
  keta <- floor(log(val, 256))+1
  v <- c()
  if( keta > 1){
    for (i in keta:1){
      v[[i]] <- as.raw(floor(val/(256**(i-1))))
      val <- val-floor(val/256**(i-1))*(256**(i-1))
    }
  }else{
    v <- as.raw(val)
  }
  if (endian=="big"){
    rev(v)
  }else{
    v
  }
}
