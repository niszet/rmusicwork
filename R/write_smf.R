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
  ntracks <- length(xxx2$tracks)
  write_header(con, ntracks, 1024)

  # rsmf_tracks <- get_tracks(rsmf)

  if( ntracks > 1){
    smf_format <- 1
  }else{
    smf_format <- 0
  }
  for (i in 1:ntracks){
    nf <- make_nf2(make_note_frame(rsmf)) %>% filter(ch==i-1)
    write_track(con, nf)
  }
}

write_header <- function(con, ntracks, tunit){
  writeChar("MThd", con, eos=NULL)
  writeBin(as.raw(c(0,0,0,6)), con, endian = "big")
  writeBin(as.raw(c(0,1)), con, endian = "big")
  writeBin(as.raw(c(0,ntracks)), con, endian = "big")
  writeBin(int_to_raws(tunit), con, endian = "big")
}

write_track <- function(con, data){
  writeChar("MTrk", con, eos=NULL)
  dn <- 0
  if (nrow(data)!=0){
  dn <- data %>% select(dtime) %>% mutate(dn=floor(dtime/128)) %>% select(dn) %>% sum()
  }
  tsize <- int_to_raws(nrow(data)*3+dn+3)
  for (i in 1:(4-length(tsize))){
    tsize <- c(as.raw(0), tsize)
  }
  writeBin(tsize, con, endian="big")
  if(nrow(data)!=0){
    write_notes(con, data)
  }
  writeBin(as.raw(c(255,47,00)),con, endian="big" )
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
      v[[i]] <- as.raw(floor(val/(128**(i-1))))
      val <- val-floor(val/128**(i-1))*(128**(i-1))
    }
  }else{
    v <- as.raw(val)
  }
  writeBin(v, con, endian = "big")
}

make_nf2 <- function(nf){
  nf2 <- nf %>% select(-end_time) %>% mutate(item=9) %>% rename(time=start_time)
  nf3 <- nf %>% select(-start_time, -val) %>% mutate(item=8, val=0) %>% rename(time=end_time)
  ch <- nf3 %>% select(ch) %>% distinct()
  nf4 <- data.frame()
  for (i in ch$ch){
    nf4 <- rbind(nf4, rbind(nf2, nf3) %>% filter(ch==i) %>% arrange(ch, time) %>%
      mutate(dtime=ifelse(is.na(time-lag(time)), time, time-lag(time))) %>% mutate(ch=ch+1)
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
