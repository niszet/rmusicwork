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

  write_header(con, rsmf)

  # nf <- make_nf2(make_note_frame(rsmf)) # %>% filter(ch==i-1)

  write_tracks(con, rsmf)
}

#' @name write_header
#' @param con connection object
#' @param rsmf rmsf object
write_header <- function(con, rsmf){
  writeChar("MThd", con, eos=NULL)
#   dsize <- as.integer(subset(rsmf$header,item=="data_size")$val)
  # dsize <- headval_to_int(rsmf$header, "data_size")
  dsize <- as.integer(rsmf$header$data_size)
  stopifnot(dsize < 256)
  # tunit <- headval_to_int(rsmf, "timeunit")
  tunit <- as.integer(rsmf$header$time_unit)
  # ntracks <- headval_to_int(rsmf, "track")
  ntracks <- as.integer(rsmf$header$track)
  # fo <- headval_to_int(rsmf, "format")
  fo <- as.integer(rsmf$header$format)

  # writeBin(as.raw(c(0,0,0), as.raw(dsize)), con, endian = "big")
  # writeBin(as.raw(c(0,fo)), con, endian = "big")
  # writeBin(as.raw(c(0,ntracks)), con, endian = "big")
  writeBin(int_to_raws(dsize, length=4), con, endian = "big")
  writeBin(int_to_raws(fo, length=2), con, endian = "big")
  writeBin(int_to_raws(ntracks, length=2), con, endian = "big")
  writeBin(int_to_raws(tunit, length=2), con, endian = "big")
}

aaa <- function(data, size=NULL){
  stopifnot(is.integer(data))

  as.raw(data)
  c(rep(0,size-dsize), 1)
}

#' @name headval_to_int
#' @param rsmf rmsf object
#' @param rowname rowname as string
headval_to_int <- function(rsmf, rowname){
  as.integer(subset(rsmf$header,item==rowname)$val)
}

write_tracks <- function(con, rsmf){
  for (i in 1:length(rsmf$tracks)){
    write_track(con, rsmf$tracks[[i]]$data, rsmf$tracks[[i]]$size)
  }
}

write_track <- function(con, data, dsize){
  writeChar("MTrk", con, eos=NULL)
  # dn <- 0
  # writeBin(as.raw(c(0,0,0), as.raw(dsize)), con, endian="big")
  writeBin(int_to_raws(dsize, length=4), con, endian="big")

#  if (nrow(data)!=0){
#    dn <- data %>% mutate(dn=floor(log(dtime,128))+1) %>%
#      select(dn) %>% sum()
#  }
#
#  if(ch!=0){
#    tsize <- int_to_raws(nrow(data)*3+dn+3+2)
#  }else{
#    tsize <- int_to_raws(22+3+4+2)
#  }
#
#  for (i in 1:(4-length(tsize))){
#    tsize <- c(as.raw(0), tsize)
#  }
#
#  writeBin(tsize, con, endian="big")
#
#  if(ch==0){
#    # tempo
#    writeBin(as.raw(c(0)), con, endian = "big")
#    writeBin(as.raw(c(255,81,3,int_to_raws(500000))), con, endian = "big")
#    # smpte offset
#    writeBin(as.raw(c(0)), con, endian = "big")
#    writeBin(as.raw(c(255,84,5,0)), con, endian = "big")
#    # beat
#    writeBin(as.raw(c(0)), con, endian = "big")
#    writeBin(as.raw(c(255,84,5,4,4,24,8)), con, endian = "big")
#    # coard
#    writeBin(as.raw(c(0)), con, endian = "big")
#    writeBin(as.raw(c(255,84,5,0,0)), con, endian = "big")
#  }
#

  if(nrow(data)!=0){
    write_notes(con, data)
  }

#  if (ch==0){
#    write_dtime(con, 737)
#  }else{
#    write_dtime(con, 737-515)
#  }
#  writeBin(as.raw(c(255,47,00)),con, endian="big")

}

write_notes <- function(con, data){
  data <- dplyr::mutate(data, dtime=ifelse(is.na(abs_time-lag(abs_time)),
                                       abs_time, abs_time-lag(abs_time)))
  for (i in 1:nrow(data)){
    # print(paste(data[i,1], data[i,2], data[i,3], data[i,4], data[i,7]))
    write_note(con, data[i,1], data[i,2], data[i,3], data[i,4], data[i,7])
  }
}


write_note <- function(con, item, ch, height, val, dtime){

  # print(paste(item,ch,height,val,dtime))
  if(!is.na(dtime)){
    write_dtime(con, dtime)
  }

#   if(item=="FF"){
#    print("FF")
#  }
  if(is.na(val)){
    # val <- NA_integer_
    val <- as.integer(val)
  }

  # warning is happened when the val is actually string
  if(!is.na(suppressWarnings(as.integer(val)))){
    val <- as.integer(val)
  }

    len <- NULL
    if(item==255){
      #if(ch==84){
      ##  len <- 5
      #}else if(ch==88){
      #  len <- 4
      #}
      if(is.character(val)){
        lin <- c(int_to_raws(item), int_to_raws(ch), int_to_raws(height), as.raw(utf8ToInt(val)))
      }else{
        lin <- c(int_to_raws(item), int_to_raws(ch), int_to_raws(height), int_to_raws(val, length = height))
      }
    }else{
      # print(val)
      lin <- c(int_to_raws(item), int_to_raws(height), int_to_raws(val, length = len))
    }
  writeBin(lin, con, endian = "big")
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

#' @name int_to_raws
#' @param val valur to convert to raw as integer
#' @param endian big or small by character
#' @param length max length of raw vector
int_to_raws <- function(val, endian="big", length=NULL){
  if (is.na(val)){
    return(NULL)
  }
  if(val==0){
    if(!is.null(length)){
      return(as.raw(rep(0, length)))
    }
      return(as.raw(0))
  }
  keta <- floor(log(val, 256))+1
  v <- c()
#  if( keta > 1){
    for (i in keta:1){
      v[[i]] <- as.raw(floor(val/(256**(i-1))))
      val <- val-floor(val/256**(i-1))*(256**(i-1))
    }
#  }else{
#    v <- as.raw(val)
#  }
  if(!is.null(length)){
    stopifnot(length >= keta)
    v <- c(v, as.raw(rep(0, length-keta)))
  }

  if (endian=="big"){
    rev(v)
  }else{
    v
  }
}

