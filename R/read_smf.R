#' SMF read funcition
#'
#' @name read_smf
#' @param file input file path
#' @export
#'
read_smf <- function(file){
  con <- file(file, "rb")
  on.exit(close(con))
  file_size <- file.info(file)[["size"]]

  # smf_header <- data.frame(stringsAsFactors=FALSE)
  # smf_header <- rbind(smf_header, c("fileSize", file_size), stringsAsFactors =FALSE)
  # colnames(smf_header) <- c("item", "val")

  # smf_header <- rbind(smf_header, read_header(con), stringsAsFactors=FALSE)
  smf_header <- read_header(con)
  # TODO the number of track is written in header. to clarify this point of view, it should be separate the each tracks.
  # smf_data <- data.frame(stringsAsFactors=FALSE)
  smf_data <- list()

  while(file_size != seek(con, where=NA)){
    tmp <- read_mtrk(con)
    abs_time <- 0
    # smf_track <- data.frame(stringsAsFactors=FALSE)
    smf_track <- list()
    smf_track$data <- data.frame(stringsAsFactors=FALSE)
    if(all(is.na(tmp))){
      stop("MTrk is needed")
    }

    # skipping to add MTrk
    # smf_track <- rbind(smf_track, tmp, stringsAsFactors=FALSE)

    # smf_data <- rbind(smf_data, tmp, stringsAsFactors=FALSE)
    track_size <- read_track_data_size(con)
    smf_track$size <- as.integer(track_size)

    # skipping to add track_data_size
    # smf_track <- rbind(smf_track, tmp, stringsAsFactors=FALSE)
    # smf_data <- rbind(smf_data, tmp, stringsAsFactors=FALSE)
    # track_end_point <- seek(con, where=NA) + as.integer(tmp[2])
    track_end_point <- seek(con, where=NA) + as.integer(track_size)

    # check the end of the track
    while(seek(con, where=NA) < track_end_point){
      tmp <- .read_dtime(con)
      # smf_data <- rbind(smf_data, tmp, stringsAsFactors=FALSE)
      abs_time <- abs_time + tmp
      # smf_track <- rbind(smf_track, tmp, stringsAsFactors=FALSE)
      tmp <- .read_ctrl(con)
      tmp[[length(tmp)+1]] <- abs_time
      # smf_data <- rbind(smf_data, tmp, stringsAsFactors=FALSE)

      # smf_track <- rbind(smf_track, tmp, stringsAsFactors=FALSE)
      smf_track$data <- rbind(smf_track$data, tmp, stringsAsFactors=FALSE)
    }
    # colnames(smf_track) <- c("item", "ch", "type", "val", "com", "abs_time")
    colnames(smf_track$data) <- c("item", "ch", "type", "val", "com", "abs_time")
    # smf_track[["abstime"]] <- 0
    # smf_data[[length(smf_data)+1]]$data <- smf_track
    smf_data[[length(smf_data)+1]] <- smf_track
  }

  # colnames(smf_data) <- c("item", "ch", "type", "val", "com")
  #smf <- list("header"=smf_header, "data"=smf_data)

  smf <- c( list("header" = smf_header),
            list("tracks" = smf_data))

  class(smf) <- "rsmf"

  smf

  # print("EOF")
}

#' This is internal function
#'
read_data_size <- function(con, an=1L, asize=4L, endian="big"){
  tmp <- readBin(con, "integer", n=an, size=asize, endian = endian)
  # list("data_size", tmp)
  # c("data_size", tmp)
  tmp
}

#' This is internal function
#'
read_mthd <- function(con){
  # tmp <- readChar(con, 4L, useBytes=TRUE)
  tmp <- readChar(con, 4L, useBytes=TRUE)
  # list("MThd", tmp)
  #c("MThd", tmp)
  tmp
}

#' This is internal function
#'
read_format <- function(con){
  #tmp <- readBin(con, "integer", n=1L, size=2L,endian = "big")
  tmp <- readBin(con, "integer", n=1L, size=2L,endian = "big")
  # list("format", tmp)
  # c("format", tmp)
  tmp
}

#' This is internal function
#'
read_track <- function(con){
  # tmp <- readBin(con, "integer", n=1L, size=2L,endian = "big")
  tmp <- readBin(con, "integer", n=1L, size=2L,endian = "big")
  # list("track", tmp)
  # c("track", tmp)
  tmp
}

#' This is internal function
#'
read_time_unit<- function(con){
  # tmp <- readBin(con, "integer", n=1L, size=2L,endian = "big")
  tmp <- readBin(con, "integer", n=1L, size=2L,endian = "big")
  # list("timeunit", tmp)
  # c("timeunit", tmp)
  tmp
}

#' This is internal function
read_header <- function(con){
  # MThd
  # tmp <- readChar(con, 4L, useBytes=TRUE)
  # print(tmp)
  head <- list()
  # smf <- data.frame(stringsAsFactors=FALSE)
  # smf <- rbind(smf, .read_mthd(con), stringsAsFactors=FALSE)
  head$mthd <- read_mthd(con)
  # colnames(smf) <- c("item", "val")
  # Data size
  # smf <- rbind(smf, .read_data_size(con), stringsAsFactors=FALSE)
  head$data_size <- read_data_size(con)

  # smf$data_size <- .read_data_size(con)
  # Format
  # smf <- rbind(smf, .read_format(con), stringsAsFactors=FALSE)
  head$format <- read_format(con)
  # smf$format <- .read_format(con)
  # track
  # smf <- rbind(smf, .read_track(con), stringsAsFactors=FALSE)
  head$track <- read_track(con)
  # smf$track <- .read_track(con)
  # time unit
  # smf <- rbind(smf, .read_time_unit(con), stringsAsFactors=FALSE)
  head$time_unit <- read_time_unit(con)
  # smf$time_unit <- .read_time_unit(con)
  head
}

#' This is internal function
read_track_data_size <- function(con){
  tmp <- readBin(con, "integer", n=1L, size=4L,endian = "big")
  # c("data_size", tmp, NA, NA)
  tmp
}

#' This is internal function
read_mtrk <- function(con){
  tmp <- readChar(con, 4L, useBytes=TRUE)
  if(tmp=="MTrk"){
    return(c("MTrk", NA, NA, NA, NA, 0))
  }
  return(list(NA,NA))
}

#' This is internal function
.read_ctrl <- function(con){
  tmp <- readBinInt(con)

  tmpu <- bitops::bitShiftR(tmp, 4) #extract upper 4bits
  tmpl <- bitops::bitAnd(tmp, 15) # extract lower 4bits

  if(tmpu==8){
    # 8n note off
    chn <- tmpl
    type <- readBinInt(con)
    val <- readBinInt(con)
    com <- "Note off"
    # return(list("8", chn, type, val, "Note Off"))
    return(list(tmp, chn, type, val, com))
  }
  if(tmpu==9){
    # 9n note on
    chn <- tmpl
    type <- readBinInt(con)
    val <- readBinInt(con)
    com <- "Note On"
    # print(paste0("9 :", chn, " ", type, " ", val))
    # return(list(tmp, chn, type, val, "Note On"))
    return(list(tmp, chn, type, val, com))
  }
  if(tmpu==10){
    # An polyphonic key
    chn <- tmpl
    type <- readBinInt(con)
    val <- readBinInt(con)
    com <- "polyphonic key"
    # return(list("A", chn, type, val, "polyphonic key"))
    return(list(tmp, chn, type, val, com))
  }
  if(tmpu==11){
    # Bn control change
    # 4byte code under some condition
    chn <- tmpl
    type <- readBinInt(con)
    val <- readBinInt(con)
    com <- "control change"
    # return(list("B", chn, type, val, "control change"))
    return(list(tmp, chn, type, val, com))
  }
  if(tmpu==12){
    # Cn program change
    chn <- tmpl
    type <- NA
    val <- readBinInt(con)
    com <- "program change"
    # return(list("C", chn, NA, val, "program change"))
    # return(list(tmp, chn, NA, val, "program change"))
    return(list(tmp, chn, type, val, com))
  }
  if(tmpu==13){
    # Dn channel pressure
    chn <- tmpl
    type <- NA
    val <- readBinInt(con)
    com <- "channel pressure"
    # return(list("D", chn, NA, val, "channel pressure"))
    # return(list(tmp, chn, NA, val, "channel pressure"))
    return(list(tmp, chn, type, val, com))
  }
  if(tmpu==14){
    # En pitch bend
    chn <- tmpl
    type <- NA
    mm <- readBinInt(con)
    ll <- readBinInt(con)
    # val <- mm*128+ll
    val <- ll*128+mm # little endian
    com <- "pitch bend"
    # return(list("E", chn, NA, val, "pitch bend"))
    return(list(tmp, chn, type, val, com))
  }

  if(tmpu==15){
    if(tmpl==15){
      # FF commands
      meta_event <- readBinInt(con)
      d_len <- readBinInt(con)
      me_data <- readBinInt(con, n=d_len)
      # sequenceNumber
      if(meta_event==0){
        # print(paste0("FF ", meta_event, " " , d_len, " " , intToUtf8(me_data)))
        # return(list("FF", meta_event, d_len, intToUtf8(me_data), "Sequence Number"))
        com <- "Sequence Number"
        return(list(tmp, meta_event, d_len, intToUtf8(me_data), com))
      }
      # text
      if(meta_event==1){
        # print(paste0("FF ", meta_event, " " , d_len, " " , intToUtf8(me_data)))
        # return(list("FF", meta_event, d_len, intToUtf8(me_data), "Text"))
        com <- "Text"
        return(list(tmp, meta_event, d_len, intToUtf8(me_data), com))
      }
      # copyright
      if(meta_event==2){
        # print(paste0("FF ", meta_event, " " , d_len, " " , intToUtf8(me_data)))
        # return(list("FF", meta_event, d_len, intToUtf8(me_data), "copy right"))
        com <- "copy right"
        return(list(tmp, meta_event, d_len, intToUtf8(me_data), com))
      }
      # sequenceName
      if(meta_event==03){
        # print(paste0("FF ", meta_event, " " , d_len, " " , intToUtf8(me_data)))
        # return(list("FF", meta_event, d_len, intToUtf8(me_data), "Sequence Name"))
        com <- "Sequence Name"
        return(list(tmp, meta_event, d_len, intToUtf8(me_data), com))
      }
      # instruments name
      if(meta_event==04){
        # print(paste0("FF ", meta_event, " " , d_len, " " , intToUtf8(me_data)))
        # return(list("FF", meta_event, d_len, intToUtf8(me_data), "Instruments Name"))
        com <- "Instruments Name"
        return(list(tmp, meta_event, d_len, intToUtf8(me_data), com))
      }
      # deviceName
      if(meta_event==09){
        # print(paste0("FF ", meta_event, " " , d_len, " " , intToUtf8(me_data)))
        # return(list("FF", meta_event, d_len, intToUtf8(me_data), "Device Name"))
        com <- "Device Name"
        return(list(tmp, meta_event, d_len, intToUtf8(me_data), com))
      }
      # SMPTE offset
      if(meta_event==84){
        # print(paste0("FF ", meta_event, " " , d_len, " " , mbyte_to_int_big(me_data)))
        # return(list("FF", meta_event, d_len, mbyte_to_int_big(me_data), "SMPTE offset"))
        com <- "SMPTE offset"
        return(list(tmp, meta_event, d_len, mbyte_to_int_big(me_data), com))
      }
      # haku
      if(meta_event==88){
        # return(list("FF", meta_event, d_len, me_data))
        # add_beat
        # nn dd cc bb
        # beat <- as.list(me_data)
        # names(beat) <- c("numerator", "denominator", "metro", "num32")
        # rsmf$beat <- beat
        # return(rsmf)
        # return(list("FF", meta_event, d_len, mbyte_to_int_big(me_data), "haku"))
        com <- "haku"
        return(list(tmp, meta_event, d_len, mbyte_to_int_big(me_data), com))
      }
      # coard
      if(meta_event==89){
        # print(me_data[1]) # number of # as positive integer, b as negative int.
        # print(me_data[2]) # 0 is majar, 1 is minar
        # return(list("FF", meta_event, d_len, me_data))
        # return(list("FF", meta_event, d_len, mbyte_to_int_big(me_data), "coard"))
        com <- "coard"
        return(list(tmp, meta_event, d_len, mbyte_to_int_big(me_data), com))
      }
      # tempo
      if(meta_event==81){
        # changed from little
        # return(list("FF", meta_event, d_len, mbyte_to_int_lit(me_data), "tempo"))
        com <- "tempo"
        return(list(tmp, meta_event, d_len, mbyte_to_int_big(me_data), com))
      }
      # track end
      if(meta_event==47){
        # print("->track end")
        # return(list("FF", meta_event, d_len, NA, "track_end"))
        com <- "track_end"
        return(list(tmp, meta_event, d_len, NA, com))
      }
        warning("unmatched FF")
        com <- "track_end"
        return(list(tmp, meta_event, d_len, NA, com))
      }
      warning("unmatched F*")
      #return(list("F*", NA, NA, NA, "F*"))
      return(list(tmp, NA, NA, NA, "F*"))
    }
  warning(paste0("unmatched ** ", tmp))
  # return(list("**", NA, NA, NA, NA))
  return(list(tmp, NA, NA, NA, NA))
}

mbyte_to_int_lit <- function(vec){
  sum(256**seq(0, length(vec)-1) * vec)
}

mbyte_to_int_big <- function(vec){
  sum(256**seq(length(vec)-1, 0, by=-1) * vec)
}

#' This is internal function
.read_dtime <- function(con){
  stmp <- 0
  tmp <- readBinInt(con)
  # delta time is defined as "signed" when the value is over 127
  while(tmp>127){
    tmp <- tmp - 128
    stmp <- (stmp + tmp)*128
    tmp <- readBinInt(con)
  }
  stmp <- stmp + tmp
  stmp
  #return(list("Delta Time", stmp, NA, NA, NA))
}

readBinInt <- function(con, what="integer", n=1L, size=1L, endian="big", signed=FALSE){
  readBin(con, what=what, n=n, size=size, endian=endian, signed=signed)
}
