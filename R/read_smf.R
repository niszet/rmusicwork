
read_data_size <- function(con, an=1L, asize=4L, endian="big"){
  tmp <- readBin(con, "integer", n=an, size=asize, endian = endian)
  # list("data_size", tmp)
  c("data_size", tmp)
}

read_mthd <- function(con){
  tmp <- readChar(con, 4L, useBytes=TRUE)
  # list("MThd", tmp)
  c("MThd", tmp)
}

read_format <- function(con){
  tmp <- readBin(con, "integer", n=1L, size=2L,endian = "big")
  # list("format", tmp)
  c("format", tmp)
}

read_track <- function(con){
  tmp <- readBin(con, "integer", n=1L, size=2L,endian = "big")
  # list("track", tmp)
  c("track", tmp)
}

read_time_unit<- function(con){
  tmp <- readBin(con, "integer", n=1L, size=2L,endian = "big")
  # list("timeunit", tmp)
  c("timeunit", tmp)
}

read_header <- function(con){
  # MThd
  # tmp <- readChar(con, 4L, useBytes=TRUE)
  # print(tmp)
  smf <- data.frame(stringsAsFactors=FALSE)
  smf <- rbind(smf, read_mthd(con), stringsAsFactors=FALSE)
  colnames(smf) <- c("item", "val")
  # Data size
  # tmp <- readBin(con, "integer", n=1L, size=4L,endian = "big")
  # print(tmp)
  smf <- rbind(smf, read_data_size(con), stringsAsFactors=FALSE)
  # Format
  # tmp <- readBin(con, "integer", n=1L, size=2L,endian = "big")
  # print(tmp)
  smf <- rbind(smf, read_format(con), stringsAsFactors=FALSE)
  # track
  # tmp <- readBin(con, "integer", n=1L, size=2L,endian = "big")
  # print(tmp)
  smf <- rbind(smf, read_track(con), stringsAsFactors=FALSE)
  # time unit
  # tmp <- readBin(con, "integer", n=1L, size=2L,endian = "big")
  # print(tmp)
  smf <- rbind(smf, read_time_unit(con), stringsAsFactors=FALSE)
}

read_track_data_size <- function(con){
  tmp <- readBin(con, "integer", n=1L, size=4L,endian = "big")
  c("data_size", tmp, NA, NA)
}

read_mtrk <- function(con){
  tmp <- readChar(con, 4L, useBytes=TRUE)
  if(tmp=="MTrk"){
    return(c("MTrk", NA, NA, NA))
  }
  return(list(NA,NA))
}

read_smf <- function(file="C:/Users/niszet/Documents/Finale Files/aaa.mid"){
  con <- file(file, "rb")
  on.exit(close(con))
  file_size <- file.info(file)[["size"]]

  smf_header <- data.frame(stringsAsFactors=FALSE)
  smf_header <- rbind(smf_header, c("fileSize", file_size), stringsAsFactors =FALSE)
  colnames(smf_header) <- c("item", "val")
  # return(smf)

  # smf2 <- rbind(smf, read_header(con))
  # return(smf)
  # return(read_header(con))
  smf_header <- rbind(smf_header,read_header(con),stringsAsFactors=FALSE)
  # smf <- rbind(smf,read_header(con),stringsAsFactors=FALSE)
  # smf <- NULL
  # print("-- end of header --")
  smf_data <- data.frame(stringsAsFactors=FALSE)

  while(file_size != seek(con, where=NA)){

    tmp <- read_mtrk(con)
    if(all(is.na(tmp))){
      stop("MTrk is needed")
    }
    smf_data <- rbind(smf_data, tmp, stringsAsFactors=FALSE)
    tmp <- read_track_data_size(con)
    smf_data <- rbind(smf_data, tmp, stringsAsFactors=FALSE)
    track_end_point <- seek(con, where=NA)+as.integer(tmp[2])

    while(seek(con, where=NA)<track_end_point){
      tmp <- read_dtime(con)
      smf_data <- rbind(smf_data, tmp, stringsAsFactors=FALSE)
      tmp <- read_ctrl(con)
      smf_data <- rbind(smf_data, tmp, stringsAsFactors=FALSE)
    }
  }
  colnames(smf_data) <- c("item", "ch","type","val")
  #smf <- list("header"=smf_header, "data"=smf_data)

  smf <- c( list("header" = smf_header),
  list("data" = smf_data))
  class(smf) <- "rsmf"
  smf

  # print("EOF")
}

read_ctrl <- function(con){
  tmp <- readBinInt(con)

  tmpu <- bitops::bitShiftR(tmp, 4) #extract upper 4bits
  tmpl <- bitops::bitAnd(tmp, 15) # extract lower 4bits

  if(tmpu==8){
    # 8n note off
    chn <- tmpl
    # print("8 :")
    type <- readBinInt(con)
    val <- readBinInt(con)
    return(list("8", chn, type, val))
  }
  if(tmpu==9){
    # 9n note on
    chn <- tmpl
    type <- readBinInt(con)
    val <- readBinInt(con)
    # print(paste0("9 :", chn, " ", type, " ", val))
    return(list("9", chn, type, val))
  }
  if(tmpu==10){
    # An polyphonic key
    chn <- tmpl
    type <- readBinInt(con)
    val <- readBinInt(con)
    return(list("A", chn, type, val))
  }
  if(tmpu==11){
    # Bn control change
    # 4byte code under some condition
    chn <- tmpl
    type <- readBinInt(con)
    val <- readBinInt(con)
    return(list("B", chn, type, val))
  }
  if(tmpu==12){
    # Cn program change
    chn <- tmpl
    val <- readBinInt(con)
    return(list("C", chn, NA, val))
  }
  if(tmpu==13){
    # Dn channel pressure
    chn <- tmpl
    val <- readBinInt(con)
    return(list("D", chn, NA, val))
  }
  if(tmpu==14){
    # En pitch bend
    chn <- tmpl
    mm <- readBinInt(con)
    ll <- readBinInt(con)
    val <- mm*128+ll
    return(list("E", chn, NA, val))
  }

  if(tmpu==15){
  # FF commands
    if(tmpl==15){
      meta_event <- readBinInt(con)
      d_len <- readBinInt(con)
      me_data <- readBinInt(con, n=d_len)
      # sequenceNumber
      if(meta_event==0){
        # print(paste0("FF ", meta_event, " " , d_len, " " , intToUtf8(me_data)))
        return(list("FF", meta_event, d_len, intToUtf8(me_data)))
      }
      # text
      if(meta_event==1){
        # print(paste0("FF ", meta_event, " " , d_len, " " , intToUtf8(me_data)))
        return(list("FF", meta_event, d_len, intToUtf8(me_data)))
      }
      # copyright
      if(meta_event==2){
        # print(paste0("FF ", meta_event, " " , d_len, " " , intToUtf8(me_data)))
        return(list("FF", meta_event, d_len, intToUtf8(me_data)))
      }
        # sequenceName
        if(meta_event==03){
          # print(paste0("FF ", meta_event, " " , d_len, " " , intToUtf8(me_data)))
          return(list("FF", meta_event, d_len, intToUtf8(me_data)))
        }
        # instruments name
        if(meta_event==04){
          # print(paste0("FF ", meta_event, " " , d_len, " " , intToUtf8(me_data)))
          return(list("FF", meta_event, d_len, intToUtf8(me_data)))
        }
        # deviceName
        if(meta_event==09){
          # print(paste0("FF ", meta_event, " " , d_len, " " , intToUtf8(me_data)))
          return(list("FF", meta_event, d_len, intToUtf8(me_data)))
        }
        # SMPTE offset
        if(meta_event==84){
          # print(paste0("FF ", meta_event, " " , d_len, " " , mbyte_to_int_big(me_data)))
          return(list("FF", meta_event, d_len, mbyte_to_int_big(me_data)))
        }
        # haku
        if(meta_event==88){
          # return(list("FF", meta_event, d_len, me_data))
          return(list("FF", meta_event, d_len, mbyte_to_int_big(me_data)))
        }
        # coard
        if(meta_event==89){
          # print(me_data[1]) # number of # as positive integer, b as negative int.
          # print(me_data[2]) # 0 is majar, 1 is minar
          # return(list("FF", meta_event, d_len, me_data))
          return(list("FF", meta_event, d_len, mbyte_to_int_big(me_data)))
        }
        # tempo
        if(meta_event==81){
          return(list("FF", meta_event, d_len, mbyte_to_int_lit(me_data)))
        }
        # track end
        if(meta_event==47){
          # print("->track end")
          return(list("FF", meta_event, d_len, NA))
        }
        print("unmatched FF")
        return(list("FF", meta_event, d_len, NA))
      }
      print("unmatched F*")
      return(list("F*", NA, NA, NA))
    }
  print(paste0("unmatched ** ", tmp))
  return(list("**", NA, NA, NA))
}

mbyte_to_int_big <- function(vec){
  sum(256**seq(0, length(vec)-1) * vec)
}

mbyte_to_int_lit <- function(vec){
  sum(256**seq(length(vec)-1, 0, by=-1) * vec)
}

read_dtime <- function(con){
  stmp <- 0
  tmp <- readBinInt(con)
  while(tmp>127){
    tmp <- tmp - 128
    stmp <- (stmp + tmp)*128
    tmp <- readBinInt(con)
  }
  stmp <- stmp + tmp
  return(list("Delta Time", stmp, NA, NA))
}

readBinInt <- function(con, what="integer", n=1L, size=1L, endian="big", signed=FALSE){
  readBin(con, what=what, n=n, size=size, endian=endian, signed=signed)
}
