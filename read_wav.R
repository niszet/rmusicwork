

read_wav <- function(file, start_time=0, end_time=NULL, unit="sec", csound=NULL){

  connection <- file(file, "rb")
  on.exit(close(connection))
  file_size <- file.info(file)[["size"]]

  if(file_size > 1e8){
    stop(paste0("This file size is too heavy to open ", file_size))
  }
  wav <- list("fileSize"= file_size)
  wav <- c(wav, check_riff(connection),
           check_file_size_riff(connection),
           check_wave(connection), # 'WAVE'
           check_fmt(connection), # 'fmt '
           check_fmt_byte(connection), #
           get_fmt_id(connection),
           get_num_channel(connection),
           get_sample_rate(connection),
           get_data_verocity(connection),
           get_block_size(connection),
           get_bit_per_sample(connection)
  )

  if( wav[["formatByte"]]>16) {
    wesize <- get_extended_size(connection)
    wav <- c(wav, wesize)

    if(wesize[["extSize"]]>0){
      wehead <- get_header_extended(connection, asize=wesize[["extSize"]])
      wav <- c(wav, wehead)
    }else{
      wav <-  c(wav, list("extHead" = NULL))
    }

  }

  wav <- c(wav,
           check_data(connection),
           get_data_size(connection)
  )

  wch <- wav[["numChannel"]]
  wbps <- wav[["bitPerSample"]]
  wsr <- wav[["sampleRate"]]
  verocity <- wav[["verocity"]]
  bsample <- wav[["blockSample"]]

  header_size <- seek(connection, where=NA)

  # size_rest <- (file_size - 46)/(wbyte/8*wch) # for print music
  # size_rest <- (file_size - header_size)/(wbps/8*wch)

  # total_number_of_sample = total_byte / (byte/sample)
  size_rest <- (file_size - header_size)/bsample
  size_rest_org <- size_rest

  # velocity = byte/sec
  # sec * velocity (byte/sec) / (byte/sample) = sample

  tmpR <- NULL
  tmpL <- NULL
  tmp_time <-  1:size_rest_org # indexing

  # start and end time must be truncated to avoid error.


  if(unit == "sec"){
    if(start_time >0 ){
      seek(connection, trunc(verocity*start_time/8)*8, "current")
    }
    if(is.null(end_time)){
      size_rest <- size_rest - start_time*verocity/bsample
      # tmp_time <- tmp_time[1:size_rest_org > start_time*verocity/(wbps/8*wch)]
      tmp_time <- (start_time*verocity/bsample+1):size_rest_org * 1/wsr
      # print(length(tmp_time))
    }else{
      if(end_time > start_time){
        tmp_time <- (start_time*verocity/bsample+1):(end_time*verocity/bsample) * 1/wsr
        # print(length(tmp_time))
        # tmp_time <- tmp_time[(size_rest_org-size_rest+1):size_rest_org <= end_time*verocity/(wbps/8*wch)]
        # size_rest <- size_rest - (size_rest_org- end_time*verocity/(wbps/8*wch))
        # print(size_rest)
        size_rest <- trunc((end_time - start_time)*verocity/bsample)
        # print(size_rest)

      }else{
        stop("end_time must be greater than start_time")
      }
    }
  }

  tmpR <- get_data(connection, an=size_rest*wch, asize = wbps/8)

  len <-  length(tmpR)
  tmpL <- tmpR[seq(1, len, +2)]
  tmpR <- tmpR[seq(0, len, +2)]

  tmp_df <- list(tmp_time, tmpL, tmpR)
  tmp_df <- as.data.frame(tmp_df)
  colnames(tmp_df) <- c("time", "Lval", "Rval")
  wav <- c(wav, list("data" = tmp_df))
  class(wav) <- 'rwav'
  wav
}

print.rwav <-  function(wav, debugmode=NULL){
  cat(paste0("fileSize : ", wav$fileSize, " Bytes", "\n"))
  if(!is.null(debugmode)){
    cat(paste0("RIFF : ",  wav$riff, "\n"))
  }
  cat(paste0("fileSize In Header : ", wav$fileSizeByRiff, "\n"))
  if(!is.null(debugmode)){
    cat(paste0("WAVE : ", wav$wave, "\n"))
    cat(paste0("fmt : ", wav$format, "\n"))
  }
  cat(paste0("formatByte : ", wav$formatByte, " Bytes \n"))
  cat(paste0("formatId : ", wav$formatId, "\n"))
  cat(paste0("Channel : ", wav$numChannel, "\n"))
  cat(paste0("sampleRate : ", wav$sampleRate, " Hz \n"))
  cat(paste0("verocity : ", wav$verocity, " Byte/sec \n"))
  cat(paste0("blockSample : ", wav$blockSample, " Byte/(Sample*Ch) \n"))
  cat(paste0("bps : ", wav$bitPerSample, " bit \n"))

  if(!is.null(debugmode)){
    cat(paste0("extendedHeaderSize : ", wav$extSize, "\n"))
    cat(paste0("extendedHeader : ", wav$extHead, "\n"))
    cat(paste0("data : ", wav$dataChar, "\n"))
  }
  cat(paste0("dataSize : ", wav$dataSize, " Bytes \n"))

  cat(str(wav$data), "\n")
}

check_riff <- function(con){
  f_riff <- readChar(con, 4L, useBytes=TRUE)
  if(f_riff != "RIFF"){
    stop("This is NOT valied wave file. 'RIFF' must be included")
  }
  list("riff" = f_riff)
}

check_file_size_riff <- function(con){
  f_size <- readBin(con, "integer", n=1L, size=4L)
  # print(f_size)
  list("fileSizeByRiff" = f_size)
}

check_wave <- function(con){
  ext_str <- readChar(con, 4L, useBytes=TRUE)
  if( ext_str != 'WAVE' ){
    stop("This is NOT valied wave file. 'WAVE' must be included")
  }
  # print(ext_str)
  list("wave" = ext_str)
}

check_fmt <- function(con){
  ext_str <- readChar(con, 4L, useBytes=TRUE)
  if(ext_str != "fmt "){
    stop("This is NOT valied wave file. 'fmt ' must be included")
  }
  # print(ext_str)
  list("format" = ext_str)
}

check_fmt_byte <-  function(con){
  f_size <- readBin(con, "integer", n=1L, size=4L)
  list("formatByte" = f_size)
}

get_fmt_id <-  function(con){
  f_size <- readBin(con, "integer", n=1L, size=2L)
  if(f_size != 1){
    stop(paste0("Format ID: ", f_size , ". This file type is NOT supported. Only PCM (format ID 1) is supported.)"))
  }
  list("formatId" = f_size)
}

get_num_channel <-  function(con){
  f_size <- readBin(con, "integer", n=1L, size=2L)
  if(f_size != 2){
    stop(paste0("Sorry, this version only supports 2 channel wave format. input is : ", f_size))
  }
  # print(f_size)
  list("numChannel" = f_size)
}

get_sample_rate <-  function(con){
  f_size <- readBin(con, "integer", n=1L, size=4L)
  # print(f_size)
  #if(f_size != 44100){
  #  stop(paste0("Sorry, this version only supports 44100 Hz sampling. input is : ", f_size))
  #}
  list("sampleRate" = f_size)
}

get_data_verocity <-  function(con){
  f_size <- readBin(con, "integer", n=1L, size=4L)
  # print(f_size)
  list("verocity" = f_size)
}

get_block_size <-  function(con){
  f_size <- readBin(con, "integer", n=1L, size=2L)
  # print(f_size)
  list("blockSample" = f_size)
}

get_bit_per_sample <-  function(con){
  f_size <- readBin(con, "integer", n=1L, size=2L)
  list("bitPerSample" = f_size)
  # print(f_size)
}

get_extended_size <-  function(con, an=1L, asize=2L){
  f_size <- readBin(con, "integer", n=an, size=asize)
  list("extSize" = f_size)
#  print(f_size)
}

get_header_extended <-  function(con, an=1L, asize=1L){
  f_size <- readBin(con, "integer", n=an, size=asize)
  list("extHead" = f_size)
#  f_size <- readBin(con, "integer", 2L, 1L)
#  print(f_size)
}

check_data <- function(con){
  ext_str <- readChar(con, 4L, useBytes=TRUE)
  list("dataChar" = ext_str)
#  print(ext_str)
}

get_data_size <- function(con, an=1L, asize=4L){
  f_size <- readBin(con, "integer", n=an, size=asize)
  list("dataSize" = f_size)
#  print(f_size)
}

get_data <- function(con, an=1L, asize=2L){
  #dd <- readBin(con, "integer" , n=an, size=asize, endian = "little", signed=FALSE)
  # dd <- readBin(con, "integer" , n=1L, size=2L )
  dd <- readBin(con, "integer" , n=an, size=asize )
  return(dd)
  # list("data"=dd)
}
