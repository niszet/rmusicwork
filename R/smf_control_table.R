
get_ctrl_chang_name <- function(val){

  tmp <- c(
  "Bank Select",
  "Modulation wheel",
  "Breath control",
  "Undefined",
  "Foot controller",
  "Portamento time",
  "Data Entry",
  "Channel Volume (formerly Main Volume)",
  "Balance",
  "Undefined",
  "Pan",
  "Expression Controller",
  "Effect control 1",
  "Effect control 2",
  "Undefined",
  "Undefined",
  "General Purpose Controller #1",
  "General Purpose Controller #2",
  "General Purpose Controller #3",
  "General Purpose Controller #4",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Bank Select",
  "Modulation wheel",
  "Breath control",
  "Undefined",
  "Foot controller",
  "Portamento time",
  "Data entry",
  "Channel Volume (formerly Main Volume)",
  "Balance",
  "Undefined",
  "Pan",
  "Expression Controller",
  "Effect control 1",
  "Effect control 2",
  "Undefined",
  "Undefined",
  "General Purpose Controller #1",
  "General Purpose Controller #2",
  "General Purpose Controller #3",
  "General Purpose Controller #4",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Damper pedal on/off (Sustain)",
  "Portamento on/off",
  "Sustenuto on/off",
  "Soft pedal on/off",
  "Legato Footswitch",
  "Hold 2",
  "Sound Controller 1 (Sound Variation)",
  "Sound Controller 2 (Timbre)",
  "Sound Controller 3 (Release Time)",
  "Sound Controller 4 (Attack Time)",
  "Sound Controller 5 (Brightness)",
  "Sound Controller 6",
  "Sound Controller 7",
  "Sound Controller 8",
  "Sound Controller 9",
  "Sound Controller 10",
  "General Purpose Controller #5",
  "General Purpose Controller #6",
  "General Purpose Controller #7",
  "General Purpose Controller #8",
  "Portamento Control",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Effects 1 Depth",
  "Effects 2 Depth",
  "Effects 3 Depth",
  "Effects 4 Depth",
  "Effects 5 Depth",
  "Data entry +1",
  "Data entry -1",
  "Non-Registered Parameter Number LSB",
  "Non-Registered Parameter Number MSB",
  "Registered Parameter Number LSB",
  "Registered Parameter Number MSB",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "Undefined",
  "All Sound Off 0",
  "Reset All Controllers 0",
  "Local control on/off 0=off 127=on",
  "All notes off 0",
  "Omni mode off (+ all notes off) 0",
  "Omni mode on (+ all notes off) 0",
  "Poly mode on/off (+ all notes off) ",
  "Poly mode on (incl mono=off +all notes off) 0")

  return(tmp[val])

}


"
# Binary Hex Dec Value Use
Bank Select	MSB
Modulation wheel	MSB
Breath control	MSB
Undefined	MSB
Foot controller	MSB
Portamento time	MSB
Data Entry	MSB
Channel Volume (formerly Main Volume)	MSB
Balance	MSB
Undefined	MSB
Pan	MSB
Expression Controller	MSB
Effect control 1	MSB
Effect control 2	MSB
Undefined	MSB
Undefined	MSB
General Purpose Controller #1	MSB
General Purpose Controller #2	MSB
General Purpose Controller #3	MSB
General Purpose Controller #4	MSB
Undefined	MSB
Undefined	MSB
Undefined	MSB
Undefined	MSB
Undefined	MSB
Undefined	MSB
Undefined	MSB
Undefined	MSB
Undefined	MSB
Undefined	MSB
Undefined	MSB
Undefined	MSB
Bank Select	LSB
Modulation wheel	LSB
Breath control	LSB
Undefined	LSB
Foot controller	LSB
Portamento time	LSB
Data entry	LSB
Channel Volume (formerly Main Volume)	LSB
Balance	LSB
Undefined	LSB
Pan	LSB
Expression Controller	LSB
Effect control 1	LSB
Effect control 2	LSB
Undefined	LSB
Undefined	LSB
General Purpose Controller #1	LSB
General Purpose Controller #2	LSB
General Purpose Controller #3	LSB
General Purpose Controller #4	LSB
Undefined	LSB
Undefined	LSB
Undefined	LSB
Undefined	LSB
Undefined	LSB
Undefined	LSB
Undefined	LSB
Undefined	LSB
Undefined	LSB
Undefined	LSB
Undefined	LSB
Undefined	LSB
Damper pedal on/off (Sustain)	<63=off >64=on
Portamento on/off	<63=off >64=on
Sustenuto on/off	<63=off >64=on
Soft pedal on/off	<63=off >64=on
Legato Footswitch	<63=off >64=on
Hold 2	<63=off >64=on
Sound Controller 1 (Sound Variation)	LSB
Sound Controller 2 (Timbre)	LSB
Sound Controller 3 (Release Time)	LSB
Sound Controller 4 (Attack Time)	LSB
Sound Controller 5 (Brightness)	LSB
Sound Controller 6	LSB
Sound Controller 7	LSB
Sound Controller 8	LSB
Sound Controller 9	LSB
Sound Controller 10	LSB
General Purpose Controller #5	LSB
General Purpose Controller #6	LSB
General Purpose Controller #7	LSB
General Purpose Controller #8	LSB
Portamento Control	Source Note
Undefined	LSB
Undefined	LSB
Undefined	LSB
Undefined	LSB
Undefined	LSB
Undefined	LSB
Effects 1 Depth	LSB
Effects 2 Depth	LSB
Effects 3 Depth	LSB
Effects 4 Depth	LSB
Effects 5 Depth	LSB
Data entry +1	N/A
Data entry -1	N/A
Non-Registered Parameter Number LSB	LSB
Non-Registered Parameter Number MSB	MSB
Registered Parameter Number LSB	LSB
Registered Parameter Number MSB	MSB
Undefined
Undefined
Undefined
Undefined
Undefined
Undefined
Undefined
Undefined
Undefined
Undefined
Undefined
Undefined
Undefined
Undefined
Undefined
Undefined
Undefined
Undefined
All Sound Off 0
Reset All Controllers 0
Local control on/off 0=off 127=on
All notes off 0
Omni mode off (+ all notes off) 0
Omni mode on (+ all notes off) 0
Poly mode on/off (+ all notes off)
Poly mode on (incl mono=off +all notes off) 0
"


"
# Binary Hex Dec Value Use
00000000 00 0 Bank Select 0-127 MSB
00000001 01 1 * Modulation wheel 0-127 MSB
00000010 02 2 Breath control 0-127 MSB
00000011 03 3 Undefined 0-127 MSB
00000100 04 4 Foot controller 0-127 MSB
00000101 05 5 Portamento time 0-127 MSB
00000110 06 6 Data Entry 0-127 MSB
00000111 07 7 * Channel Volume (formerly Main Volume) 0-127 MSB
00001000 08 8 Balance 0-127 MSB
00001001 09 9 Undefined 0-127 MSB
00001010 0A 10 * Pan 0-127 MSB
00001011 0B 11 * Expression Controller 0-127 MSB
00001100 0C 12 Effect control 1 0-127 MSB
00001101 0D 13 Effect control 2 0-127 MSB
00001110 0E 14 Undefined 0-127 MSB
00001111 0F 15 Undefined 0-127 MSB
00010000 10 16 General Purpose Controller #1 0-127 MSB
00010001 11 17 General Purpose Controller #2 0-127 MSB
00010010 12 18 General Purpose Controller #3 0-127 MSB
00010011 13 19 General Purpose Controller #4 0-127 MSB
00010100 14 20 Undefined 0-127 MSB
00010101 15 21 Undefined 0-127 MSB
00010110 16 22 Undefined 0-127 MSB
00010111 17 23 Undefined 0-127 MSB
00011000 18 24 Undefined 0-127 MSB
00011001 19 25 Undefined 0-127 MSB
00011010 1A 26 Undefined 0-127 MSB
00011011 1B 27 Undefined 0-127 MSB
00011100 1C 28 Undefined 0-127 MSB
00011101 1D 29 Undefined 0-127 MSB
00011110 1E 30 Undefined 0-127 MSB
00011111 1F 31 Undefined 0-127 MSB
00100000 20 32 Bank Select 0-127 LSB
00100001 21 33 Modulation wheel 0-127 LSB
00100010 22 34 Breath control 0-127 LSB
00100011 23 35 Undefined 0-127 LSB
00100100 24 36 Foot controller 0-127 LSB
00100101 25 37 Portamento time 0-127 LSB
00100110 26 38 Data entry 0-127 LSB
00100111 27 39 Channel Volume (formerly Main Volume) 0-127 LSB
00101000 28 40 Balance 0-127 LSB
00101001 29 41 Undefined 0-127 LSB
00101010 2A 42 Pan 0-127 LSB
00101011 2B 43 Expression Controller 0-127 LSB
00101100 2C 44 Effect control 1 0-127 LSB
00101101 2D 45 Effect control 2 0-127 LSB
00101110 2E 46 Undefined 0-127 LSB
00101111 2F 47 Undefined 0-127 LSB
00110000 30 48 General Purpose Controller #1 0-127 LSB
00110001 31 49 General Purpose Controller #2 0-127 LSB
00110010 32 50 General Purpose Controller #3 0-127 LSB
00110011 33 51 General Purpose Controller #4 0-127 LSB
00110100 34 52 Undefined 0-127 LSB
00110101 35 53 Undefined 0-127 LSB
00110110 36 54 Undefined 0-127 LSB
00110111 37 55 Undefined 0-127 LSB
00111000 38 56 Undefined 0-127 LSB
00111001 39 57 Undefined 0-127 LSB
00111010 3A 58 Undefined 0-127 LSB
00111011 3B 59 Undefined 0-127 LSB
00111100 3C 60 Undefined 0-127 LSB
00111101 3D 61 Undefined 0-127 LSB
00111110 3E 62 Undefined 0-127 LSB
00111111 3F 63 Undefined 0-127 LSB
01000000 40 64 * Damper pedal on/off (Sustain) <63=off >64=on
01000001 41 65 Portamento on/off <63=off >64=on
01000010 42 66 Sustenuto on/off <63=off >64=on
01000011 43 67 Soft pedal on/off <63=off >64=on
01000100 44 68 Legato Footswitch <63=off >64=on
01000101 45 69 Hold 2 <63=off >64=on
01000110 46 70 Sound Controller 1 (Sound Variation) 0-127 LSB
01000111 47 71 Sound Controller 2 (Timbre) 0-127 LSB
01001000 48 72 Sound Controller 3 (Release Time) 0-127 LSB
01001001 49 73 Sound Controller 4 (Attack Time) 0-127 LSB
01001010 4A 74 Sound Controller 5 (Brightness) 0-127 LSB
01001011 4B 75 Sound Controller 6 0-127 LSB
01001100 4C 76 Sound Controller 7 0-127 LSB
01001101 4D 77 Sound Controller 8 0-127 LSB
01001110 4E 78 Sound Controller 9 0-127 LSB
01001111 4F 79 Sound Controller 10 0-127 LSB
01010000 50 80 General Purpose Controller #5 0-127 LSB
01010001 51 81 General Purpose Controller #6 0-127 LSB
01010010 52 82 General Purpose Controller #7 0-127 LSB
01010011 53 83 General Purpose Controller #8 0-127 LSB
01010100 54 84 Portamento Control 0-127 Source Note
01010101 55 85 Undefined 0-127 LSB
01010110 56 86 Undefined 0-127 LSB
01010111 57 87 Undefined 0-127 LSB
01011000 58 88 Undefined 0-127 LSB
01011001 59 89 Undefined 0-127 LSB
01011010 5A 90 Undefined 0-127 LSB
01011011 5B 91 Effects 1 Depth 0-127 LSB
01011100 5C 92 Effects 2 Depth 0-127 LSB
01011101 5D 93 Effects 3 Depth 0-127 LSB
01011110 5E 94 Effects 4 Depth 0-127 LSB
01011111 5F 95 Effects 5 Depth 0-127 LSB
01100000 60 96 Data entry +1 N/A
01100001 61 97 Data entry -1 N/A
01100010 62 98 Non-Registered Parameter Number LSB 0-127 LSB
01100011 63 99 Non-Registered Parameter Number MSB 0-127 MSB
01100100 64 100 * Registered Parameter Number LSB 0-127 LSB
01100101 65 101 * Registered Parameter Number MSB 0-127 MSB
01100110 66 102 Undefined ?
01100111 67 103 Undefined ?
01101000 68 104 Undefined ?
01101001 69 105 Undefined ?
01101010 6A 106 Undefined ?
01101011 6B 107 Undefined ?
01101100 6C 108 Undefined ?
01101101 6D 109 Undefined ?
01101110 6E 110 Undefined ?
01101111 6F 111 Undefined ?
01110000 70 112 Undefined ?
01110001 71 113 Undefined ?
01110010 72 114 Undefined ?
01110011 73 115 Undefined ?
01110100 74 116 Undefined ?
01110101 75 117 Undefined ?
01110110 76 118 Undefined ?
01110111 77 119 Undefined ?
01111000 78 120 All Sound Off 0
01111001 79 121 * Reset All Controllers 0
01111010 7A 122 Local control on/off 0=off 127=on
01111011 7B 123 * All notes off 0
01111100 7C 124 Omni mode off (+ all notes off) 0
01111101 7D 125 Omni mode on (+ all notes off) 0
01111110 7E 126 Poly mode on/off (+ all notes off) **
01111111 7F 127 Poly mode on (incl mono=off +all notes off) 0
"

