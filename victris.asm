;https://www.cc65.org/doc/ca65.html#toc11
; https://github.com/bbbradsmith/NES-ca65-example/blob/master/example.s
; /usr/bin/flatpak run --branch=stable --arch=x86_64 --command=xvic net.sf.VICE --autostart test
; D64 file disk image maker is in the flatpak distro too.
; https://dl.dropboxusercontent.com/s/cl7391x5hqwk8zf/GoatPowerExample.asm
; rm test.o test; cl65 test.asm -t vic20 -C vic20-aliens-inv/vic20alinv.cfg -o test
;
; https://unfinishedbitness.info/2014/09/26/6502-string-to-integer-and-reverse/
;
; Compile: create label file
; rm test.o test; cl65 test.asm -Ln test.lbl -m map.txt -t vic20 -C vic20-aliens-inv/vic20alinv.cfg -o test
; Get "random" value form clock low bit at $00A2
; Modulus function from
; https://gist.github.com/hausdorff/5993556
; https://github.com/bbbradsmith/prng_6502


;.debuginfo on

SCREEN = $1E00          ; Start of screen memory
COLOR_OFFSET = $7800    ; Start of color memory, $7800 bytes above character memory.
BOARD_LEFT = $76        ; Left border character  
BOARD_RIGHT = $75       ; Right border character
BLOCK_CH = $A0          ; Block charater: reverse space
BUFF_LEN = $10          ; Length of buffer of rendered piece in bytes: 16
LEFT_MARGIN = $04       ; Screen splace left of board
BOARD_WIDTH = $0A       ; Width of board. 10 characters.
SCREEN_WIDTH = $16      ; Default screen width, in bytes: 22
SCREEN_HEIGHT = $17     ; Default screen height, in bytes: 23
SPACE_CH = $20          ; Space chracter
WHITE = $01             ; Color code for white
LTBLUE_BLK = $E8        ; Screen: black border and light blue background
BORDER_REG = $900F      ; Screen background and border register
INITIAL_POS = $1E4A     ; Initial location of lower right of piece
CLOCK_LOW = $A2         ; Low byte of clock. Incremented every 1/60 sec.
LAST_ROW = $1FE4        ; Start of last row of screen memory

.segment "RODATA"

; Tetris pieces encoded as bytes
PIECES: 
    .res 1, %11100100   ; T $E4
    .res 1, %00001111   ; I $0F
    .res 1, %10001110   ; J $8E
    .res 1, %11001100   ; O $CC
    .res 1, %00010111   ; L $1E
    .res 1, %01101100   ; S $6C
    .res 1, %11000110   ; Z $C6

; The colors for the pieces
COLORS:
    .res 1, $04         ; T Magenta
    .res 1, $03         ; I Cyan
    .res 1, $06         ; J Blue
    .res 1, $07         ; O Yellow
    .res 1, $00         ; L Black
    .res 1, $05         ; S Green
    .res 1, $02         ; Z Red

.segment "ZEROPAGE"


CurPieceIdx:    .res 1      ; Currently active piece
RotState:       .res 1      ; Orientation of the active piece
PieceBuff:      .res 16     ; 16 byte buffer for unpacking encoded pieces

DrawPtrLo:      .res 1      ; Low byte of screen pointer
DrawPtrHi:      .res 1      ; High byte of screen pointer
ColorPtrLo:     .res 1      ; Low byte of color mem pointer
ColorPtrHi:     .res 1      ; High byte of color 
DrawIndex:      .res 1      ; Inexing variable

PieceLoc:       .res 2      ; 16 bit pointer to lower right of current piece on screen.
CurChar:        .res 1      ; Current output character. Read by DrawBuff
CurColor:       .res 1      ; Color of current character. Read by DrawBuff
BuffPtr:        .res 1      ; Pointer into character buffer. Used by drawBuff.
IsCollision:    .res 1      ; Flag set when piece can no longer move. Set by Check collision.
seed:           .res 1      ; Generated random number. 

; For 16 bit addition and subtraction
; Used by 'add' and 'sub' functions
num1lo: .res 1
num1hi: .res 1
num2lo: .res 1
num2hi: .res 1	
reslo: .res 1
reshi: .res 1

.segment "STARTUP"
.segment "LOWCODE"
.segment "INIT"
.segment "GRCHARS"
.segment "CODE"

main:      

; Init vars to sane values
; and clear rendered piece
; buffer.
            lda #$00
            sta CurPieceIdx
            sta RotState
            sta IsCollision

            lda #LTBLUE_BLK         ; Setup bg and border
            sta BORDER_REG
            
            jsr ClearScreen
            jsr SetupBoard


            lda #$11                ; TODO: init seed from timer
            sta seed

@game:
            jsr ClearBuff           ; Clear the buffer containing the unpacked piece
            jsr GetRandPiece        ; Pick a random piece
            jsr unpack000           ; Unpack the current piece to the buffer

            lda #<INITIAL_POS       ; Set up to introduce new piece
            sta PieceLoc
            lda #>INITIAL_POS
            sta PieceLoc + 1



@move_piece:
            lda #BLOCK_CH            ; Character to draw the piece
            sta CurChar
            ldx CurPieceIdx         ; Get the color of the current piece
            lda COLORS, X            
            sta CurColor

      
            jsr drawBuff            ; Draw the piece that is in the buffer

            jsr Delay

            jsr CheckCollision

            lda #$FF
            and IsCollision
            bne @game

            lda #SPACE_CH           ; Setup to erase the rendered piece
            sta CurChar
            lda #WHITE
            sta CurColor

            jsr drawBuff

            lda PieceLoc            ; Advance
            sta num1lo
            lda PieceLoc + 1
            sta num1hi

            lda #SCREEN_WIDTH
            sta num2lo
            lda #$00
            sta num2hi

            jsr add

            lda reslo
            sta PieceLoc
            lda reshi
            sta PieceLoc + 1
          
            jmp @move_piece

foo:        clc
            bcc foo

; Y is the govenor of the delay
Delay:      ldx #$FF
            ldy #$A0 
@loop:      dex
            bne @loop
            dey
            bne @loop
            rts


; Draw two lines from top to bottom of screen
; ten columns apart
SetupBoard:
            lda #<SCREEN
            sta num1lo
            sta reslo
            lda #>SCREEN
            sta num1hi
            sta reshi
        
            lda #SCREEN_WIDTH   ; Set up second number to
            sta num2lo          ; advance to next row (23 chars)
            lda #$00
            sta num2hi            

            ldx #SCREEN_HEIGHT
@loop_x:  
            lda #BOARD_LEFT     ; Draw left char
            ldy #LEFT_MARGIN
            sta (reslo), Y
            lda #BOARD_RIGHT    ; Draw right char
            ldy #LEFT_MARGIN + BOARD_WIDTH
            sta (reslo), Y

            dex
            beq @done

            lda reslo           ; Copy prev. sum to input for next round
            sta num1lo
            lda reshi
            sta num1hi

            jsr add             ; Update pointer to next row.
            jmp @loop_x
@done:
            rts


; https://codebase64.org/doku.php?id=base:16bit_addition_and_subtraction
; 16 bit add
add:  
            clc				; clear carry
	        lda num1lo
	        adc num2lo
	        sta reslo			; store sum of LSBs
	        lda num1hi
	        adc num2hi			; add the MSBs using carry from
	        sta reshi			; the previous calculation
	        rts

;subtracts number 2 from number 1 and writes result out
; 16 bit subtraction
sub:
        	sec				; set carry for borrow purpose
        	lda num1lo
        	sbc num2lo			; perform subtraction on the LSBs
        	sta reslo
        	lda num1hi			; do the same for the MSBs, with carry
        	sbc num2hi			; set according to the previous result
        	sta reshi
        	rts

; Clear the screen
ClearScreen: 
            lda #$93
            jsr $FFD2
            rts


; Pick a random piece. 
; https://codebase64.org/doku.php?id=base:small_fast_8-bit_prng
GetRandPiece:

            lda seed
            beq @doEor
            asl
            beq @noEor ;if the input was $80, skip the EOR
            bcc @noEor
@doEor:     eor #$1D
@noEor:     sta seed

            and #%00000111      ; CurPieceIdx must be between 0 and 6, inclusive.
            cmp #$07 
            beq GetRandPiece

            sta CurPieceIdx
            rts


; Clear the piece buffer
ClearBuff:  
            ldx #BUFF_LEN
            lda #$00
@loop_x:    dex 
            sta PieceBuff, X          
            bne @loop_x
            rts


; Unpack the encoded piece with not roation
; to the piece buffer.
; Start with lsb and write to end of buffer
; Assumes buffer has ben cleared 
; Looks up current piece by CurPieceIdx
; and writes FF for each set bit.
unpack000:          
            ldx CurPieceIdx     ; Lookup current piece
            lda PIECES, X
            ldx #$08            ; Loop 8 times, one for each bit
            ldy #BUFF_LEN       ; End of buffer
@loop:      dey
            lsr                 ; Pop LSB. sets carry on 1
            bcc @skip
            pha                 ; Save A to stack then save FF to buff
            lda #$FF
            sta PieceBuff, Y            
            pla           
@skip:      dex
            bne @loop
            rts

; Draw the piece buffer, starting from the
; bottom right. Draw the piece if the given 
; byte is set in the buffer. Set the color
; of the piece based on the current piece index.
; Color location calculated by offset from 
; character location.
drawBuff:
            lda #PieceBuff + 16 ; End of unpacked piece buffer
            sta BuffPtr

            lda PieceLoc            ; Copy current location to working poitner
            sta DrawPtrLo
            lda PieceLoc + 1
            sta DrawPtrHi

            ldx #$04        ; X is  counter: on for each row
            ldy #$03        ; Y is an index into buffer.    
            lda #BUFF_LEN   ; Run one iteration for each byte in piece buffer.
            sta DrawIndex

@loop_draw: 

            txa                 ; Need X for indexing buffer
            pha                 ; so save a copy of it on stack
            ldx DrawIndex
            dex
            lda PieceBuff, X   
            and #$FF                ; If the buffer character is
            beq @skip_draw          ; set, output a block

            lda CurChar             ; Get character to draw
            sta (DrawPtrLo), Y

            lda DrawPtrLo           ; Calculate offset into screen color area.
            sta num2lo              ; Colors are set $7800 bytes higher than
            lda DrawPtrHi           ; the character memory.
            sta num2hi
            lda #<COLOR_OFFSET
            sta num1lo
            lda #>COLOR_OFFSET
            sta num1hi
            jsr add 
      
            lda CurColor            ; Get the color of the current piece
            sta (reslo), Y

@skip_draw: pla                     ; Restore X from stack
            tax
            dec BuffPtr             ; Next buffer character
            dey            
            dex                     ; After drawing 4 chars
            bne @row_end            ; decrement pointer
            
            ldx #$04
            ldy #$03
            lda DrawPtrLo           ; Set up number1 - number2
            sta num1lo
            lda DrawPtrHi
            sta num1hi

            lda #SCREEN_WIDTH       ; Set up for row subtraction
            sta num2lo
            lda #$00
            sta num2hi

            jsr sub

            lda reslo
            sta DrawPtrLo
            lda reshi
            sta DrawPtrHi    

@row_end:   dec DrawIndex
            bne @loop_draw
            rts

; Compare the buffer with
; non space characters
; Sets or clears IsCollision
CheckCollision:
       
            lda PieceLoc + 1        ; Check if on bottom row of board
            cmp #>LAST_ROW          ; 16 bit comparison with beginning of
            bne @is_lower           ; last row of screen. 
            lda PieceLoc
            cmp #<LAST_ROW
            bcc @is_lower

            lda #$01
            sta IsCollision
            rts

@is_lower:  lda #$00
            sta IsCollision
            rts

