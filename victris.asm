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
.macpack cbm              ; Enable scrcode macro (ASCII to PETSKII)

SCREEN = $1E00          ; Start of screen memory
BORDER_REG = $900F      ; Screen background and border register
BLOCK_CH = $A0          ; Block charater: reverse space
BOARD_LEFT = $76        ; Left border character  
BOARD_RIGHT = $75       ; Right border character
BOARD_WIDTH = $0A       ; Width of board. 10 characters.
BOARD_BOTTOM = $1FE7    ; Bottom left of playing area
BUFF_COLS = $04         ; Four columns in piece buffer
BUFF_LEN = $10          ; Length of buffer of rendered piece in bytes: 16
BUFF_ROWS = $04         ; Four rows in piece buffer
CHAR_DROP =  $20        ; Drop piece: space
CHAR_LEFT = $41         ; Move left 'A'
CHAR_RIGHT = $44        ; Move right 'D'
CHAR_ROTATE = $53       ; Rotate 'S'
CLOCK_LOW = $A2         ; Low byte of clock. Incremented every 1/60 sec.
COLOR_OFFSET = $7800    ; Start of color memory, $7800 bytes above character memory.
GETIN = $FFE4           ; Kernal function to get keyboard input
INITIAL_POS = $1E4A     ; Initial location of lower right of piece
LAST_ROW = $1FE4        ; Start of last row of screen memory
LEFT_MARGIN = $02       ; Screen splace left of board
LTBLUE_BLK = $E8        ; Screen: black border and light blue background
NOT_SEEN = $FF          ; Flag value used in finding edge of piece
SCORE_SCREEN = $1E25    ; Location of score text on screen
SCREEN_HEIGHT = $17     ; Default screen height, in bytes: 23
SCREEN_WIDTH = $16      ; Default screen width, in bytes: 22
SPACE_CH = $20          ; Space chracter
WHITE = $01             ; Color code for white

MOVE_ROTATE = $01       ; Flag to indicate piece should rotate
MOVE_DROP   = $02       ; Flag to indicate piece should drop
MOVE_LEFT   = $04       ; Flag to indicate piece should move left
MOVE_RIGHT  = $08       ; Flag to indicate piece should move right

.segment "RODATA"

; Tetris pieces encoded as bytes
PIECES: 
    .res 1, %11100100   ; T $E4
    .res 1, %00001111   ; I $0F
    .res 1, %10001110   ; J $8E
    .res 1, %11001100   ; O $CC
    .res 1, %00101110   ; L $1E
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

; Text strings are is terminated with '@', PETSCII 0.
TXT_SCORE:      scrcode "score:@"
TXT_TITLE:      scrcode "tetris@"
TXT_CONTROLS_1: scrcode "a: move left@"
TXT_CONTROLS_2: scrcode "s: rotate@"
TXT_CONTROLS_3: scrcode "d: move right@"
TXT_CONTROLS_4: scrcode "space: drop@"
TXT_START:      scrcode "press any key@"

.segment "ZEROPAGE"


CurPieceIdx:    .res 1      ; Currently active piece
RotState:       .res 1      ; Orientation of the active piece
PieceBuff:      .res 16     ; 16 byte buffer for unpacking encoded pieces
DrawPtrLo:      .res 1      ; Low byte of screen pointer
DrawPtrHi:      .res 1      ; High byte of screen pointer
ColorPtr:       .res 2      ; Pointer into colour memory
DrawIndex:      .res 1      ; Inexing variable
PieceLoc:       .res 2      ; 16 bit pointer to lower right of current piece on screen.
CurChar:        .res 1      ; Current output character. Read by DrawBuff
CurColor:       .res 1      ; Color of current character. Read by DrawBuff
BuffPtr:        .res 1      ; Pointer into character buffer. Used by drawBuff.
IsCollision:    .res 1      ; Flag set when piece can no longer move. Set by Check collision.
seed:           .res 1      ; Generated random number. 
Score:          .res 2      ; Current score
TextPtr:        .res 2      ; Pointer to text. Read by PrintString
ScoreScnPtr:    .res 2      ; Pointer to score on screen
BuffColBlocks:  .res 1      ; Set when piece buffer column has a block.
MoveFlag:       .res 1      ; Movement flag. See MOVE_* flags above for values.
RotCol:         .res 1      ; Housekeeping variable in buffer rotaiton routine


CollisionPtr:   .res 2

XIDX: .res 1    ; TODO: consolidate XIDX and YIDX into one *or* use registers.
YIDX: .res 1



; BCD stuff
Res:            .res 3      ; 24 bit BVD encoded score
Val :           .res 2      ; Work vars for BCD encoding

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
            lda #$00
            sta CurPieceIdx
            sta RotState
            sta IsCollision
            sta Score
            sta Score + 1

            lda #LTBLUE_BLK         ; Setup bg and border
            sta BORDER_REG
            
            jsr ClearScreen
            jsr SetupBoard

            lda #$11                ; TODO: init seed from timer + 1
            sta seed

            lda #<TXT_SCORE         ; Print 'score', upper right
            sta TextPtr
            lda #>TXT_SCORE
            sta TextPtr + 1
            lda #<SCORE_SCREEN
            sta DrawPtrLo
            lda #>SCORE_SCREEN
            sta DrawPtrHi            
            jsr PrintString

            lda #<SCORE_SCREEN  ; Set up pointer to score value on screen.
            sta ScoreScnPtr
            lda #>SCORE_SCREEN
            sta ScoreScnPtr + 1

@game:
            jsr checkLineComplete

            lda #$00
            sta IsCollision         ; Clear collision flag
            sta MoveFlag            ; Clear movement and rotation flags
            jsr ClearBuff           ; Clear the buffer containing the unpacked piece


            jsr GetRandPiece        ; Pick a random piece

            jsr unpack000           ; Unpack the current piece to the buffer with no rotation

            lda #<INITIAL_POS       ; Set up to introduce new piece
            sta PieceLoc
            lda #>INITIAL_POS
            sta PieceLoc + 1

            jsr drawBuff
  
@move_piece:

            jsr GetInput            ; Get user input: left, right, rotate, or drop.
                                    ; Set MoveFlag if any input set.

            lda #MOVE_ROTATE         ; Was a rotation requested?
            and MoveFlag
            beq @no_rotate
            lda #$00
            sta MoveFlag
            jsr rotateCw

            

@no_rotate:

            lda #BLOCK_CH           ; Character to draw the piece
            sta CurChar
            ldx CurPieceIdx         ; Get the color of the current piece
            lda COLORS, X            
            sta CurColor
      ; handle rotation here?
            jsr drawBuff            ; Draw the piece that is in the buffer
;jsr checkCollideRight
;j;mp *
            jsr Delay

            jsr CheckCollision      ; Check if piece can continue downward

            lda #$FF
            and IsCollision
            bne @game

            lda #SPACE_CH           ; Setup to erase the rendered piece
            sta CurChar
            lda #WHITE
            sta CurColor

            jsr drawBuff

@left:      lda #MOVE_LEFT          ; Move piece left
            and MoveFlag
            beq @right
            eor MoveFlag            ; Clear flag

            ; Check can move piece left
            ; jump to 'default' if not
            jsr checkCollideLeft    ; Sets IsCollision
            lda IsCollision
            bne @default 

            sta MoveFlag
            lda #(SCREEN_WIDTH - 1) ; Move to left
            jmp @add

@right:     lda #MOVE_RIGHT         ; Move piece right
            and MoveFlag
            beq @default
            eor MoveFlag            ; Clear flag

            ; Check can move piece right
            ; jump to default if not
            jsr checkCollideRight
            lda IsCollision
            bne @default

            sta MoveFlag            
            lda #(SCREEN_WIDTH + 1) ; Move to the right
            jmp @add

@default:   lda #$00                ; May have been set in left/right collide check, above.
            sta IsCollision

            lda #SCREEN_WIDTH       ; Default: advance to next line with no left/right movement

@add:       sta num2lo              ; A set to some value depending on movement, set above
            lda #$00
            sta num2hi
            lda PieceLoc            ; Advance to next line. Set up
            sta num1lo              ; addition acording to movement flag
            lda PieceLoc + 1
            sta num1hi            
            jsr add

            lda reslo
            sta PieceLoc
            lda reshi
            sta PieceLoc + 1
          
            jsr IncScore

            jmp @move_piece


; Y is the govenor of the delay
; If the drop flag is set on ActioFlag, do not loop.
Delay:

            lda #MOVE_DROP
            and MoveFlag
            beq @no_drop
            rts
@no_drop:
            ldx #$FF            
            ldy #$FF ; TODO: decrement by current level
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
            ldy #(LEFT_MARGIN + BOARD_WIDTH + 1)
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

Bin2BCD:
            lda Score
            sta Val
            lda Score + 1
            sta Val + 1

            lda #0          ; Clear the result area
            sta Res+0
            sta Res+1
            sta Res+2
            ldx #16         ; Setup the bit counter
            sed             ; Enter decimal mode
@loop_x:    asl Val+0       ; Shift a bit out of the binary
            rol Val+1       ; ... value
            lda Res+0       ; And add it into the result, doubling
            adc Res+0       ; ... it at the same time
            sta Res+0
            lda Res+1
            adc Res+1
            sta Res+1
            lda Res+2
            adc Res+2
            sta Res+2
            dex             ; More bits to process?
            bne @loop_x
            cld             ; Leave decimal mode

            ldy #$16        ; Write to row below score label
            ldx #$02        ; Start at ScoreScnPtr and use Y             
@loop_res:  lda Res, X      ; Note: PrintBCD modifies Y            
            jsr PrintBCD
            dex
            bpl @loop_res
            rts

PrintBCD:   pha             ; Save the BCD value
            lsr             ; Shift the four most significant bits
            lsr             ; ... into the four least significant
            lsr
            lsr
            clc
            adc #$30 ;  Make a screen code char
            sta (ScoreScnPtr), Y
            iny
            pla             ; Recover the BCD value
            and #$0F        ; Mask out all but the bottom 4 bits
            clc
            adc #$30   ; Make an screen code char
            sta (ScoreScnPtr), Y
            iny
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

; Print string to screen. Reads from TextPtr.
; Assumes string is terminated with 0, '@' in PETSCII.
; Location to print to is given by DrawPtrLo
PrintString:
            ldy #$00
@loop_y:    lda (TextPtr), Y
            beq @end
            sta (DrawPtrLo), Y
            iny
            jmp @loop_y
@end:       rts

; Incrementthe score (16 bits) and
; then encode as BCD and print to screen
IncScore:
            lda Score
            clc
            adc #$01
            sta Score
            bcc @skip
            inc Score + 1            
@skip:      jsr Bin2BCD
            rts

; Pick a random piece.
; https://codebase64.org/doku.php?id=base:small_fast_8-bit_prng
GetRandPiece:
; TODO: just get low bit from $
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
;lda #$01
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




; Unpack the encoded piece with no roation
; to the piece buffer.
; Start with lsb and write to end of buffer
; Assumes buffer has ben cleared 
; Looks up current piece by CurPieceIdx
; and writes FF for each set bit.
; 0 1 2 3
; 4 5 6 7
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

; Rotates piece buffer clockwise, 90 degrees.
; Steps throught the buffer in the desired order
; and writes buffer content to the stack.
; Then reads from the stack back to the buffer.
rotateCw:           
            lda #$0C ; Last row, first column
            sta RotCol

@loop_col:  lda RotCol  ; rotCol contains start positoin
            tax

@start_col: ldy #BUFF_ROWS ; 4 rows

@loop_row:  lda PieceBuff, X
            
            pha

            txa
            sec		
            sbc #$04
            tax

            dey
            bne @loop_row

            inc RotCol
            lda #$0F
            cmp RotCol
            bpl @loop_col

            ldx #BUFF_LEN - 1       ; Set up for writeing from stack to buffer 
@rewrite:   pla    
            
            sta PieceBuff, X
            dex
            bpl @rewrite

; "lower" the piece into the bottom last corner

@shift_rows_down:
            lda #$00
            sta XIDX            ; Set when bottom row has a block

            ldy #BUFF_COLS
            ldx #BUFF_LEN - 1

@loop_y:    lda PieceBuff, X
            beq @clear
            sta XIDX              ; TODO: if A is set, break

@clear:     dex
            dey            
            bne @loop_y
         
            lda XIDX
            bne @done

            ldy #$00
            ldx #((BUFF_COLS * 3) - 1) ; End of third row

@shift_row:                         ; Shift rows down
            txa
            clc
            adc #BUFF_COLS
            tay
            lda PieceBuff, X
            sta PieceBuff, Y

            dex
            bpl @shift_row

            lda #$00
            ldx #BUFF_COLS - 1

@fill_row:  lda #$00                ; FIll top row with zeroes
            sta PieceBuff, X
            dex
            bpl @fill_row


            lda PieceBuff + $0D     ; Is anoter round of lowering required? Check byte
            beq @shift_rows_down    ; at lower left of buffer

@done:      rts

; If XIDX is not set, shift everything down by 4
          
;@done:  nop

;@sf: nop
;jmp @sf
;    rts

; Draw the piece buffer, starting from the
; bottom right. Draw the piece if the given 
; byte is set in the buffer. Set the color
; of the piece based on the current piece index.
; Color location calculated by offset from 
; character location.
drawBuff:
            lda #PieceBuff + 16 ; End of unpacked piece buffer
            sta BuffPtr ; TODO: is BuffPtr variable required?

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
            beq @skip_draw          ; If the buff character is set, output a block

            lda CurChar             ; Get character to draw
            sta (DrawPtrLo), Y

            lda DrawPtrLo           ; Calculate offset into screen color area.
            sta num2lo              ; Colors are set $7800 bytes higher than
            lda DrawPtrHi           ; the character memory.
            sta num2hi
            lda #<COLOR_OFFSET  ; TODO: maintain color pointer here and index with Y indead of this addition.
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


; Get user input. Piece can be moved one
; square left or right, rotated, or dropped.
; Gets input with Kernel function GETIN $FFE4.
; Sets apropriate flag on MoveFlag.
GetInput:
            jsr GETIN               ; A is zero if no input read
            bne @rotate
            rts                      

@rotate:    cmp #CHAR_ROTATE        ; Set rotate flag.
            bne @drop
            lda #MOVE_ROTATE
            jmp @done

@drop:      cmp #CHAR_DROP          ; Set DropFlag
            bne @left
            lda #MOVE_DROP
            jmp @done

@left:      cmp #CHAR_LEFT          ; Move to left
            bne @right
            lda #MOVE_LEFT
            jmp @done

@right:     cmp #CHAR_RIGHT         ; Move to right
            bne @default           
            lda #MOVE_RIGHT
            jmp @done
@default:                           ; Default: do nothing
            rts

@done:      sta MoveFlag
            rts


CheckCollision:

            lda PieceLoc + 1        ; Check if on bottom row of board
            cmp #>LAST_ROW          ; 16 bit comparison with beginning of
            bne @chk_edge_setup           ; last row of screen. 
            lda PieceLoc
            cmp #<LAST_ROW
            bcc @chk_edge_setup

            lda #$FF                ; Set collision flag
            sta IsCollision
            rts

@chk_edge_setup:

            lda #$00
            sta IsCollision
            lda #$00
            sta XIDX 
            sta YIDX 

@chk_edge:  lda #$00                ; Clear pointer to screen item
            sta CollisionPtr
            sta CollisionPtr + 1
            sta BuffColBlocks       ; Clear "column has blocks" flag

            lda PieceLoc            ; Get a pointer to the TOP of the
            sta num1lo              ; piece on screen. This pointer
            lda PieceLoc + 1        ; will be maintained as the loop
            sta num1hi              ; steps though the piece buffer.
            lda #(SCREEN_WIDTH * 2) ; Shifted down to next row.
            sta num2lo
            lda #00
            sta num2hi
            jsr sub
            lda reslo               ; Save result to draw pointer
            sta DrawPtrLo
            lda reshi
            sta DrawPtrHi

            lda YIDX                ; Adjust start to column 0 - 3
            clc
            adc DrawPtrLo
            sta DrawPtrLo
            bcc @no_carry
            inc DrawPtrHi

@no_carry:

            lda #SCREEN_WIDTH       ; Setup for more maintaining of DrawPtrLo
            sta num2lo
            lda #$00
            sta num2hi

            lda XIDX                ; Offset into piece buffer
            tax

            ldy #BUFF_ROWS          ; Once for each row of buffer           

@loop_x: 
            lda #$FF                ; Check if byte in buffer is set
            and PieceBuff, X
            beq @buff_clear 
            sta BuffColBlocks       ; Set "Column has block" flag

            lda DrawPtrLo           ; Save screen position of the buffer block
            sta CollisionPtr        
            lda DrawPtrHi
            sta CollisionPtr + 1

@buff_clear:
            lda DrawPtrLo           ; Update screen pointer to next row
            sta num1lo
            lda DrawPtrHi
            sta num1hi
            jsr add
            lda reslo
            sta DrawPtrLo
            lda reshi
            sta DrawPtrHi

            txa                     ; Increment X to the next row.
            clc
            adc #BUFF_COLS
            tax
            dey
            bne @loop_x     

            lda BuffColBlocks       ; Does the column contain a block?
            beq @skip

            lda #SPACE_CH           ; If the position on the screen is
            cmp (CollisionPtr), Y   ; not a space, a collision will occur
            bne @collision
           
            lda #(BUFF_COLS - 1)
            cmp YIDX
            beq @skip
            inc YIDX           
            inc XIDX

            jmp @chk_edge

@skip:      lda #$00
            sta IsCollision
            rts
@collision:
            lda #$FF
            sta IsCollision
            rts


checkCollideLeft:

            lda PieceLoc            ; Copy current location to working poitner,
            sta num1lo              ; but one position to the left (lower)
            lda PieceLoc + 1
            sta num1hi

            lda #$01
            sta num2lo
            lda #$00
            sta num2hi
            jsr sub

            lda reslo
            sta CollisionPtr
            lda reshi
            sta CollisionPtr + 1

            lda #SCREEN_WIDTH       ; Setup for screen row by screen row iteration
            sta num2lo
            lda #$00
            sta num2hi

            ldy #BUFF_ROWS          ; Y is row counter
            ldx #(BUFF_COLS * 3)    ; Start of last row

@loop_y:
            lda PieceBuff, X

            beq @skip_compare       ; Block defined at location in buff?

            tya                     ; Save Y
            pha
            ldy #$00
            lda (CollisionPtr), Y
            cmp #SPACE_CH
            bne @collision

            pla                     ; Restore Y
            tay

 @skip_compare:
            dey
            beq @done

            txa                     ; Set X to start of previous row in buffer
            sec
            sbc #BUFF_COLS
            tax

            lda CollisionPtr        ; Set screen pointer to previous line
            sta num1lo
            lda CollisionPtr + 1
            sta num1hi            
            jsr sub
            lda reslo
            sta CollisionPtr
            lda reshi
            sta CollisionPtr + 1

            jmp @loop_y

@done:      lda #$00
            sta IsCollision
            rts
@collision:
            pla                     ; recover Y, but discard it
            lda #$FF
            sta IsCollision
            rts


; Find right edge of piece
checkCollideRight:

            ldy #BUFF_COLS
      
            lda #$00
            sta BuffColBlocks       ; Set as soon as a column encountered with blocks
            sta IsCollision

@setup_col: tya
            sta YIDX

            lda #((BUFF_COLS * 3) - 1)      ; set X to some position on last row, bsed on Y counter
            clc
            adc YIDX
            tax

            lda PieceLoc            ; Copy current location to working poitner,
            sta num1lo              ; but one position beyond the right edge (higher)
            lda PieceLoc + 1
            sta num1hi

            tya
            sta num2lo
            lda #$00
            sta num2hi
            jsr add
            lda reslo
            sta CollisionPtr
            lda reshi
            sta CollisionPtr + 1

            lda #BUFF_ROWS
            sta XIDX

            lda #SCREEN_WIDTH       ; Setup for screen row by screen row iteration
            sta num2lo
            lda #$00
            sta num2hi

@loop_rows: lda PieceBuff, X            
            beq @no_block
            lda $FF
            sta BuffColBlocks       ; Set flag: column has blocks in buffer

            tya                     ; Check screen for a block
            pha
            ldy #$00
            lda (CollisionPtr), Y
            cmp #SPACE_CH           
            beq @restore_y
            pla
            jmp @collision

@restore_y: pla                     ; Restore Y
            tay

@no_block:  txa
            sec
            sbc #BUFF_COLS
            tax

            lda CollisionPtr
            sta num1lo
            lda CollisionPtr + 1
            sta num1hi
            jsr sub
            lda reslo
            sta CollisionPtr
            lda reshi
            sta CollisionPtr + 1

            dec XIDX
            bne @loop_rows

            lda BuffColBlocks
            bne @done

            dey
            bne @setup_col
@done:
            rts

@collision: lda #$FF
            sta IsCollision         ; Set collision flag
            rts
 
 
checkLineComplete:

            lda #<BOARD_BOTTOM ;bottom left of board
            sta DrawPtrLo
            lda #>BOARD_BOTTOM 
            sta DrawPtrLo + 1

            lda #SCREEN_WIDTH       ; Setup for screen row by screen row iteration
            sta num2lo
            lda #$00
            sta num2hi

            ldx #SCREEN_HEIGHT - 2
@loop_row: 
            lda #BLOCK_CH
            ldy #BOARD_WIDTH - 1
@loop_col:             
            and (DrawPtrLo), Y      ; If a space found, go on to next row
            cmp #BLOCK_CH
            bne @next_row

            dey
            bpl @loop_col

            cmp #BLOCK_CH           ; No spaces found: row is complete.
            bne @next_row

            jsr shiftDown           ; Shift above rows down.

            jmp @loop_row           ; Recheck current row, a complete row may
                                    ; have moved down into current line

 @next_row: 
          
            lda DrawPtrLo
            sta num1lo
            lda DrawPtrLo + 1
            sta num1hi
            jsr sub
            lda reslo
            sta DrawPtrLo
            lda reshi
            sta DrawPtrHi

            dex
            bne @loop_row

            rts            

; Shift the game board contents down one row.
; Reads the position ot start shifting from
; DrawPtr. Restores the value fo DrawPtr before
; return.
shiftDown:
            pha                     ; Save registers and DrawPointer
            txa             
            pha
            tya
            pha
            lda DrawPtrLo           
            pha
            lda DrawPtrLo + 1
            pha

            lda DrawPtrLo           ; Setup pointer into colour memory.
            sta num1lo
            lda DrawPtrLo + 1
            sta num2hi

 ;           lda #<COLOR_OFFSET
 ;           sta num2lo
 ;           lda #>COLOR_OFFSET
 ;           sta num2hi
  ;          jsr add 
  ;          lda reslo
  ;          sta ColorPtr
  ;          lda reshi
  ;          sta ColorPtr + 1


            lda #SCREEN_WIDTH       ; Setup for screen row by screen row iteration
            sta num2lo
            lda #$00
            sta num2hi

@loop_row: ldy #BOARD_WIDTH - 1

            lda DrawPtrLo           ; reslo will contain pointer to previous line
            sta num1lo
            lda DrawPtrLo + 1
            sta num1hi
            jsr sub

@loop_col:  lda (reslo), Y          ; Copy character from row above to row below
            sta (DrawPtrLo), Y

            dey
            bpl @loop_col

 @next_row: lda reslo               ; Make the row above the new current row
            sta DrawPtrLo
            lda reshi            
            sta DrawPtrHi
            
            dex
            bne @loop_row

            pla                     ; Restore registers and draw pointer
            sta DrawPtrLo + 1
            pla 
            sta DrawPtrLo
            pla      
            tay
            pla
            tax
            pla

            rts