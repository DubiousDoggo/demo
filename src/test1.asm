; ================================ ;
; == COMMODORE 64 UNTITLED DEMO == ;
; ==    JACK T FOX   2024-25    == ;
; ================================ ;


; ----------------------------
; ==== ZEROPAGE VARIABLES ====
; ----------------------------

FRAME_COUNT     := $02

WIDE_CHAR_BUF   := $03  ; buffer for wide characters in the scroller
WIGGLE_INDEX    := $04

IRQ_LINE        := $05
SCROLL_Y        := $06  ; how many lines to stall in scrolled section
SCROLL_STALL    := $07  ; counter for scroll stall
SCROLLERX       := $08
SCROLL_INDEX    := $09  ; index of scrolled text

STALL_CALLBACK    := $10  ; callback after stall routine
STALL_CALLBACK_LO := $10
STALL_CALLBACK_HI := $11

PREV_CHAR       := $12  ; previous character in scroller

TEMP2           := $13
TEMP            := $14

FNT_P           := $15  ; font pointer
FNT_P_LO        := $15
FNT_P_HI        := $16

FNT_P_TILE      := $17
FNT_P_SHIFT     := $18

COL_PTR         := $20  ; column pointer
COL_PTR_LO      := $20
COL_PTR_HI      := $21

TEXT_CTR        := $22  ; text counter
ERASE_CTR       := $23  ; erase counter

COL_BUF       := $0200

; --------------------------
; ==== VIC-II ADDRESSES ====
; --------------------------

; https://www.zimmers.net/cbmpics/cbm/c64/vic-ii.txt

VIC_SPR0_X      := $D000        ; sprite X and Y positions
VIC_SPR0_Y      := $D001        
VIC_SPR1_X      := $D002
VIC_SPR1_Y      := $D003
VIC_SPR2_X      := $D004
VIC_SPR2_Y      := $D005
VIC_SPR3_X      := $D006
VIC_SPR3_Y      := $D007
VIC_SPR4_X      := $D008
VIC_SPR4_Y      := $D009
VIC_SPR5_X      := $D00A
VIC_SPR5_Y      := $D00B
VIC_SPR6_X      := $D00C
VIC_SPR6_Y      := $D00D
VIC_SPR7_X      := $D00E
VIC_SPR7_Y      := $D00F
VIC_SPR_X_MSB   := $D010        ; Hi bit of sprite x position, 1 bit per sprite
VIC_CTRL1       := $D011        ; RST8 | ECM | BMM | DEN | RSEL | YSCROLL
                                ; RST8    - Hi bit of Raster Counter
                                ; ECM     - Extended Color Mode
                                ; BMM     - Bit Map Mode
                                ; DEN     - Display Enable
                                ; RSEL    - Row Select           - display window height 1 = 25 character, 0 = 24 character 
                                ; YSCROLL - 3 bits

VIC_RASTER      := $D012        ; Raster Counter
VIC_LPEN_X      := $D013        ; Light pen X position
VIC_LPEN_Y      := $D014        ; Light pen Y position
VIC_SPR_ENA     := $D015        ; Sprite enable, 1 bit per sprite
VIC_CTRL2       := $D016        ; RES | MCM | CSEL | XSCROLL
                                ; RES     - "The RES bit (bit 5) of register $d016 has no function on the VIC 6567/6569 examined as yet. On the 6566, this bit is used to stop the VIC." - zimmers
                                ; MCM     - Multi Color Mode
                                ; CSEL    - Column Select - 0 = 38 character, 1 = 40 character
                                ; XSCROLL - 3 bits

VIC_SPR_EXP_Y   := $D017        ; Sprite vertical expand, 1 bit per sprite
VIC_VIDEO_ADR   := $D018        ; Memory Pointers - high 4 bits remaps where video ram is read from, default is from $0400
                                ; in text mode: low 4 bits * 1024 is where the character set is read from, lowest bit is always 0

VIC_IRR         := $D019        ; Interrupt request register - clear bit to ack interrupt
                                ; IRQ | - | - | - | ILP | IMMC | IMBC | IRST

VIC_IMR         := $D01A        ; Interrupt mask register
                                ; ELP | EMMC | EMBC | ERST
                                ; ELP  - Enable 
                                ; EMMC - Enable
                                ; EMBC - Enable
                                ; ERST - Enable Raster Interrupts

VIC_SPR_BG_PRIO := $D01B        ; Sprite priority, 1 bit per sprite
VIC_SPR_MCOLOR  := $D01C        ; Sprite color mode, set for multicolor 12x21, clear for 24 x 21 mono, 1 bit per sprite
VIC_SPR_EXP_X   := $D01D        ; Sprite horizontal expand, 1 bit per sprite

VIC_SPR_COLL    := $D01E        ; Sprite-sprite collision
VIC_SPR_BG_COLL := $D01F        ; Sprite-background collision

VIC_BORDERCOLOR := $D020
VIC_BG_COLOR0   := $D021
VIC_BG_COLOR1   := $D022
VIC_BG_COLOR2   := $D023
VIC_BG_COLOR3   := $D024

VIC_SPR_MCOLOR0 := $D025
VIC_SPR_MCOLOR1 := $D026

VIC_SPR0_COLOR  := $D027
VIC_SPR1_COLOR  := $D028
VIC_SPR2_COLOR  := $D029
VIC_SPR3_COLOR  := $D02A
VIC_SPR4_COLOR  := $D02B
VIC_SPR5_COLOR  := $D02C
VIC_SPR6_COLOR  := $D02D
VIC_SPR7_COLOR  := $D02E

; ------------------------------------------------
; ==== SID - SOUND INTERFACE DEVICE ADDRESSES ====
; ------------------------------------------------

SID1_FLO        := $D400        ; Voice 1 frequency lo byte 
SID1_FHI        := $D401        ; Voice 1 frequency hi byte
SID1_PWLO       := $D402        ; Voice 1 pulse width lo byte
SID1_PWHI       := $D403        ; Voice 1 pulse width hi 4 bits
SID1_CTRL       := $D404        ; Voice 1 control
                                ; Bit #0: Gate
                                ; Bit #1: Synchronization enabled (voice 3)
                                ; Bit #2: Ring modulation enabled (voice 3)
                                ; Bit #3: Disable
                                ; Bit #4: Triangle waveform enable
                                ; Bit #5: Saw waveform enable
                                ; Bit #6: Rectangle waveform enable
                                ; Bit #7: Noise enable
SID1_AD         := $D405        ; Voice 1 Attack and Decay length
                                ; Bits #0-#3: Decay length, 6ms to 24s
                                ; Bits #4-#7: Attack length, 2ms to 8s
SID1_SR         := $D406        ; Voice 1 Sustain volume and Release length
                                ; Bits #0-#3: Release length. 6ms to 24 s.
                                ; Bits #4-#7: Sustain volume.

SID2_FLO        := $D407
SID2_FHI        := $D408
SID2_PWLO       := $D409
SID2_PWHI       := $D40A
SID2_CTRL       := $D40B
SID2_AD         := $D40C
SID2_SR         := $D40D

SID3_FLO        := $D40E
SID3_FHI        := $D40F
SID3_PWLO       := $D410
SID3_PWHI       := $D411
SID3_CTRL       := $D412
SID3_AD         := $D413
SID3_SR         := $D414

SID_FLLO        := $D415        ; Filter cutoff frequency lo nybble
SID_FLHI        := $D416        ; Filter cutoff frequency hi byte
SID_FLCTRL      := $D417        ; Filter control bits
                                ; Bit #0: 1 = Voice #1 filtered.
                                ; Bit #1: 1 = Voice #2 filtered.
                                ; Bit #2: 1 = Voice #3 filtered.
                                ; Bit #3: 1 = External voice filtered.
                                ; Bits #4-#7: Filter resonance. 
SID_AMP         := $D418        ; Volume and filter modes
                                ; Bits #0-#3: Volume.
                                ; Bit #4: 1 = Low pass filter enabled.
                                ; Bit #5: 1 = Band pass filter enabled.
                                ; Bit #6: 1 = High pass filter enabled.
                                ; Bit #7: 1 = Voice #3 disabled.
SID_PADDLEX     := $D419        ; X value of paddle
SID_PADDLEY     := $D41A        ; Y value of paddle

SID3_OSC_READ   := $D41B        ; Voice 3 read oscillator
SID3_ENV_READ   := $D41C        ; Voice 3 read envelope

; ---------------------------------------------------
; ==== CIA - COMPLEX INTERFACE ADAPTER ADDRESSES ====
; ---------------------------------------------------

CIA1_PRA        := $DC00        ; Port A
CIA1_PRB        := $DC01        ; Port B
CIA1_DDRA       := $DC02        ; Data direction register for port A
CIA1_DDRB       := $DC03        ; Data direction register for port B
CIA1_TA         := $DC04        ; 16-bit timer A
CIA1_TB         := $DC06        ; 16-bit timer B
CIA1_TOD10      := $DC08        ; Time-of-day tenths of a second
CIA1_TODSEC     := $DC09        ; Time-of-day seconds
CIA1_TODMIN     := $DC0A        ; Time-of-day minutes
CIA1_TODHR      := $DC0B        ; Time-of-day hours
CIA1_SDR        := $DC0C        ; Serial data register
CIA1_ICR        := $DC0D        ; Interrupt control register
CIA1_CRA        := $DC0E        ; Control register for timer A
CIA1_CRB        := $DC0F        ; Control register for timer B

CIA2_PRA        := $DD00
CIA2_PRB        := $DD01
CIA2_DDRA       := $DD02
CIA2_DDRB       := $DD03
CIA2_TA         := $DD04
CIA2_TB         := $DD06
CIA2_TOD10      := $DD08
CIA2_TODSEC     := $DD09
CIA2_TODMIN     := $DD0A
CIA2_TODHR      := $DD0B
CIA2_SDR        := $DD0C
CIA2_ICR        := $DD0D
CIA2_CRA        := $DD0E
CIA2_CRB        := $DD0F

; -------------------
; ==== CONSTANTS ====
; -------------------

TOP_SCROLL      := $03  ; scroll value to use for the top 3rd of the screen
SCROLL_ROW      := 9    ; text row to start scroller at
FONT_HEIGHT     := $03  ; number of rows in scroller    

TEXT_START_POS  := 40*8 ; pixel column to load in text at
COL_ROLLOVER    := 50   ; text column to roll over font buffer at

COL_HEIGHT      := FONT_HEIGHT * 8 ; pixel height of font

                ; line to start scroller effect on - must be one line before a badline
SCROLL_START    := (SCROLL_ROW+6)*8+TOP_SCROLL-1
SCROLL_MAX      := $18  ; max value the scroll routine expects
                        ; total lines the scroll takes is FONT_HEIGHT*8+SCROLL_MAX

; ----------------
; ==== MACROS ====
; ----------------

; set the IRQ vector
.macro  irq_addr addr
        lda #<addr      
        sta $FFFE
        lda #>addr
        sta $FFFF
.endmacro

; set the IRQ line
.macro  irq_line line   ; TODO: this wastes cycles
        lda line        ; interrupt on line
        sta VIC_RASTER  ; set IRQ line
        asl VIC_IRR     ; ack IRQ 
.endmacro

; 16 bit increment operand p
.macro  i16 p         
        inc p+1
        bne :+
        inc p+2
:
.endmacro

; save registers before irq
.macro  irq_start
        sta irq_a+1
        stx irq_x+1
        sty irq_y+1
.endmacro

; create a label inside a repeat block
.macro makeident lname, count
    .ident(.concat(lname,.sprintf("%d", count))):
.endmacro

; -----------------------------
; ==== PROGRAM ENTRY POINT ====
; -----------------------------

.ifndef _main
.forceimport __EXEHDR__ ; BASIC stub
.endif

_scroller_init:
.export _scroller_init

        sei             ; disable interrupts

        lda #$7F        ; disable CIA interrupts
        sta CIA1_ICR
        sta CIA2_ICR

        and VIC_CTRL1   ; clear hi bit of RASTER
        sta VIC_CTRL1

        lda #%0001      ; enable raster interrupts
        sta VIC_IMR

        ; https://www.c64-wiki.com/wiki/Bank_Switching
        lda #%101       ; unmap BASIC and KERNAL, I/O enabled 
        sta $01

        lda VIC_CTRL2   ; set 38 column mode
        and #%11110111  
        sta VIC_CTRL2

        ldx #$02        ; clear zero page
        lda #$00
@zp:    sta $00,X
        inx
        bne @zp

        ; clear character page
        ldy #$08
@chr_clr:
        ldx #$00  
@chr_s: sta $2000,X
        inx
        bne @chr_s
        inc @chr_s+2
        dey
        bne @chr_clr

        ;  select custom character set
        lda VIC_VIDEO_ADR
        and #%11110000
        ora #%00001000
        sta VIC_VIDEO_ADR

        ; clear screen
        lda #$ff
        ldy #$04
@again: ldx #$00 
@base:  sta $0400
        i16 @base
        inx
        bne @base
@done:  dey
        bne @again

        ; print scroller pattern
        ldy #39
        lda #FONT_HEIGHT*40-1
        sec
@pat:   .repeat FONT_HEIGHT, i
        sta $0400+40*(SCROLL_ROW+FONT_HEIGHT-1-i),Y
        sbc #$01
        .endrepeat
        dey
        bpl @pat

        ; text start column
        lda #<TEXT_START_POS
        sta FNT_P_LO
        lda #>TEXT_START_POS
        sta FNT_P_HI

        ; initial width of characters to load
        lda #$fb ; -5
        sta TEXT_CTR

        ; track where the left edge of the scroll region is
        lda #$00
        sta ERASE_CTR

        irq_addr irq1   ; set up IRQ chain
        irq_line #$00 
        cli             ; enable interrupts

.ifdef _main
spin:   rts
_scroller_spin:
.export _scroller_spin
.else
spin:
.endif   

        lda TEXT_CTR    ; spin and wait for signals
        bpl @clear      ; if the text counter is negative,
        jsr load_text   ; load more text until its positive
@clear: lda ERASE_CTR 
        beq spin        ; if the erase counter is positive, 
                        ; erase columns until its zero

erase_text:
        ; decrement the erase counter
        dec ERASE_CTR 

        ; erase the column that scrolled off
        lda #$00
        ldx #COL_HEIGHT-1
@erase: sta $2000,X
        dex
        bpl @erase
        
        ; increment the erase pointer
        clc
        lda @erase+1
        adc #COL_HEIGHT
        sta @erase+1
        bcc :+
        inc @erase+2
:
        lda @erase+2  ; check for roll over 
        cmp #>($2000 + (COL_HEIGHT*COL_ROLLOVER))
        bne :+
        lda @erase+1
        cmp #<($2000 + (COL_HEIGHT*COL_ROLLOVER))
        bcc :+
        lda #$20
        sta @erase+2
        lda #$00
        sta @erase+1
:
        jmp spin


; the meat and potatos
load_text:
        jsr load_char   ; the potatos
        tay
        jsr place_char  ; and the meat
        rts


; load the next character from scroll_text into A
load_char:
@p_scr: lda scroll_text         ; load the next character 
        cmp #$FF
        beq @scroll_reset       ; hit terminator
        i16 @p_scr              ; advance to next char
        rts
@scroll_reset:
        lda #<scroll_text       ; if we hit the end of the scroll text reset
        sta @p_scr+1            ; to the start of the buffer
        lda #>scroll_text
        sta @p_scr+2
        jmp load_char


; write the character in Y to the screen
; FNT_P is the pixel column of PREV_CHAR 
place_char:

        ; set up character data pointer
        lda font_lo,Y   ; cdp = font[Y]
        sta @cdp+1
        lda font_hi,Y
        sta @cdp+2

        ; clear column buffer
        lda #$00        
        ldx #COL_HEIGHT-1 
:       sta COL_BUF,X
        dex
        bpl :-

        ; calculate kerning
        
        clc             ; FNT_P += kern[PREV_CHAR][Y]
        ldx PREV_CHAR  
        lda kern_lo,X
        sta @kern+1
        lda kern_hi,X
        sta @kern+2     
@kern:  lda $FFFF,Y     
        sta TEMP        
        bmi @neg
        adc FNT_P_LO
        sta FNT_P_LO
        bcc @kd
        inc FNT_P_HI
        jmp @kd
@neg:   adc FNT_P_LO
        sta FNT_P_LO
        bcs @kd
        dec FNT_P_HI
@kd:    sty PREV_CHAR

        clc             ; TEXT_CTR += kern[PREV_CHAR][Y]
        lda TEXT_CTR
        adc TEMP
        sta TEXT_CTR

        lda FNT_P_HI    ; FNT_P %= 8 * COL_ROLLOVER
        cmp #>(COL_ROLLOVER*8)
        bcc @noroll
        lda FNT_P_LO
        cmp #<(COL_ROLLOVER*8)
        bcc @noroll
        sbc #<(COL_ROLLOVER*8)
        sta FNT_P_LO
        lda FNT_P_HI
        sbc #>(COL_ROLLOVER*8)
        sta FNT_P_HI
@noroll:

        ; calculate column pointer and shift amount

        lda FNT_P_LO   ; FNT_P_SHIFT = FNT_P % 8
        and #%00000111
        sta FNT_P_SHIFT

        lda FNT_P_LO   ; COL_PTR = (FNT_P & ~%111) * 3
        and #%11111000
        sta COL_PTR_LO
        lda FNT_P_HI     
        sta COL_PTR_HI
        lda COL_PTR_LO
        asl
        rol COL_PTR_HI
        clc
        adc COL_PTR_LO
        sta COL_PTR_LO
        bcc :+
        inc COL_PTR_HI
        clc
:       lda FNT_P_HI
        adc COL_PTR_HI
        sta COL_PTR_HI
        
        lda COL_PTR_HI  ; COL_PTR |= $2000 (start of char page)
        ora #$20
        sta COL_PTR_HI

        ldx font_columns,Y
@col_loop:
        stx TEMP

        ldy #COL_HEIGHT-1
@do_buf:

        lda (COL_PTR),Y ; or in remainder of previous shift
        ora COL_BUF,Y
        sta (COL_PTR),Y

@cdp:   lda $FFFF,Y     ; load in character data
        sta TEMP2

        lda #$00        ; split character data across TEMP2 and COL_BUF by p_shift
        ldx FNT_P_SHIFT
        jmp @e
@shift: lsr TEMP2
        ror A
        dex
@e:     bne @shift
        sta COL_BUF,Y

        lda (COL_PTR),Y  ; or in shifted result
        ora TEMP2
        sta (COL_PTR),Y

        dey
        bpl @do_buf

        clc             ; increment character data pointer to next column
        lda @cdp+1
        adc #COL_HEIGHT
        sta @cdp+1
        bcc :+
        inc @cdp+2
        clc
:
        lda COL_PTR_LO   ; increment column pointer to next column
        adc #COL_HEIGHT 
        sta COL_PTR_LO   
        bcc :+
        inc COL_PTR_HI
:
        lda COL_PTR_HI  ; check for roll over 
        cmp #>($2000 + (COL_HEIGHT*COL_ROLLOVER))
        bne :+
        lda COL_PTR_LO
        cmp #<($2000 + (COL_HEIGHT*COL_ROLLOVER))
        bcc :+
        lda #$00
        sta COL_PTR_LO
        lda #$20
        sta COL_PTR_HI
:
        ldx TEMP
        dex
        bne @col_loop
        

        ldy #COL_HEIGHT-1
@last_col:
        lda (COL_PTR),Y  ; or in remainder of previous shift
        ora COL_BUF,Y
        sta (COL_PTR),Y
        dey
        bpl @last_col

        rts


; -----------------
; ====== IRQ ======
; -----------------

irq1:   irq_start   

        inc FRAME_COUNT ; update frame count
        lda FRAME_COUNT
        and #%11
        asl
        sta SCROLLERX   ; update X scroll
        bne noshift     ; shift every 4 frames     
        
shift:  ; increment scrolling region tiles 
        ldx #FONT_HEIGHT*40-1
@scroll_shift:
        lda $0400+40*SCROLL_ROW,X
        clc
        adc #FONT_HEIGHT
        cmp #FONT_HEIGHT*COL_ROLLOVER
        bcc :+
        sbc #FONT_HEIGHT*COL_ROLLOVER
:       sta $0400+40*SCROLL_ROW,X
        dex
        bpl @scroll_shift

        ; signal to erase column that scrolled off
        inc ERASE_CTR

        ; signal to load more text
        sec
        lda TEXT_CTR
        sbc #$08
        sta TEXT_CTR

noshift:
        ldx WIGGLE_INDEX
        lda FRAME_COUNT 
        and #%11        ; update vertical wiggle every 4th frame
        bne @skipwiggle 
        cpx #$00
        bne @dx
        ldx #14
@dx:    dex
        stx WIGGLE_INDEX

@skipwiggle:
        lda FRAME_COUNT
        and #%01111111
        tax
        lda sine_table,X
        sta SCROLL_Y 
        sta SCROLL_STALL  

        lda VIC_CTRL1   ; reset vertical scroll
        and #%11111000   
        ora #%00010000 | TOP_SCROLL   ; set BMM, initial scroll
        sta VIC_CTRL1
        
        lda VIC_CTRL2   
        and #%11111000  ; reset horizontal scroll
        ora #%00000000  ; set MCM
        sta VIC_CTRL2

        lda #<stall_callback_1  ; set callback
        sta STALL_CALLBACK_LO
        lda #>stall_callback_1
        sta STALL_CALLBACK_HI

        lda #SCROLL_START       ; IRQ one line before badline, line 7 of row
        sta IRQ_LINE
        sta VIC_RASTER  ; set IRQ line
        irq_addr irq_stall
        asl VIC_IRR     ; ack IRQ 
        jmp irq_done

; 
; IRQ STALL TECHNIQUE
;       stalls rendering by a given number of scanlines
;       
; ASSUMES:      SCROLL_STALL is the number of lines to stall
;               IRQ_LINE is set, and is the line before the next badline
irq_stall:
        irq_start

        ; vertical scroll        
        lda SCROLL_STALL ; number of lines left to stall for from IRQ_LINE
        cmp #$04        ; less than 4, jump to done phase to set remainder
        bcc @irq_stall_done   
        sbc #$04        ; sub 4 from remaining stall time, carry is still set
        sta SCROLL_STALL

        lda VIC_CTRL1   ; delay upcomming badline by 4 lines
        and #%11110111  ; prevent overflow into RSEL
        adc #$03        ; carry is set, this is +4 and clc
        ora #%00001000  ; re-set RSEL
        sta VIC_CTRL1

        ; chain irq in 4 lines
        lda IRQ_LINE       
        adc #$04        
        sta IRQ_LINE
        sta VIC_RASTER  ; set IRQ
        asl VIC_IRR     ; ack IRQ 
        jmp irq_done

        ; ASSUMES:  A contains the scroll value to add
        ;           carry is clear
@irq_stall_done:       
        sta @add+1
        lda VIC_CTRL1  
        and #%11100111  ; clear scroll and BMM, prevent overflow into RSEL
@add:   adc #$00        ; set scroll
        ora #%00001000  ; re-set RSEL
        sta VIC_CTRL1

        jmp (STALL_CALLBACK)

; first IRQ stall callback
stall_callback_1:       ; called at beginning of scroller

        lda VIC_CTRL2
        and #%11101000 ; clear MCM
        ora SCROLLERX  ; set horizontal scroll
        eor #%00000111
        sta VIC_CTRL2

        lda #<stall_callback_2  ; set callback to the next one in sequence
        sta STALL_CALLBACK_LO
        lda #>stall_callback_2
        sta STALL_CALLBACK_HI

        ; set next IRQ to one line before badline after scrolled rows
        lda IRQ_LINE    ;
        clc             ;
        adc #FONT_HEIGHT*8
        adc SCROLL_STALL ;
        sta IRQ_LINE    ;
        sta VIC_RASTER  ; set IRQ line
                        ; leave interrupt as irq_stall

        lda #SCROLL_MAX
        sec
        sbc SCROLL_Y    ; set stall amount for bottom gap
        sta SCROLL_STALL  

        asl VIC_IRR     ; ack IRQ 
        jmp irq_done        

; second IRQ stall callback
stall_callback_2:       ; called at end of scroller
        
        ; stop scroll horizontal
        lda VIC_CTRL2
        and #%11111000
        sta VIC_CTRL2

        irq_addr irq1   ; chain back to the start
        irq_line #$00

irq_done:
irq_a:  lda #$00        ; restore registers and return
irq_x:  ldx #$00
irq_y:  ldy #$00
        rti

; ----------------
; ==== TABLES ====
; ----------------

        .include "./font.inc"

; sine table 
; 128 entries - range: [$00,$18]
sine_table:       
        .byte $00,$00,$00,$00,$00,$00,$01,$01
        .byte $01,$01,$01,$02,$02,$02,$03,$03
        .byte $04,$04,$04,$05,$05,$06,$06,$07
        .byte $07,$08,$09,$09,$0A,$0A,$0B,$0B
        .byte $0C,$0D,$0D,$0E,$0E,$0F,$0F,$10
        .byte $11,$11,$12,$12,$13,$13,$14,$14
        .byte $14,$15,$15,$16,$16,$16,$17,$17
        .byte $17,$17,$17,$18,$18,$18,$18,$18
        .byte $18,$18,$18,$18,$18,$18,$17,$17
        .byte $17,$17,$17,$16,$16,$16,$15,$15
        .byte $14,$14,$14,$13,$13,$12,$12,$11
        .byte $11,$10,$0F,$0F,$0E,$0E,$0D,$0D
        .byte $0C,$0B,$0B,$0A,$0A,$09,$09,$08
        .byte $07,$07,$06,$06,$05,$05,$04,$04
        .byte $04,$03,$03,$02,$02,$02,$01,$01
        .byte $01,$01,$01,$00,$00,$00,$00,$00


        .include "./font_map.inc"

;  scroll_text:
;         .byte "aaabacadaeafagahaiajakalamanaoapaqarasatauavawaxayaza a-a,a.a;a'a&"
;         .byte "babbbcbdbebfbgbhbibjbkblbmbnbobpbqbrbsbtbubvbwbxbybzb b-b,b.b;b'b&"
;         .byte "cacbcccdcecfcgchcicjckclcmcncocpcqcrcsctcucvcwcxcyczc c-c,c.c;c'c&"
;         .byte "dadbdcdddedfdgdhdidjdkdldmdndodpdqdrdsdtdudvdwdxdydzd d-d,d.d;d'd&"
;         .byte "eaebecedeeefegeheiejekelemeneoepeqereseteuevewexeyeze e-e,e.e;e'e&"
;         .byte "fafbfcfdfefffgfhfifjfkflfmfnfofpfqfrfsftfufvfwfxfyfzf f-f,f.f;f'f&"
;         .byte "gagbgcgdgegfggghgigjgkglgmgngogpgqgrgsgtgugvgwgxgygzg g-g,g.g;g'g&"
;         .byte "hahbhchdhehfhghhhihjhkhlhmhnhohphqhrhshthuhvhwhxhyhzh h-h,h.h;h'h&"
;         .byte "iaibicidieifigihiiijikiliminioipiqirisitiuiviwixiyizi i-i,i.i;i'i&"
;         .byte "jajbjcjdjejfjgjhjijjjkjljmjnjojpjqjrjsjtjujvjwjxjyjzj j-j,j.j;j'j&"
;         .byte "kakbkckdkekfkgkhkikjkkklkmknkokpkqkrksktkukvkwkxkykzk k-k,k.k;k'k&"
;         .byte "lalblcldlelflglhliljlklllmlnlolplqlrlsltlulvlwlxlylzl l-l,l.l;l'l&"
;         .byte "mambmcmdmemfmgmhmimjmkmlmmmnmompmqmrmsmtmumvmwmxmymzm m-m,m.m;m'm&"
;         .byte "nanbncndnenfngnhninjnknlnmnnnonpnqnrnsntnunvnwnxnynzn n-n,n.n;n'n&"
;         .byte "oaobocodoeofogohoiojokolomonooopoqorosotouovowoxoyozo o-o,o.o;o'o&"
;         .byte "papbpcpdpepfpgphpipjpkplpmpnpopppqprpsptpupvpwpxpypzp p-p,p.p;p'p&"
;         .byte "qaqbqcqdqeqfqgqhqiqjqkqlqmqnqoqpqqqrqsqtquqvqwqxqyqzq q-q,q.q;q'q&"
;         .byte "rarbrcrdrerfrgrhrirjrkrlrmrnrorprqrrrsrtrurvrwrxryrzr r-r,r.r;r'r&"
;         .byte "sasbscsdsesfsgshsisjskslsmsnsospsqsrssstsusvswsxsyszs s-s,s.s;s's&"
;         .byte "tatbtctdtetftgthtitjtktltmtntotptqtrtstttutvtwtxtytzt t-t,t.t;t't&"
;         .byte "uaubucudueufuguhuiujukulumunuoupuqurusutuuuvuwuxuyuzu u-u,u.u;u'u&"
;         .byte "vavbvcvdvevfvgvhvivjvkvlvmvnvovpvqvrvsvtvuvvvwvxvyvzv v-v,v.v;v'v&"
;         .byte "wawbwcwdwewfwgwhwiwjwkwlwmwnwowpwqwrwswtwuwvwwwxwywzw w-w,w.w;w'w&"
;         .byte "xaxbxcxdxexfxgxhxixjxkxlxmxnxoxpxqxrxsxtxuxvxwxxxyxzx x-x,x.x;x'x&"
;         .byte "yaybycydyeyfygyhyiyjykylymynyoypyqyrysytyuyvywyxyyyzy y-y,y.y;y'y&"
;         .byte "zazbzczdzezfzgzhzizjzkzlzmznzozpzqzrzsztzuzvzwzxzyzzz z-z,z.z;z'z&"
;         .byte " a b c d e f g h i j k l m n o p q r s t u v w x y z   - , . ; ' &"
;         .byte "-a-b-c-d-e-f-g-h-i-j-k-l-m-n-o-p-q-r-s-t-u-v-w-x-y-z- ---,-.-;-'-&"
;         .byte ",a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z, ,-,,,.,;,',&"
;         .byte ".a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z. .-.,...;.'.&"
;         .byte ";a;b;c;d;e;f;g;h;i;j;k;l;m;n;o;p;q;r;s;t;u;v;w;x;y;z; ;-;,;.;;;';&"
;         .byte "'a'b'c'd'e'f'g'h'i'j'k'l'm'n'o'p'q'r's't'u'v'w'x'y'z' '-','.';'''&"
;         .byte "&a&b&c&d&e&f&g&h&i&j&k&l&m&n&o&p&q&r&s&t&u&v&w&x&y&z& &-&,&.&;&'&&"
;         .byte $FF

scroll_text:
        .byte "whenever i find myself growing grim abou"
        .byte "t the mouth; whenever it is a damp, driz"
        .byte "zly november in my soul; whenever i find"
        .byte " myself involuntarily pausing before cof"
        .byte "fin warehouses, & bringing up the rear"
        .byte " of every funeral i meet; & especially"
        .byte " whenever my hypos get such an upper han"
        .byte "d of me, that it requires a strong moral"
        .byte " principle to prevent me from deliberate"
        .byte "ly stepping into the street, & methodi"
        .byte "cally knocking people's hats off-then, "
        .byte "i account it high time to get to sea as "
        .byte "soon as i can. this is my substitute for "
        .byte "pistol & ball. with a philosophical "
        .byte "flourish cato throws himself upon his sword;"
        .byte " i quietly take to the ship.                "
        .byte "           "
        .byte $FF

; .feature string_escapes
; scroll_text:
;     .byte "the land-tenure regime in ptolemaic upper egypt* "
;     .byte "   j. g. manning.   "


;      .byte "\"the land which was held by the temples, and, especially in the south, was in the "
;      .byte "8 hands of hereditary tenants or owners, some of whom belonged to the higher and "
;      .byte "lower clergy, probably escaped the pressure of the government and was cultivated "
;      .byte "in the old-fashioned way.\""
;      .byte "- m. i. rostovtzeff, social and economic history of the hellenistic world n, 1200. "

;      .byte "*this paper is part of a larger book project in which i am engaged entitled peasants, local power and "
;      .byte "the ptolemies. toward a rural history of the nile valley in the hellenistic period. the present discussion should be considered prelinary until a further account of all the evidence is rendered therein. "

;      .byte "    -introduction-    "
    
;      .byte "previous views of the land-tenure regime in egypt under the ptolemies have "
;      .byte "not neglected consideration of the balance between the state and the individual "
;      .byte "but they have tended to emphasise one area of the country, the fayyum, and one "
;      .byte "aspect of economic organisation, the so-called royal economy imposed on the "
;      .byte "country by ptolemy ii [1]. both emphases have been directed by the nearly exclusive use of the greek papyri in constructing the historical narrative. we owe "
;      .byte "much of the basic picture of land-tenure in egypt during the hellenistic period "
;      .byte "to the influential work of michael rostovtzeff and claire preaux, scholars who "
;      .byte "both relied heavily on greek sources from the fayyum, among which the anon "
;      .byte "archive from philadelphia, dating to the third century bce, has occupied a prominent position [2]. the large number of documents illustrating the administration of "
;      .byte "a large estate granted by ptolemy ii philadelphus to one of his leading ministers can hardly be expected to offer a representative picture even for the fayyum. "
;      .byte "this was, in any case an unusual region in many respects and the emphasis on "
;      .byte "it has crowded out of the picture the nile valley and egyptian traditions of landholding, both of which are important aspects of any study of egypt as a whole "
;      .byte "under the ptolemies [3]. a better appreciation of the comparisons to be made "
;      .byte "between the evidence of the greek papyri and that of the demotic should allow "
;      .byte "us to redress the balance and to approach a description of the economy and society of ptolemaic egypt which will take account of the way in which institutions "
;      .byte "and practices appear in documents from two different social and linguistic "
;      .byte "traditions. "

;     .byte "rostovtzeff and those who have followed him have argued, or in some cases "
;     .byte "assumed, that ptolemaic governance of egypt was characterised by strong 'state' "
;     .byte "control of egypt's resources which, it is usually argued, was a tradition going "
;     .byte "back to the pharaohs. tight and efficient management of egypt's natural "
;     .byte "resources, and the elaborate taxation system placed upon these resources, in addition to a greatly increased degree of monetisation of the economy, stretched peasants and private economic activity to the point of breaking. such a scheme of "
;     .byte "state control of resources under the ptolemies is coming under increasing doubt "
;     .byte "as indeed it has for the pharaonic period as well [4]. that egypt was never as centrally, tightly, or uniformly managed as this argument holds is suggested by many "
;     .byte "general factors, not least the geography of the land and the nature of the long "
;     .byte "narrow river valley. the fundamental issue in controlling egypt was manageiment of water and its distribution. although it has been held that this was most "
;     .byte "effectively accomplished by despotic regimes, i would rather argue that it was "
;     .byte "always a matter for local concern and therefore it is the behaviour of local and "
;     .byte "regional power structures which must be studied and brought into relation with "
;     .byte "national or state institutions to whatever extent the latter existed [5]. the tension "
;     .byte "between 'state' and local authority is a theme which runs through egyptian history, and it became an iqcreasingly thorny issue in the hellenistic period with "
;     .byte "the political centre even further removed from the nile valley in the new city "
;     .byte "of alexandria. i argue in this paper that the ptolemies did not fundamentally "
;     .byte "alter the local institutional structures in the rural nile valley but adapted to what "
;     .byte "was already in place, grafting new administrative mechanisms on to traditional "
;     .byte "rural structures? this contrasts sharply with the fayyum, where royal interest "
;     ; p.85    
;     .byte "was more direct and government impact more profound because of the reclamation project which was followed by new settlements, the influx of new populations and experimentation in new crops, not the least of which were the olive "
;     .byte "and a new strain of wheat (triticum durum)[7]. to describe merely the royal economy, then, is to describe only part of the picture of the land-tenure regime in "
;     .byte "hellenistic egypt. the symbiosis of old and new in hellenistic egypt allowed "
;     .byte "significafit private economic activity. there continued to be a variety of landholding patterns and, operating underneath the royal economy, village and "
;     .byte "regiod economies, what braudel has termed the infra-economy.8 such a scenario suggests something far from a monolithic state economy. how these local "
;     .byte "economies were tied into the 'national' economy is perhaps the least-explored "
;     .byte "area of hellenistic economic history. furthermore, if bingen is correct, greeks "
;     .byte "had less access to land than has previously been thought, the corollary of which "
;     .byte "is a greater engagement in other types of economic acti~ity.~ this is another coneast1 to the egyptian scene, where egyptians and others continued to work land "
;     .byte "in traditional modes. but even on kleruchic holdings, egyptian lessees played a "
;     .byte "prominent role, not being displaced from the land but in fact performing the "
;     .byte "bulk of the farming for the nominal greek landholders [10]."
;     .byte $FF
; ;     .byte "the fayyum depression has been the focus of much of the scholarly attention accorded to rural history under the ptolemies because of the greek papyri, "
;     .byte "acquired either by excavation or purchase, from sites there and subsequently "
;     .byte "quickly published. the survival of greek papyri in great numbers here may in "
;     .byte "part be explained by the royal emphasis on this part of the country in the early "
;     .byte "ptolemaic period. but their use has led to an overemphasis of the notion of the "
;     .byte "hierarchical, smooth-running bureaucracy and a centrally administered economy [11]. "
;     .byte "since official administrative records tend to be prominent among the "
;     .byte "greek papyri, they may well offer a slanted and somewhat idealised view of "
;     .byte "the actual practice of the ptolemaic administration.12 this is not to diminish the "
;     .byte "importance which the early ptolemies attributed to the fayyum. kleruchs, or "
;     .byte "reservist soldiers, although also present in upper egypt were settled in greater "
;     .byte "' cf. rowlandson (1996), 29, 'royal interest in the ge hiera (sacred land) . . . is more evident in the "
;     .byte "fayyum than in the thebaid.' on the new crops, see thompson, below, ch. 6. "
;     .byte "braudel (1981), 24. "
;     .byte "see samuel (1989), 59. "
;     .byte "lo bingen (1978). "
;     .byte "'i even in relation to the evidence of the greek papyri, the role of state-directed authority over the "
;     .byte "countryside may be called into question. the annual survey of crops was not something dictated "
;     .byte "from alexandria hut was rather based on reports at the local level which were then sent to the city. "
;     .byte "see vidal-naquet (1967), but note that some of vidal-naquet's readings have been questioned (see "
;     .byte "the comments of thompson below, p. 136). "
;     .byte "'* a large proportion of ptolemaic greek papyri derive from mummy cartonnage made from papyri "
;     .byte "obtained from government record offices; the %non archive is an exception. "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "86 j. g. manning "
;     .byte "numbers in the fayyum where they reclaimed tracts of land in exchange for "
;     .byte "service in the army when called upon,13 and it was easier and politically more "
;     .byte "expedient to reclaim land there rather than seizing temple property in the nile "
;     .byte "valley.i4 the early ptolemies were very keen to keep egyptian temples and the "
;     .byte "native priesthood on their side. the use of new lands to settle soldiers was very "
;     .byte "important-it enabled the early ptolemies to have a ready and, more importantly, a loyal fighting force (in contrast to other hellenistic kings who more "
;     .byte "than once experienced soldiers defecting to the other side), and it also served as "
;     .byte "way of reclaiming land by forcing the kleruchs themselves to take on this task. "
;     .byte "a comparison with the middle kingdom fayyumic reclamation project under "
;     .byte "sesostris 11 and subsequent pharaohs of the twelfth dynasty links both, inter7 "
;     .byte "estingly, with the establishment of new, northem-based regimes.i5 "
;     .byte "i argue in this paper that in attempting to manage the countryside in upper "
;     .byte "egypt the ptolemies adopted pre-existing institutions and left unchanged traditional patterns of landholding. the nile valley received less direct royal attenrl "
;     .byte "tion than the fayyum (the history of the delta is largely unknown in the "
;     .byte "ptolemaic period) and thus remained as it had been before the arrival d "
;     .byte "alexander. i conclude therefore that in upper egypt there was strong continu: "
;     .byte "ity in the land-tenure regime from pre-ptolemaic times.16 one feature of economic continuity in upper egypt was the private holding and transfer of land "
;     .byte "within the context of the temple estates.17 after presenting some general consid-1 "
;     .byte "erations about the nature of the demotic egyptian documents of land conveyance, "
;     .byte "i will discuss the status-titles of parties to land conveyances and will then focus "
;     .byte "on one demotic egyptian family archive from third-century bce ed& "
;     .byte "(apollinopolis magna). i conclude this brief attempt at microhistory by discussing how more detailed analysis of the egyptian demotic material might affect "
;     .byte "previous views of land-tenure in egypt and how some of the evidence from "
;     .byte "upper egypt contrasts with that from the fayyum. in order to write a history of "
;     .byte "land-tenure in egypt in any period, i conclude, one must take into account "
;     .byte "l3 for the contrast between kleruchs, who had themselves to reclaim the land given to them, and "
;     .byte "egyptians holding old family estates, see clarysse (1979b), 742 and cf. samuel (1983). 45. on the "
;     .byte "large state-backed reclamation project under blemy ii and 111, see westermann (1917), butzer "
;     .byte "(1976), 37. "
;     .byte "l4 shaw (1992), 281 has recently stated that greeks in the early ptolemaic period proceeded to seize "
;     .byte "egyptian 'land, property and wealth' and that this constituted 'one of the greatest \"take-overs\"' in "
;     .byte "all of antiquity.' this is certainly an exaggeration of the situation. there is no evidence of which i "
;     .byte "am aware that the ptolemies ever seized temple land. "
;     .byte "see hayes (1971), 510-11, butzer (1976), 36-7. "
;     .byte "l6 such continuity with earlier periods contrasts with substantive structural change throughout egypt "
;     .byte "under augustus. see bowman and rathbone (1992); rowlandson (1996), 29-31. on rostovtzeffs "
;     .byte "view of 'continuance where possible', see samuel (1989), 53. "
;     .byte "l7 on private- ownership of land, see also turner (1984), 148; manning (19%). "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "land-tenure in ptolemaic upper egypt 87 "
;     .byte "regional differences in geography, landholding patterns, and social customs as "
;     .byte "well as the relationship of these to the variable degree of political control exerted "
;     .byte "by the central government. "
;     .byte "upper egyptian conveyance of land "
;     .byte "there are approximately eighty demotic egyptian conveyances of land from "
;     .byte "upper egypt and some additional sale receipts written on ostraka. they are a "
;     .byte "heterogeneous group of texts which, although couched in terms of sale, record "
;     .byte "many different types of transactions from true sales to mortgages, inheritance "
;     .byte "and forfeitures after a legal dispute. i focus on one type of documentation (conveyances) because it is the right to convey property which is a central element "
;     .byte "in any definition of private ownership of property. whether the existence of such "
;     .byte "a right is a suficient condition for the identification of the existence of private "
;     .byte "ownership of property is a question which might call for further discussion. be "
;     .byte "that as it may, i argue in this paper that this degree of private control of real "
;     .byte "property was an important element in the local economies of the nile valley. in "
;     .byte "these documents there is almost always a temple context of the sales; parties "
;     .byte "involved had status on temple estates or were women who gained status through "
;     .byte "their husbands. very few cases survive of the transfer of land from parents to "
;     .byte "their children by means of conveyance, yet we know that children regularly "
;     .byte "inherited land from their parents, and had an expectation of doing so, not always "
;     .byte "without problems.'s that land was regularly handed down from parents to their "
;     .byte "children is attested in the boundary descriptions in conveyances of land which "
;     .byte "frequently mention 'the land of so-and-so which is [now] in the possession of "
;     .byte "his children'. the explanation for the paucity of conveyance documents recording family transfers is that transfer normally occurred not through conveyances "
;     .byte "but by inheritance, the usual mechanism of which was a written marriage agreement between husband and wife which established the line of inheritance. if the "
;     .byte "land was held jointly by several siblings there may have been no need to draw "
;     .byte "up documentation at all. real divisions of land between siblings was effected "
;     .byte "through the deed of division (sh dny.t pa of which only a very few survive. if "
;     .byte "i am' right in arguing for a largely informal, family-based land-tenure regime "
;     .byte "here, the practice may be likened to the custom of athariyya-transfer in nineteenth century egypt.19 such informal intrafamilial transfer of real property "
;     .byte "would serve to avoid 'transaction costs' such as the cost of writing up a document of conveyance and transfer tax on the transaction.20 "
;     .byte "i' for a bitter family dispute over land at asyut in the 2nd century bce see thompson (1934) and "
;     .byte "below, p. 88. "
;     .byte "marsot (1984), 144. "
;     .byte "silver (1995), 132. "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "88 j. g. manning "
;     .byte "given the amount of arable land under cultivation in egypt, we know precious little about the disposition of much of it from documentary sources.z' such "
;     .byte "absence from the written record, may be explained, in part, by the accident of "
;     .byte "preservation. but i believe there is a second factor which also accounts for the "
;     .byte "lack of records. if small-scale family landholding was predominant, with land "
;     .byte "often jointly held within a family, it would have been unnecessary to convey "
;     .byte "land within the family, under normal circumstances, by written legal instrument.22 "
;     .byte "such conveyances record unusual transfers rather then the normal transfer of "
;     .byte "family property from one generation to the next. physical division of family land "
;     .byte "did occur and highlighted the tension between individual rights in real property "
;     .byte "and the desire to keep family land from fragmenting. since egyptians practised "
;     .byte "partible inheritance, further pressure on family property fragmentation was certainly exerted by the upward demographic trend in the hellenistic period.23 we "
;     .byte "can observe the result of this tension between individuals and the family unit in "
;     .byte "a famous family dispute over inheritance of real property in asyut in the second century bce. the dispute was over the inheritance of the ownership of two "
;     .byte "plots of land held by two half-brothers. the land was originally controlled jointly "
;     .byte "('without division', demotic ws'p$ and leased out. the phrase is often seen in2 "
;     .byte "the private legal papyri and indicates joint control, usually of family land. in the "
;     .byte "asyut dispute, the land, divided into two parcels, was inherited by two halfbrothers. at some point, we do not know exactly why (one of the brothers "
;     .byte "claimed he was being 'defrauded' and the ultimate tension may have been a "
;     .byte "problem in the division of the harvest from the land), a real division of the land "
;     .byte "was requested and a bitter court battle ensued, of which we have the verbatim "
;     .byte "record. the suit was brought by the eldest brother's wife on behalf of her chilrl "
;     .byte "dren to lay claim to the rights to both plots of land as the inheritance guaranteed to fall to her children by her marriage agreement. thus holding land as a "
;     .byte "family unit, while it made economic sense, may not always have reduced family tensions. such tension over the division of family land in part also derived "
;     .byte "from the tendency in egypt to divide the land into long narrow plots from the "
;     .byte "nile to the desert edge. this was a function of irrigation and similar family tension over land has continued in upper egypt even into the recent pa~t.2~ "
;     .byte "while we do not have sufficient numbers of texts to make the bar graphs statistically meaningful (the edfu graph represents, for example, one family "
;     .byte "archive), we can say that the sites best represented are the upper egyptian population centres which also had arable land in the immediate vicinity (thebes, "
;     .byte "pathyris, apollinopolis magna (edfu); see figure 4.1). elephantine (aswan), "
;     .byte "21 butzer (1976), 83 estimates 10, ooo km2 of arable land in the nile valley for the mid-ptolemaic "
;     .byte "period (150 bce). *' on such undocumented family land in the byzantine period see bagnall(1993), 149. "
;     .byte "23 bowman (1996), 17; butzer (1976), 91-2, with fig.13. "
;     .byte "24 ammar (1954), 24. "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "land-tenure in ptolemaic upper egypt 89 "
;     .byte "30 "
;     .byte "25 "
;     .byte "20 "
;     .byte "15 "
;     .byte "10 "
;     .byte "5 "
;     .byte "figure 4.1 upper egyptian demotic conveyances of land by site. "
;     .byte "otherwise a town of considerable importance in the ptolemaic period, is conspicuous by its absence, indicative of the small amount of arable land in that "
;     .byte "region. as publication of demotic ostraka continues, i would expect the numberd of recorded sales (in the form of receipts from sales of land) particularly "
;     .byte "from thebes to increase, perhaps signifi~antly.~~ under the heading of conveyance of real property come several varieties of land arable land, gardens, "
;     .byte "building plots, tomb sites. the decline in the number of demotic conveyances "
;     .byte "in the course of the ptolemaic period contrasts with the greek evidence for conveyance of land, which is almost all datable to the second and first centuries "
;     .byte "bce. the decrease in the demotic evidence in the first century bce here is consistent with the standard view on the decline of demotic as a legal language "
;     .byte "(figure 4.2).26 "
;     .byte "the size of the plot of land in demotic conveyances is generally small (table "
;     .byte "4.1):' there is, however, evidence that larger plots were in private hands in "
;     .byte "upper egypt and that both temples and priests claimed land as a heritable right.28 "
;     .byte "the private egyptian documentation often leaves out several important pieces "
;     .byte "of information such as the size of the conveyed plot or the origins of the property itself. in several cases the land was originally acquired by means of the "
;     .byte "royal auction, which is clearly stated in the document.29 we may conjecture that, "
;     .byte "' for new receipts, see devauchelle (1983). 155 (odl 92); vleeming (1994), texts 53, 56. "
;     .byte "26 lewis (1993); bagnall(l993). 236. "
;     .byte "\" this conforms to the views of rostovtzeff (1941). 11, 289 and pr6aux (1939) on ktema, private "
;     .byte "property. ** in the famous legal case of hermias from the theban west bank in the second century bce, we "
;     .byte "read by way of an aside in the transcript of the trial that a priest of amun had complained that a "
;     .byte "certain party had sold illegally 'about twenty arouras of grain-producing land (ge sitophoros) . . . "
;     .byte "although they were his ancestral property.' see up2 11, 162,4.2-3. for other sizeable plots held by "
;     .byte "egyptian families, see clarysse (197913). 734. "
;     .byte "29 see further manning (forthcoming). "
;     .byte "i "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "90 j. g. manning "
;     .byte "45 "
;     .byte "40 "
;     .byte "35 "
;     .byte "30 "
;     .byte "25 "
;     .byte "20 "
;     .byte "15 "
;     .byte "10 "
;     .byte "5 "
;     .byte "0 "
;     .byte "1 no conveyances1 "
;     .byte "3rd c 2nd c. 1st c "
;     .byte "figure 4.2 upper egyptian demotic conveyances of land by date. "
;     .byte "table 4.1 the number of demotic conveyances by size of plot. "
;     .byte "size of plot number of conveyances "
;     .byte "five arouras or more "
;     .byte "five arouas or less "
;     .byte "unspecified size "
;     .byte "19 "
;     .byte "43 "
;     .byte "17 "
;     .byte "as in ancient times, temple land may have been given out in payment to those "
;     .byte "who served the temple as well. i argue below, for example, that the hauswaldt "
;     .byte "papyri, from the third century bce, suggest that herdsmen in the service of the "
;     .byte "temple in edfu received land there in exchange for such service. the land seems "
;     .byte "to have been subsequently treated as 'private' since it could be passed on to children and sold, even to those without status on the temple estate. "
;     .byte "within the sphere of the temple estate, land was bought and sold by private "
;     .byte "individuals, men as well as women. with the exception of one case, there seems "
;     .byte "to have been no temporal restriction on conveyances. the exceptional case limits the transfer of a very small empty plot of land to a term of 99 years.3o "
;     .byte "alternatively, transfers of this length of time may have been a device to prevent "
;     .byte "fragmentation of temple property by nominally keeping it within the temple "
;     .byte "estate at least over the long term. the nature of the plot, an empty one suitable "
;     .byte "for building a house, probably played a role in limiting the transfer as well since "
;     .byte "there are other cases of building plots conveyed for a term of 99 years.31 "
;     .byte "there is great variation in the socio-economic background of the parties to "
;     .byte "conveyance of land in upper egypt (table 4.2). the edfu texts, for example, "
;     .byte "concern a family archive of herdsmen attached to the local temple while the "
;     .byte "texts from pathyris concern soldiers stationed there. rostovtzeff assumed that "
;     .byte "those who farmed temple land were slaves of the god.32 in understanding non3\" f! warsaw 148.288; see pestman (1977), text 10. pestman has argued that hs may be an indication that the vendor, a priest, was making an illegal transfer. "
;     .byte "31 taubenschlag (1955), 270. "
;     .byte "32 rostovtzeff (1941) i, 280. "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "land-tenure in ptolemaic upper egypt 91 "
;     .byte "table 4.2 titles of parties in demotic land conveyances from upper egypt. "
;     .byte "- status title of number of status title of number of "
;     .byte "zt vendor conveyances purchaser conveyances c "
;     .byte "templdreligious 32 templdreligious 26 "
;     .byte "woman 15 woman 17 "
;     .byte "greewsoldier 12 gree wsoldier 16 "
;     .byte "others 8 other 4 "
;     .byte "priestly staff of the temples as 'slaves' or tied peasants, he was no doubt translating the egyptian word b3k which has the meaning of 'slave' as well as 'servant'. there is no reason to assume that temple staff or tillers of temple land "
;     .byte "provided forced labour, and although many parties in demotic conveyances and "
;     .byte "leases of land did use the title b3k + divine name, greeks, nubians, soldiers, "
;     .byte "and women were all parties to conveyances of land located within a temple "
;     .byte "estate. i have argued elsewhere that in these legal contexts the term b3k was an "
;     .byte "honorific title used to indicate a particular status of relationship to a temple "
;     .byte "estate.33 before this title its holder's occupation (herdsman, farmer, etc.) is given. "
;     .byte "the frekpency of men with this kind of title in land conveyance documents suggests that they were in some economic relationship with the temple. the form "
;     .byte "which this relationship took may have been the land given to them in exchange "
;     .byte "for their service to the temple estate. the linking of the holding of land to social "
;     .byte "status has a long history in egypt and while the title consisting of occupation + "
;     .byte "servant of a particular god occurs almost exclusively in hellenistic egyptian "
;     .byte "legal papyri, its origins probably date to the saite period and it can thus be "
;     .byte "viewed as another continuity between the pharaonic and the hellenistic periods.35 "
;     .byte "the frequent appearance of these temple workers in egyptian land conveyances suggests that this type of transfer of land occurred within a defined "
;     .byte "social milieu and within a specific community. status titles in demotic conveyances of land suggest that there was a relationship between the holding of "
;     .byte "temple land and personal status on the temple estate. having status on the temple estate meant that one had access to land and had perhaps, as an added benefit, protection from the g~vernment.~~ those who had status on temple land "
;     .byte "may have benefited from personal protection from the government in a similar "
;     .byte "way to 'royal farmers'. the availability of some valuable evidence in egyptian "
;     .byte "documents for an area in which egyptian temples were clearly dominant as institutional landholders offers the opportunity to analyse these relationships in the "
;     .byte "land-tenure pattern in some detail. i approach this by concentrating on one town "
;     .byte "site in upper egypt and on one family archive from that town. "
;     .byte "' manning (1994). "
;     .byte "14 hughes (1952), 46. '' manning (1994). 168. "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "92 j. g. manning "
;     .byte "edfu and the edfu nome "
;     .byte "the town of edfu (apollinopolis magna) had always been a central place in "
;     .byte "egyptian history and its political and economic importance continued into the "
;     .byte "hellenistic and roman periods. in addition to its religious significance, the political and economic importance of the town was assured by its strategic location "
;     .byte "at a bend in the nile, affording considerable cultivable land in the immediate "
;     .byte "area of the town. the bend at the nile here, in addition to the confluence of caravan routes from the eastern and western deserts, made edfu an important military post as well as economic hub in southern upper egypt. the road, built or "
;     .byte "improved by ptolemy 11, ran from edfu to berenike on the red sea coast.36 "
;     .byte "heavily fortified town sites in the edfu region which are attested well before "
;     .byte "and after the ptolemaic period suggest that edfu had always been a strategic "
;     .byte "place.37 in 237 bce the rebuilding of the local temple was begun for the local "
;     .byte "cult of horns the behdedite, and the temple became a very important site for "
;     .byte "the cult of kingship, a subject of some concern to the new royal family in egypt. "
;     .byte "there can be no doubt at all this great project is central to the need of the "
;     .byte "ptolemaic regime to construct a good relationship with the egyptian religious "
;     .byte "establishment in this area. the reasons for so doing are emphasised by the fact "
;     .byte "that soon after the building project was begun work was halted by the rebels "
;     .byte "who instigated the great upper egyptian revolt which broke out in 207 bce>* "
;     .byte "the ptolemies apparently lost complete control of southern egypt until 187 bce "
;     .byte "when the area was retaken by force and more permanent means of control were "
;     .byte "set up. access to the gold-mining regions and the flow of elephants for the "
;     .byte "ptolemaic army would have been the principal economic concern of the "
;     .byte "ptolemies in the third century, but the region was always intended to be a part "
;     .byte "of egypt despite an economic policy largely dictated by the needs of "
;     .byte "mediterranean trade. the nome should not be characterised as 'situated in poor "
;     .byte "country' as kees suggested; on the contrary, it must have been agriculturally significant since there was considerable arable land in its vicinity.39 agricultural "
;     .byte "production was apparently sufficient in edfu for a man in the second "
;     .byte "intermediate period (c.1700 bce) to boast that he fed his village 'and the entire "
;     .byte "country' in a period of famine.'\"' the physical nome itself was probably the most "
;     .byte "stable in egypt, with natural boundaries to the north and south, the gebel south "
;     .byte "of el-kab and the limestone quarries at gebel es-silsileh respectively.4' edfu, "
;     .byte "at the crossroads of caravan routes from the eastern and western deserts, lay well "
;     .byte "36 meredith (1953), 95, n.1. "
;     .byte "37 bagnall (1976). 34-9; jaritz (1986), 37-9. "
;     .byte "38 see most recently pestman (1995). "
;     .byte "39 bietak (1979), 111. "
;     .byte "41 meeks (1972). 143. "
;     .byte "i "
;     .byte "stela cmo 20537, 11. 5-6, cited by vemus, lexikon der agyptologie vi (1986), 328. "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "land-tenure in ptolemaic upper egypt 93 "
;     .byte "within the nubian contact zone.\"2 just south of the town, up to gebel es-silsileh, "
;     .byte "the desert comes right up to the river bank, leaving no agricultural land on the "
;     .byte "east bank and virtually none on the west. the cultivable strip of land is so narrow thatsurveys of plots and boundary descriptions do not mention the east-west "
;     .byte "dimenhn but merely the length of the plot along the nile!3 "
;     .byte "one good reason for concentrating on this region in the context of land-tenure "
;     .byte "is theifact that we happen to possess more information about the disposition of "
;     .byte "land 'in. the southernmost nomes than in other areas of egypt in the third century bce, principally because of the so-called edfu donation text. the donation "
;     .byte "text, recording a cadastral survey of land in the sacred domain of the god horus "
;     .byte "of edfoa; documents several separate but related events: first, donations of land "
;     .byte "to the temple of horus by pharaohs at the time of the origins of the temple; "
;     .byte "second, donations of the 'sacred domain' of horus by several pharaohs subsequent to the land being donated; third, survey of the temple domain lands, probably by the first ptolemy early in his reign sometime before 305 bce; fourth, a "
;     .byte "fictiond donation of land by ptolemy alexander i; and last, inscription of the "
;     .byte "cadastral survey, at this time merely an historic 'relic' some time between 107 "
;     .byte "and 88 bce.~ a distinction must be made between the actual endowment of the "
;     .byte "land to the temple and the donation of the sacred domain, a purely symbolic, "
;     .byte "religious act. in fact, as meeks points out, the royal act of nectenebo and darius "
;     .byte "recorded in the donation text was the gift to the temple of its sacred domain "
;     .byte "rather than the land itself which the temple had had in its possession for some "
;     .byte "time. such royal ritual of 'donation'was performed at the beginning of a reign "
;     .byte "as a [sign of renewal. as in the satrap stela of ptolemy, in which the first ptolemy "
;     .byte "while still satrap 'donated' temple property originally given by the pharaoh "
;     .byte "khababash in the fourth century bce, the religious acts recorded by such texts "
;     .byte "are a record of pharaonic piety rather than a statement of ptolemaic largesse to "
;     .byte "an egyptian tem~le.4~ subsequent pharaohs merely 'reiterate' a donation of a "
;     .byte "previous king.& the donations of land as recorded occurred in the reigns of "
;     .byte "nectenebo i and darius i. "
;     .byte "so much for the donation of land and the royal ritual of donation of the temple's sacred domain. as for the land survey, like the donation ceremony, it would "
;     .byte "have occurred on a periodic basis to account for changes in land patterns over "
;     .byte "42 kees (l%l), 308. "
;     .byte "43 for a survey of land giving only the width of plots along the nile, see e! heidelberg 1289 published in spiegelberg (1920), 27, 57 and plate; thompson (1925), 151-3. presumably the length of "
;     .byte "the plots extended from the nile to the desert. "
;     .byte "for the chronology of events see meeks (1972), 13 1-5. mr thorolf cbnstensen is at present working on a greek text of uncertain date whch appears to be a survey of land in the apollinopolite "
;     .byte "nome. the text, phaun.inv.407, may matenally alter our views of the edfu donation text and the "
;     .byte "degree to which it reflects the reality of the land-tenure picture in the third century bce. "
;     .byte "45 meeks (1972), 133. "
;     .byte "the language used in the donation text is whm, lit. 'to repeat'. see meeks (1972), 62, n. 41. "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "94 j. g. manning "
;     .byte "the normal course of time in the nile valley. the text as we have it is most "
;     .byte "likely to reflect the state of the temple domain according to a cadastral survey "
;     .byte "probably carried out under ptolemy i while still ~atrap.4~ it is important to keep "
;     .byte "in mind that the building of the ptolemaic temple at edfu did not begin until "
;     .byte "237 bce. although meeks' arguments seem sound regarding the actual date of "
;     .byte "the survey, one could make an argument that the proper context of the re-survey of temple land occurred in the reign of ptolemy 11, which saw fundamen? "
;     .byte "tal changes in the economic organisation of egypt.'@ a text which might be "
;     .byte "brought to bear in this argument is the so-called karnak ostrakon, found at the "
;     .byte "sacred lake in karnak temple, luxor. it records, in a demotic translation of an "
;     .byte "original greek text, an order by ptolemy ii (year 28 = 258 bce) to survey egypt "
;     .byte "'nome by nome' and 'field by field.'49 "
;     .byte "the edfu donation text provides us with valuable information about temple "
;     .byte "estate land in upper egypt which we would otherwise not have except through "
;     .byte "the text of an egyptian land survey. such surveys of course have a long history "
;     .byte "in egypt but the edfu text is unique in being inscribed on the outer retaining "
;     .byte "wall of the temple in hieroglyphic egyptian rather than in greek or demotic "
;     .byte "egyptian, the latter two languages being used for documents recorded on "
;     .byte "papyrus.5o the estate of horus consisted of several tracts of land and was concentrated in the edfu nome. the plots were surrounded by land belonging to "
;     .byte "other temples in the south or by royal land. some of the temple land was stated "
;     .byte "to be for wheat-growing. the economic interdependence of the temple estates "
;     .byte "which the cadastral survey suggests was perhaps reinforced by temple rituals "
;     .byte "such as the visitation of the sacred bark of hathor of dendera to the edfu temple each year.5' "
;     .byte "the donation text records 'the total [amount of land] of the domain (&-ntr) "
;     .byte "of horus the behdedite, the great god, lord of heaven, from the origins up to "
;     .byte "year 18 of the son of re nectenebo i1 [the last year of his reign], [total size 00 "
;     .byte "fields: 13,209 1/8 ar0ura.s.' what follows is a list of fields controlled by the temple throughout the pathyrite, esna, edfu, and ombite nomes. land in the edfu "
;     .byte "nome itself comprised three-quarters of the total amount of temple domain land.52 "
;     .byte "the text concludes with a recapitulation of the donations under the various kings "
;     .byte "who donated land to the temple. the cadastral survey thus does not account for "
;     .byte "all the land in these nomes. the fields are generally specified as either island "
;     .byte "land or high land, the two basic egyptian categories of land. "
;     .byte "4 "
;     .byte "47 meeks (1972), 134. "
;     .byte "see above n. 1. "
;     .byte "49 for the text see bresciani (1983); for a translation and further bibliography see burstein (1985). "
;     .byte "50 on land surveys, see crawford (1971). 5-38. "
;     .byte "52 meeks (1972), 147. "
;     .byte "122-3. "
;     .byte "for the ritual see alliot (1949-1954), 297-99. "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "land-tenure in ptolemaic upper egypt 95 "
;     .byte "in the private conveyances of land from edfu discussed in the next section, "
;     .byte "it is ialways specified that island land is in the temple domain while high land "
;     .byte "is royal land. a plot of land is named, it is occasionally called 'wheat-bearing "
;     .byte "land', its size is given, and its boundaries at the four compass points are specified. in demotic land conveyances the boundaries on all four sides are also "
;     .byte "given, but in the private legal texts it is individuals who are named as holders "
;     .byte "of adjacent plots. again, the differences between the survey and private documentation is one between public, institutional interests and those of private persons, and neither type of text confirms or denies the existence of private property "
;     .byte "by itself. both private and institutional interest in land was concurrent. land "
;     .byte "which was found to be waterlogged was subtracted from the area of a given "
;     .byte "plot the survey covers the four southern nomes in upper egypt. "
;     .byte "as meeks has keenly observed, the estate of horus at edfu appears to have "
;     .byte "been stable throughout periods of political instability and change in the country, the second persian occupation and then the coming of alexander and the "
;     .byte "ptolemaic dynasty. although we do not know the extent to which the temple of "
;     .byte "horus actually controlled or adminstered all the land specified in the donation "
;     .byte "text, that the temple estate continued into the ptolemaic period is strongly suggested by private land conveyances contained within a family archive known as "
;     .byte "the hauswaldt papyri from edfu during the third century bce. "
;     .byte "no explanation has been offered either for the survey being placed on the "
;     .byte "temple wall or for the date of its inscription on the wall. one possibility i would "
;     .byte "suggest is that the temple authorities (i.e. the native priesthood) placed the text "
;     .byte "in 'public view' in order to assert the temple's claim to the land in a time of "
;     .byte "political disturbance, perhaps in this case connected with trouble in the 130s "
;     .byte "bce. such public display of an administrative document has parallels in the "
;     .byte "ptolemaic period.53 this pseudo-epigraphic text, then, couches the royal donation in historic terms in order to increase the cachet, and thus the authority, surrounding the donation. "
;     .byte "the hauswaldt papyri "
;     .byte "in 1909, a group of demotic papyri was purchased for the egyptian museum in "
;     .byte "berlin by georg hauswaldt from an antiquities dealer in qena. the texts were "
;     .byte "preliminarily published by wilhelm spiegelberg in 1913 under the name of the "
;     .byte "' a similar text is the so-called famine stela, translated by lichtheim (1980), 94-103. the text "
;     .byte "records a donation of revenue of djoser from the third dynasty to the temple of khnum at "
;     .byte "elephantine in exchange for the god's promise to relieve the country from a famine. it used to be "
;     .byte "thought that the text dated to the old kingdom but more recent work confirms some scholars' suspicions that the text is actually ptolemaic in date but couched as an old kingdom donation, presumably with the intention of using the claim to antiquity to increase its authority. "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "96 j. g. manning "
;     .byte "hauswaldt papyri.s4 though long since available to historians, the documents "
;     .byte "have not really received the attention they deserve. the texts comprise a family archive of herdsmen and record land conveyances and marriages from 265 "
;     .byte "to 208 bce.~~ what is unique about this archive is the number of private conveyances of land, mostly couched in the form of sales (table 4.3).56 "
;     .byte "the content of each egyptian family archive is different and thus it is not "
;     .byte "possible to make general conclusions or assumptions about typical transactions "
;     .byte "within egyptian families. in the case of the hauswaldt archive, most of the "
;     .byte "transactions relate to the holding of land, and the land conveyance 'theme' of "
;     .byte "the archive may be likened to the adler papyri, a family archive from secondcentury bce pathyri~.~~ in demotic egyptian legal texts, there were two separate texts which together comprised a real conveyance of property. these two "
;     .byte "documents were termed the sh db3 !zd (= greek prusis), a 'document in exchange "
;     .byte "for money', which recorded the postfucto agreement to sell, and a sh (n) wy (= "
;     .byte "greek sungruphe upostusiou), a 'document of quitclaim' which recorded the vendor's agreement to cede all claim to the conveyed property and guaranteed to "
;     .byte "expel any third-party contingent interest in the property. both these instruments "
;     .byte "could be written separately to pledge land and to cede land. in the hauswaldt "
;     .byte "papyri, the real sales of land had both documents written on the same sheet of "
;     .byte "papyrus side by side. only the month and year are specified, but it is to be presumed that the 'sale' and 'cession' occurred simultaneously and thus i use the "
;     .byte "term 'real sale' or 'conveyance' for these transactions. "
;     .byte "a 'typical' sh db3 @ in the hauswaldt archive may be summarised as follows: "
;     .byte "regnal year of ptolemy, protocol of priests in the ptolemaic dynastic cult. vendor "
;     .byte "has declared to buyer: 'you have satisfied my heart with the purchase price of my "
;     .byte "land, located within the temple estate of horus (or within the royal fields). names "
;     .byte "of the neighbours, or a landmark (the desert edge, a canal etc.) south, north, east, "
;     .byte "west. this is your property, no one else has any claim on it and i give you all the "
;     .byte "legal documents pertaining thereto. i will swear an oath to guarantee your rights. "
;     .byte "16 witness-names to the agreement written on the verso. "
;     .byte "a typical sh (n) wy document maybe summarised: "
;     .byte "54 i have recently completed a re-edition of these papyri which will appear in demofische szudien. "
;     .byte "ss the demotic word 'zm, usually translated 'herdsman,' has been interpreted in other ways, from "
;     .byte "an ethnic to a geographic designation. see the summary of the evidence in manning (1994), 150-6. "
;     .byte "56 i use the term sale for the egyptian documents consisting of two texts, a writing for money (sl! "
;     .byte "db3 @) and a cession (sh n wy). these egyptian documents were used to record transactions other "
;     .byte "than sale and thus i use the general term 'conveyance' when refemng to transactions involving these "
;     .byte "texts. "
;     .byte "57 on each family archive having a different theme see pestman (1985), 289. the adler papyri were "
;     .byte "published by adler e? al. (1939). "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "land-tenure in ptolemaic upper egypt 97 "
;     .byte "nblp 43 bpes of document in the hauswaldt archive. "
;     .byte "type of document number of texts "
;     .byte "marriage agreement "
;     .byte "sale of land' "
;     .byte "cession of land "
;     .byte "mortgage & forfeiture of land "
;     .byte "group acquisition of land "
;     .byte "gift of land -~ "
;     .byte "i there. are several other fragmentary land conveyances in the archive "
;     .byte "regnal year of ptolemy, protocol of priests in the ptolemaic dynastic cult. vendor "
;     .byte "has declared to buyer: i am far from you with respect to the sold property, located "
;     .byte "within the temple estate (or within the royal fields). names of the neighbours, or "
;     .byte "a hdmark south, north, east, west. i have no right to this property. as for anyone who claims an interest in this land i shall expel them. you have a legal claim "
;     .byte "on me to execute the legal rights in these documents. 16 witness-names to the "
;     .byte "agreement written on the verso. "
;     .byte "with the exception of one text, we do not know the amount of land conveyed "
;     .byte "but i make the assumption that small plots were involved. the one exception, "
;     .byte "f! hduswuldt 3, conveys a plot with an area of 1/4 aroura. the data from most "
;     .byte "of the egyptian conveyances of land lend support to the thesis that generally "
;     .byte "quite small plots of land were involved in private conveyance^.^^ in the case of "
;     .byte "the hauswaldt papyri themselves, the fact that the location of the land involved "
;     .byte "in these private conveyances was the far south end of the edfu nome, just below "
;     .byte "gebdl es-silsileh, where the breadth of the cultivable land is quite narrow, lends "
;     .byte "additional support to the likelihood of small plot conveyance. "
;     .byte "the type of land involved in the conveyances was termed 'high land' within "
;     .byte "'the'land of pharaoh', and 'island land' within 'the land of horus of edfu', with "
;     .byte "a cburtyard in between (figure 4.3).59 the distinction between high and island "
;     .byte "lana is generally believed to relate to the way in which water reached the land. "
;     .byte "high land is thought to be higher-lying land irrigated by artificial means while "
;     .byte "island land, lying closer to the nile and lower on the floodplain, was irrigated by "
;     .byte "the flooding of the nile. this explanation of the distinction involves some difficulties, not the least of which is that the nile floodplain does not rise smoothly "
;     .byte "from the river to the desert but is convex in shape.60 nevertheless, the hauswaldt "
;     .byte "papyri clearly show that island land lay closer to the river than did high land. "
;     .byte "58 for the reading of 114 aroura see manning (1994), 153, n. 32. ' the courtyards (termed ink n bpr) may have been mud-brick walled enclosures to protect the "
;     .byte "palm trees from pests and blown sand. "
;     .byte "mi butzer (1976), 15; manning (1995). 264. similar conclusion were reached by vleeming (1993). "
;     .byte "46-7. "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "98 "
;     .byte "south "
;     .byte "j. g. manning "
;     .byte "@ "
;     .byte "'inh n bpr "
;     .byte "high land "
;     .byte "nile "
;     .byte "figure 4.3 the configuration of the land in the hauswaldt conveyances. "
;     .byte "each conveyance of land transfers concurrently temple and royal land from "
;     .byte "one individual to another with no reference made to the king or to obligations "
;     .byte "to the crown in the form of crops to be grown. royal land in the fayyum would "
;     .byte "not have been the subject of private conveyance but it may be that the terminology used in the egyptian texts is an archaic usage of the terms 'royal' and "
;     .byte "'temple estate' land, an old bifurcation in land terminology which predates the "
;     .byte "ptolemies and is used, for example, in the edfu donation text. could it be that "
;     .byte "such usage of the terms 'royal' and 'temple estate' land in the third century bce "
;     .byte "is simply the old egyptian terminology carried into the legal papyri and does "
;     .byte "not necessarily correspond to the same meaning as the greek fiscal terms hiera "
;     .byte "ge (and hiera prosodos) and busilike ge? rather than positing a completely different land-tenure scheme for the nile valley, i am suggesting that the conveyance of both royal and temple land in the hauswaldt archive may be "
;     .byte "reflecting older terminology which directs the flow of rent or taxes rather than "
;     .byte "reflecting the absence of direct control of the king or the temple estate.6l "
;     .byte "one of the special features of these papyri is the frequent mention of palm "
;     .byte "and sycamore trees conveyed in the texts. since the men in the papyri bore the "
;     .byte "title 'herdsman' one is tempted to connect the conveyance of palm trees with "
;     .byte "the occupation of herding.62 certainly, temples had sacred herds as part of their "
;     .byte "h' on the conservative and formal nature of demotic, see ray (1994). "
;     .byte "the connection being the use of palm leaves as fodder, wright (1976). "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "land-tenure in ptolemaic upper egym 99 "
;     .byte "endowment and edfu may have had some specific connection with herding, an "
;     .byte "activity which the very narrow cultivable strip to the south of the town might "
;     .byte "have enc0uraged.6~ additionally, some of the conveyances in the hauswaldt "
;     .byte "papyri involved men with nubian ethnic designations (blemmyes and "
;     .byte "megabafhs). nubians were a common sight in this part of egypt in ancient times "
;     .byte "and they may have served both temple estates in the role of herdsmen and the "
;     .byte "ptolemks as guides in the eastern desert, an area of concern for the ptolemies both "
;     .byte "for the bow of gold and the much-vaunted but not very successful war elephants.64 "
;     .byte "as stated above, we can localise fairly specifically the plots conveyed by the "
;     .byte "hauswddt documents. the edfu donation text mentions names of fields at the "
;     .byte "southern end of the edfu nome, on the west bank, just below gebel es-silsileh, "
;     .byte "and these same locations occur in the specification of the plots of land in the "
;     .byte "hauswaldt papyri. we can therefore link through time the fourth-century donations of land to the temple, the re-survey of the land'in the early ptolemaic "
;     .byte "period, and the same fields, still referred to as lying within the temple domain "
;     .byte "of horus, in third-century private ~onveyances.6~ according to one of the "
;     .byte "hauswaldt documents (z-? huuswuldf 18), a market-place was located here so we "
;     .byte "can presume that a small village was in the vicinity. thus the occupation and "
;     .byte "social status of the parties to the hauswaldt land conveyances, the localised area "
;     .byte "of the land being conveyed, the emphasis on fruit tree production, all suggest "
;     .byte "that the conveyances of land occurred within a specifically defined social group "
;     .byte "in one rather smali region. "
;     .byte "conclbsions "
;     .byte "that familiar historical theme of continuity and change is very much to the point "
;     .byte "in considering the land-tenure patterns of hellenistic egypt. in the third "
;     .byte "century, the land regime was altered by the ptolemies where they could do so "
;     .byte "without discomfort or disruption to suit their needs. the area most affected by "
;     .byte "development and change was the fayyum depression. in the nile valley, life, "
;     .byte "and the land-tenure regime, continued much as it had before the arrival of "
;     .byte "alexander. i have argued that the fayyum experienced substantive physical and "
;     .byte "socialschanges by reclamation, new crop and animal experimentation, and new "
;     .byte "populations. the nile valley, more removed from the centre of political control "
;     .byte "in alexandria, and long used to the natural basin irrigation system, was altered "
;     .byte "' there are several sales of oxen from the saite and persian periods from edfu, for which see cruzunbe (1985). and festivals at the edfu temple mention an abundance of cattle (de rochemonteix "
;     .byte "and chassinat, le tempze d'edfou iv.3-1-8). for fourth-century bce 'herdsmen, servants of horus "
;     .byte "of edfu' involved in selling cows, see menu (1981). with the comments of vleeming (1984). "
;     .byte "64 on eastern desert nomads as guides see burstein (1989). 61. \"' for the location, see the 'hauswaldt zone' on the map. "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "100 j. g. manning "
;     .byte "much less by the ptolemies. rather, they hoped to control it enough to extract "
;     .byte "grain levies. "
;     .byte "although the use of demotic conveyance documents from one region alone "
;     .byte "no doubt yields a skewed vision of the total picture of land-tenure in upper "
;     .byte "egypt just as using greek papyri alone does for the fayyum, the nature of the "
;     .byte "documentation from upper egypt suggest a general picture of continuity in these "
;     .byte "traditions of landholding which the ptolemies had no need to change. the temples were endowed with estate land at their foundation from which income was "
;     .byte "derived to maintain the cult, and this practice of temple estate land seems to "
;     .byte "have continued under the ptolemies. in the fayyum, however, and perhaps in "
;     .byte "the delta, on newly reclaimed land, the ptolemies exerted direct control, giving "
;     .byte "it out to kleruchs in exchange for military service, leasing it to 'royal farmers' "
;     .byte "and ceding it as large gift estates (doreai) to high officials. although greeks certainly lived throughout the nile valley, their presence in upper egypt was much "
;     .byte "less marked than in the north. this appears to be truer of the third century than "
;     .byte "the second but caution is called for since the capricious survival of textual evidence may give a misleading impression.@ in edfu, a 'greek born in egypt' "
;     .byte "(wynn ms n kmy) appears as a money-lender to whom several plots of temple "
;     .byte "land were handed over upon default of the l0an.6~ although the numbers of "
;     .byte "greeks may have been smaller, the ptolemies did have a continuing interest in "
;     .byte "upper egypt and over time more greeks (albeit, perhaps, defined by less rigorous ethnic criteria) settled in the valley. after the theban revolt was put down "
;     .byte "in 186 bce, towns were garrisoned at the narrowest point in the upper egyptian "
;     .byte "valley, at krokodilopolis and pathyris, which also led to an increase in the number of greeks in the valley.68 "
;     .byte "ptolemaic policy toward the temples gave them special status and privileges, "
;     .byte "for the ptolemies needed the clite egyptian priesthood on their side and they no "
;     .byte "doubt hoped to use it to win over the hearts and minds of the egyptian peasantry. the kings had neither the manpower nor the motive to take over or seize "
;     .byte "the assets of temples. a more nagging problem is the extent to which the temples actually controlled endowment land, an issue which will have to ,be "
;     .byte "addressed in future work. that there was a close association between king and "
;     .byte "temple is clear. the temples certainly received income directly from sale of land "
;     .byte "in necropoleis owned by them and high priests were involved in land sales within "
;     .byte "their temple estates as well.69 temples were the local centres of power and had "
;     .byte "the infrastructure, in the form of organised personnel, to control their hinter66 for creeks in thebes, a 'small minority', see clarysse (1995). "
;     .byte "67 i! hauswaldt 18, 212./211 bce. "
;     .byte "after the theban insurrection was put down, the ptolemies made a concerted effort to restore "
;     .byte "order by taking back illegally seized land and auctioning off the property. on this process, see "
;     .byte "clarysse (1979a). - on temple income derived from sales of plots, see vleeming (1994), 115-16. "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "land-tenure in ptolemaic upper egyft 101 "
;     .byte "lands. the difference, then, between temple and royal land may have been one "
;     .byte "of management (and rent collection?) and not reflective of who 'owned' the land. "
;     .byte "there is no evidence for the old view that temple land was managed by the "
;     .byte "crow in the same way as royal land, nor was royal power asserted on nonroyal land except when the land became derelict or taxes were not paid.70 the "
;     .byte "ptolemies did introduce officials in charge of monitoring the temples' finances - "
;     .byte "the episfufes, the pruktor-but these officials were grafted on to existing structures ,whose local character did not change.71 "
;     .byte "the.evidence of non-official egyptian documents from the nile valley suggests &a revision in the 'estatist' model of hellenistic egypt; this is often attributed by historians to the influence of rostovtzeff but, as the quotation at the "
;     .byte "beginning of this article shows, the matter is not so simple. rostovtzeff's concluding remarks in the social and economic history of the hellenistic world on "
;     .byte "the status of temple land in upper egypt does not harmonise with the model of "
;     .byte "centralised state control which was the essential feature, in his view, of ptolemaic "
;     .byte "egypt. if the ptolemies allowed 'continuance where possible' and if having status on temple land was an economic safety valve allowing holders to 'escape the "
;     .byte "presswe of the government', then we can hardly characterise hellenistic egypt "
;     .byte "as a centralised bureaucracy in which 'everything was for the state.'72 "
;     .byte "i have argued in this paper that the system of control under the ptolemies was "
;     .byte "informal rather than centralised, and regionally variable rather than uniform "
;     .byte "throughout egypt. the ptolemies adapted in a practical manner to the realities "
;     .byte "of egypt. although they developed a large bureaucracy, its responses in exploiting the, countryside were to a great extent adaptive to existing local conditions "
;     .byte "and practices.73 the dominant force in the egyptian countryside had been in the "
;     .byte "past and continued under the ptolemies to be represented by the native temple "
;     .byte "and its landed estates. as in other parts of the hellenistic world, a multitude of "
;     .byte "diverse economic relationships continued to exist (including what amounted, in "
;     .byte "my view, to effective private ownership of real property) and regional differences continued to play an important role.74 within this regional diversity, i have "
;     .byte "argued that personal status on the temple estate of horus at edfu played an "
;     .byte "important role in the local economy of that area. i have focused on one area in "
;     .byte "the south-edfu and its hinterland-and on one type of document-conveyance of real property-in order to demonstrate this regional diversity, and "
;     .byte "i have used it to highlight an implicit contrast with the fayyum depression. the "
;     .byte "social and economic history of edfu in the ptolemaic period suggests a strong "
;     .byte "degree of continuity with pre-ptolemaic egypt. "
;     .byte "'\" on crown management, see shelton (1971). 115 n.1; keenan and shelton (1976). 17. "
;     .byte "\" local variations in temple organisation of land-tenure appear to have been maintained. at akoris "
;     .byte "(tehneh), for example, there was an official in charge of leasing out temple lands (hry 34, 'overseer of field') who does not seem to occur in other temple estates, see z? loeb. "
;     .byte "j2 rostovtzeff (1920). 164. "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "102 j. g. manning "
;     .byte "the ptolemies, in setting up new economic structures designed to extract as "
;     .byte "much wealth from the countryside as possible for their grain trade in the "
;     .byte "mediterranean, imposed a system of controls on the land which, while looking "
;     .byte "effective on paper, was rather more reactive than planned. the ptolemies could "
;     .byte "more easily impose the new royal system on reclaimed areas than on old temple estates in upper egypt which had long-standing relationships with each other; "
;     .byte "the royal economy was mediated in the nile valley by old social and institutional structures. the importance of family landholding and small-scale possession of land has been underestimated in the modem reconstruction of the "
;     .byte "hellenistic economy. at one level, above the household economy, inter-village "
;     .byte "and inter-regional connections strengthened local social cohesion and this was "
;     .byte "left undisturbed by the ptolemies. regional ties were reinforced by cultic connections between the temples in the nile valley, at dendera and edfu, for example. in the case of the temple of khnum of elephantine, where virtually no "
;     .byte "agricultural land existed, access to land in the edfu nome was accorded. such "
;     .byte "economic interconnections pre-dated the pt0lemies.7~ this is not to say that other "
;     .byte "areas did not have coherent social cohesion. inter-village connections based on "
;     .byte "landholding were also strong in the fayy~m.~~ but strong regional social ties "
;     .byte "probably affected the way the ptolemies dealt with the region and may be one "
;     .byte "reason why reclamation of land in the fayyum was an important economic strategy for the early ptolemies. "
;     .byte "as always in egypt, local conditions and local power-bases dictated methods "
;     .byte "of control of the land and thus, to some extent at least, the land-tenure system. "
;     .byte "in upper egypt, the large temples continued as managers of their estates, with "
;     .byte "support staff given land in exchange for service. tenure on the land was not precarious, as long as taxes were paid. "


;     .byte "[1] on ptolemy ii philadelphus as the innovator of the royal economy, see turner (1984), 133-59. "
;     .byte "[2] the economic structure of hellenistic egypt is summarised in rostovtzeff (1941) i, 267-332. in "
;     .byte "addition to tumer (1984), see the recent critiques of rostovtzeff's views by samuel (1989), 51-65; "
;     .byte "austin (1986); pr6aux (1939) remains the standard reference work on the functioning of the royal "
;     .byte "economy. on the zenon archive see thompson, below, p. 125. "
;     .byte "[3] thompson, in this volume (chs. 5 and 6) stresses the atypicality of the fayyum. butzer (1976). "
;     .byte "58, has pointed out the factors which make the fayyum a 'distinct ecozone'. the royal focus on the "
;     .byte "fayyum, crop experimentation and the burgeoning population in alexandria all exerted new pressures on the fayyum. on the suggestion of the importance of regional differences, see crawford "
;     .byte "(1973), 223. "
;     .byte "[4] see e.g. the comments of butzer (1976), 5cl. "
;     .byte "[5] butzer (1976), 51; cf. samuel (1989). 56. "
;     .byte "[6] prkaux (1939), 429-31 described the adrmmstrative structure as consisting of several layers "

;     .byte "proceedings of the british academy, 96, 83-105. (c) the british academy 1999. "

;     .byte "73 samuel (1989). 54. "
;     .byte "74 for the seleucid empire, see the remarks by sherwin-white and kuhrt (1993), 69. "
;     .byte "7s demotic papyri from edfu from the fourth century bce demonstrate that soldiers from elephantine "
;     .byte "owned houses in edfu. from the pharaonic period, a letter from the hesside period known as i! "
;     .byte "valenpy 1 (katary (1989), 214-15) mentions a mayor of elephantine who had to farm a plot of "
;     .byte "land in edfu as part of his official duties. "
;     .byte "76 bagnall (1995). 50-1 summarising hobson (1984). "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "land-tenure jn ptolemaic upper egypt 103 "
;     .byte "bibliography "
;     .byte "e. n. adler et al. (1939), the adler papyri "
;     .byte "m. ailiot (1949-1954), le culte d'horus iz edfou au temps des ptoldmdes "
;     .byte "h. ammar (1954), growing up in an egyptian village "
;     .byte "m. m. austin (1986), 'hellenistic kings, war, and the economy,' cq 36, 450-66 "
;     .byte "r. s. bagnall (1976), the florida ostraka. documents from the roman army in upper "
;     .byte "r. s. bagnall(l992). 'landholding in late roman egypt: the distribution of wealth,' jrs "
;     .byte "r. s. bagnall (1993), egypt in late antiquity "
;     .byte "r. s. bagnall (1995), reading papyri, writing ancient history "
;     .byte "m. bietak (1979), 'the town problem in ancient egypt,' in k. weeks (ed.) egyptology "
;     .byte "j. bingen (1978), 'the third-century bc land-leases from qolthis,' illinois classical "
;     .byte "a. k. bowman (1996). egypt &er the pharaohs (2nd paperback edn) "
;     .byte "a. k. bowman, d. w. rathbone (1992), 'cities and administration in roman egypt,' "
;     .byte "e braudel(1981), the structures of everyday life. civilization and capitalism 15th-18th "
;     .byte "century, 1 "
;     .byte "e. bresciani (1983), 'registrazione catastale e ideologia politica nell'egitto tolemaico. a "
;     .byte "completamento di la spedizione di tolomeo ii in siria in un ostrakon demotic0 inedit0 da kamak,' evo 3, 15-31 "
;     .byte "s. burstein (1985), the hellenistic age from the battle of ipsos to the death of cleopatra "
;     .byte "vi1 (translated documents of greece and rome 3) "
;     .byte "s. burstein (1989), agatharcides of cnidus. on the erythraean sea "
;     .byte "k. w. butzer (1976), early hydraulic civilization in egypt. a study of cultural "
;     .byte "ecology "
;     .byte "w. clarysse (1979a), 'ptolemaic papyri from lycopolis,' actes du we congrts intemational de papyrologie (papyrologica bruxellensia 19), iv, 1014 "
;     .byte "w. \"larysse (1979b), 'egyptian estate-holders in the ptolemaic period,' in e. lipinski "
;     .byte "(4.) state and temple economy in the ancient near east. vol. 2, 73143. "
;     .byte "w. clarysse (1995), 'greeks in ptolemaic thebes,' in s. p. vleeming (ed.), hundredgated thebes. acts of a colloquium on thebes and the theban area in the graecoroman period (i?l.bat. 27), 1-19 "
;     .byte "egypt "
;     .byte "82, 12849 "
;     .byte "and the social sciences "
;     .byte "studies 3, 74-80 "
;     .byte "jrs 82, 107-27 "
;     .byte "d. j. crawford (1971), kerkeosiris. an egyptian village in the ptolemaic period "
;     .byte "d. i. crawford (thompson) (1973). 'the opium poppy: a study in ptolemaic agriculture,' "
;     .byte "e. cruz-uribe (1985). suite and persian demotic cattle documents. a study in legal "
;     .byte "h. cuvigny (1983, l'arpentage par esptces duns 1'egypte ptoldmai'que d'aprts les "
;     .byte "d. devauchelle (1983), ostraka ddmotiques du musde du louvre, 1: regus "
;     .byte "w. c. hayes (1971), 'the middle kingdom in egypt,' in i. e. s. edwards, c. j. gadd, "
;     .byte "n. g. l. hammond (eds), cambridge ancient history i (3rd edn), part 2, 464- "
;     .byte "53 1 "
;     .byte "in m. i. finley (ed.) probltmes de la terre en grtce ancienne, 223-51 "
;     .byte "forms and principles in ancient egypt (asp 26) "
;     .byte "papyrus grecs (papyrologica bruxellensia 20) "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "104 j. g. manning "
;     .byte "d. w. hobson (1984), 'agricultural land and economic life in soknopaiou nesos,' basp "
;     .byte "g. r. hughes (1952), suite demotic land leases "
;     .byte "j. j. janssen (1979), 'the role of the temple in the egyptian economy during the new "
;     .byte "kingdom,' in e. lipinski (ed.) state and temple economy in the ancient near east, "
;     .byte "21, 89-109 "
;     .byte "2, 505-15 "
;     .byte "h. jatitz (1986), 'on three townsites in the upper thebaid,' cripel 8, 37-9 "
;     .byte "s. l. d. katary (1989), land tenure in the ramesside period "
;     .byte "j. g. keenan, j. c. shelton (1976), the tebtunis papyri 4 "
;     .byte "h. kees (1961), ancient egypt. a cultural topography "
;     .byte "n. lewis (1993), 'the demise of demotic: when and why?' jea 79, 276-81 "
;     .byte "m. lichtheim (1980), ancient egyptian literature. a book of readings 3: the late period "
;     .byte "j. g. manning (1994), 'land and status in ptolemaic egypt: the status designation "
;     .byte "\"occupation title + b3k + divine name\",' in s. allam (ed.), grund und boden in altugypten (rechtliche und sozio-okonomische symposions tiibingen 18. -20. juni 1990), "
;     .byte "j. g. manning (1995), 'irrigation terminology in the hauswaldt papyri and other texts' "
;     .byte "from edfu during the ptolemaic period,' in b. menu (ed.), les probl2mes institutionnels de 1 'eau en egypte ancienne et duns l'antiquitd mdditerrandenne (colloque "
;     .byte "vogiid 1992) "
;     .byte "j. g. manning (1996), 'demotic egyptian instruments of transfer as evidence for private "
;     .byte "ownership of real property,' chicago-kent law review 70, 101-32 "
;     .byte "j. g. manning (forthcoming), 'the auction of pharaoh,' in emily teeter & john larsen "
;     .byte "(eds.), studies presented to edward e wente "
;     .byte "a. marsot (1984). egypt in the reign of muhammed ali "
;     .byte "d. meeks (1972), le grand texte des donations au temple d'edfou "
;     .byte "b. menu (1981), 'deux contrats de vente datks du rkgne de nectknebo 11,' bifao 85, "
;     .byte "d. meredith (1953), 'the roman remains in the eastern desert of egypt (continued) jea "
;     .byte "p. w. pestman (1977), recueil de textes ddmtiques et bilingues "
;     .byte "p. w. pestman (1985), 'some aspects of egyptian law in graeco-roman egypt. title deeds "
;     .byte "and dn&kqpa,' in e. van't dack, p. van dessel, w. van gucht (eds), egypt and "
;     .byte "the hellenistic world (studia hellenistica 27) "
;     .byte "p. w. pestman (1995), 'haronnophris and chaonnophris. two indigenous pharaohs in "
;     .byte "ptolemaic egypt,' in s. p. vleeming (ed.), hundred-gated thebes. acts of a colloquium on thebes and the theban area in the graeco-roman period (rlbat. 27), "
;     .byte "147-75 "
;     .byte "45-52, pls 9-13 "
;     .byte "39, 95-106 "
;     .byte "101-37 "
;     .byte "c. prkaux (1939), l'dconomie royale des lagides "
;     .byte "j. d. ray (1994), 'how demotic is demotic?' in e. bresciani (ed.), acta demotica. acts "
;     .byte "of the fifth international conference for demotists, pisa 4th-8th september 1993, "
;     .byte "25 1-64 "
;     .byte "m. i. rostovtzeff (1920), 'the foundations of social and economic life in egypt in hellenistic times,' jea 6, 161-78 "
;     .byte "m. i. rostovtzeff (1941), the social and economic history of the hellenistic world "
;     .byte "j. l. rowlandson (1983) landholding in the oxyrhynchite nome 30 bc-c. ad 300 (oxford "
;     .byte "d.phi1. thesis) "
;     .byte "copyright © british academy 1999 - all rights reserved "
;     .byte "land-tenure in ptolemaic upper egypt 105 "
;     .byte "j. l. rowlandson (1996), landowners and tenants in roman egypt. the social relations "
;     .byte "a. e. samuel (19831, from athens to alexandria: hellenism and social goals in "
;     .byte "a. e. samuel (1989). the shifing sands of history: interpretations of ptolemaic egypt "
;     .byte "b. shaw (1992), 'explaining incest: brother-sister marriage in graeco-roman egypt,' "
;     .byte "j. c. shelton (1971), 'ptolemaic land kv &$6on: an observation on the terminology,' ce "
;     .byte "s. m. sherwin-white, a. kuhrt (1993), from samarkhand to sardis. a new approach "
;     .byte "m. silver (1w5), economic structures of antiquity "
;     .byte "w. spiegelberg (1913), die demotischen papyri hauswaldt. vertrage der ersten haljie "
;     .byte "w. spiegelberg ( 1920), koptische etymologien. beitrage zu einem koptischen worterbuch "
;     .byte "r. taubenschlag (1955), the law of greco-roman egypt in the light of the papyri (2nd "
;     .byte "h. thompson (1925), 'length-measures in ptolemaic egypt,' jea 11, 151-3 "
;     .byte "h. thompson (1934), a family archive from siut "
;     .byte "e. g. turner (1984), 'ptolemaic egypt,' in f. w. walbank, a. e. astin, m. w. frederiksen, "
;     .byte "r. m. ogilvie (eds), cambridge ancient history vi1 (new edn), part 1, 118-74 "
;     .byte "p. vidal-naquet (1967), le bordereau d'ensemencement dans 1 'egypt. ptoldmai'que "
;     .byte "s. p. vleeming (1984), 'some notes on p. fao 901 & 902,' enchoria 12, 57-62 "
;     .byte "s. p. vleemhg (1993), papyrus reinhardt: an egyptian land listfrom the tenth century "
;     .byte "s. p. vleeming (1994), ostraka varia (plbat 26) "
;     .byte "w. l. westermann (1917), 'land reclamation in the fayyum under ptolemy philadelphus "
;     .byte "t.' j. wright (1976), 'amos and the \"sycamore fig\",' vetus testamentum 26, 362-8 "
;     .byte "of agriculture in t "

     




