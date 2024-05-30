
; == ZEROPAGE ==
FRAME_COUNT     := $02

WIDE_CHAR_BUF   := $03  ; buffer for wide characters in the scroller
WIGGLE_INDEX    := $04

IRQ_LINE        := $05
SCROLL_Y        := $06          ; how many lines to stall in scrolled section
SCROLL_STALL    := $07          ; counter for scroll stall
SCROLLERX       := $08
SCROLL_INDEX    := $09          ; index of scrolled text

STALL_DONE      := $10  ; callback after stall routine
STALL_DONE_LO   := $10
STALL_DONE_HI   := $11
; == VIC-II ==
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

; == SID - SOUND INTERFACE DEVICE ==

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

; == CIA - COMPLEX INTERFACE ADAPTER ==

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


; == CONSTANTS ==
TOP_SCROLL      := $03  ; scroll value to use for the top 3rd of the screen
SCROLL_ROW      := 9  ; text row to start scroller at
SCROLL_ROWS     := $03  ; number of rows in scroll    

                ; line to start scroller effect on - must be one line before a badline
SCROLL_START    := (SCROLL_ROW+6)*8+TOP_SCROLL-1
SCROLL_MAX      := $18  ; max value the scroll routine expects
                        ; total lines the scroll takes is SCROLL_ROWS*8+SCROLL_MAX

; == MACROS ==
.macro  irq_addr addr
        lda #<addr      ; set IRQ vector
        sta $FFFE
        lda #>addr
        sta $FFFF
.endmacro
.macro  irq_line line   ; TODO: this wastes cycles
        lda line        ; interrupt on line
        sta VIC_RASTER  ; set IRQ line
        asl VIC_IRR     ; ack IRQ 
.endmacro

; save registers before irq
.macro  irq_start
        sta irq_a+1
        stx irq_x+1
        sty irq_y+1
.endmacro

; mapping for strings to screencodes
.macro screencode
    .repeat $20, i
        .charmap $40 + i, $40 + i + $00
    .endrepeat
    .repeat $20, i
        .charmap $60 + i, $60 + i - $60
    .endrepeat
    .repeat $20, i
        .charmap $80 + i, $80 + i + $40
    .endrepeat
    .repeat $20, i
        .charmap $A0 + i, $A0 + i - $40
    .endrepeat
    .repeat $3F, i
        .charmap $C0 + i, $C0 + i - $80
    .endrepeat
    .charmap $FF, $5E
.endmacro

; create label inside repeat block
.macro makeident lname, count
    .ident(.concat(lname,.sprintf("%d", count))):
.endmacro

; == ENTRY POINT ==


init:
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
        
        ldy #$08
@chr_cpy:
        ldx #$00         ; copy character rom into $2000
@chr_c: lda font,X
@chr_s: sta $2000,X
        inx
        bne @chr_c
        inc @chr_c+2
        inc @chr_s+2
        dey
        bne @chr_cpy

        ; select custom character set
        lda VIC_VIDEO_ADR
        and #%11110000
        ora #%00001000
        sta VIC_VIDEO_ADR


        ; print hello world message
        ldy #41
@again: ldx #$00
@loop:  lda hello,X
        beq @done 
@base:  sta $0400
        inc @base+1
        bne :+
        inc @base+2
:       inx
        jmp @loop
@done:  inc four
        dey
        bne @again

        irq_addr irq1   ; set up IRQ chain
        irq_line #$00 
        cli             ; enable interrupts

spin:   jmp spin        ; wait for IRQ
        
        
hello:  
        screencode
        .byte "hello commodore 6"
four:   .byte "4!  ",0

        ; == IRQ ==

irq1:   irq_start   

        inc FRAME_COUNT ; update frame count
        lda FRAME_COUNT
        and #%111
        sta SCROLLERX   ; update X scroll
        bne noshift    ; shift every 8 frames


        ; load the next character from the scroller
.repeat SCROLL_ROWS, r
makeident "scroll", r
        ldx #$00
@scroll_shift:
        lda $0401+((SCROLL_ROW+r)*40),X
        sta $0400+((SCROLL_ROW+r)*40),X
        inx
        cpx #39
        bne @scroll_shift
.endrepeat
        lda WIDE_CHAR_BUF
        bne @scroll_write       ; if character was buffered, output it now
@scroll_load:
        lda scroll_text         ; operand is incremented as text is loaded
        bne @scroll_write
        lda #<scroll_text
        sta @scroll_load+1
        lda #>scroll_text
        sta @scroll_load+2
        jmp @scroll_load
@scroll_write:
        sta $0400+((SCROLL_ROW+1)*40),X ; write the character to the screen
        
        cmp #$0D ; 'm'          ; check for wide characters
        beq @wide_boi
        cmp #$17 ; 'w'
        beq @wide_boi
        
        tay     ; transfer to Y for ascender/descender lookup             

        lda #$00        ; not a wide character, clear buffer
        sta WIDE_CHAR_BUF 

        lda font_asc,Y  ; write font ascenders
        sta $0400+((SCROLL_ROW+0)*40),X
        lda font_des,Y  ; write font descenders
        sta $0400+((SCROLL_ROW+2)*40),X
        
        inc @scroll_load+1 ; advance to next character
        bne @scroll_done
        inc @scroll_load+2
        jmp @scroll_done
@wide_boi:      ; we got a wide character, buffer it
        ora #$40
        sta WIDE_CHAR_BUF
@scroll_done:

noshift:
        ldx WIGGLE_INDEX
        lda FRAME_COUNT 
        and #%11        ; update wiggle every 4th frame
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


        lda #<irq_stall_done_1  ; set callback
        sta STALL_DONE_LO
        lda #>irq_stall_done_1
        sta STALL_DONE_HI

        lda #SCROLL_START       ; IRQ one line before badline, line 7 of row
        sta IRQ_LINE
        sta VIC_RASTER  ; set IRQ line
        irq_addr irq_stall
        asl VIC_IRR     ; ack IRQ 
        jmp irq_done


        ; IRQ STALL
        ; ASSUMES:
        ;       SCROLL_STALL is the number of lines to stall
        ;       IRQ_LINE is set, and is the line before the next badline
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

        jmp (STALL_DONE)

irq_stall_done_1:       ; called at beginning of scroller

        lda VIC_CTRL2
        and #%11101000 ; clear MCM
        ora SCROLLERX  ; set horizontal scroll
        eor #%00000111
        sta VIC_CTRL2

        lda #<irq_stall_done_2  ; set callback
        sta STALL_DONE_LO
        lda #>irq_stall_done_2
        sta STALL_DONE_HI

        ; set next IRQ to one line before badline after scrolled rows
        lda IRQ_LINE    ;
        clc             ;
        adc #SCROLL_ROWS*8
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


irq_stall_done_2:       ; called at end of scroller
        
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
        
;== TABLES ==

sine_table:     ; sine table - range: [$00,$18]
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

scroll_text:
        screencode
        ; þ ꝥ ð ſ ß
        .byte "abcdefghijklmnopqrstuvwxyz&",$4F,$53,$55,$49,$4E
        .byte "whenever i find myself growing grim abou"
        .byte "t ",$4F,"e mou",$55,"; whenever it is a damp, driz"
        .byte "zly november in my soul; whenever i find"
        .byte " myself involuntarily pausing before cof"
        .byte "fin warehouses, and bringing up ",$4F,"e rear"
        .byte " of every funeral i meet; & especially"
        .byte " whenever my hypos get such an upper han"
        .byte "d of me, ",$53," it requires a strong moral"
        .byte " principle to prevent me from deliberate"
        .byte "ly stepping into ",$4F,"e street, and me",$4F,"odi"
        .byte "cally knocking people's hats off--",$4F,"en, "
        .byte "i account it high time to get to sea as "
        .byte "soon as i can. ",$4F,"is is my substitute for"
        .byte " pistol and ball. wi",$55," a philosophical f"
        .byte "lourish cato ",$4F,"rows himself upon his swo"
        .byte "rd; i quietly take to ",$4F,"e ship.        "
        .repeat $fe, i
        .byte i+1
        .endrep
        .byte 0


        .include "../fonts/fontmap.inc"



