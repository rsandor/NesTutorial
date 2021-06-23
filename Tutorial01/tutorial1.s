.segment "HEADER"
  .byte $4E, $45, $53, $1A  ; iNES header identifier
  .byte 2                   ; 2x 16KB PRG-ROM Banks
  .byte 1                   ; 1x  8KB CHR-ROM
  .byte $01, $00            ; mapper 0, vertical mirroring

.segment "VECTORS"
  .addr NonMaskingInterrupt ; The non-masking interrupt, called every frame
  .addr PowerOnReset        ; What procedure to call on power-on and reset
  .addr 0                   ; IRQ (Unused)

.segment "STARTUP"  ; This segment is required by the linker, but is unused.

.segment "CODE"

;
; Sets the NES Memory, APU, and PPU into a known state on system start and
; soft resets. This routine is referenced by the reset vector (above) and is
; the first code to be executed upon startup.
;
; This is a common routine and you can reuse it in practically all of the games
; that you create. You may need *want* to handle some more things here such as
; mapper setup, depending on the game, but it's not required to do so.
;
.proc PowerOnReset
  sei               ; Disable IRQ
  cld               ; Disable Decimal Mode (unused by NES)
  ldx #%01000000    ; Disable APU's frame IRQ
  stx $4017
  ldx #$ff          ; Setup the stack
  txs
  ldx #0
  stx $2000         ; Disable the NMI
  stx $2001         ; Disable Rendering
  stx $4010         ; Disable "Delta Modulation Channel" (DMC) intterupts (IRQ)
  bit $2002         ; Reset the VBLANK flag
@vblankWait1:       ; Wait for a VBLANK
  bit $2002
  bpl @vblankWait1
@clearMemory:       ; Zero-out all RAM
  lda #$00
  sta $0000, x
  sta $0100, x
  sta $0200, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  inx
  bne @clearMemory
@vblankWait2:       ; Wait for one more VBLANK
  bit $2002
  bpl @vblankWait2
.endproc

;
; The main startup routine for the game. This is called directly after the
; reset routine and handles the frame-independent "loop" for the game. In this
; tutorial it simply sets up the palettes and backgrounds for the game prior
; to looping indefinitely.
;
.proc Main
  jsr LoadPalettes          ; Load the initial palettes
  jsr LoadStarfield
  lda #%10000000	          ; Enable NMI
  sta $2000
  lda #%00011000	          ; Enable Sprites & Background
  sta $2001
forever:
  jmp forever
.endproc

;
; Loads the initial palettes for the game. Right now this has a fixed behavior
; to load the values specifically at the label "palettes" in the ROM. In the
; future I'll show you how to abstract this into a reusable subroutine so that
; full palettes can be swapped in and out during gameplay.
;
.proc LoadPalettes
  ; Set the starting PPU VRAM address to the palette section ($3F00)
  bit $2002
  lda #$3f
  sta $2006
  lda #$00
  sta $2006
  ; Load each of the 8 palettes into video memory
  ldx #0
@loop:
  lda palettes, x
  sta $2007
  inx
  cpx #$20
  bne @loop
  rts
.endproc

;
; Loads the starfield nametable and attribute table into the PPU's video memory.
; This is a very specific routine, but it can pretty easily be abstracted to
; any nametable you could want. For the sake of this tutorial it will suffice,
; but I'll cover this topic more in the future.
;
.proc LoadStarfield
  ; Load the starfield address into the zero page
  lda #.LOBYTE(starfield_nametable)
  sta $00
  lda #.HIBYTE(starfield_nametable)
  sta $01
  ; Set the starting PPU VRAM address to the top left tile of the nametable
  bit $2002
  lda #$20
  sta $2006
  lda #$00
  sta $2006
  ; Transfer data from PRG-ROM into the PPU VRAM by making successive writes.
  ; Note: this will transfer the 960 bytes for the nametable along with the
  ; additional 64 bytes of attribute table data at the end of the table.
  ldx #0
@page:
  ldy #0
@loop:
  lda ($00), y
  sta $2007
  iny
  bne @loop
  inx
  inc $01
  cpx #4
  bne @page
  rts
.endproc

;
; The Non Masking Interrupt, or NMI, is executed once per frame during a
; vertical blank. The vertical blank, aka VBLANK, occurs when a CRT's electron
; gun is being repositioned back to the top left of the screen after a frame
; has been rendered.
;
; Games will often perform "per-frame" updates in the NMI, especially graphics
; updates, as the VBLANK is the only "safe" time to update PPU memory so as to
; avoid graphical glitches during gameplay.
;
; For this tutorial we will use the NMI to animate the palette colors to create
; a twinkling star effect
;
.proc NonMaskingInterrupt
  ; Perform the "twinkling star" animation effect
  jsr AnimateStarTwinkle
  ; Reset the VRAM address
  ; Note: this is generally done as a safety in case the PPU address was set to
  ; a different value in any of the routines above. If the address is not reset
  ; then the PPU will not be able to render the nametable correctly and you'll
  ; encounter graphical glitches.
  bit $2002
  lda #0
  sta $2006
  sta $2006
  rti
.endproc

;
; Routine for handling the "star twinkle" palette animation for the game. This
; basic implementation uses a table to replace the third color of the background
; palette every 16 frames. The colors have been chosen in such a way that the
; result is a twinkling effect for any background sprite that uses color 3.
;
.proc AnimateStarTwinkle
  ; Increment and check the animation timer. We limit the animation to switch
  ; the palette color only once every 16 frames to limit the speed of the
  ; the twinkling effect.
  inc $11
  lda $11
  cmp #16
  bne return
  ; Reset the animation timer
  lda #0
  sta $11
  ; Increment the color index, and reset to 0 if we reached the end of the table
  inc $10
  ldx $10
  cpx #8
  bne @skipReset
  ldx #0
  stx $10
@skipReset:
  ; Set the VRAM address to color 3 of the first background palette.
  bit $2002
  lda #$3F
  sta $2006
  lda #$03
  sta $2006
  ; Load the current color and store it into the PPU's palette VRAM
  lda twinkleColorTable, x
  sta $2007
return:
  rts
twinkleColorTable:
  ; This is the table of palette colors we cycle through to create the effect:
  .byte $0f, $00, $13, $24, $36, $24, $13, $00
.endproc

;
; The palettes for the game
;
palettes:
  ; Background Palettes
  .byte $0f, $20, $00, $0f
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00
  ; Sprite Palettes
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00


;
; This is the nametable and attribute table for the starfield that we render in
; this tutorial. Usually you wouldn't build nametables by hand like this, but it
; is nice to start with this since it's relatively easy to understand and is
; completely self contained.
;
; In future tutorials we will show how to use tools to build nametables and
; levels and include them as binary files during ROM assembly.
;
starfield_nametable:
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2
  .byte 0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0
starfield_attributes:
  .byte 0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0
  .byte 0,0,0,0,0,0,0,0


;
; CHR-ROM / Pattern Table
;
; We define the ROM's 8KB CHR-ROM pattern data here. This segment will be used
; directly by emulators for the game's pattern table. For this tutorial, this
; will work, but in the future we will show how to use dynamic pattern tables
; via the use of different mappers that support CHR-RAM emulation.
;
; The data here is also presented in ca65 macro assembly using the ".byte"
; control command. While this will work for small tutorials like this, we will
; show how to package and link against binary files built using external tools
; in the future.
;
.segment "CHARS"
  ; $00 - Blank Tile
  .byte 0, 0, 0, 0, 0, 0, 0, 0
  .byte 0, 0, 0, 0, 0, 0, 0, 0

  ; $01 - Starfield 1
  .byte %00000000
  .byte %01000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000

  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000

  ; $02 - Starfield 2
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000

  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000100
  .byte %00000000
  .byte %00000000

  ; $03 - Starfield 3
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00010000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000

  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00010000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000

  ; $04 - Starfield 4
  .byte %00000000
  .byte %00000000
  .byte %00010000
  .byte %00111000
  .byte %00010000
  .byte %00000000
  .byte %00000000
  .byte %00000000

  .byte %00000000
  .byte %00000000
  .byte %00010000
  .byte %00111000
  .byte %00010000
  .byte %00000000
  .byte %00000000
  .byte %00000000


  ; $05 - Starfield 5
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00001100
  .byte %00011100
  .byte %00011000
  .byte %00001000
  .byte %00000000

  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000000
  .byte %00000010
  .byte %00000110
  .byte %00000100
  .byte %00000000
