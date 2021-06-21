.segment "HEADER"
  .byte $4E, $45, $53, $1A  ; iNES header identifier
  .byte 2                   ; 2x 16KB PRG code
  .byte 1                   ; 1x  8KB CHR data
  .byte $01, $00            ; mapper 0, vertical mirroring

.segment "VECTORS"
  .addr NonMaskingInterrupt
  .addr PowerOnReset
  .addr 0

.segment "STARTUP"
  ; Note: This section is required by the linker, but is unused.

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
  sei               ; Disable IRQs (unused by NES)
  cld               ; Disable Decimal Mode (unused by NES)
  ldx #%01000000    ; Disable APU's frame IRQ
  stx $4017
  ldx #$ff          ; Setup the stack
  txs
  ldx #0
  stx $2000         ; Disable the NMI
  stx $2001         ; Disable Rendering
  stx $4010         ; Disable "Delta Modulation Channel" (DMC) intterupts (IRQ)
@vblankWait1:
  bit $2002
  bpl @vblankWait1
@clearMemory:
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
@vblankWait2:
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
  ; Load Palettes
  lda $2002
  lda #$3f
  sta $2006
  lda #$00
  sta $2006
  ldx #$00
@loop:
  lda palettes, x
  sta $2007
  inx
  cpx #$20
  bne @loop
  ; Enable Rendering
  lda #%10000000	; Enable NMI
  sta $2000
  lda #%00010000	; Enable Sprites
  sta $2001
forever:
  jmp forever
.endproc

;
; The Non Masking Interrupt, or NMI, is executed once per frame during a
; vertical blank or VBLANK. This is the point when the CRT's electron gun is
; being repositioned back to the top left of the screen after a frame has been
; rendered.
;
; Games will often perform "per-frame" updates in the NMI, especially graphics
; updates, as the VBLANK is the only "safe" time to update PPU memory so as to
; avoid graphical glitches during gameplay.
;
.proc NonMaskingInterrupt
  ldx #$00 	; Set SPR-RAM address to 0
  stx $2003
@loop:	lda hello, x 	; Load the hello message into SPR-RAM
  sta $2004
  inx
  cpx #$1c
  bne @loop
  rti
.endproc


hello:
  .byte $00, $00, $00, $00
  .byte $00, $00, $00, $00
  .byte $6c, $00, $00, $6c
  .byte $6c, $01, $00, $76
  .byte $6c, $02, $00, $80
  .byte $6c, $02, $00, $8A
  .byte $6c, $03, $00, $94

palettes:
  ; Background Palette
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00

  ; Sprite Palette
  .byte $0f, $20, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00
  .byte $0f, $00, $00, $00

; Character memory
.segment "CHARS"
  .byte %11000011	; H (00)
  .byte %11000011
  .byte %11000011
  .byte %11111111
  .byte %11111111
  .byte %11000011
  .byte %11000011
  .byte %11000011
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11111111	; E (01)
  .byte %11111111
  .byte %11000000
  .byte %11111100
  .byte %11111100
  .byte %11000000
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %11000000	; L (02)
  .byte %11000000
  .byte %11000000
  .byte %11000000
  .byte %11000000
  .byte %11000000
  .byte %11111111
  .byte %11111111
  .byte $00, $00, $00, $00, $00, $00, $00, $00

  .byte %01111110	; O (03)
  .byte %11100111
  .byte %11000011
  .byte %11000011
  .byte %11000011
  .byte %11000011
  .byte %11100111
  .byte %01111110
  .byte $00, $00, $00, $00, $00, $00, $00, $00
