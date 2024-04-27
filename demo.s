; Noel Andres Vargas pad1illa 801-19-7297
PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
PPUSCROLL = $2005
PPUADDR   = $2006
PPUDATA   = $2007

OAMADDR   = $2003
OAMDATA   = $2004
OAMDMA    = $4014

sprite_buffer = $0200

CONTROLLER1 = $4016
CONTROLLER2 = $4017

collision_m_left = $0300  
collision_map_right = $0400

BTN_RIGHT   = %00000001
BTN_LEFT    = %00000010
BTN_DOWN    = %00000100
BTN_UP      = %00001000
BTN_START   = %00010000
BTN_SELECT  = %00100000
BTN_B       = %01000000
BTN_A       = %10000000

SPRITE_Y_BASE_ADDR = $00
SPRITE_TILE_BASE_ADDR = $01
SPRITE_ATTR_BASE_ADDR = $02
SPRITE_X_BASE_ADDR = $03


.segment "HEADER"
  ; .byte "NES", $1A      ; iNES header identifier
  .byte $4E, $45, $53, $1A
  .byte 2               ; 2x 16KB PRG code
  .byte 1               ; 1x  8KB CHR data
  .byte $01, $00        ; mapper 0, vertical mirroring

.segment "VECTORS"
  ;; When an NMI happens (once per frame if enabled) the label nmi:
  .addr nmi
  ;; When the processor first turns on or is reset, it will jump to the label reset:
  .addr reset
  ;; External interrupt IRQ (unused)
  .addr 0

; "nes" linker config requires a STARTUP section, even if it's empty
.segment "STARTUP"

.segment "ZEROPAGE"
; Args for prepare_sprites subroutine
render_x: .res 1
render_y: .res 1

render_tile: .res 1
oam_offset: .res 1

; Animation state for sprites
count_frames: .res 1 
animation: .res 1 
vblank_flag: .res 1 ; Flag for vblank
isMoving: .res 1
changed_direction: .res 1

offset_static_sprite: .res 1
direction: .res 1

; Args for prepare_sprites subroutine
pos_x: .res 1
pos_y: .res 1
tile_num: .res 1

pad1: .res 1

nametbl_ptr: .res 2
curr_namtable: .res 2
select_attr: .res 2
write_this_tile: .res 1
DECODED_BYTE_IDX: .res 1
decode_byte: .res 1
curr_bits: .res 1
pos_x_scroll: .res 1
pos_y_scroll: .res 1
megatiles_ptr: .res 2
need_update_nametable: .res 1
curr_stage_side: .res 1 ; 0 = left, 1 = right
curr_stage: .res 1
scroll_stage: .res 1

player_x: .res 1
player_y: .res 1
ptr_to_collision_m: .res 2
check_x_collision: .res 1
check_y_collision: .res 1
p_megatile_index: .res 1

; Main code segment for the program
.segment "CODE"

reset:
  sei		; disable IRQs
  cld		; disable decimal mode
  ldx #$40
  stx $4017	; disable APU frame IRQ
  ldx #$ff 	; Set up stack
  txs		;  .
  inx		; now X = 0
  stx PPUCTRL	; disable NMI
  stx PPUMASK 	; disable rendering
  stx $4010 	; disable DMC IRQs

;; first wait for vblank to make sure PPU is ready
vblankwait1:
  bit PPUSTATUS
  bpl vblankwait1

clear_memory:
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
  bne clear_memory
  
;; second wait for vblank, PPU is ready after this
vblankwait2:
  bit PPUSTATUS
  bpl vblankwait2

main:

; initialize 
 ldx #0
 stx offset_static_sprite
 stx direction
 stx animation
 stx count_frames
 stx isMoving
 stx vblank_flag
stx changed_direction


  ; Init collision map pointer to start at base address collision_m_left
  lda #>collision_m_left
  sta ptr_to_collision_m+1
  lda #<collision_m_left
  sta ptr_to_collision_m

  clear_oam:
    ldx #0
    loop_clear_oam:
      lda #$FF ; load byte x of sprite list
      sta OAMDATA ; 
      inx
      cpx #255
      bne loop_clear_oam

  load_palettes:
    lda PPUSTATUS
    lda #$3f
    sta PPUADDR
    lda #$00
    sta PPUADDR

    ldx #$00
    @loop:
      lda palettes, x
      sta PPUDATA
      inx
      cpx #$20
      bne @loop

    ldx #0
    stx pos_x
    ldy #144
    sty pos_y
    ldy #0

    lda pos_y
    sta render_y ; Store y position of the sprite
    lda pos_x
    sta render_x ; Store x position of the sprite
    lda left_tank_tiles, y ; Load tile number of the sprite

    sta render_tile
    jsr prepare_sprites

    ; Weird bug, PPU writes the tile in x+1, y+1, so player_x and player_y are offset by 1
    lda #0
    sta player_x
    lda #144
    sta player_y

    lda #%00010000	; Enable NMI
    sta PPUCTRL
load_nametable:
  ; Set stage to 1
  lda #1
  sta curr_stage

  ; Set current stage side to 0
  lda #0
  sta curr_stage_side

  ; Select first nametable
  lda #<stage_one_left_packaged
  sta curr_namtable
  lda #>stage_one_left_packaged
  sta curr_namtable+1

  ; Select first attribute table
  lda #<stage_one_left_attributes
  sta select_attr
  lda #>stage_one_left_attributes
  sta select_attr+1

  ; $2000 for first nametable
  lda #$20
  sta nametbl_ptr
  lda #$00
  sta nametbl_ptr+1
  jsr write_nametable

  ; $23C0 for first attribute table
  lda #$23
  sta nametbl_ptr
  lda #$C0
  sta nametbl_ptr+1
  jsr load_attributes

  ; Set current stage side to 1 (right)
  lda #1
  sta curr_stage_side

  ; Increment high byte of ptr_to_collision_m to point to the next 240 bytes (0x0400)
  ; This is because the collision map is 240 bytes long, and we cant overwrite
  ; the first 240 bytes of the collision map with the second stage collision map
  inc ptr_to_collision_m+1

  ; Select second nametable
  lda #<stage_one_right_packaged
  sta curr_namtable
  lda #>stage_one_right_packaged
  sta curr_namtable+1

  ; Select second attribute table
  lda #<stage_one_right_attributes
  sta select_attr
  lda #>stage_one_right_attributes
  sta select_attr+1

  ; $2400 for second nametable
  lda #$24
  sta nametbl_ptr
  lda #$00
  sta nametbl_ptr+1
  jsr write_nametable

  ; $27C0 for second attribute table
  lda #$27
  sta nametbl_ptr
  lda #$C0
  sta nametbl_ptr+1
  jsr load_attributes

  ; Reset current stage side to 0
  lda #0
  sta curr_stage_side
enable_rendering:

; Set PPUSCROLL to 0,0
  lda #$00
  sta PPUSCROLL
  lda #$00
  sta PPUSCROLL

  lda #%10000000	; Enable NMI
  sta PPUCTRL
  lda #%00011110
  sta PPUMASK

forever:
  lda vblank_flag
  cmp #1
  bne not_sync
    jsr handle_input
    jsr handle_nametable_change
    jsr update_player
    jsr update_sprites
  not_sync:
    jsr game_dynamics_loop
    jsr handle_coll
    jmp forever



nmi:
    pha
    txa
    pha
    tya
    pha
  ; Set vblank_flag to 1
  lda #1
  sta vblank_flag

  ; Start OAMDMA transfer
  lda #$02         
  sta OAMDMA        

  lda count_frames ; Load count_frames
  cmp #30 ; Compare count_frames to 30
  bne jmp_rst_timer ; If count_frames is not 60, skip resetting it
  lda #$00 ; Reset count_frames to 0
  sta count_frames ; Store 0 in count_frames
  jmp scroll_screen_check ; Jump to scroll_screen_check subroutine
  jmp_rst_timer: ; Skip resetting count_frames and prepare_sprites subroutine
  inc count_frames ; Increase count_frames by 1

  scroll_screen_check:
  lda scroll_stage
  cmp #0
  beq skip_scroll_screen

  ; Scroll screen right 1px and player left 1px
  lda pos_x_scroll
  clc
  adc #1
  sta pos_x_scroll
  jsr move_player_left

  skip_scroll_screen:
  lda pos_x_scroll
  sta PPUSCROLL
  lda pos_y_scroll
  sta PPUSCROLL

  ; Pop registers from stack
  pla
  tay
  pla
  tax
  pla

  rti

prepare_sprites:
    pha
    txa
    pha
    tya
    pha
  
    jsr store_in_sprite_buffer  

    ; Render second tile of the sprite
    lda render_x
    clc
    adc #$08
    sta render_x ; x = x + 8
    lda render_tile
    clc
    adc #$01 ; Load next tile of the sprite
    sta render_tile
    jsr store_in_sprite_buffer  

    ; Render third tile of the sprite
    lda render_y
    clc
    adc #$08
    sta render_y ; y = y + 8

    lda render_tile
    clc
    adc #$10
    sta render_tile
    jsr store_in_sprite_buffer  

    ; Render fourth tile of the sprite
    ; Only update x to move left by 8 pixels
    lda render_x
    sbc #8 
    tay
    iny 
    sty render_x ; x = x - 8

    ldy render_tile 
    dey
    sty render_tile
    jsr store_in_sprite_buffer  

    pla
    tay
    pla
    tax
    pla
    
    RTS
; Render a single tile of the sprite
store_in_sprite_buffer:
  ; store in stack
  pha
  txa
  pha
  tya
  pha
  
    ldx oam_offset ; Offset for OAM buffer

    lda render_y
    sta sprite_buffer, x ; Store y position of the sprite
    inx

    lda render_tile
    sta sprite_buffer, x
    inx

    lda #$20
    sta sprite_buffer, x
    inx

    lda render_x
    sta sprite_buffer, x
    inx

    stx oam_offset ; Update oam_offset to the next available OAM buffer index`
    
  ; pop from stack
  pla
  tay
  pla
  tax
  pla
    rts
handle_input:
    ; No input read if scroll_stage is not 0
    lda scroll_stage
    cmp #0
    beq input_read
    rts

    input_read:
    ; Save registers to stack
    pha
    txa
    pha
    tya
    pha

    lda #$01
    sta CONTROLLER1  ; Latch the controller state
    lda #$00
    sta CONTROLLER1  ; Complete the latch process

    lda #$00
    sta pad1    ; Initialize 'pad' to 0

    ldx #$08   ; Prepare to read 8 buttons

    read_button_loop:
        lda CONTROLLER1       ; Read a button state
        lsr             ; Shift right, moving the button state into the carry
        rol pad1         ; Rotate left through carry, moving the carry into 'pad'
        dex             ; Decrement the count
        bne read_button_loop  ; Continue until all 8 buttons are read

    ; Pop registers from stack
    pla
    tay
    pla
    tax
    pla
    rts
update_sprites:
    ; Save registers to stack
    pha
    txa
    pha
    tya
    pha
    ; Exit subroutine if count_frames is not 29
    lda count_frames
    cmp #29
    bne skip_update_sprites

    ; Dont update sprites if vblank_flag is not set
    lda vblank_flag
    cmp #1
    bne skip_update_sprites

    lda isMoving
    cmp #0
    beq reset_state_animation
    jmp skip_reset_animation
    reset_state_animation:
    ; Reset animation 
    lda #$00
    sta animation
    jmp skip_update_sprites

    skip_reset_animation:
    ; Update animation state
    lda animation
    clc
    adc #1
    sta animation

    cmp #2 ; Check if animation is at the last frame
    bcc animate
    lda #0
    sta animation
    jsr NOAnimated_sprite
    jmp skip_update_sprites
    

  animate:
    ldx #1 ; offset for buffer, where the tile data for tile 1 is stored
    ldy #0
    update_sprites_loop:
    lda sprite_buffer, x ; Load tile data for tile y
    clc
    adc #2 ; Add 2 to the tile data to change the sprite to the next frame
    sta sprite_buffer, x ; Store the updated tile data back to the buffer

    txa ; Load x to a
    clc
    adc #4 ; Add 4 to x to move to the next tile data
    tax ; Store the updated x back to x
    iny ; Increase y by 1
    cpy #16
    bne update_sprites_loop ; If y is not 16, loop back to update_sprites_loop, since we have not updated all sprites
    inc animation

    skip_update_sprites:
    lda #$00 ; Reset vblank_flag
    sta vblank_flag
    ; pop from stack
    pla
    tay
    pla
    tax
    pla
    rts
; Loads, decodes and writes a nametable at  
; from a packaged nametable in ROM
write_nametable:

  ; Save registers to stack
  pha
  txa
  pha
  tya
  pha

  ; Based on curr_stage, select the correct megatiles
  lda curr_stage
  cmp #1
  ; If stage 1, load stage one megatiles
  beq get_cave_megatiles
  cmp #2
  beq get_netherrealm_tiles
  
  ;choose the correct megatiles
  get_cave_megatiles:
      ; Load the megatiles for the cave
      lda #<cave_megatiles
      ; Load the low byte of the address of the megatiles
      sta megatiles_ptr
      ; Load the high byte of the address of the megatiles
      lda #>cave_megatiles
      sta megatiles_ptr+1
      ; Jump to the dec_write_nmtable subroutine
      jmp dec_write_nmtable
  
  get_netherrealm_tiles:
      lda #<nether_megatiles
      sta megatiles_ptr
      lda #>nether_megatiles
      sta megatiles_ptr+1
      ; jmp dec_write_nmtable

  dec_write_nmtable:
  ldx #0
  loop_read_nmtable:
      txa
      tay
      lda (curr_namtable), y
      sta decode_byte
      jsr dec_write_byte

      txa ; Load x to a
      clc ; Clear carry
      adc #1 ; Add 1 to x
      and #%00000011 ; Mask x to 0-3
      ; If x is 0, increment nametbl_ptr
      beq increment_nametbl_ptr ; If x is 0, increment nametbl_ptr
      jmp skip_increment_nametbl_ptr

      increment_nametbl_ptr:
          lda nametbl_ptr+1
          clc
          adc #32
          sta nametbl_ptr+1
      
          ; Check if carry, need to increment high byte
          bcc skip_increment_nametbl_ptr
          inc nametbl_ptr
      
      skip_increment_nametbl_ptr:
          inx 
          cpx #60
          bne loop_read_nmtable
  
    ; Done with subroutine, pop registers from stack
    ; Reset ptr_to_collision_m to base address collision_m_left
    lda #>collision_m_left
    sta ptr_to_collision_m+1
    lda #<collision_m_left
    sta ptr_to_collision_m
  pla
  tay
  pla
  tax
  pla

  rts
; Decodes a byte and writes the corresponding 2x2 region of the nametable
dec_write_byte:

    pha
    txa
    pha
    tya
    pha

    ; Loop through 2-bit pairs of the byte
    ; Each 2-bit pair corresponds to the top left tile of a 2x2 megatile, 
    ; can be used to index megatile array
    ldx #0
    read_bits_loop:
        lda #$00
        ; we use this to read 2 bits at a time
        sta curr_bits ; Clear curr_bits
        
        lda decode_byte ; Load byte to decode
        clc
        asl ; Shift to read 1 bit into carry
        rol curr_bits ; Rotate carry into curr_bits
        asl 
        rol curr_bits 
        sta decode_byte ; Save byte back to decode_byte

        ldy curr_bits ; Save the 2-bit pair to X register
        lda (megatiles_ptr), y ; Load tile from megatiles based on 2-bit pair
        sta write_this_tile 
        ; Otherwise, set to 0
        lda write_this_tile
        cmp #$04
        beq set_one_collision_m
        cmp #$24
        beq set_one_collision_m
        cmp #$06
        beq set_one_collision_m
        cmp #$26
        beq set_one_collision_m
        jmp set_zero_collision_m

        set_one_collision_m:
            lda #1
            ldy #0
        sta (ptr_to_collision_m), y
        jmp skip_set_zero_collision_m
        set_zero_collision_m:
            lda #0
            ldy #0
            sta (ptr_to_collision_m), y

        skip_set_zero_collision_m:
        inc ptr_to_collision_m

        ; From write_this_tile, call write_region_2x2_nametable 
        ; based on the top left tile of the mega tile 
        jsr write_2x2

        ; Move nametbl_ptr to next 2x2 region
        lda nametbl_ptr+1
        clc
        adc #2
        sta nametbl_ptr+1

        ; Increment x to move to next 2-bit pair
        inx
        cpx #4
        bne read_bits_loop
    pla
    tay
    pla
    tax
    pla

    rts

; Writes a 2x2 region 
write_2x2:

    pha
    txa
    pha
    tya
    pha

    ; Write first tile of 2x2 region
    lda nametbl_ptr
    sta PPUADDR
    lda nametbl_ptr+1
    sta PPUADDR
    lda write_this_tile
    sta PPUDATA

    ; Write second tile of 2x2 region
    lda nametbl_ptr
    sta PPUADDR
    lda nametbl_ptr+1
    clc
    adc #1
    sta PPUADDR
    lda write_this_tile
    clc
    adc #1
    sta PPUDATA

    ; Write third tile of 2x2 region
    lda nametbl_ptr
    sta PPUADDR
    lda nametbl_ptr+1
    clc
    adc #32
    sta PPUADDR
    lda write_this_tile
    clc
    adc #16
    sta PPUDATA

    ; Write fourth tile of 2x2 region
    lda nametbl_ptr
    sta PPUADDR
    lda nametbl_ptr+1
    clc
    adc #33
    sta PPUADDR
    lda write_this_tile
    clc
    adc #17
    sta PPUDATA

    ; Pop registers from stack
    pla
    tay
    pla
    tax
    pla

    rts
; Writes attributes to  from attributes in ROM
load_attributes:
  ; Save registers to stack
  pha
  txa
  pha
  tya
  pha

  ldx #0
  read_attribute_loop:
      txa
      tay
      lda (select_attr), y
      sta PPUDATA
      inx
      cpx #64
      bne read_attribute_loop
  ; Done writing attributes

  ; Pop registers from stack
  pla
  tay
  pla
  tax
  pla

  rts
handle_nametable_change:
    ; Save registers to stack
    pha
    txa
    pha
    tya
    pha

    ; If A was not pressed, skip to end
    lda pad1
    and #BTN_A
    beq dont_change_nametable

    ; Disable disable NMI and screen
    lda PPUCTRL
    and #%01111111
    sta PPUCTRL
    lda PPUMASK
    and #%11100000
    sta PPUMASK

    vblankwait3:
        bit PPUSTATUS
        bpl vblankwait3


    ; If in stage one, set to stage two and vice versa
    lda curr_stage
    cmp #1
    beq set_cave
    cmp #2
    beq set_nether

    set_cave:
        lda #1
        sta need_update_nametable
        lda #2
        sta curr_stage
        jmp update_nmtable_helper
    
    set_nether:
        lda #1
        sta need_update_nametable
        lda #1
        sta curr_stage
        jmp update_nmtable_helper
    
    update_nmtable_helper:
        ; Set scroll position to 0,0
        lda #$00
        sta pos_x_scroll
        sta pos_y_scroll
        jsr update_nametable

    dont_change_nametable:

    ; Restore NMI and screen
    lda #%10010000
    sta PPUCTRL
    lda #$1e
    sta PPUMASK

    ; Pop registers from stack
    pla
    tay
    pla
    tax
    pla
    
    rts

update_nametable:
    ; Save registers to stack
    pha
    txa
    pha
    tya
    pha

    ; Check if need_update_nametable is set
    lda need_update_nametable
    cmp #1
    bne skip_update_nametable_intermediate

    ; Select nametable based on curr_stage
    lda curr_stage
    cmp #1
    beq select_stage_one

    lda curr_stage
    cmp #2
    beq select_stage_two

    select_stage_one:
        ; Load stage one left nametables
        lda #<stage_one_left_packaged
        sta curr_namtable
        lda #>stage_one_left_packaged
        sta curr_namtable+1

        ; Set current stage side to 0 (left)
        lda #0
        sta curr_stage_side
        jsr update_collision_ptr


        lda #$20
        sta nametbl_ptr
        lda #$00
        sta nametbl_ptr+1
        jsr write_nametable

        ; Load stage one left attributes
        lda #<stage_one_left_attributes
        sta select_attr
        lda #>stage_one_left_attributes
        sta select_attr+1

        lda #$23
        sta nametbl_ptr
        lda #$C0
        sta nametbl_ptr+1
        jsr load_attributes

        ; Load stage one right nametables
        lda #<stage_one_right_packaged
        sta curr_namtable
        lda #>stage_one_right_packaged
        sta curr_namtable+1

        ; Set current stage side to 1 (right)
        lda #1
        sta curr_stage_side
        jsr update_collision_ptr

        lda #$24
        sta nametbl_ptr
        lda #$00
        sta nametbl_ptr+1
        jsr write_nametable

        ; Load stage one right attributes
        lda #<stage_one_right_attributes
        sta select_attr
        lda #>stage_one_right_attributes
        sta select_attr+1

        lda #$27
        sta nametbl_ptr
        lda #$C0
        sta nametbl_ptr+1
        jsr load_attributes

        jmp skip_update_nametable


    skip_update_nametable_intermediate:
        jmp skip_update_nametable
    
    select_stage_two:
        ; Load stage two left nametables
        lda #<stage_two_left_packaged
        sta curr_namtable
        lda #>stage_two_left_packaged
        sta curr_namtable+1

        lda #$20
        sta nametbl_ptr
        lda #$00
        sta nametbl_ptr+1
        jsr write_nametable

        ; Load stage two left attributes
        lda #<stage_two_left_attributes
        sta select_attr
        lda #>stage_two_left_attributes
        sta select_attr+1

        ; Set current stage side to 0 (left)
        lda #0
        sta curr_stage_side
        jsr update_collision_ptr

        lda #$23
        sta nametbl_ptr
        lda #$C0
        sta nametbl_ptr+1
        jsr load_attributes

        ; Load stage two right nametables
        lda #<stage_two_right_packaged
        sta curr_namtable
        lda #>stage_two_right_packaged
        sta curr_namtable+1

        ; Set current stage side to 1 (right)
        lda #1
        sta curr_stage_side
        jsr update_collision_ptr

        lda #$24
        sta nametbl_ptr
        lda #$00
        sta nametbl_ptr+1
        jsr write_nametable

        ; Load stage two right attributes
        lda #<stage_two_right_attributes
        sta select_attr
        lda #>stage_two_right_attributes
        sta select_attr+1

        lda #$27
        sta nametbl_ptr
        lda #$C0
        sta nametbl_ptr+1
        jsr load_attributes

        jmp skip_update_nametable

    skip_update_nametable:
    ; Set need_update_nametable to 0
    lda #0
    sta need_update_nametable

    ; Pop registers from stack
    pla
    tay
    pla
    tax
    pla
    rts

handle_coll:
    ; Ignore collision check if scroll_stage is 1
    lda scroll_stage
    cmp #1
    bne collision_check
    rts

    collision_check:
    ; Save registers to stack
    pha
    txa
    pha
    tya
    pha

    ; Depending on direction, collision check will be different
    ; If player is moving up, check (x, y) and (x+15, y)
    lda direction
    cmp #0
    beq check_up_collision
    cmp #1
    beq check_down_collision
    cmp #2
    beq check_left_collision
    cmp #3
    beq check_right_collission_intermediate

    check_up_collision:
        ; Check top left boundary of player (x, y)
        lda player_x
        sta check_x_collision
        lda player_y
        sta check_y_collision
        jsr coord_to_megatile

        ldy p_megatile_index
        lda (ptr_to_collision_m), y
        cmp #1
        bne continue_check_up_collision
        jsr move_player_down
        jmp halt_collision_check
        
        continue_check_up_collision:
        ; Check top right boundary of player (x+15, y)
        lda player_x
        clc
        adc #15
        sta check_x_collision
        lda player_y
        sta check_y_collision
        jsr coord_to_megatile

        ldy p_megatile_index
        lda (ptr_to_collision_m), y
        cmp #1
        bne halt_collision_check
        jsr move_player_down
        jmp halt_collision_check
    
    check_down_collision:
        ; Check bottom left boundary of player (x, y+15)
        lda player_x
        sta check_x_collision
        lda player_y
        clc
        adc #15
        sta check_y_collision
        jsr coord_to_megatile

        ldy p_megatile_index
        lda (ptr_to_collision_m), y
        cmp #1
        bne continue_check_down_collision
        jsr move_player_up
        jmp halt_collision_check

        continue_check_down_collision:
        ; Check bottom right boundary of player (x+15, y+15)
        lda player_x
        clc
        adc #15
        sta check_x_collision
        lda player_y
        clc
        adc #15
        sta check_y_collision
        jsr coord_to_megatile

        ldy p_megatile_index
        lda (ptr_to_collision_m), y
        cmp #1
        bne halt_collision_check
        jsr move_player_up
        jmp halt_collision_check
    
    halt_collision_check:
        jmp end_collision_check

    check_right_collission_intermediate:
        jmp check_right_collision
    
    check_left_collision:
        ; Check top left boundary of player (x, y)
        lda player_x
        sta check_x_collision
        lda player_y
        sta check_y_collision
        jsr coord_to_megatile

        ldy p_megatile_index
        lda (ptr_to_collision_m), y
        cmp #1
        bne continue_check_left_collision
        jsr move_player_right
        jmp end_collision_check

        continue_check_left_collision:
        ; Check bottom left boundary of player (x, y+15)
        lda player_x
        sta check_x_collision
        lda player_y
        clc
        adc #15
        sta check_y_collision
        jsr coord_to_megatile

        ldy p_megatile_index
        lda (ptr_to_collision_m), y
        cmp #1
        bne end_collision_check
        jsr move_player_right
        jmp end_collision_check

    check_right_collision:
        ; Check top right boundary of player (x+15, y)
        lda player_x
        clc
        adc #15
        sta check_x_collision
        lda player_y
        sta check_y_collision
        jsr coord_to_megatile

        ldy p_megatile_index
        lda (ptr_to_collision_m), y
        cmp #1
        bne continue_check_right_collision
        jsr move_player_left
        jmp end_collision_check

        continue_check_right_collision:
        ; Check bottom right boundary of player (x+15, y+15)
        lda player_x
        clc
        adc #15
        sta check_x_collision
        lda player_y
        clc
        adc #15
        sta check_y_collision
        jsr coord_to_megatile

        ldy p_megatile_index
        lda (ptr_to_collision_m), y
        cmp #1
        bne end_collision_check
        jsr move_player_left
        jmp end_collision_check

    end_collision_check:
    pla
    tay
    pla
    tax
    pla

    rts

coord_to_megatile:
    ; Will convert player's x and y to megatile index 
    ; in the nametable and replace the megatile with a different one

    ; Save registers to stack
    pha
    txa
    pha
    tya
    pha

    ; Calulate player's x and y to megatile index using formula (y/16)*16 + (x/16)
    lda check_x_collision
    lsr
    lsr
    lsr
    lsr
    sta p_megatile_index

    ; Divide player_y by 16 to get the y megatile index
    lda check_y_collision
    lsr
    lsr
    lsr
    lsr

    ; Multiply y megatile index by 16
    asl
    asl
    asl
    asl

    ; Add x megatile index to y megatile index
    clc
    adc p_megatile_index
    sta p_megatile_index

    ; Pop registers from stack
    pla
    tay
    pla
    tax
    pla

    rts

game_dynamics_loop:
    ; Save registers to stack
    pha
    txa
    pha
    tya
    pha

    jsr update_collision_ptr ; update collision pointer to see where we which side we are at
    
    continue_game_loop:
    ; If scroll_stage == 1 and pos_x_scroll == 255, stop scrolling, means we are at the end of the stage
    lda scroll_stage
    cmp #1
    bne check_if_need_scroll
    lda pos_x_scroll
    cmp #255
    bne skip_side_change

    ; Stop scrolling since pos_x_scroll == 255
    lda #0
    sta scroll_stage

    ; Move player 16 pixels to the right
    ldx #0
    move_p_16_right:
        jsr move_player_right
        inx
        cpx #16
        bne move_p_16_right

    jmp skip_side_change

    check_if_need_scroll:
    ; If player reached end of stage one left, change current_side
    lda player_x
    cmp #248
    bne skip_side_change
    lda player_y
    cmp #160
    bne skip_side_change
    lda #1
    sta curr_stage_side

    ; Start transition to scroll to the right
    lda #1
    sta scroll_stage

    skip_side_change:
    ; Pop registers from stack
    pla
    tay
    pla
    tax
    pla

    rts

update_collision_ptr:
    pha
    txa
    pha
    tya
    pha

    ; Set ptr_to_collision_m based on current side
    lda curr_stage_side
    cmp #0
    beq set_collision_m_left
    cmp #1
    beq set_collision_map_right

    set_collision_m_left:
        lda #<collision_m_left
        sta ptr_to_collision_m
        lda #>collision_m_left
        sta ptr_to_collision_m+1
        jmp end_update_collision_ptr
    
    set_collision_map_right:
        lda #<collision_map_right
        sta ptr_to_collision_m
        lda #>collision_map_right
        sta ptr_to_collision_m+1
    
    end_update_collision_ptr:
    pla
    tay
    pla
    tax
    pla

    rts

update_player:
  ; Disable player movement if scroll_stage is not 0
  lda scroll_stage
  cmp #0
  beq continue
  rts
  continue:
    pha
    txa
    pha
    tya
    pha
    ; Assume no movement initially
    lda #0
    sta isMoving

    ; Check each direction
    lda pad1
    and #BTN_UP
    beq check_down  ; If not pressed, check next button
    lda #0          ; Direction for up
    sta direction
    lda #1          ; Indicate walking
    sta isMoving
    jsr move_player_up
    jmp end_update ; Skip further checks

    check_down:
    lda pad1
    and #BTN_DOWN
    beq check_left
    lda #1
    sta direction
    lda #1
    sta isMoving
    jsr move_player_down
    jmp end_update

    check_left:
    lda pad1
    and #BTN_LEFT
    beq check_right
    lda #2
    sta direction
    lda #1
    sta isMoving
    jsr move_player_left
    jmp end_update

    check_right:
    lda pad1
    and #BTN_RIGHT
    beq end_update
    lda #3
    sta direction
    lda #1
    jsr move_player_right
    sta isMoving


    end_update:
    lda direction
    cmp changed_direction ; Check if the direction has changed
    beq no_change_direction ; If the direction has not changed, skip changing the sprite
    lda direction 
    sta changed_direction ; Update changed_direction to the new direction
    jsr NOAnimated_sprite 
    no_change_direction:

    pla
    tay
    pla
    tax
    pla
    rts

NOAnimated_sprite:
  ; Get the offset for the sprite
  pha
  txa
  pha
  tya
  pha

  jsr get_offset_for_direction_sprite

    ldx #1 ; offset for buffer, where the tile data for tile 1 is stored
    ldy #0 ; offset for left_tank_tiles and 4 count
    reset_sprites_loop:
    tya ; Load y to a
    pha ; Push y to the stack

    ldy offset_static_sprite ; Load offset_static_sprite to x
    lda left_tank_tiles, y ; Load tile data for tile y
    sta sprite_buffer, x ; Store the tile data in the buffer
    
    lda offset_static_sprite ; Load offset_static_sprite to a
    clc
    adc #1
    sta offset_static_sprite ; Store the updated offset_static_sprite back to offset_static_spri
    pla
    tay
    ; ; pop in stack variables
    txa ; Load x to a
    clc
    adc #4 ; Add 4 to x to move to the next tile data
    tax ; Store the updated x back to x
    
    iny
    cpy #4 ; Check if y is 4
    bne reset_sprites_loop

  pla
  tay
  pla
  tax
  pla
  rts

get_offset_for_direction_sprite:

  pha
  txa
  pha
  tya
  pha
  ; i will traverse through left_tank_tiles to get the offset of the sprite
  LDA direction     
  CMP #3         ; Compare offset_static_sprite with 3
  BEQ SetValue3  
  CMP #2
  BEQ SetValue2  ; If offset_static_sprite is 2, branch to code that sets Y to the desired value for this case
  CMP #1
  BEQ SetValue1 

  ; If none of the above, we assume offset_static_sprite is 0 and fall through to SetValue0
  SetValue0:
      LDA #0         ; Set offset_static_sprite to the value corresponding to offset_static_sprite being 0
      STA offset_static_sprite
      JMP Continue   ; Jump to the rest of the code
  SetValue1:
      LDA #4       ; Set offset_static_sprite to the value corresponding to offset_static_sprite being 1
      STA offset_static_sprite
      JMP Continue
  SetValue2:
      LDA #8        ; Set offset_static_sprite to the value corresponding to offset_static_sprite being 2
      STA offset_static_sprite
      JMP Continue
  SetValue3:
      LDA #12         
      STA offset_static_sprite
      ; here
  Continue:
  ; Pop registers from stack
  PLA
  TAY
  PLA
  TAX
  PLA
  RTS

move_player_up:

    pha
    txa
    pha
    tya
    pha

    ldx #SPRITE_Y_BASE_ADDR
    ldy #0
    move_player_up_loop:
        lda sprite_buffer, x
        sec
        sbc #1
        sta sprite_buffer, x
        txa
        clc
        adc #4
        tax
        iny
        cpy #4
        bne move_player_up_loop
    ; Update player's y position
    lda player_y
    sec
    sbc #1
    sta player_y

    pla
    tay
    pla
    tax
    pla
    rts


move_player_down:
    ; Save registers to stack
    pha
    txa
    pha
    tya
    pha
    ldx #SPRITE_Y_BASE_ADDR
    ldy #0
    move_player_down_loop:
        lda sprite_buffer, x
        clc
        adc #1
        sta sprite_buffer, x
        txa
        clc
        adc #4
        tax
        iny
        cpy #4
        bne move_player_down_loop

    lda player_y
    clc
    adc #1
    sta player_y

    pla
    tay
    pla
    tax
    pla
    rts

move_player_left:
    pha
    txa
    pha
    tya
    pha

    ldx #SPRITE_X_BASE_ADDR
    ldy #0
    move_player_left_loop:
        lda sprite_buffer, x
        sec
        sbc #1
        sta sprite_buffer, x
        txa
        clc
        adc #4
        tax
        iny
        cpy #4
        bne move_player_left_loop
    
    lda player_x
    sec
    sbc #1
    sta player_x

    pla
    tay
    pla
    tax
    pla

    rts

move_player_right:
    pha
    txa
    pha
    tya
    pha

    ldx #SPRITE_X_BASE_ADDR
    ldy #0
    move_player_right_loop:
        lda sprite_buffer, x
        clc
        adc #1
        sta sprite_buffer, x
        txa
        clc
        adc #4
        tax
        iny
        cpy #4
        bne move_player_right_loop
    lda player_x
    clc
    adc #1
    sta player_x

  ; Pop registers from stack
    pla
    tay
    pla
    tax
    pla
    rts


palettes:
; background palette
.byte $00, $21,$00,$10 
.byte $00, $20,$00,$10 
.byte $00, $01,$12,$10
.byte $00, $15,$06,$10

; sprite palette
.byte $0F, $1C, $2C, $1A
.byte $00, $00, $00, $00
.byte $00, $00, $00, $00
.byte $00, $00, $00, $00

sprites:
.byte $00, $02, $00, $00
.byte $00, $03, $00, $08
.byte $08, $12, $00, $00
.byte $08, $13, $00, $08

; Megatiles
cave_megatiles:
;  spiderweb, diamondB, brick, nothing
.byte $02, $04, $06, $08 
nether_megatiles:
; fence, water, nether
.byte $22, $24, $26, $08

left_tank_tiles:
      ; 0   1     2   3     4   5     6    7   8     9   A   B     C    D     E   F
.byte $02, $03, $13, $12, $06, $07, $17, $16, $22, $23, $33, $32, $26, $27, $37, $36

integers:
.byte $40, $41, $42, $43, $44, $45, $46, $47, $48, $49

; Stage one nametables and attributes
stage_one_left_packaged:
.incbin "/components/nametable/stage_one_left_packaged.bin"
stage_one_left_attributes:
.incbin "/components/nametable/stage_one_left_attributes.bin"
stage_one_right_packaged:
.incbin "/components/nametable/stage_one_right_packaged.bin"
stage_one_right_attributes:
.incbin "/components/nametable/stage_one_right_attributes.bin"

; Stage two nametables and attributes
stage_two_left_packaged:
.incbin "/components/nametable/stage_two_left_packaged.bin"
stage_two_left_attributes:
.incbin "/components/nametable/stage_two_left_attributes.bin"
stage_two_right_packaged:
.incbin "/components/nametable/stage_two_right_packaged.bin"
stage_two_right_attributes:
.incbin "/components/nametable/stage_two_right_attributes.bin"

; Character memory
.segment "CHARS"
.incbin "Cave.chr"
