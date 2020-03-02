


; PROJETO DE PROCESSADORES - ELC 1094 - PROF. CARARA
; PROCESSADOR R8
; CARLOS GEWEHR E EMILIO FERREIRA

; DESCRIÇÃO:
; PROCESSADOR R8 COM SUPORTE A INTERRUPÇÕES DE I/O VIA PIC E TRAPS

; APLICAÇÃO ATUAL:
; CARGA DE CODIGO EM MEMORIA RAM POR RECEPTOR SERIAL

; CHANGELOG:
;

; TODO:
;

; OBSERVAÇÕES:
;   - Respeitar o padrão de registradores estabelecidos
;   - Novas adições ao código deve ser o mais modular possível
;   - Subrotinas importantes devem começar com letra maiuscula
;   - Subrotinas auxiliares devem começar com letra minuscula e serem identadas com 2 espaços
;   - Instruções devem ser identadas com 4 espaços

; REGISTRADORES:
; --------------------- r0  = 0
; --------------------- r1  = SYSCALL ID
; --------------------- r2  = PARAMETRO para subrotina
; --------------------- r3  = PARAMETRO para subrotina
; --------------------- r14 = Retorno de subrotina
; --------------------- r15 = Retorno de subrotina

;////////////////////////////////////////////////////////////////////////////////////////////////////////////

; irq[7] = port_irq[15]
; irq[6] = port_irq[14]
; irq[5] = port_irq[13]
; irq[4] = port_irq[12]
; irq[3] = OPEN
; irq[2] = OPEN
; irq[1] = UART RX
; irq[0] = OPEN

; port_io[15] = OPEN
; port_io[14] = OPEN
; port_io[13] = OPEN
; port_io[12] = OPEN
; port_io[11] = OPEN
; port_io[10] = OPEN
; port_io[9] = OPEN
; port_io[8] = OPEN
; port_io[7] = OPEN
; port_io[6] = OPEN
; port_io[5] = OPEN
; port_io[4] = OPEN
; port_io[3] = OPEN
; port_io[2] = OPEN
; port_io[1] = OPEN
; port_io[0] = OPEN

.org #0000h

.code

;-----------------------------------------------------BOOT---------------------------------------------------

    xor r0, r0, r0
    xor r4, r4, r4

;   Seta a Mascara do vetor de interrupções (Desabilita todas)
    ldl r4, #02h        ; Atualiza o indexador para carregar a mascara em arrayPIC

    ldh r7, #arrayPIC   ; Carrega endereço da mascara de interrupções
    ldl r7, #arrayPIC   ; 
    ld r7, r4, r7       ; &mask

    ldh r8, #00h
    ldl r8, #00h        ; Carrega a Mascara para o PIC [ r8 <= "0000_0000_0000_0000"]

    st r8, r0, r7       ; arrayPIC [MASK] <= "xxxx_xxxx_0000_0010"

; Array de registradores do controlador de interrupções
; arrayPIC [ IrqID(0x80F0) | IntACK(0x80F1) | Mask(0x80F2) ]
;arrayPIC:                 db #80F0h, #80F1h, #80F2h

;   r1 <= &arrayPorta
    ldh r1, #arrayPorta ; Carrega &Porta
    ldl r1, #arrayPorta ; Carrega &Porta
    ld r1, r0, r1

    xor r4, r4, r4

;   Seta todos os bits de PortConfig como entrada
    ldl r4, #01h   ; Atualiza indexador de arrayPorta [ arrayPorta[r4] -> &PortConfig ]
    ldh r5, #FFh   ; r5 <= "11111111_11111111"
    ldl r5, #FFh   ; Seta todos os bits de PortConfig como entrada
    st r5, r1, r4  ; PortConfig <= "11111111_11111111"

;   Desabilita interrupções
    ldl r4, #03h   ; Atualiza indexador de arrayPorta [ arrayPorta[r4] -> &irqtEnable ]
    ldh r5, #00h   ; r5 <= "00000000_00000000"
    ldl r5, #00h   ; Desabilita todas interrupções
    st r5, r1, r4  ; irqtEnable <= "00000000_00000000"

;   Desabilita portas
    ldl r4, #02h   ; Atualiza indexador de arrayPorta [ arrayPorta[r4] -> &PortEnable ]
    ldh r5, #00h   ; r5 <= "00000000_00000000"
    ldl r5, #00h   ; Desabilita acesso a todos os bits da porta de I/O
    st r5, r1, r4  ; PortEnable <= "00000000_00000000"

;   Seta RATE_FREQ_BAUD = 869 (0x364) (57600 baud @ 50 MHz)
    ldh r1, #arrayUART_RX
    ldl r1, #arrayUART_RX
    addi r1, #1
    ld r1, r0, r1
    ldh r5, #03h
    ldl r5, #64h
    st r5, r0, r1
    
;   Seta RATE_FREQ_BAUD = 869 (0x364) (57600 baud @ 50 MHz)
    ldh r1, #arrayUART_TX
    ldl r1, #arrayUART_TX
    addi r1, #1
    ld r1, r0, r1
    ldh r5, #03h
    ldl r5, #64h
    st r5, r0, r1

;   Inicializa registradores

;   r6 <= &intACK    
    ldh r1, #arrayPIC
    ldl r1, #arrayPIC
    addi r1, #1
    ld r6, r0, r1
    
;   r1 <= &irqREG
    ldh r1, #arrayPIC
    ldl r1, #arrayPIC
    addi r1, #3
    ld r1, r0, r1
    
;   Line and Offset start @ 0 (first RAM position, save new byte on higher part)
    xor r0, r0, r0
    xor r2, r2, r2
    xor r3, r3, r3
    
;   r7 <= "xxxx_xxxx_0000_0010" (irq(1) mask)
    ldh r7, #0
    ldl r7, #2
    
;   r8 <= "0000_0000_1111_1111" (mask for erasing higher part)
    ldh r8, #0
    ldl r8, #FFh
    
;   r10 <= 32767 (last memory position)
    ldh r10, #7Fh
    ldl r10, #FFh

    jmpd #main

; END BOOT
;____________________________________________________________________________________________________________


;------------------------------------------- PROGRAMA PRINCIPAL ---------------------------------------------

main:

UartRxDataAVPollingLoop:

;   r5 <= irqREG
    ld r5, r0, r1
    and r5, r7, r5

;   if irq(1) = 1 (UART RX DATA AV), transfer UART RX data to RAM, else, waits for data to be available
    jmpzd #UartRxDataAVPollingLoop
    jmpd #TransferByte

TransferByte:
; Register Table:
; r1 = &irqREG
; r2 = Current RAM position Offset (0 signals byte being transfered to be saved on 0-7, 1 signals byte to be saved on 8-15)
; r3 = Current RAM position (RAM pointer)
; r4 = Current Byte (being transfered from UART RX)
; r5 = Completed Instruction (byte from UART RX being saved on higher part of current RAM position)
; r6 = &IntACK
; r7 = Irq(1) Mask
; r8 = Offset mask
; r9 = SaveOnLower backup (used on SaveOnHgher in order to not execute a load instruction) 

;   r4 <= Nova parte da instrução
    ldh r4, #arrayUART_RX
    ldl r4, #arrayUART_RX
    ld r4, r0, r4 ; r4 <= &RX_DATA
    ld r4, r0, r4 ; r4 <= RX_DATA

    add r2, r0, r2 ; Gera flag
    jmpzd #appTransferSaveOnHigher
    jmpd #appTransferSaveOnLower

  AppTransferSaveOnHigher:

;   Shifta byte atual até sua posição (a ser salvo na parte alta)
    sl0 r4, r4 ; MSB @ 8
    sl0 r4, r4 ; MSB @ 9
    sl0 r4, r4 ; MSB @ 10
    sl0 r4, r4 ; MSB @ 11
    sl0 r4, r4 ; MSB @ 12
    sl0 r4, r4 ; MSB @ 13
    sl0 r4, r4 ; MSB @ 14
    sl0 r4, r4 ; MSB @ 15
    
;   Salva byte em r4 (elimina necessidade de instruçao de load quando estiver carregado parte baixa)
    add r9, r0, r4

;   Proximo byte a ser salvo na parte baixa 
    ldl r2, #1

;   Retorna
    jmpd #UartRXDataAVACK 
    
  AppTransferSaveOnLower:

;   Combina os dois bytes (Atual (parte baixa), e o salvo em r9 na etapa SaveOnHigher (parte alta))
    or r5, r4, r9

;   Salva a instrução pronta ( RAM[r3] <= parte baixa + parte alta )
    st r5, r0, r3
    
;   Proximo byte a ser salvo na parte alta
    xor r2, r2, r2 ; r2 <= 0
    
;   Verifica se r3 aponta para ultima posiçãoi de memoria
    xor r11, r3, r10
    jmpzd #EndTransfer ; Se r3 = 32767, finaliza programa
    
;   Incrementa Ponteiro da RAM
    addi r3, #1

;   Retorna
    jmpd #UartRXDataAVACK

  UartRXDataAVACK:   

;   intACK <= 1 (UART RX)
    ldh r5, #0
    ldl r5, #1
    st r5, r0, r6

;   Returns to polling loop
    jmpd #UartRxDataAVPollingLoop
    
  EndTransfer: ; Loops until next reset
    jmpd #EndTransfer

.endcode
;=============================================================================================================
;=============================================================================================================
;=============================================================================================================
;=============================================================================================================
;=============================================================================================================

.data
;--------------------------------------------VARIAVEIS DO KERNEL---------------------------------------------

; Array de registradores da Porta Bidirecional
; arrayPorta [ PortData(0x8000) | PortConfig(0x8001) | PortEnable(0x8002) | irqtEnable(0x8003) ]
arrayPorta:               db #8000h, #8001h, #8002h, #8003h

; Array de registradores do controlador de interrupções
; arrayPIC [ IrqID(0x80F0) | IntACK(0x80F1) | Mask(0x80F2) ] IrqREG(0x80F3) ]
arrayPIC:                 db #80F0h, #80F1h, #80F2h, #80F3h

; Array de registradores do controlador de interrupções
; arrayUART_TX [ RX_DATA(0x80A0) | RATE_FREQ_BAUD(0x80A1) ]
arrayUART_RX:             db #80A0h, #80A1h

; Array de registradores do controlador de interrupções
; arrayUART_TX [ TX_DATA(0x80A0) | RATE_FREQ_BAUD(0x80A1) | READY(0x80A2) ]
arrayUART_TX:             db #8080h, #8081h, #80A2h

.enddata
