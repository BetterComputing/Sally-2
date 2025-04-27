;
SALLYBUILD	EQU	1
;
;
		ORG	05A8h
;
		INCLUDE equs.mac
;
;
;
;	PHASE TO RAM-RESIDENT PART OF PROGRAM
;
MONCOPY	EQU	$
	.PHASE	0F000H
;
; Sally starts code here
;
MONITOR	EQU	$
	JP	RESTART
	JP	MINIMON		;MONITOR WARM ENTRY POINT
CSV:	JP	CONST		;CONSOLE STATUS
CIV:	JP	CONIN		;CONSOLE INPUT
COV:	JP	CONOUT		;CONSOLE OUTPUT
DISKV:	JP	DISKDVR		;DISK HANDLER
LISTV:	JP	CENTOUT		;PARALLEL PRINTER OUT
	JP	CENTRDY		;PARALLEL PRINTER STATUS
RENEW:	JP	CINIT2		;CONSOLE PORT INITAILZATION
;
RESTART:	
	DI
	XOR	A
	OUT	(BANKSW),A
	JP	0		;JUMP TO ROM
;	
;	
;	
	INCLUDE	diskio.mac
	INCLUDE	minimon.mac
	INCLUDE	printer.mac
	DEFB 95h                ; **** Exists on original ROM? ****
		dw	0, 0, 0, 0, 0, 0
;	defs	(($ and 0ff00h)+100h)-$
	INCLUDE	serial.mac
;	
;		CODE PAST THIS POINT IS ONLY USED IN ATARI DISK MODE
;	
	INCLUDE	bitbang.mac
	INCLUDE	atari.mac
	INCLUDE	format.mac
;
	.DEPHASE
MONSIZE	EQU	$-MONCOPY
;
;
varcopy	equ	$
		.phase	0ff00h+32
ram	equ	$-32		;put ram on start of 256 byte page
KEYBUF	equ	ram		;16 byte keyboard input fifo
CTCVEC	equ	ram+16	;8 word interrupt vector table
		INCLUDE global.mac
;
		.DEPHASE
;
;--------------------------------------------------
; set to zero in startup code until 0ffffh
;--------------------------------------------------
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
;
;LAST	EQU	$
;	IF ($+16) GT (1000H)
;		.ERROR /4K ROM BOUNDARY CROSSED/
;	ELSE
;		DEFS (1000H-16)-LAST
		DB  'ROLLI 1 Rev 1.00'
;	ENDIF
;
;
;
NOHIGHSPEEDSIO	EQU	0
;
;--------------------------------------------------
; Track-Buffer 26*256 bytes
;--------------------------------------------------
TRKBUFSZ	EQU	00800h

SIONORMAL	EQU	40
SIOFAST		EQU	8

;--------------------------------------------------
; Code executed after Reset
;--------------------------------------------------
			ORG	00000h

reset:		DI							;disable interrupt
			XOR		A					;set a to zero
;			LD		B, A
restime:	DEC		A					;do 256 times nothing
			JR		NZ, restime			;loop

			LD		HL, portval			;init 11 ports with values at 0a3h
			LD		B, portlen/2
portinit:	LD		C, (HL)
			INC     HL
			OUTI
			JR		NZ, portinit		; loop

			LD		HL,MONITOR			;test ram @F000-FFFF
			LD		A, 01h				;write 1,2,4,8,16,32,64,128
testram2:	LD		B, 10h
testram:	LD		(HL), A
			RLCA
			INC		L
			JR		NZ, testram
			INC		H
			DJNZ    testram

			LD		C, 10h
testram1:	DEC     HL
			RRCA
			CP		(HL)
ramerr:		JR		NZ, ramerr			;if error, loop forever
			DJNZ    testram1
			DEC		C
			JR		NZ, testram1
			ADD     A, A
			JR		NZ, testram2

			LD		HL, MONCOPY			;copy BIOS 0f000h
			LD		DE, MONITOR
			LD		BC, MONSIZE			;length always 0f00h?
			LDIR						;COPY RESIDENT CODE INTO RAM

			LD		HL, VARCOPY			;copy initial values to 0ff20h
			LD		DE, glbvars			;length $2F
			LD		BC, glbsize
			LDIR

			XOR     A					;fill up to $FFFF with zeros
ramfill:	LD		(DE), A
			INC		E
			JR		NZ, ramfill

			LD		SP, KEYBUF+16		;stack-pointer to 0ff10h - Note: same as CTCVEC

			LD		A, HIGH ram			;load interrupt-vector register
			LD		I, A				;with 0ffh
			IM		2					;enable interrupt mode 2 (vectored)

;--------------------------------------------------
; step 5 times in and then out to trk00
; set bit 6 for each online floppy in ff5eh
; percom block (16 bytes, byte 8 bit 6)
;--------------------------------------------------

			LD		A, FBS+DRVSEL4+DRVSEL3+DRVSEL2+DRVSEL1	;select drive 1-4, Motor off, side 0, B/S=1, DD
			OUT     (LATCH), A

			LD		D, A				;KEEP PATTERN IN D (d = 4fh)
			LD		B, 5				;step 5 times
stepinloop:
			LD		A, STEPIN+NOWAITMTR+STEPRATE		;4b: 4 = step-in, b = NOWAITMTR+STEPRATE3
			CALL    slyCMDOUT			;write A to FDC command and wait
			DJNZ    stepinloop

			LD		B, 64h				;seek track 00 for all 4 drives - DO 100 STEPS TOWARD TRACK ZERO to support 100 track drives
outloop1:	LD		A, D				;select all drives
			OUT     (LATCH), A
			EX		(SP),HL				;Use this benign operation pair
			EX		(SP),HL				;to wait 38 T-states
			LD		A, STEPOUT+NOWAITMTR+STEPRATE		;6b: 6 = step-out, b = NOWAITMTR+STEPRATE3
			CALL    slyCMDOUT			;write A to FDC command and wait

			LD		E, DRVSEL1			;PREPARE TO TEST TK0 INDICATORS
outloop:	LD		A, E
			OR		FBS
			OUT     (LATCH), A			;select one drive
			EX		(SP),HL				;Use this benign operation pair
			EX		(SP),HL				;to wait 38 T-states
		IF SALLYBUILD
			IN		A, (STSREG)
		ELSE
			CALL    FORCE				;FORCE INTERRUPT TO SET TYPE 1 STATUS
		ENDIF
			BIT     2, A
			JR		Z, excldrv			;bit not set, not at track 00
			LD		A, E				;drive at track 00
			CPL							;exclude drive from seeking
			AND		D
			LD		D, A
excldrv:
			SLA		E
			BIT		4, E
			JR		Z, outloop			;status checked for all 4 drives?
			DJNZ    outloop1			;step out again - REPEAT STEP OUT 100 TIMES
		IF SALLYBUILD <> 1
			CALL    shutdown			;deselect floppies and seek current track?
		ELSE
			LD		A, FDCRESET+FBS		;reset FDC and deselct
			OUT		(LATCH), A
			LD		A, FBS				;reset FDC and deselct
			OUT		(LATCH), A
		ENDIF

			LD		HL, DMATRIX+DSKBITS	;set bit for each connected floppy?
			LD		BC, 0010h
			LD		A, 04h				;PREP TO SET FLAGS FOR DRIVES PRESENT
nextdrv:	RR		D
			JR		C, noset			;JUMP IF DRIVE(N) NOT AT TRACK ZERO
			SET     PRESENT, (HL)
noset:		ADD     HL, BC
			DEC     A
			JR		NZ, nextdrv

		IF SALLYBUILD
			LD		HL, 00000h			; source
			LD		DE, 08000h			; dest
			LD		BC, 02000h
			LDIR
			LD		HL, code8000
			SET		7, H
			JP		(HL)
code8000:	LD		A, 1
			OUT		(BANKSW), A
			LD		HL, 08000h
			LD		DE, 00000h
			LD		BC, 02000h
			LDIR
			JP		code0000
		ENDIF
code0000:
			LD		SP, IOBUFF			;NEW PLACE FOR STACK

;--------------------------------------------------
; firmware patch
;--------------------------------------------------
		IF SALLYBUILD
			LD		HL, 0ffffh			;reset drive / track buffer
			LD		(drive), HL

			LD		HL, DISKTAB+2
			LD		DE, dsktb+3
			LD		BC, 3*7
			LDIR

			LD		HL, 8
			LD		(dsktb),HL
			LD		A, '?'
			LD		(dsktb+2), A
			EX		DE, HL

			LD		A, getspeed & 255
			LD		(HL), A
			INC		HL
			LD		A, getspeed / 256
			LD		(HL), A
			INC		HL

			LD		HL, dsktb
			LD		(DISKID), HL
			LD		(DISKID+2), HL
			LD		(DISKID+4), HL
			LD		(DISKID+6), HL

			LD		A, SIONORMAL
			LD		(pokeydiv), A
			XOR		A
			LD		(hispeed), A
			LD		A, 2
			LD		(direct), A
			LD		A, 0c3h				;'JP' instruction
			LD		(XMITBUF), A
			LD		(RXBLOCK), A
			LD		(CMDWAIT), A
			LD		(LOGNHUH), A
			LD		(DISKWRT1), A
			LD		(DISKRD1), A

			LD		HL, xmitbuffn
			LD		(XMITBUF+1), HL
			LD		HL, rxblck
			LD		(RXBLOCK+1), HL
			LD		HL, cmdwaitfn
			LD		(CMDWAIT+1), HL
			LD		HL, dskreadfn
			LD		(ALTDISKREAD+1), HL
			LD		HL, dskwrite
			LD		(ALTDISKWRITE+1), HL
			LD		HL, logonfn
			LD		(LOGNHUH+1), HL
			LD		HL, dwrt
			LD		(DISKWRT1+1), HL
			LD		HL, dskrd1
			LD		(DISKRD1+1), HL

;--------------------------------------------------
; test code
;--------------------------------------------------
;			LD		A, 0c3h				;'JP' instruction
;			LD		(SEL4), A
;			LD		HL, slysel4
;			LD		(SEL4+1), HL
;
;			LD		A, 255
;			LD		(CTCVEC+2), A
;
;			LD		B, 3
;			LD		A, ' '
;stars:		CALL	seroutfn
;			DJNZ	stars
;
;			LD		IX, testdcb
;			CALL	sercr
;			CALL	DISKV
;			CALL	dumpdcb
;
;			LD		A, GETID
;			LD		(testdcb), A
;			CALL	DISKV
;			CALL	dumpdcb
;			CALL	dumprid
;
;			LD		HL, 512
;			LD		(testdcb+DSKAUX), HL
;			LD		A, GETSEC
;			LD		(testdcb), A
;
;			LD		A, '*'
;			CALL	seroutfn
;
;			LD		C, 1
;secloop1:	LD		B, 18
;			LD		HL, sectab
;secloop:	LD		A, (HL)
;			LD		(testdcb + DSKSEC), A
;			INC		HL
;			PUSH	BC
;			PUSH	HL
;			CALL	DISKV
;			POP		HL
;			POP		BC
;			DJNZ	secloop
;			DEC		C
;			JR		NZ, secloop1
;
;			LD		A, '-'
;			CALL	seroutfn
;
;loop:		JR		loop
;
;

;
;dumprid:
;			LD		HL, (testdcb + DSKPTR)
;			CALL	sercr
;			LD		B, 6
;dumprid1:	LD		A, (HL)
;			INC		HL
;			CALL	serhex
;			CALL	serspace
;			DJNZ	dumprid1
;			RET
;
;dummy:		XOR		A
;			CALL	serhex
;			RET
;
;testdcb:	DB	TSTRDY					;DISK OPERATION CODE
;			DB	1						;DRIVE# (WITH SIDE# IN BIT 7)
;			DB	0						;TRACK#
;			DB	1						;SECTOR#
;			DW	08000h					;READ/WRITE POINTER
;			DW	0						;AUXILLIARY PARAMETERS (2 BYTES)
;			DB	0						;OPERATION COMPLETION STATUS
;
;sectab:
;			DB	1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18
;			DB	1, 3, 5, 7, 9, 11, 13, 15, 17, 2, 4, 6, 8, 10, 12, 14, 16, 18
;			DB	1, 4, 7, 10, 13, 16, 2, 5, 8, 11, 14, 17, 3, 6, 9, 12, 15, 18
;			DB	1, 5, 9, 13, 17, 3, 7, 11, 15, 2, 6, 10, 14, 18, 4, 8, 12, 16
;--------------------------------------------------
; end of test code
;--------------------------------------------------
		ENDIF

			JP		EMULATOR			;GO INITIALIZE FOR ATARI OR CP/M

logonfn:	LD		(RWMAX),A			;DO LESS RETRIES IN ATARI MODE

			LD		A, 0c3h				;'JP' instruction
			LD		(SEL4), A
			LD		HL, slysel4
			LD		(SEL4+1), HL
			LD		HL, shtdwnfn
			LD		(SHUTDOWN+1), HL
			JP		LOGNHUH+3


shtdwnfn:	CALL	0f228h
			LD		A, 00000111B		;CTC1 4uS pulses (4Mhz / 1*16)
			OUT		(CTC1), A
			LD		A, 1
			OUT		(CTC1), A
			RET
;
;
;
slysel4:		BIT		6, B				;8" found?
			JR		Z, slysel4ex			;yes, do nothing

			PUSH	BC					;save registers
			PUSH	DE
			PUSH	HL
			PUSH	IX

			LD		A, B				;switch HD on
			RES		6, A
			OUT		(LATCH), A

			PUSH	IX					;load HL with IX
			POP		HL
			LD		DE, dcb
			LD		BC, 9
			LDIR						;copy dcb

			LD		IX, dcb
			LD		HL, id
			LD		(dcb + DSKPTR), HL
			LD		A, RIDCMD
			LD		(CMDBYT), A
			LD		A, 018h				;substitute JR	Z,xx by JR xx
			LD		(DISK3 + 031h), A
			LD		B, 6
slysel4a:		PUSH	BC
			CALL	DISK3+6				;DISK3: READ 6 BYTE ID RECORD
			POP		BC
			DJNZ	slysel4a

			LD		A, 028h				;reset JR Z,xx
			LD		(DISK3 + 031h), A

			POP		IX
			POP		HL
			POP		DE
			POP		BC
			LD		A, (dcb + DSKSTS)	;check disk status
			OR		A
			JR		NZ, slysel4ex			;not zero, no HD
			RES		6, B				;
slysel4ex:
			LD		A, B
			LD		D, 0
			JP		SEL4+3

;--------------------------------------------------
; hook for read directSector if aux1/2 = 0
;--------------------------------------------------
dskrd1:
			LD		A, 2
			LD		(direct), A			;clear direct

			LD		HL, (CFRAME + 2)	;load DAUX1/2, test if zero
			LD		A, L
			OR		H
			JR		Z, dskrd2			;if zero, use direct sector number
			CALL	SECTRAN				;else use original code, call SECTRAN
			JP		DISKRD1+3			;and procees

dskrd2:		LD		IX, DKIOCB
			LD		(IX + DSKOP), TSTRDY
;			CALL	dumpdcb
			PUSH	HL
			CALL	DISKV
			POP		HL

			CALL	sec2track			;compute sector, track, side from direct sector
			LD		(IX + DSKOP), GETSEC
;			CALL	dumpdcb
			PUSH	HL
			CALL	dskreadfn			;CALL DISK I/O HANDLER
			POP		HL

			LD		A, (DKIOCB+DSKSTS)
			CALL	SETSTAT				;call SETSTAT
			LD		D, 0				;no invert
			JP		SENDBUFF			;jump SENDBUFF

;--------------------------------------------------
;compute sector, track, side from direct sector
;--------------------------------------------------
sec2track:	XOR		A					;clear carry and a
			LD		B, 0ffh				;also b
			LD		HL, (dsector)		;compute track and side numer
			LD		DE, 18				;18 secs per track
sec2track1:	INC		B					;b holds track-number
			sbc		HL, DE
			JR		NC, sec2track1		;subtract 18 as long carry clear
			LD		A, L
			ADD		19					;add 19 to get sector number + 1
			LD		(DKIOCB+DSKSEC), A
			SRL		B					;divide track by two, (we have two sides)
			LD		A, B
			LD		(DKIOCB+DSKTRK), A
			RRA							;shift-in side-number from previously lowest-bit
			AND		080h				;mask out bit 0-6
			LD		HL, DKIOCB+DSKDRV
			RES		7, (HL)
			OR		(HL)
			LD		(HL), A

			CALL	serhex
			CALL	serspace
			LD		A, 'S'
			CALL	seroutfn
			LD		A, (DKIOCB+DSKSEC)
			CALL	serhex
			CALL	serspace
			LD		A, 'T'
			CALL	seroutfn
			LD		A, (DKIOCB+DSKTRK)
			CALL	serhex
			CALL	sercr

			LD		HL, 512
			LD		(DKIOCB+DSKAUX), HL
			LD		HL, IOBUFF
			LD		(DKIOCB+DSKPTR), HL
			LD		IX, DKIOCB
			RET

;--------------------------------------------------
; hook for setDirectSector if aux1/2 = 0
;--------------------------------------------------
dwrt:		PUSH	HL
			LD		(LOGSIZ),HL			;SAVE DATA BLOCK LENGTH

			LD		HL, (CFRAME + 2)	;load DAUX1/2
			LD		A, L
			OR		H
			JR		Z, dwrta			;if zero, do special stuff

			POP		HL					;otherwise continue
			JP		DISKWRT1+3			;normal

dwrta:		CALL	SENDACK

			LD		HL, direct			;direct
;			LD		A, (HL)
;			CALL	serhex
			DEC		(HL)
			JR		Z, dwrtd			;first sector
			JP		M, dwrtb			;second sector

			LD		B, 64
			DJNZ	$					;wait some time

			LD		HL, (IOBUFF)		;save 2-byte sector number (0-xxxx)
			LD		(dsector), HL

dwrtc:		POP		HL
			LD		A, 'C'
			JP		SENDCHAR

dwrtd:		LD		HL, IOBUFF
			LD		DE, IOBUFF+LEN+2
			LD		BC, 0100h			;rec first half
			LDIR
			JR		dwrtc

dwrtb:		LD		(HL), 2				;direct = 2

			LD		HL, 512				;LOGSIZ 512 bytes in this case
			LD		(LOGSIZ), HL

			LD		HL, IOBUFF
			LD		DE, IOBUFF+0100h
			LD		BC, 0100h
			LDIR
			LD		HL, IOBUFF+LEN+2
			LD		DE, IOBUFF
			LD		BC, 0100h
			LDIR

			CALL	sec2track

			LD		(IX + DSKOP), PUTSEC
			CALL	dskwrite

			JR		dwrtc
;--------------------------------------------------
; dskwrite: write through sector
;--------------------------------------------------
dskwrite:
;			LD		A, 'W';
;			CALL	seroutfn

;			JP		DISKV

			CALL	checktrack
			JR		NZ, dskwrite1

;			LD		HL, (LOGSIZ)
;			CALL	seraddr

			CALL	compbufadr
;			CALL	seraddr

			EX		DE, HL
			LD		H, (IX + DSKPTR+1)
			LD		L, (IX + DSKPTR)
			LD		BC, (LOGSIZ)

			LDIR

dskwrite1:	JP		DISKV

;--------------------------------------------------
; Debug routine
;--------------------------------------------------
;debug:		CALL	seroutfn
;			LD		A, (IX + DSKDRV)
;			CALL	serhex
;			LD		A, 't'
;			CALL	seroutfn
;			LD		A, (IX + DSKTRK)
;			CALL	serhex
;			LD		A, 's'
;			CALL	seroutfn
;			LD		A, (IX + DSKSEC)
;			CALL	serhex
;			LD		A, (IX + DSKPTR+1)
;			CALL	serhex
;			LD		A, (IX + DSKPTR)
;			CALL	serhex
;			LD		A, (IX + DSKAUX+1)
;			CALL	serhex
;			LD		A, (IX + DSKAUX)
;			CALL	serhex
;
;			CALL	serspace
;			LD		A, (CFRAME+3)
;			CALL	serhex
;			LD		A, (CFRAME+2)
;			CALL	serhex
;			CALL	serspace
;			LD		A, (IY+NSECS+1)
;			CALL	serhex
;			LD		A, (IY+NTRKS)
;			CALL	serhex
;
;			CALL	sercr
;			RET
;
dumpdcb:	PUSH	AF
			PUSH	BC
			PUSH	HL

			PUSH	IX
			POP		HL
;			CALL	sercr
			LD		B, 9
dumpdcb1:	LD		A, (HL)
			INC		HL
			CALL	serhex
			CALL	serspace
			DJNZ	dumpdcb1
			CALL	sercr

			POP		HL
			POP		BC
			POP		AF
			RET
;--------------------------------------------------
; dskreadfn: cache a track
;--------------------------------------------------
dskreadfn:
;			LD		A, 'R'
;			CALL	debug

			CALL	checktrack
			JP		Z, match

			LD		(drive), DE			;save new drive and track
			PUSH	IX					;save IX

			PUSH	IX					;load HL with IX
			POP		HL
			LD		DE, dcb
			LD		BC, 9
			LDIR						;copy dcb

			LD		IX, dcb				;load IX with new dcb

			LD		A, (dcb + DSKAUX+1)	;get sector length high-byte
			CP		2
			JR		NZ, readtrack2		;no MS-DOS disk


			LD		HL, TRKBUFSZ
			LD		BC, 18 * 256 + 1	;b=18, c = 1

readtrack3:	LD		(dcb + DSKPTR), HL
			LD		(IX + DSKSEC),	C

;			CALL	dumpdcb

			PUSH	HL
			PUSH	BC
			CALL	DISKV
			POP		BC
			POP		HL
;			CALL	dumpsec
			LD		A, (dcb + DSKSTS)	;error occured?
			OR		A
			JR		NZ, readtrack4		;yes
			INC		H
			INC		H
			INC		C
			DJNZ	readtrack3
			JR		readtrack5

readtrack2:	LD		B, 0				;compute skew-list from media type
			LD		C, (IY + MEDIA)
			LD		HL, skewtab
			ADD		HL, BC
			LD		A, (HL)
			INC		HL
			LD		H, (HL)
			LD		L, A
			LD		B, (IY + NSECS+1)

readtrack1:	LD		(secptr), HL

readtrack:	LD		HL, (secptr)
			LD		A, (HL)
			LD		(dcb + DSKSEC), A
			INC		HL
			LD		(secptr), HL

			PUSH	BC
			CALL	compbufadr
			LD		(dcb + DSKPTR), HL
			CALL	DISKV
			POP		BC

			LD		A, (dcb + DSKSTS)	;error occured?
			OR		A
			JR		Z, readtrack6		;no
readtrack4:	;CALL	dumpdcb
			LD		HL, 0ffffh
			LD		(drive), HL
			POP		IX					;yes, store in original dcb
			JR		match2
readtrack6:	DJNZ	readtrack
readtrack5:	POP		IX

match:		CALL	compbufadr
			LD		D, (IX + DSKPTR+1)
			LD		E, (IX + DSKPTR)
			LD		BC, (LOGSIZ)
			LD		A, (dcb + DSKAUX+1)	;get sector length high-byte
			CP		2
			JR		NZ, match1
			LD		BC, 512
match1:		LDIR

			XOR		A
match2:		LD		(IX + DSKSTS), A

			LD		A, FINCMD			;keeps motor spinning
			CALL	slyCMDOUT

			JP		ACTIVON


;--------------------------------------------------
; dump 512 bytes at HL
;--------------------------------------------------
dumpsec:	PUSH	BC
			PUSH	HL
			CALL	seraddr
			CALL	sercr
			LD		BC, 512
match3a:	LD		A, (HL)
			CALL	serhex
			LD		A, B
			AND		15
			JR		NZ, match3
			CALL	sercr
match3:		INC		HL
			DEC		C
			JR		NZ, match3a
			DJNZ	match3a
			POP		HL
			POP		BC
			JP		sercr

;--------------------------------------------------
; HL = TRKBUF + DSKSEC * (128/256/512)
;--------------------------------------------------
compbufadr:	LD		HL, TRKBUFSZ
			LD		B, (IX + DSKSEC)
			DEC		B
			LD		C, 0
			LD		A, (dcb + DSKAUX + 1)	;load seclen highbyte
			OR		A
			JR		Z, compbufadr2		;if zero, assume 128 bytes length
			CP		1
			JR		Z, compbufadr1		;if 1, assume 256 bytes
			SLA		B					;512 bytes
			JR		compbufadr1
compbufadr2:		SRL	B				;128 bytes
			RR		C
compbufadr1:		ADD	HL, BC
			RET

checktrack:	LD		HL, (drive)			;load
			LD		D, (IX + DSKTRK)	;high
			LD		E, (IX + DSKDRV)	;low
			OR		A					;clear carry
			sbc		HL, DE
			RET

;--------------------------------------------------
; get Pokeydivisor command '?'
;--------------------------------------------------
getspeed:
;			LD		A, '?'
;			CALL	seroutfn

			CALL	DRVINDEX			;POINT IY TO DRIVE'S DATA AREA
			RET		C					;EXIT IF NOT A DRIVE IN OUR BOX

;			CALL	HASPARMS
;			RET		Z					;EXIT IF DISK PARAMS NOT KNOWN

			CALL	SENDACK				;SEND 'ACK' FOR COMMAND FRAME

			LD		HL, IOBUFF+LEN-1

			IF NOHIGHSPEEDSIO <> 1
			LD		A, SIOFAST
			LD		(HL), A
			LD		(hispeed), A
			ELSE
			LD		(HL), SIONORMAL
			ENDIF

			LD		DE, 'C'
			JP		SENDBUFF			;SEND 'C' AND PARAMS DATA FRAME


;--------------------------------------------------
; cmdwaitfn
;--------------------------------------------------
cmdwaitfn:	LD		A, (CMDFLG)
			OR		A					;SEE IF COMMAND FRAME HAS ARRIVED
			RET		Z					;EXIT IF NOTHING HAS HAPPENED

;			CALL	sercmd				;5-byte command frame

			LD		A, (CMDFLG)
			CP		1

			DI							;ELSE RESET INTERRUPT AND START AGAIN
			LD		A, 00000011B
			OUT		(CTC0),A
			EI

			JP		Z, CMDL4			;good cmd-frame

			CALL	togglebaud
			JP		CMDL5



;--------------------------------------------------
; xmitbuffn
;--------------------------------------------------
xmitbuffn:
			DI
			LD		A, (pokeydiv)		;is fast?
			CP		SIONORMAL
			JR		NZ, xmitfast		;yes, jump
			LD		BC, STARBIT
			JP		XMITBUF+4

xmitfast:
			LD		A, 00000111B		;CTC1 4uS pulses (4Mhz / 1*16)
			OUT		(CTC1), A
			LD		A, 1
			OUT		(CTC1), A

xmitfast1:	LD		A, (HL)				;7
			INC		HL					;6
			XOR		D					;4
			LD		C, A				;4
			ADD		A, E				;4
			ADC		0					;7
			LD		E, A				;4
			CALL	fastsend			;17 send byte in c
			LD		A, H				;4
			CP		HIGH (IOBUFF+LEN)	;7
			JR		C, xmitfast1		;12/7 loop if buffer end not reached

			LD		A, 00000011B
			OUT		(CTC1), A
			EI
			RET

fastsend:	XOR		A
			OUT		(ATROUT), A

			PUSH	IX					;15
			POP		IX					;14
			LD		A,	C				;4
			LD		B, 8				;7
			INC		BC					;6
			NOP							;4

fastsend1:	NOP							;4
			CP		1					;7
			OUT		(ATROUT), A			;11
			RRCA						;4
			PUSH	IX					;15
			POP		IX					;14
			DJNZ	fastsend1			;13 / 8

			LD		A, r				;9
			LD		A, 1				;7
			OUT		(ATROUT), A			;11
			RET							;10

;--------------------------------------------------
; rxblck
;--------------------------------------------------
rxblck:		LD		A, (pokeydiv)		;is fast?
			CP		SIONORMAL
			JP		Z, rxblck1

			CALL	fastrecv			;yes, fast speed
			LD		A, 00000011B		;reset timer
			OUT		(CTC3), A
			LD		C, D				;checksum in c
			RET

rxblck1:
			LD		BC, 0				;no, normal speed
			JP		RXBLOCK+3

;--------------------------------------------------
; togglebaud
;--------------------------------------------------
togglebaud:	;LD		A, (hispeed)
;			OR		A
;			RET		Z

			LD		A, (pokeydiv)
			CP		SIONORMAL
			LD		A, SIOFAST
			JR		Z, togglebaud1
			LD		A, SIONORMAL
togglebaud1:	LD		(pokeydiv), A
			RET

;--------------------------------------------------
; set 4ms watchdog
;--------------------------------------------------
irq4ms:		POP		AF					;pop irq-addr
			OR		A					;clear carry
irq4ms1:	EI
			RETI

;--------------------------------------------------
; SIO receive 57600 baud
;--------------------------------------------------
fastrecv:
			DI
			LD		BC, irq4ms
			LD		(CTCVEC+6),bc		;SET VECTOR TO irq4ms ROUTINE
			LD		BC, CTC3			;clear b, load c with CTC3
			LD		DE, 10100111B		;clear d, load e with a7h
			OUT		(c), E				;12 PUT CTC3 IN TIMER MODE, PRESCALE 256
			OUT		(c),	C
			EI

fastrecv1:	IN		A, (ATARI)			;11
			RLA							;4
			JP		C, fastrecv1		;10	NEW BYTE IS COMING IF START BIT LOW
										;14-25 / 7
			LD		A, D				;4
			ADD		A, B				;4
			ADC		A, 0				;7 ACCUMULATE CHECKSUM ATARI STYLE
			LD		D, A				;4

			LD		B, 07fh				;7
			JP		fastrecv2a			;10 = 50
;
; SERIAL->PARALLEL CONVERSION AT 17,36 MICROSECONDS PER BIT
;
fastrecv2:	PUSH	AF					;11
			POP		AF					;10
fastrecv2a:	LD		A, (HL)				;7
			LD		A, (HL)				;7

			IN		A, (ATARI)			;11 CYCLES
			RLA							; 4 CYCLES
			RR		B					; 8 CYCLES
			JR		C, fastrecv2		;12/7 = 70 / 65 cycles

			LD		(HL), B				;7 THEN STORE IN MEMORY BUFFER @HL
			INC		HL					;6
			LD		A, H				;4
			CP		HIGH (IOBUFF+LEN)	;7
			CCF							;4
			RET		C					;5 RETURN WITH CARRY SET IF BUFFER FILLED

			OUT		(c), E				;12 PUT CTC3 IN TIMER MODE, PRESCALE 256
			OUT		(c),	C			;12 COUNT MOD 256

			JP		fastrecv1			;10


;serdumpcpl:	PUSH	HL
;			PUSH	AF
;			PUSH	BC
;			PUSH	DE
;			LD		D, 255
;			JR		serdump1

;--------------------------------------------------
; RS232 sercmd
;--------------------------------------------------
sercmd:		PUSH	HL
			LD		HL, CFRAME
			CALL	sercr
			JR		serdump2

;--------------------------------------------------
; RS232 dump
;--------------------------------------------------
serdump:	PUSH	HL
serdump2:	PUSH	AF
			PUSH	BC
			PUSH	DE

			LD		D, 0

serdump1:	LD		A, (HL)
			XOR		D
			CALL	serhex
			CALL	serspace
			INC		HL
			LD		A, H
			CP		A, 0c3h
			JR		C, serdump1

			POP		DE
			POP		BC
			POP		AF
			POP		HL
			RET

;--------------------------------------------------
; RS232 <space>
;--------------------------------------------------
serspace:	PUSH	AF
			LD		A, ' '
			CALL	seroutfn
			POP		AF
			RET

;--------------------------------------------------
; RS232 <CR>
;--------------------------------------------------
sercr:		PUSH	AF
			LD		A, CR
			CALL	seroutfn
			POP		AF
			RET

;--------------------------------------------------
; RS232 output HL in hex
;--------------------------------------------------
seraddr:	LD		A, H
			CALL	serhex
			LD		A, L
			CALL	serhex
			JR		serspace
;--------------------------------------------------
; RS232 output A in hex
;--------------------------------------------------
serhex:		PUSH	AF
			PUSH	AF
			RRCA
			RRCA
			RRCA
			RRCA
			CALL	sernib
			POP		AF
			CALL	sernib
			POP		AF
			RET

sernib:		AND		0fh
			ADD		'0'
			CP		'9' + 1
			JR		C, sernib1
			ADD		7
sernib1:
;--------------------------------------------------
; RS232 out	208 T-States
;--------------------------------------------------
seroutfn:
			PUSH	AF
			PUSH	BC
			LD		B, A
			XOR		A
			DI
			OUT		(SEROUT), A			;startbit
			CALL	time19600			;17

			LD		A, B
			LD		B, 8				;7
serout1:	OUT		(SEROUT), A			;11
			CALL	time19600			;17
			RRCA						;4
			DJNZ	serout1				;8
			EI
			LD		A, 1				;7
			OUT		(SEROUT), A			;11
			CALL	time19600			;17

			POP		BC
			POP		AF
			RET


;--------------------------------------------------
; RS232 in	208 T-States
;--------------------------------------------------
serinfn:	PUSH	BC
serin2:		IN		A, (SERIN)
			RLCA
			JR		C, serin2

			EX		(SP), HL			;19, 4.75uS
			EX		(SP), HL			;19  9uS

			LD		B, 80h
serin1:		CALL	time19600
			IN		A, (SERIN)
			RLCA
			RR		B
			JR		NC, serin1

			LD		A, B
			POP		BC
			RET

time19600:	LD		C, 9				;4
time19600a:	DEC		C					;4
			JR		NZ, time19600a		;12/7
			RET							;10

slyCMDOUT:
			OUT		(CMDREG),A			;OUTPUT DISK CONTROLLER COMMAND BYTE
slyCMDT1:		LD		A,14
slyCMDT2:		DEC		A
			JR		NZ,slyCMDT2			;DELAY 56 MICROSECONDS
CMDT3:		IN		A, (STSREG)
			BIT		0, A
			JR		NZ, slyCMDT1
			RET

;--------------------------------------------------
; 11 times port:value
;--------------------------------------------------
portval:	DB	ATROUT, 001h			;Bit0	set ATARI DATA TO MARK STATE
			DB	SEROUT, 001h			;Bit1	set RS232 TX TO MARK STATE
			DB	CTC0, 003h				;CTC	Channel 0 reset
			DB	CTC0, LOW CTCVEC		;CTC	Channel 0 interrupt vector
			DB	CTC1, 007h				;CTC	Channel 1 reset + set time constant
			DB	CTC1, 001h				;CTC	Channel 1 time contant (DIVIDE BY 1)
			DB	CTC2, 003h				;CTC	Channel 2 reset
			DB	CTC3, 003h				;CTC	Channel 3 reset
			DB	CDMUX, 001h				;Bit7	ATARI RXD - SET MUX TO PASS ATARI DATA
			DB	LATCH, 050h				;DRIVE CONTROL reset FDC
			DB	LATCH, 040h				;DRIVE CONTROL 8Mhz
portlen	EQU	$-portval

;--------------------------------------------------
; variables and data structure
;--------------------------------------------------
;
; 32 bytes for dsktb
; 7 commands copied in from DISKTAB, 3x7=21
; 2 commands added '?' and 0xDD (setDirectSector)
;
dsktb:		DW	0, 0, 0, 0, 0, 0, 0, 0
			DW	0, 0, 0, 0, 0, 0, 0, 0

;
; direct MSDOS sector for each drive
;
dsector:	DB	0, 0
direct:		DB	0

pokeydiv:	DB	SIONORMAL
hispeed:	DB	0

drive:		DB	255
thetrack:	DB	255
secptr:		DW	0

skewtab:	DW	SKEWSD
			DW	SKEW13
			DW	SKEWDD
			DW	SKEW17

dcb:		DB	0						;DISK OPERATION CODE
			DB	0						;DRIVE# (WITH SIDE# IN BIT 7)
			DB	0						;TRACK#
			DB	0						;SECTOR#
			DW	0						;READ/WRITE POINTER
			DW	0						;AUXILLIARY PARAMETERS (2 BYTES)
			DB	0						;OPERATION COMPLETION STATUS

id:			DW	0, 0, 0

