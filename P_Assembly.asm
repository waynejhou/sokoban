include Irvine32.inc
include winmm.inc
includelib winmm.lib


ExitProcess proto,dwExitCode:dword
readstage proto, mpa_name : PTR BYTE
main_munu_control proto, ins_c: WORD
pause_munu_control proto, ins_c_s : WORD
stage_control proto, ins_c_t : WORD
convert_mem_to_xy proto, mem : dword
convert_xy_to_mem proto , xy_in : coord
changestagebyte proto, co:COORD, charuse:byte

.data
	map_file byte "main_menu.bmap",0
	stage_chose  byte "nmaps/001.map", 0
	pause_frame byte "pause_menu.bmap", 0
	stage_size = 3000
	stage byte stage_size dup(?), 0;
	pause_s byte stage_size dup(? ), 0;
	main_s byte stage_size dup(? ), 0;
	bytesRead DWORD ?
	main_op_choose byte 0
	now_frame byte 0
	next_frame byte 0
	outputHandle DWORD 0
	bytesWritten DWORD 0
	xyPosition COORD <0, 0>
	dexyPosition COORD <0, 0>
	cellsWritten DWORD ?
	color_title WORD 44 dup(0Eh)
	color_op WORD 25 dup(0ch)
	blank_op  WORD 25 dup(0ah)
	clear_all_char WORD 120 dup(00h)
	clear_all_attr WORD 120 dup(0ah)
	position_of_player COORD <0, 0>
	next_position COORD <0, 0>
	screen_info byte 25 dup(0),0
	old_screen_r COORD <120d, 29d>
	xy_w word 2 dup(0)
	color_lGreen word 2 dup(07aah)
	color_lMan word 2 dup(0bh)
	color_lC word 2 dup(0fd4h)
	color_yellow word 2 dup(0e6h)
	color_black word 2 dup(00h)
	color_red word 2 dup(0ffcch)
	pause_op word 0h
	pause_op_color word 9 DUP(0dh)
	pause_op_blank word 9 DUP(0ah)
	now_stage byte 1
	read_console_character word 50 dup(0)
	file_h dword 0
	all_box dword 0
	all_box_on_spot dword 0
	all_spot dword 0
	step_counter dword 0
	step_info byte "步數：",0
	time_info byte "時間：",0
	stage_info byte "現在是第   關",0
	start_time dword 0
	pause_time dword 0
	now_time dword 0
	if_first_move byte 0
	message byte "\o/\o/\o/恭喜你過關\o/\o/\o/",0
	message_2 byte "按下ENTER鍵繼續ㄛ 揪咪（>ω、）", 0
	message_3 byte "[w][s][a][d][^][v][<][>] 上下左右移動   [ESC]暫停   [r]重新開始   [ENTER]確認", 0
	message_4 byte "[w][s][^][v]  上下選擇    [a][d][<][>] 切換BGM   [m]開啟/關閉BGM  [ENTER]確認", 0
	openbgm1 db "Open bgm001.mp3 type MPEGVideo Alias myBGM",0
	openbgm2 db "Open bgm002.mp3 type MPEGVideo Alias myBGM",0
	openbgm3 db "Open bgm003.mp3 type MPEGVideo Alias myBGM",0
	openbgm4 db "Open bgm004.mp3 type MPEGVideo Alias myBGM",0
	openbgm5 db "Open bgm005.mp3 type MPEGVideo Alias myBGM",0
	clearname byte "                                                                                 ",0
	namebgm1 byte "bgm001",0
	namebgm2 byte "bgm002",0
	namebgm3 byte "bgm003",0
	namebgm4 byte "bgm004",0
	namebgm5 byte "bgm005",0
	play    db "Play myBGM from 0 repeat",0
	close	dB "close myBGM", 0
	num_bgm word 1
	bgm_info byte "現在BGM：",0
	the_title byte "倉庫番",0
	openvoice_vic db "Open voice_vic.mp3 type MPEGVideo Alias voice_vic",0
	openvoice_walk db "Open voice_walk.mp3 type MPEGVideo Alias voice_walk",0
	openvoice_sele db "Open voice_sele.mp3 type MPEGVideo Alias voice_sele",0
	openvoice_50 db "Open voice_50.mp3 type MPEGVideo Alias voice_50",0
	openvoice_retry db "Open voice_retry.mp3 type MPEGVideo Alias voice_retry",0
	close_vic	dB "close voice_vic", 0
	close_walk	dB "close voice_walk", 0
	close_sele	dB "close voice_sele", 0
	close_50	dB "close voice_50", 0
	close_retry	dB "close voice_retry", 0
	play_vic    db "Play voice_vic from 0",0
	play_walk   db "Play voice_walk from 0",0
	play_sele   db "Play voice_sele from 0",0
	play_50   db "Play voice_50 from 0",0
	play_retry   db "Play voice_retry from 0",0
	stopbgm 	dB "pause myBGM", 0
	rebgm 	dB "resume myBGM", 0
	is_mute byte 0
	;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
	bufferpos COORD <0, 0>
	bufferpos2 COORD <0, 0>
	bufferread byte 0
	bufferread2 byte 0
	bufferbyte byte 0
	player_on_spot byte 0
	charline byte "  OO..##++@@&&"
	charcolor word 2 dup (0), 2 dup(0ah), 2 dup(0), 2 dup(0dh), 2 dup(0eh), 2 dup(0bh), 2 dup(0ch)
	memresult dword 0
	;................................................
.code
main proc
	invoke SetConsoleTitle, ADDR the_title
	invoke  mciSendStringA, ADDR openbgm1, 0, 0, 0
	invoke  mciSendStringA, ADDR play, 0, 0, 0
	invoke  mciSendStringA, ADDR openvoice_vic, 0, 0, 0
	invoke  mciSendStringA, ADDR openvoice_walk, 0, 0, 0
	invoke  mciSendStringA, ADDR openvoice_sele, 0, 0, 0
	invoke  mciSendStringA, ADDR openvoice_50, 0, 0, 0
	invoke  mciSendStringA, ADDR openvoice_retry, 0, 0, 0
	INVOKE GetStdHandle, STD_OUTPUT_HANDLE
	mov outputHandle, eax
	call clear_all_thing
	call readmain
	call readpause
	call show_main_menu
	Main_Loop :
		mov ah, now_frame
		mov al, next_frame
		cmp ah, al
		jne change_frame
       LookForKey :
			mov dexyPosition.x, 0
			mov dexyPosition.y, 0
			INVOKE SetConsoleCursorPosition, outputHandle, dexyPosition
			call if_resize
			.IF now_frame == 11h
			mov dexyPosition.x, 0
			mov dexyPosition.y, 2
			INVOKE SetConsoleCursorPosition, outputHandle, dexyPosition
			mov edx, OFFSET stage_info
			call writestring
			mov dexyPosition.x, 9
			mov dexyPosition.y, 2
			INVOKE SetConsoleCursorPosition, outputHandle, dexyPosition
			movzx eax, now_stage
			call writedec
			mov dexyPosition.x, 30
			mov dexyPosition.y, 3
			INVOKE SetConsoleCursorPosition, outputHandle, dexyPosition
			mov edx, OFFSET step_info
			call writestring
			mov eax, step_counter
			call writedec
			mov dexyPosition.x, 5
			mov dexyPosition.y, 3
			INVOKE SetConsoleCursorPosition, outputHandle, dexyPosition
			mov edx, offset time_info
			call writestring
			.IF if_first_move == 2
				call GetTickCount
				sub eax, start_time
				add eax, pause_time
				mov ebx, 1000
				div ebx
				call writedec
				mov eax, '.'
				call writechar
				mov eax, edx
				call writedec
			.ELSE
			mov eax, 0
			call writedec
			.ENDIF
				mov dexyPosition.x, 0
				mov dexyPosition.y, 18
				INVOKE SetConsoleCursorPosition, outputHandle, dexyPosition
				mov edx, OFFSET message_3
				call writestring
		.ENDIF
		mov dexyPosition.x, 0
		mov dexyPosition.y, 21
		INVOKE SetConsoleCursorPosition, outputHandle, dexyPosition
		mov edx, OFFSET clearname
		call writestring
		mov dexyPosition.x, 0
		mov dexyPosition.y, 20
		INVOKE SetConsoleCursorPosition, outputHandle, dexyPosition
		.IF now_frame==0
			mov edx, OFFSET message_4
		.ELSE
			mov edx, OFFSET clearname
		.ENDIF
		call writestring
		mov dexyPosition.x, 0
		mov dexyPosition.y, 21
		INVOKE SetConsoleCursorPosition, outputHandle, dexyPosition
		mov edx, OFFSET bgm_info
		call writestring
		.IF num_bgm == 1
			mov edx, OFFSET namebgm1
		.ENDIF
		.IF num_bgm == 2
			mov edx, OFFSET namebgm2
		.ENDIF
		.IF num_bgm == 3
			mov edx, OFFSET namebgm3
		.ENDIF
		.IF num_bgm == 4
			mov edx, OFFSET namebgm4
		.ENDIF
		.IF num_bgm == 5
			mov edx, OFFSET namebgm5
		.ENDIF
		call writestring
		;movzx eax, position_of_player.x
		;call writedec
		;call crlf
		;movzx eax, position_of_player.y
		;call writedec
		;call crlf
		;mov dexyPosition.x, 0
		;mov dexyPosition.y, 0
		;INVOKE SetConsoleCursorPosition, outputHandle, dexyPosition
		;mov eax, all_box
		;call writedec
		;call crlf
		;mov eax, all_spot
		;call writedec
		;call crlf
		;mov eax, all_box_on_spot
		;call writedec
		;mov edx, OFFSET read_console_character
		;call writestring
		mov  eax, 50
		call Delay
		call ReadKey
		jz   LookForKey
		cmp now_frame, 0
		je control_main_menu
		cmp now_frame, 1
		je control_stage
		cmp now_frame, 11h
		je control_stage
		cmp now_frame, 3
		je control_pause
		jmp Main_Loop
		control_main_menu:
			INVOKE main_munu_control, ax
			invoke  mciSendStringA, ADDR play_sele, 0, 0, 0
				jmp Main_Loop
		control_stage :
			INVOKE stage_control, ax
				jmp Main_Loop
		control_pause:
			INVOKE pause_munu_control, ax
			invoke  mciSendStringA, ADDR play_sele, 0, 0, 0
				jmp Main_Loop
	jmp Main_Loop
	change_frame:
		mov ah, al
		mov next_frame, ah
		mov now_frame, ah
		cmp ah, 0
		je change_to_main
		cmp ah, 1
		je change_to_stage
		cmp ah, 2
		je end_game
		cmp ah, 3
		je change_to_pause
		cmp ah, 11h
		je change_to_stage_t
		jmp Main_Loop
		change_to_stage:
			call clear_all_thing
			INVOKE readstage,ADDR stage_chose
			call show_stage
			mov next_frame, 11h
			jmp Main_Loop
		change_to_stage_t:
			call clear_all_thing
			call show_stage
			jmp Main_Loop
		change_to_main:
			call clear_all_thing
			call readmain
			call show_main_menu
			jmp Main_Loop
		change_to_pause:
			call clear_all_thing
			call show_pause
			jmp Main_Loop
	end_game:
	call clear_all_thing
	invoke  mciSendStringA, ADDR close, 0, 0, 0
	invoke  mciSendStringA, ADDR close_vic, 0, 0, 0
	invoke  mciSendStringA, ADDR close_walk, 0, 0, 0
	invoke  mciSendStringA, ADDR close_sele, 0, 0, 0
	invoke  mciSendStringA, ADDR close_50, 0, 0, 0
	invoke  mciSendStringA, ADDR close_retry, 0, 0, 0
	Exit
main endp
;===========================================================================================================================
readmain PROC
	mov edx, OFFSET map_file
	call OpenInputFile
	mov file_h, eax
	mov edx, OFFSET main_s
	mov ecx, stage_size
	call ReadFromFile
	mov bytesRead, eax
	mov eax, file_h
	call CloseFile
	ret
readmain endp
;===========================================================================================================================
readstage PROC,
		mpa_name : PTR BYTE
	mov edx, mpa_name
	call OpenInputFile
	mov file_h, eax
	mov edx, OFFSET stage
	mov ecx, stage_size
	call ReadFromFile
	mov bytesRead, eax
	mov eax, file_h
	call CloseFile
	mov all_box, 0
	mov all_spot, 0
	mov all_box_on_spot, 0
	mov step_counter, 0
	mov pause_time,0
	mov if_first_move, 1
	ret
readstage endp
;===========================================================================================================================
readpause PROC
	mov edx, OFFSET pause_frame
	call OpenInputFile
	mov file_h, eax
	mov edx, OFFSET pause_s
	mov ecx, stage_size
	call ReadFromFile
	mov bytesRead, eax
	mov eax, file_h
	call CloseFile
	ret
readpause endp
;===========================================================================================================================
show_main_menu PROC
	INVOKE GetStdHandle, STD_OUTPUT_HANDLE
	mov outputHandle, eax
	mov ecx, 5
	mov esi, OFFSET main_s
	mov xyPosition.x, 2
	mov xyPosition.y, 1
	main_men_a:
		push ecx
			INVOKE WriteConsoleOutputAttribute,
			outputHandle,
			ADDR color_title,
			44,
			xyPosition,
			ADDR bytesWritten
			inc xyPosition.y
		pop ecx
		loop main_men_a
	mov ecx, 20
	mov esi, OFFSET main_s
	mov xyPosition.x, 0
	mov xyPosition.y, 0
	main_men_l:
		push ecx;
		push eax
		mov  eax, 10
		call Delay
		pop eax
		INVOKE WriteConsoleOutputCharacter,
			outputHandle,
			esi,
			50,
			xyPosition,
			ADDR cellsWritten
		inc xyPosition.y
		add esi, 51
		pop ecx
		loop main_men_l
			mov now_stage, 1
			mov stage_chose + 7, '0'
			mov al, now_stage
			add al, '0'
			.IF al<10 + '0'
				mov stage_chose+8, al
			.ENDIF
			.IF al>=10 + '0' && al<20 + '0'
			sub al, 10
				mov stage_chose + 7, 1+'0'
				mov stage_chose + 8, al
			.ENDIF
			.IF al >= 20 + '0' && al<30 + '0'
			sub al, 20
			mov stage_chose + 7, 2+'0'
			mov stage_chose + 8, al
			.ENDIF
			.IF al >= 30 + '0' && al<40 + '0'
			sub al, 30
			mov stage_chose + 7, 3+'0'
			mov stage_chose + 8, al
			.ENDIF
			.IF al >= 40 + '0' && al<50 + '0'
			sub al, 40
			mov stage_chose + 7, 4+'0'
			mov stage_chose + 8, al
			.ENDIF
			.IF al >= 50 + '0' && al<51 + '0'
			sub al, 50
			mov stage_chose + 7, 5+'0'
			mov stage_chose + 8, al
			.ENDIF
ret
show_main_menu endp
;===========================================================================================================================
show_stage PROC
	mov all_box, 0
	mov all_spot,0
	mov all_box_on_spot,0
	mov player_on_spot, 0
	change_char :
	; convert from txt number data to char displayable
	mov esi, offset stage
	step :
	mov bl, [esi]
	cmp bl, '0'; not in ground
	jz c_0
	cmp bl, '1'; boundary
	jz c_1
	cmp bl, '2'; floor
	jz c_2
	cmp bl, '3'; box
	jz c_3
	cmp bl, '4'; spot
	jz c_4
	cmp bl, '5'; player
	jz c_5
	cmp bl, '9'; box on spot
	jz c_9
	cmp bl, ' '; not in ground
	jz c_0
	cmp bl, 'O'; boundary
	jz c_1
	cmp bl, '.'; floor
	jz c_2
	cmp bl, '#'; box
	jz c_3
	cmp bl, '+'; spot
	jz c_4
	cmp bl, '@'; player
	jz c_5
	cmp bl, '&'; box on spot
	jz c_9
	jmp endstep
	c_0 :
	mov bl, ' '
	mov [esi], bl
	jmp endstep
	c_1 :
	mov bl, 'O'
	mov[esi], bl
	push ecx
	mov edx, esi
	sub edx, OFFSET stage
	INVOKE convert_mem_to_xy,edx
	INVOKE GetStdHandle, STD_OUTPUT_HANDLE
	mov outputHandle, eax
	INVOKE WriteConsoleOutputAttribute,
		outputHandle,
		ADDR color_lGreen,
		1,
		xyPosition,
		ADDR bytesWritten
	pop ecx
	jmp endstep
	c_2 :
	mov bl, '.'
	mov[esi], bl
		push ecx
	mov edx, esi
	sub edx, OFFSET stage
	INVOKE convert_mem_to_xy,edx
	INVOKE GetStdHandle, STD_OUTPUT_HANDLE
	mov outputHandle, eax
	INVOKE WriteConsoleOutputAttribute,
		outputHandle,
		ADDR color_black,
		1,
		xyPosition,
		ADDR bytesWritten
	pop ecx
	jmp endstep
	c_3 :
	inc all_box
	mov bl, '#'
	mov[esi], bl
			push ecx
	mov edx, esi
	sub edx, OFFSET stage
	INVOKE convert_mem_to_xy,edx
	INVOKE GetStdHandle, STD_OUTPUT_HANDLE
	mov outputHandle, eax
	INVOKE WriteConsoleOutputAttribute,
		outputHandle,
		ADDR color_lC,
		1,
		xyPosition,
		ADDR bytesWritten
	pop ecx
	jmp endstep
	c_4 :
	inc all_spot
	mov bl, '+'
	mov[esi], bl
			push ecx
	mov edx, esi
	sub edx, OFFSET stage
	INVOKE convert_mem_to_xy,edx
	INVOKE GetStdHandle, STD_OUTPUT_HANDLE
	mov outputHandle, eax
	INVOKE WriteConsoleOutputAttribute,
		outputHandle,
		ADDR color_yellow,
		1,
		xyPosition,
		ADDR bytesWritten
	pop ecx
	jmp endstep
	c_5 :
	mov bl, '@'
	mov[esi], bl
			push ecx
	mov edx, esi
	sub edx, OFFSET stage
	INVOKE convert_mem_to_xy,edx
	INVOKE GetStdHandle, STD_OUTPUT_HANDLE
	mov outputHandle, eax
	INVOKE WriteConsoleOutputAttribute,
		outputHandle,
		ADDR color_lMan,
		1,
		xyPosition,
		ADDR bytesWritten
	mov ax, xyPosition.x
	mov bx, xyPosition.y
	dec ax;
	mov position_of_player.x, ax
	mov position_of_player.y, bx
	pop ecx
	jmp endstep
	c_9 :
	inc all_box_on_spot
	mov bl, '&'
	mov[esi], bl
		push ecx
	mov edx, esi
	sub edx, OFFSET stage
	INVOKE convert_mem_to_xy,edx
	INVOKE GetStdHandle, STD_OUTPUT_HANDLE
	mov outputHandle, eax
	INVOKE WriteConsoleOutputAttribute,
		outputHandle,
		ADDR color_red,
		1,
		xyPosition,
		ADDR bytesWritten
	pop ecx
	jmp endstep
	endstep :
	cmp bl, 0
	jz endchangechar
	add esi, 1
	jmp step
	endchangechar :
	mov ecx, 20
	mov esi, OFFSET stage
	mov xyPosition.x, 0
	mov xyPosition.y, 0
	stage_l:
		push ecx;
		INVOKE WriteConsoleOutputCharacter,
			outputHandle,
			esi,
			40,
			xyPosition,
			ADDR cellsWritten
		inc xyPosition.y
		add esi, 42
		pop ecx
	loop stage_l
	mov eax, all_box_on_spot
	add all_box, eax
	mov eax, all_box_on_spot
	add all_spot, eax
	ret
show_stage ENDP
;===========================================================================================================================
show_pause PROC

	INVOKE GetStdHandle, STD_OUTPUT_HANDLE
	mov outputHandle, eax
	mov ecx, 5
	mov esi, OFFSET stage
	mov xyPosition.x, 2
	mov xyPosition.y, 1
	pause_l_a:
		push ecx
			INVOKE WriteConsoleOutputAttribute,
			outputHandle,
			ADDR color_title,
			44,
			xyPosition,
			ADDR bytesWritten
			inc xyPosition.y
		pop ecx
		loop pause_l_a
	mov ecx, 5
	mov esi, OFFSET stage
	mov xyPosition.x, 5
	mov xyPosition.y, 14
	pause_l_a_b:
		push ecx
			INVOKE WriteConsoleOutputAttribute,
			outputHandle,
			ADDR color_title,
			40,
			xyPosition,
			ADDR bytesWritten
			inc xyPosition.y
		pop ecx
		loop pause_l_a_b
	mov ecx, 20
	mov esi, OFFSET pause_s
	mov xyPosition.x, 0
	mov xyPosition.y, 0
	pause_l_c:
		push ecx;
		push eax
		mov  eax, 10
		call Delay
		pop eax
		INVOKE WriteConsoleOutputCharacter,
			outputHandle,
			esi,
			50,
			xyPosition,
			ADDR cellsWritten
		inc xyPosition.y
		add esi, 51
		pop ecx
		loop pause_l_c
							mov bx, pause_op
							push eax
							INVOKE GetStdHandle, STD_OUTPUT_HANDLE
							mov outputHandle, eax
							mov ecx, 4
							mov xyPosition.x, 17
							mov xyPosition.y, 11
							pause_op_l_sp:
							.IF cx==bx
								push ecx
								INVOKE WriteConsoleOutputAttribute,
								outputHandle,
								ADDR pause_op_color,
								9,
								xyPosition,
								ADDR bytesWritten
								pop ecx
							.ENDIF
							.IF cx!=bx
								push ecx
								INVOKE WriteConsoleOutputAttribute,
								outputHandle,
								ADDR pause_op_blank,
								9,
								xyPosition,
								ADDR bytesWritten
								pop ecx
							.ENDIF
							dec xyPosition.y
							loop pause_op_l_sp
							pop eax
	ret
show_pause ENDP
;===========================================================================================================================
main_munu_control PROC USES eax,
		ins_c :WORD
		mov ax, ins_c
		cmp al, 'w'
		je c_up
		cmp al, 's'
		je c_down
		cmp ah, 48h ; up
		je c_up
		cmp ah, 50h ; down
		je c_down
		cmp al, 'a'
		je c_left
		cmp al, 'd'
		je c_right
		cmp ah, 4bh ; left
		je c_left
		cmp ah, 4dh ; right
		je c_right
		cmp ah, 1ch
		je c_enter
		cmp al, 'm'
		je c_mutebgm
		jmp c_end
		c_up:
			mov main_op_choose, 1
			INVOKE GetStdHandle, STD_OUTPUT_HANDLE
			mov outputHandle, eax
			mov ecx, 5
			mov esi, OFFSET color_op
			mov xyPosition.x, 11
			mov xyPosition.y, 9
			main_men_op_up:
				push ecx
				INVOKE WriteConsoleOutputAttribute,
				outputHandle,
					esi,
					25,
					xyPosition,
					ADDR bytesWritten
				inc xyPosition.y
				pop ecx
			loop main_men_op_up
			mov ecx, 5
			mov esi, OFFSET color_op
			mov xyPosition.x, 11
			mov xyPosition.y, 14
			main_men_op_down_b:
				push ecx
				INVOKE WriteConsoleOutputAttribute,
				outputHandle,
					ADDR blank_op,
					25,
					xyPosition,
					ADDR bytesWritten
				inc xyPosition.y
				pop ecx
			loop main_men_op_down_b
		jmp c_end
		c_down:
			mov main_op_choose, 2
			INVOKE GetStdHandle, STD_OUTPUT_HANDLE
			mov outputHandle, eax
			mov ecx, 5
			mov esi, OFFSET color_op
			mov xyPosition.x, 11
			mov xyPosition.y, 14
			main_men_op_down:
				push ecx
				INVOKE WriteConsoleOutputAttribute,
				outputHandle,
					esi,
					25,
					xyPosition,
					ADDR bytesWritten
				inc xyPosition.y
				pop ecx
			loop main_men_op_down
			mov ecx, 5
			mov esi, OFFSET color_op
			mov xyPosition.x, 11
			mov xyPosition.y, 9
			main_men_op_up_b:
				push ecx
				INVOKE WriteConsoleOutputAttribute,
				outputHandle,
					ADDR blank_op,
					25,
					xyPosition,
					ADDR bytesWritten
				inc xyPosition.y
				pop ecx
			loop main_men_op_up_b
		jmp c_end
		c_left:
			.IF num_bgm == 1d
				mov num_bgm, 5
				invoke  mciSendStringA, ADDR close, 0, 0, 0
				invoke  mciSendStringA, ADDR openbgm5, 0, 0, 0
			.ELSEIF num_bgm == 2d
				dec num_bgm
				invoke  mciSendStringA, ADDR close, 0, 0, 0
				invoke  mciSendStringA, ADDR openbgm1, 0, 0, 0
			.ELSEIF num_bgm == 3d
				dec num_bgm
				invoke  mciSendStringA, ADDR close, 0, 0, 0
				invoke  mciSendStringA, ADDR openbgm2, 0, 0, 0
			.ELSEIF num_bgm == 4d
				dec num_bgm
				invoke  mciSendStringA, ADDR close, 0, 0, 0
				invoke  mciSendStringA, ADDR openbgm3, 0, 0, 0
			.ELSEIF num_bgm == 5d
				dec num_bgm
				invoke  mciSendStringA, ADDR close, 0, 0, 0
				invoke  mciSendStringA, ADDR openbgm4, 0, 0, 0
			.ENDIF
		invoke  mciSendStringA, ADDR play, 0, 0, 0
		jmp c_end
		c_right:
			.IF num_bgm == 1d
				inc num_bgm
				invoke  mciSendStringA, ADDR close, 0, 0, 0
				invoke  mciSendStringA, ADDR openbgm2, 0, 0, 0
			.ELSEIF  num_bgm == 2d
				inc num_bgm
				invoke  mciSendStringA, ADDR close, 0, 0, 0
				invoke  mciSendStringA, ADDR openbgm3, 0, 0, 0
			.ELSEIF  num_bgm == 3d
				inc num_bgm
				invoke  mciSendStringA, ADDR close, 0, 0, 0
				invoke  mciSendStringA, ADDR openbgm4, 0, 0, 0
			.ELSEIF  num_bgm == 4d
				inc num_bgm
				invoke  mciSendStringA, ADDR close, 0, 0, 0
				invoke  mciSendStringA, ADDR openbgm5, 0, 0, 0
			.ELSEIF num_bgm == 5d
				mov num_bgm, 1
				invoke  mciSendStringA, ADDR close, 0, 0, 0
				invoke  mciSendStringA, ADDR openbgm1, 0, 0, 0
			.ENDIF
		invoke  mciSendStringA, ADDR play, 0, 0, 0
		jmp c_end
		c_mutebgm:
			.IF is_mute == 0
				mov is_mute, 1
				invoke  mciSendStringA, ADDR stopbgm, 0, 0, 0
			.ELSEIF is_mute == 1
				mov is_mute, 0
				invoke  mciSendStringA, ADDR rebgm, 0, 0, 0
			.ENDIF
			jmp c_end
		c_enter:
			mov al, main_op_choose
			mov next_frame, al
		c_end:
			ret
main_munu_control ENDP
;===========================================================================
stage_control PROC USES eax,
				ins_c_t : WORD
		mov ax, ins_c_t
		cmp al, 'r'
		je	reset_level
		cmp al, '`'
		je	win_level
		cmp ah, 01h
		je c_S_esc
		cmp al, 77h ; w
		je move_up
		cmp ah, 48h ; up
		je move_up
		cmp al, 73h ; s
		je move_down
		cmp ah, 50h ; down
		je move_down
		cmp al, 61h ; a
		je move_left
		cmp ah, 4bh ; left
		je move_left
		cmp al, 64h ; d
		je move_right
		cmp ah, 4dh ; right
		je move_right
		jmp c_S_end
		c_S_esc:
			call GetTickCount
			sub eax, start_time
			add pause_time, eax
			mov next_frame, 3
			jmp c_S_end
		;               0 2 4 6 8 1012
		;charline byte "  OO..##++@@&&"
		move_up:
			invoke  mciSendStringA, ADDR play_walk, 0, 0, 0
			.IF if_first_move == 1
				call GetTickCount
				mov start_time, eax
				mov if_first_move, 2
			.ENDIF
			invoke GetStdHandle, STD_OUTPUT_HANDLE
			mov outputHandle, eax
			mov ax, position_of_player.x
			mov dx, position_of_player.y
			sub dx, 1
			mov bufferpos.x, ax
			mov bufferpos.y, dx
			invoke convert_xy_to_mem, bufferpos
			mov eax, memresult
			mov bl, [stage + eax]
			mov bufferread, bl
			mov al, bufferread
			.IF bufferread == 'O'
				jmp c_S_end
			.ENDIF
			.IF bufferread == '.'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				invoke WriteConsoleOutputCharacter, outputHandle,
					offset charline+10, 2, bufferpos, ADDR cellsWritten
				invoke WriteConsoleOutputAttribute, outputHandle,
					offset color_lMan, 2, bufferpos, addr bytesWritten
				invoke changestagebyte, bufferpos, '@'
				.IF player_on_spot == 0
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+4, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_black, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '.'
				.ELSE
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+8, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_yellow, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '+'
				.ENDIF
				inc step_counter
				mov player_on_spot, 0
			.ENDIF
			.IF bufferread == '#'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				mov ax, bufferpos.X
				mov dx, bufferpos.Y
				sub dx, 1
				mov bufferpos2.X, ax
				mov bufferpos2.Y, dx
				invoke convert_xy_to_mem, bufferpos2
				mov eax, memresult
				mov bl, [stage + eax]
				mov bufferread2, bl
				.IF bufferread2 == 'O'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '.'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+6, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lC, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '#'
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 0
				.ENDIF
				.IF bufferread2 == '#'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '+'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+12, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_red, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '&'
					add all_box_on_spot, 2
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 0
				.ENDIF
				.IF bufferread2 == '&'
					jmp c_S_end
				.ENDIF
			.ENDIF
			.IF bufferread == '+'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				invoke WriteConsoleOutputCharacter, outputHandle,
					offset charline+10, 2, bufferpos, ADDR cellsWritten
				invoke WriteConsoleOutputAttribute, outputHandle,
					offset color_lMan, 2, bufferpos, addr bytesWritten
				invoke changestagebyte, bufferpos, '@'
				.IF player_on_spot == 0
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+4, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_black, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '.'
				.ELSE
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+8, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_yellow, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '+'
				.ENDIF
				inc step_counter
				mov player_on_spot, 1
			.ENDIF
			.IF bufferread == '&'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				mov ax, bufferpos.X
				mov dx, bufferpos.Y
				sub dx, 1
				mov bufferpos2.X, ax
				mov bufferpos2.Y, dx
				invoke convert_xy_to_mem, bufferpos2
				mov eax, memresult
				mov bl, [stage + eax]
				mov bufferread2, bl
				.IF bufferread2 == 'O'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '.'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+6, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lC, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '#'
					sub all_box_on_spot, 2
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 1
				.ENDIF
				.IF bufferread2 == '#'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '+'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+12, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_red, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '&'
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 1
				.ENDIF
				.IF bufferread2 == '&'
					jmp c_S_end
				.ENDIF
			.ENDIF
			dec position_of_player.Y
			jmp c_S_end
		move_down:
			invoke  mciSendStringA, ADDR play_walk, 0, 0, 0
			.IF if_first_move == 1
				call GetTickCount
				mov start_time, eax
				mov if_first_move, 2
			.ENDIF
			invoke GetStdHandle, STD_OUTPUT_HANDLE
			mov outputHandle, eax
			mov ax, position_of_player.x
			mov dx, position_of_player.y
			add dx, 1
			mov bufferpos.x, ax
			mov bufferpos.y, dx
			invoke convert_xy_to_mem, bufferpos
			mov eax, memresult
			mov bl, [stage + eax]
			mov bufferread, bl
			mov al, bufferread
			.IF bufferread == 'O'
				jmp c_S_end
			.ENDIF
			.IF bufferread == '.'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				invoke WriteConsoleOutputCharacter, outputHandle,
					offset charline+10, 2, bufferpos, ADDR cellsWritten
				invoke WriteConsoleOutputAttribute, outputHandle,
					offset color_lMan, 2, bufferpos, addr bytesWritten
				invoke changestagebyte, bufferpos, '@'
				.IF player_on_spot == 0
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+4, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_black, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '.'
				.ELSE
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+8, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_yellow, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '+'
				.ENDIF
				inc step_counter
				mov player_on_spot, 0
			.ENDIF
			.IF bufferread == '#'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				mov ax, bufferpos.X
				mov dx, bufferpos.Y
				add dx, 1
				mov bufferpos2.X, ax
				mov bufferpos2.Y, dx

				invoke convert_xy_to_mem, bufferpos2
				mov eax, memresult
				mov bl, [stage + eax]
				mov bufferread2, bl
				.IF bufferread2 == 'O'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '.'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+6, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lC, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '#'
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 0
				.ENDIF
				.IF bufferread2 == '#'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '+'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+12, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_red, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '&'
					add all_box_on_spot, 2
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 0
				.ENDIF
				.IF bufferread2 == '&'
					jmp c_S_end
				.ENDIF
			.ENDIF
			.IF bufferread == '+'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				invoke WriteConsoleOutputCharacter, outputHandle,
					offset charline+10, 2, bufferpos, ADDR cellsWritten
				invoke WriteConsoleOutputAttribute, outputHandle,
					offset color_lMan, 2, bufferpos, addr bytesWritten
				invoke changestagebyte, bufferpos, '@'
				.IF player_on_spot == 0
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+4, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_black, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '.'
				.ELSE
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+8, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_yellow, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '+'
				.ENDIF
				inc step_counter
				mov player_on_spot, 1
			.ENDIF
			.IF bufferread == '&'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				mov ax, bufferpos.X
				mov dx, bufferpos.Y
				add dx, 1
				mov bufferpos2.X, ax
				mov bufferpos2.Y, dx
				invoke convert_xy_to_mem, bufferpos2
				mov eax, memresult
				mov bl, [stage + eax]
				mov bufferread2, bl
				.IF bufferread2 == 'O'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '.'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+6, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lC, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '#'
					sub all_box_on_spot, 2
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 1
				.ENDIF
				.IF bufferread2 == '#'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '+'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+12, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_red, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '&'
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 1
				.ENDIF
				.IF bufferread2 == '&'
					jmp c_S_end
				.ENDIF
			.ENDIF
			inc position_of_player.Y
			jmp c_S_end
		move_left:
			invoke  mciSendStringA, ADDR play_walk, 0, 0, 0
			.IF if_first_move == 1
				call GetTickCount
				mov start_time, eax
				mov if_first_move, 2
			.ENDIF
			invoke GetStdHandle, STD_OUTPUT_HANDLE
			mov outputHandle, eax
			mov ax, position_of_player.x
			mov dx, position_of_player.y
			sub ax, 2
			mov bufferpos.x, ax
			mov bufferpos.y, dx
			invoke convert_xy_to_mem, bufferpos
			mov eax, memresult
			mov bl, [stage + eax]
			mov bufferread, bl
			mov al, bufferread
			.IF bufferread == 'O'
				jmp c_S_end
			.ENDIF
			.IF bufferread == '.'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				invoke WriteConsoleOutputCharacter, outputHandle,
					offset charline+10, 2, bufferpos, ADDR cellsWritten
				invoke WriteConsoleOutputAttribute, outputHandle,
					offset color_lMan, 2, bufferpos, addr bytesWritten
				invoke changestagebyte, bufferpos, '@'
				.IF player_on_spot == 0
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+4, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_black, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '.'
				.ELSE
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+8, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_yellow, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '+'
				.ENDIF
				inc step_counter
				mov player_on_spot, 0
			.ENDIF
			.IF bufferread == '#'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				mov ax, bufferpos.X
				mov dx, bufferpos.Y
				sub ax, 2
				mov bufferpos2.X, ax
				mov bufferpos2.Y, dx
				invoke convert_xy_to_mem, bufferpos2
				mov eax, memresult
				mov bl, [stage + eax]
				mov bufferread2, bl
				.IF bufferread2 == 'O'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '.'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+6, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lC, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '#'
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 0
				.ENDIF
				.IF bufferread2 == '#'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '+'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+12, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_red, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '&'
					add all_box_on_spot, 2
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 0
				.ENDIF
				.IF bufferread2 == '&'
					jmp c_S_end
				.ENDIF
			.ENDIF
			.IF bufferread == '+'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				invoke WriteConsoleOutputCharacter, outputHandle,
					offset charline+10, 2, bufferpos, ADDR cellsWritten
				invoke WriteConsoleOutputAttribute, outputHandle,
					offset color_lMan, 2, bufferpos, addr bytesWritten
				invoke changestagebyte, bufferpos, '@'
				.IF player_on_spot == 0
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+4, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_black, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '.'
				.ELSE
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+8, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_yellow, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '+'
				.ENDIF
				inc step_counter
				mov player_on_spot, 1
			.ENDIF
			.IF bufferread == '&'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				mov ax, bufferpos.X
				mov dx, bufferpos.Y
				sub ax, 2
				mov bufferpos2.X, ax
				mov bufferpos2.Y, dx
				invoke convert_xy_to_mem, bufferpos2
				mov eax, memresult
				mov bl, [stage + eax]
				mov bufferread2, bl
				.IF bufferread2 == 'O'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '.'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+6, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lC, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '#'
					sub all_box_on_spot, 2
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 1
				.ENDIF
				.IF bufferread2 == '#'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '+'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+12, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_red, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '&'
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 1
				.ENDIF
				.IF bufferread2 == '&'
					jmp c_S_end
				.ENDIF
			.ENDIF
			dec position_of_player.X
			dec position_of_player.X
			jmp c_S_end
		move_right:
			invoke  mciSendStringA, ADDR play_walk, 0, 0, 0
			.IF if_first_move == 1
				call GetTickCount
				mov start_time, eax
				mov if_first_move, 2
			.ENDIF
			invoke GetStdHandle, STD_OUTPUT_HANDLE
			mov outputHandle, eax
			mov ax, position_of_player.x
			mov dx, position_of_player.y
			add ax, 2
			mov bufferpos.x, ax
			mov bufferpos.y, dx
			invoke convert_xy_to_mem, bufferpos
			mov eax, memresult
			mov bl, [stage + eax]
			mov bufferread, bl
			mov al, bufferread
			.IF bufferread == 'O'
				jmp c_S_end
			.ENDIF
			.IF bufferread == '.'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				invoke WriteConsoleOutputCharacter, outputHandle,
					offset charline+10, 2, bufferpos, ADDR cellsWritten
				invoke WriteConsoleOutputAttribute, outputHandle,
					offset color_lMan, 2, bufferpos, addr bytesWritten
				invoke changestagebyte, bufferpos, '@'
				.IF player_on_spot == 0
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+4, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_black, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '.'
				.ELSE
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+8, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_yellow, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '+'
				.ENDIF
				inc step_counter
				mov player_on_spot, 0
			.ENDIF
			.IF bufferread == '#'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				mov ax, bufferpos.X
				mov dx, bufferpos.Y
				add ax, 2
				mov bufferpos2.X, ax
				mov bufferpos2.Y, dx

				invoke convert_xy_to_mem, bufferpos2
				mov eax, memresult
				mov bl, [stage + eax]
				mov bufferread2, bl
				.IF bufferread2 == 'O'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '.'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+6, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lC, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '#'
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 0
				.ENDIF
				.IF bufferread2 == '#'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '+'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+12, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_red, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '&'
					add all_box_on_spot, 2
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 0
				.ENDIF
				.IF bufferread2 == '&'
					jmp c_S_end
				.ENDIF
			.ENDIF
			.IF bufferread == '+'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				invoke WriteConsoleOutputCharacter, outputHandle,
					offset charline+10, 2, bufferpos, ADDR cellsWritten
				invoke WriteConsoleOutputAttribute, outputHandle,
					offset color_lMan, 2, bufferpos, addr bytesWritten
				invoke changestagebyte, bufferpos, '@'
				.IF player_on_spot == 0
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+4, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_black, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '.'
				.ELSE
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+8, 2, position_of_player, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_yellow, 2, position_of_player, addr bytesWritten
					invoke changestagebyte, position_of_player, '+'
				.ENDIF
				inc step_counter
				mov player_on_spot, 1
			.ENDIF
			.IF bufferread == '&'
				invoke GetStdHandle, STD_OUTPUT_HANDLE
				mov outputHandle, eax
				mov ax, bufferpos.X
				mov dx, bufferpos.Y
				add ax, 2
				mov bufferpos2.X, ax
				mov bufferpos2.Y, dx
				invoke convert_xy_to_mem, bufferpos2
				mov eax, memresult
				mov bl, [stage + eax]
				mov bufferread2, bl
				.IF bufferread2 == 'O'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '.'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+6, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lC, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '#'
					sub all_box_on_spot, 2
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 1
				.ENDIF
				.IF bufferread2 == '#'
					jmp c_S_end
				.ENDIF
				.IF bufferread2 == '+'
					invoke GetStdHandle, STD_OUTPUT_HANDLE
					mov outputHandle, eax
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+10, 2, bufferpos, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_lMan, 2, bufferpos, addr bytesWritten
					invoke changestagebyte, bufferpos, '@'
					invoke WriteConsoleOutputCharacter, outputHandle,
						offset charline+12, 2, bufferpos2, ADDR cellsWritten
					invoke WriteConsoleOutputAttribute, outputHandle,
						offset color_red, 2, bufferpos2, addr bytesWritten
					invoke changestagebyte, bufferpos2, '&'
					.IF player_on_spot == 0
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+4, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_black, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '.'
					.ELSE
						invoke WriteConsoleOutputCharacter, outputHandle,
							offset charline+8, 2, position_of_player, ADDR cellsWritten
						invoke WriteConsoleOutputAttribute, outputHandle,
							offset color_yellow, 2, position_of_player, addr bytesWritten
						invoke changestagebyte, position_of_player, '+'
					.ENDIF
					inc step_counter
					mov player_on_spot, 1
				.ENDIF
				.IF bufferread2 == '&'
					jmp c_S_end
				.ENDIF
			.ENDIF
			inc position_of_player.X
			inc position_of_player.X
			jmp c_S_end
		c_S_end:
		mov bufferread2, 0
		mov bufferread, 0
		mov eax, all_box_on_spot
		mov ebx, all_spot
		cmp eax, ebx
		je next_one
		ret
		win_level:
			jmp next_one
		ret
		next_one:
		.IF now_stage == 50
			invoke  mciSendStringA, ADDR play_50, 0, 0, 0
		.ELSE
			invoke  mciSendStringA, ADDR play_vic, 0, 0, 0
		.ENDIF

		call GetTickCount
			mov now_time, eax
				nest_lloop:
					mov dexyPosition.x, 0
					mov dexyPosition.y, 0
					INVOKE SetConsoleCursorPosition, outputHandle, dexyPosition
					call if_resize
					mov dexyPosition.x, 5
					mov dexyPosition.y, 3
					INVOKE SetConsoleCursorPosition, outputHandle, dexyPosition
					mov edx, offset time_info
					call writestring
					mov eax, now_time
					sub eax, start_time
					add eax, pause_time
					mov ebx, 1000
					;edx:eax / ebx
					mov edx, 0
					div ebx
					call writedec
					mov eax, '.'
					call writechar
					mov eax, edx
					call writedec
				mov dexyPosition.x, 30
				mov dexyPosition.y, 3
				INVOKE SetConsoleCursorPosition, outputHandle, dexyPosition
				mov edx, OFFSET step_info
				call writestring
				mov eax, step_counter
				call writedec
				mov dexyPosition.x, 8
				mov dexyPosition.y, 0
				INVOKE SetConsoleCursorPosition, outputHandle, dexyPosition
				mov edx, OFFSET message
				call writestring
				mov dexyPosition.x, 8
				mov dexyPosition.y, 1
				INVOKE SetConsoleCursorPosition, outputHandle, dexyPosition
				mov edx, OFFSET message_2
				call writestring
				mov  eax, 50
				call Delay
				call ReadKey
				jz   nest_lloop
				cmp al, 'r'
				je	reset_level
				cmp ah, 1ch
				jne nest_lloop

			mov al, now_stage
			.IF al < 50
			inc al
			.ELSE
				mov next_frame, 0
				ret
			.ENDIF
			mov now_stage, al
			add al, '0'
			.IF al<10 + '0'
				mov stage_chose+8, al
			.ENDIF
			.IF al>=10 + '0' && al<20 + '0'
			sub al, 10
				mov stage_chose + 7, 1+'0'
				mov stage_chose + 8, al
			.ENDIF
			.IF al >= 20 + '0' && al<30 + '0'
			sub al, 20
			mov stage_chose + 7, 2+'0'
			mov stage_chose + 8, al
			.ENDIF
			.IF al >= 30 + '0' && al<40 + '0'
			sub al, 30
			mov stage_chose + 7, 3+'0'
			mov stage_chose + 8, al
			.ENDIF
			.IF al >= 40 + '0' && al<50 + '0'
			sub al, 40
			mov stage_chose + 7, 4+'0'
			mov stage_chose + 8, al
			.ENDIF
			.IF al >= 50 + '0' && al<51 + '0'
			sub al, 50
			mov stage_chose + 7, 5+'0'
			mov stage_chose + 8, al
			.ENDIF
			reset_level:
			invoke  mciSendStringA, ADDR play_retry, 0, 0, 0
			mov next_frame, 1
	ret
stage_control ENDP
;===========================================================================
pause_munu_control PROC USES eax ebx,
				ins_c_s : WORD
		mov ax, ins_c_s
		mov bx, pause_op
		cmp ah, 01h
		je c_esc
		cmp al, 'w'
		je c_up_down
		cmp al, 's'
		je c_up_down
		cmp ah, 48h; up
		je c_up_down
		cmp ah, 50h; down
		je c_up_down
		cmp ah, 1ch
		je c_D_enter
		jmp c_D_end
			c_esc:
				call GetTickCount
				mov start_time, eax
				mov next_frame, 11h
				jmp c_D_end
			c_up_down:
				.IF bx != 0
					.IF bx==1
						.IF al == 's' || ah == 50h
							mov bx, 2
							jmp c_color
						.ENDIF
					.ENDIF
					.IF bx == 2
						.IF al == 'w' || ah == 48h
							mov bx, 1
							jmp c_color
						.ENDIF
						.IF al == 's' || ah == 50h
							mov bx, 3
							jmp c_color
						.ENDIF
					.ENDIF
					.IF bx == 3
						.IF al == 'w' || ah == 48h
							mov bx, 2
							jmp c_color
						.ENDIF
						.IF al == 's' || ah == 50h
							mov bx, 4
							jmp c_color
						.ENDIF
					.ENDIF
					.IF bx == 4
						.IF al == 'w' || ah == 48h
							mov bx, 3
							jmp c_color
						.ENDIF
					.ENDIF
				.ENDIF
				.IF bx == 0
					mov bx, 1
					jmp c_color
				.ENDIF
					jmp c_color
			c_D_enter:
				.IF bx == 1
					mov next_frame, 1
				.ENDIF
				.IF bx == 2
					mov al, now_stage
					.IF al < 50
						inc al
					.ENDIF
					mov now_stage, al
					add al, '0'
					.IF al<10 + '0'
						mov stage_chose+8, al
					.ENDIF
					.IF al>=10 + '0' && al<20 + '0'
					sub al, 10
						mov stage_chose + 7, 1+'0'
						mov stage_chose + 8, al
					.ENDIF
					.IF al >= 20 + '0' && al<30 + '0'
					sub al, 20
					mov stage_chose + 7, 2+'0'
					mov stage_chose + 8, al
					.ENDIF
					.IF al >= 30 + '0' && al<40 + '0'
					sub al, 30
					mov stage_chose + 7, 3+'0'
					mov stage_chose + 8, al
					.ENDIF
					.IF al >= 40 + '0' && al<50 + '0'
					sub al, 40
					mov stage_chose + 7, 4+'0'
					mov stage_chose + 8, al
					.ENDIF
					.IF al >= 50 + '0' && al<51 + '0'
					sub al, 50
					mov stage_chose + 7, 5+'0'
					mov stage_chose + 8, al
					.ENDIF
					mov next_frame, 1
				.ENDIF
				.IF bx == 3
					mov next_frame, 0
				.ENDIF
				.IF bx == 4
					mov next_frame, 2
				.ENDIF
				jmp c_D_end
			c_color:
							push eax
							INVOKE GetStdHandle, STD_OUTPUT_HANDLE
							mov outputHandle, eax
							mov ecx, 4
							mov xyPosition.x, 17
							mov xyPosition.y, 11
							pause_op_l:
							.IF cx==bx
								push ecx
								INVOKE WriteConsoleOutputAttribute,
								outputHandle,
								ADDR pause_op_color,
								9,
								xyPosition,
								ADDR bytesWritten
								pop ecx
							.ENDIF
							.IF cx!=bx
								push ecx
								INVOKE WriteConsoleOutputAttribute,
								outputHandle,
								ADDR pause_op_blank,
								9,
								xyPosition,
								ADDR bytesWritten
								pop ecx
							.ENDIF
							dec xyPosition.y
							loop pause_op_l
							pop eax
		c_D_end:
			mov pause_op,bx
	ret
pause_munu_control ENDP
;===========================================================================================================================
clear_all_thing PROC
	INVOKE GetStdHandle, STD_OUTPUT_HANDLE
	mov outputHandle, eax
	mov ecx, 30
	mov xyPosition.x, 0
	mov xyPosition.y, 0
	clear_a_a:
		push ecx
			INVOKE WriteConsoleOutputAttribute,
			outputHandle,
			ADDR clear_all_attr,
			120,
			xyPosition,
			ADDR bytesWritten
			inc xyPosition.y
		pop ecx
		loop clear_a_a
	mov ecx, 30
	mov xyPosition.x, 0
	mov xyPosition.y, 0
	clear_a_c:
		push ecx
			INVOKE WriteConsoleOutputCharacter,
			outputHandle,
			ADDR clear_all_char,
			120,
			xyPosition,
			ADDR cellsWritten
			inc xyPosition.y
		pop ecx
		loop clear_a_c
ret
clear_all_thing ENDP
;================================================================================
if_resize PROC
		INVOKE GetStdHandle, STD_OUTPUT_HANDLE
		mov outputHandle, eax
		INVOKE GetConsoleScreenBufferInfo, outputHandle, ADDR screen_info
		movzx ax, screen_info
		movzx bx, screen_info+16
		cmp old_screen_r.x, ax
		jne change_r
		cmp old_screen_r.y, bx
		jne change_r
		ret
		change_r:
			mov old_screen_r.x, ax
			mov old_screen_r.y, bx
			.IF now_frame == 00h
				call clear_all_thing
				call show_main_menu
			.ENDIF
			.IF  now_frame == 01h
				call clear_all_thing
				call show_stage
			.ENDIF
			.IF  now_frame == 03h
				call clear_all_thing
				call show_pause
			.ENDIF
			.IF  now_frame == 11h
				call clear_all_thing
				call show_stage
			.ENDIF

		ret
if_resize ENDP
;===================================================================================
convert_mem_to_xy PROC USES eax ebx edx, mem: dword
	mov eax, mem
	mov ebx, 42
	mov edx, 0
	div ebx
	mov xyPosition.x, dx
	mov xyPosition.y, ax
	ret
convert_mem_to_xy ENDP
;===================================================================================
convert_xy_to_mem PROC USES eax ebx edx, xy_in: coord
	; the result is in memresult
	mov ax, xy_in.y
	mov bx, 42
	mul bx
	and eax, 0000ffffh
	shl edx, 16
	or eax, edx
	movzx ebx, xy_in.x
	add eax, ebx
	mov memresult, eax
	ret
convert_xy_to_mem ENDP
;===================================================================================
changestagebyte proc uses eax ebx ecx edx, co:COORD, charuse:byte
	invoke convert_xy_to_mem, co
	mov eax, offset stage
	add eax, memresult
	mov bl, charuse
	mov [eax], bl
	add eax, 1
	mov [eax], bl
	ret
changestagebyte endp


end main
