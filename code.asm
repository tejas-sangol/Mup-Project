#make_bin#

#LOAD_SEGMENT=FFFFh#
#LOAD_OFFSET=0000h#

#CS=0000h#
#IP=0000h#

#DS=0000h#
#ES=0000h#

#SS=0000h#
#SP=FFFEh#

#AX=0000h#
#BX=0000h#
#CX=0000h#
#DX=0000h#
#SI=0000h#
#DI=0000h#
#BP=0000h#
  
  



; add your code here
         jmp     st1 
         db     509 dup(0)

         dw     cancel_key
         dw     0000
         db     508 dup(0) 
         st1:      cli 
;main program

 
system_ready 	db 'system ready$'
select_mode 	db "SELECT MODE", '$'
spaces          db "                ",'$'
transact_confirm db "ENTER TRANSACTION MODE Y/N ?" ,'$'  
decode_table dw 0ef80h,0ef40h,0eec0h,0edc0h,0ebc0h,0e7c0h,0df80h,0df40h,0dec0h,0ddc0h,0dbc0h,0d7c0h,0bf80h,0bf40h,0bec0h,0bdc0h,0bbc0h,0b7c0h,07f80h,07f40h,07ec0h,07dc0h,07bc0h,077c0h
 dummy db 0


last_string db 20 dup(0),'$'
last_string_count dw 0

number_of_items dw 0
item_cost dw 100 dup(0)
item_nos db 0 

; intialize ds, es,ss to start of RAM
          mov       ax,0200h
          mov       ds,ax
          mov       es,ax
          mov       ss,ax 
          mov       sp,0FFFEH
;intialise portb as input &portc as output
          mov       al,10001000b
		  out 		06h,al 
 
            mov al, 000000000b
            out 04,al
		 
		  mov ah,5
		 call initialize_data 
		 
		    
		 repeat_till_off: 
		 
		     
		 
		     call initialize_LCD
		                      
		      call initialize_8255_2                 
               
               call initialize_8259
         
		    call display_system_ready
		    lea si,dummy
		    mov [si],07fh
		    call display_char
		  
		    get_mode:  
		    
		        call get_input
		    
		        cmp dx, 7f80h
		    
		        jnz get_mode
		    
		    
		    mode:
		    
		    
             call initialize_LCD 
		    call display_select_mode
            trans_program:
            
                call get_input
            
                cmp dx,7f40h
                
                je transact
            
                cmp dx,7ec0h
                
                je prog
                
               jmp trans_program
         
            
          
	      transact: 
	            
	            call initialize_LCD_opp
                call display_transact_confirm
	        
	            transact2 :
	            call get_input
	         
	            cmp dx,0dbc0h
                
                jg transact
                
            
                cmp dx,0d7c0h
                
                jl transact
                je mode
                
                call transaction
                jmp repeat_till_off 
	        
	        prog:
	        
	         call program
	         ;call display_select_mode
	        jmp mode
		 
		 
		 jmp repeat_till_off

 cancel_key:
            call clear_display
            
            call delay_30ms
            
            
            ;;mov al,00100000b
            ;out 10,al 
 
 
 
 iret
 

          
delay_40us proc

  
         mov       al,00110001b
		  out       0Eh,al
		  mov       al,00h
		  out       08h,al
		  mov       al,90h
		  out       08h,al 
		  mov al,00000001b
		  out 04,al
		  
		  x2:
		  
		  in al, 04h
		  and al,80h
		  jz x2
		  ret

endp 


delay_30ms proc
    
    mov       al,00110001b
		  out       0Eh,al
		  mov       al,00h
		  out       08h,al
		  mov       al,07h
		  out       08h,al 
		  mov al,00000001b
		  out 04,al
		  
		  x300:
		  
		  in al, 04h
		  and al,80h
		  jz x300
    
    
    
    
    ret
    
endp




initialize_LCD proc    
    
    
    mov al,00000001b
		 out 02,al
		     
	mov al,30h
		 out 00,al   
		  
		  mov al,00000000b
		 out 02,al
		 call delay_30ms 
		           
		 mov al,00000001b
		 out 02,al 
		 
		 
		 mov al,0eh
		 out 00,al  
		  
		  mov al,00000000b
		 out 02,al         
		 call delay_30ms 
		 
		 mov al,00000001b
		 out 02,al
		 mov al,06h
		 out 00,al  
		  
		  mov al,00000000b
		 out 02,al

		 call delay_30ms

		   mov al,00000001b
		 out 02,al
		 
		 mov al,01h
		 out 00,al
		    
		    mov al,00000000b
		 out 02,al
		 
		 call delay_30ms 
		              
		 


	ret

endp  




initialize_LCD_opp proc    
    
    
    mov al,00000001b
		 out 02,al
		     
	mov al,30h
		 out 00,al   
		  
		  mov al,00000000b
		 out 02,al
		 call delay_30ms 
		           
		 mov al,00000001b
		 out 02,al 
		 
		 
		 mov al,0eh
		 out 00,al  
		  
		  mov al,00000000b
		 out 02,al         
		 call delay_30ms 
		 
		 mov al,00000001b
		 out 02,al
		 mov al,07h
		 out 00,al  
		  
		  mov al,00000000b
		 out 02,al

		 call delay_30ms

		   mov al,00000001b
		 out 02,al
		 
		 mov al,01h
		 out 00,al
		    
		    mov al,00000000b
		 out 02,al
		 
		 call delay_30ms 
		              
		 


	ret

endp
display_char proc
    
    
    
    mov al,00000101b
    out 02,al
    call delay_30ms
    mov al,[si]
    out 00,al
    
    call delay_30ms
    
    mov al,00000100b
    out 02,al
    ret
    
    
    
endp

initialize_8259 proc
	
	mov al,13h  ; ICW1
	out 10h,al

	mov al,80h	; ICW2 vector starting at 80h
	out 12h,al
               
    
	mov al,03h	;ICW4
	out 12h,al

	mov al, 0feh	;OCW1 Only interrupt-0 is unmasked 
	out 12h,al
    sti
	ret
endp

initialize_8255_2 proc

	mov al,10001001b
	out 1eh,al

	
	
	ret

endp


clear_display proc
     
     
     call delay_30ms   
     
         mov al,01h
         out 02,al
         
         mov al,01h
         out 00,al
         
         mov al,00h
         out 02,al  
         
         
     call delay_30ms
	ret

endp  



display_system_ready proc  
    
    call clear_display 
    
   
    call delay_30ms
    
    lea si,system_ready
    sys_ready:
    
    call display_char
    inc si
    cmp [si],'$'
    jnz sys_ready                    
    ret
    
    
endp    




get_input proc   ; bx will have the index of the number in the decode_table
              
    mov al,00h
    out 18h,al
       
    
    i2:
    in al,1ch
    and al,0f0h
    out 1Ah,al
    cmp al,0f0h  
    jnz i2
    
    
    
      
       
    
    mov al,00h
    out 18h,al

    i3:
    in al,1ch
    and al,0f0h
    cmp al,0f0h
    jz i3
    
     
    
     

    ;col?
    
    mov al,11111000b
    mov bl,al
    mov cl,al
    out 18h,al
    in al,1Ch
    and al,0f0h
    cmp al,0f0h
    jnz i4

    mov al,11110100b
    mov bl,al
    mov cl,al
    out 18h,al
    in al,1Ch
    and al,0f0h
    cmp al,0f0h
    jnz i4

    mov al,11101100b
    mov bl,al
    mov cl,al
    out 18h,al
    in al,1Ch
    and al,0f0h
    cmp al,0f0h
    jnz i4
     
      
    mov al,11011100b
    mov bl,al
    mov cl,al
    out 18h,al
    in al,1Ch
    and al,0f0h
    cmp al,0f0h
    jnz i4

    mov al,10111100b
    mov bl,al
    mov cl,al
    out 18h,al
    in al,1Ch
    and al,0f0h
    cmp al,0f0h
    jnz i4

    mov al,01111100b
    mov bl,al
    mov cl,al
    out 18h,al
    in al,1Ch
    and al,0f0h
    cmp al,0f0h
    jz i3

    i4:
    
    ror bl,1
    ror bl,1
    ror bl,1
    ror bl,1
    and bl,0fh
    or al,bl
    mov dh,al
    ror cl,1
    ror cl,1
    ror cl,1
    ror cl,1
    and cl,0f0h
    mov dl,cl
    lea si,decode_table
    mov bl,0  
    
    

    
         
    ret
    
    
endp  

display_select_mode proc
    
     call clear_display 
      
    call delay_30ms
    
    lea si,select_mode
    sel_mode:
    call display_char 
    inc si
    cmp [si],'$'
    jnz sel_mode
    
    ret
    
endp


initialize_data proc
    
    lea si,system_ready
     mov [si],'S'
    
                      
    mov [si+1],'Y'
    
    
    mov [si+2],'S'
    
    
    mov [si+3],'T'
    
    
    mov [si+4],'E'
    
    
    mov [si+5],'M'
   
    
    mov [si+6],' '
    
    
    mov [si+7],'R'
    
   
    mov [si+8],'E'
    
    
    mov [si+9],'A'
    
    
    mov [si+10],'D'
    
                      
    mov [si+11],'Y'
    
    mov [si+12],'$'  
    
    
    lea si, select_mode
    
    mov [si],'S'
    
                      
    mov [si+1],'E'
    
    
    mov [si+2],'L'
    
    
    mov [si+3],'E'
    
    
    mov [si+4],'C'
    
    
    mov [si+5],'T'
   
    
    mov [si+6],' '
    
    
    mov [si+7],'M'
    
   
    mov [si+8],'O'
    
    
    mov [si+9],'D'
    
    
    mov [si+10],'E'
    
     mov [si+11],'$'
     
     lea si,transact_confirm

     mov [si],'T'

    mov [si+1],'R'
    
    
    mov [si+2],'A'
    
    
    mov [si+3],'N'
    
    
    mov [si+4],'S'
    
    
    mov [si+5],'A'
   
    
    mov [si+6],'C'
    
    
    mov [si+7],'T'
    
   
    mov [si+8],' '
    
    
    mov [si+9],'C'
    
    
    mov [si+10],'O'
    
                      
    mov [si+11],'N'
    
    mov [si+12],'F' 

    mov [si+13],'I'
    
    
    mov [si+14],'R'
    
    
    mov [si+15],'M'
    
    
    mov [si+16],' '
    
    
    mov [si+17],'Y'
   
    
    mov [si+18],'/'
    
    
    mov [si+19],'N'
    
   
    mov [si+20],' '
    
    
    mov [si+21],'?'
    
    
    mov [si+22],'$'
    
                      
    lea si,decode_table
      
    mov [si],0ef80h

    mov [si+2],0ef40h
    
    
    mov [si+4],0eec0h
    
    
    mov [si+6],0edc0h
    
    
    mov [si+8],0ebc0h
    
    
    mov [si+10],0e7c0h
   
    
    mov [si+12],0df80h
    
    
    mov [si+14],0df40h
    
   
    mov [si+16],0dec0h
    
    
    mov [si+18],0ddc0h
    
    
    mov [si+20],0dbc0h
    
                      
    mov [si+22],0d7c0h
    
    mov [si+24],0bf80h

	mov [si+26],0bf40h
    
                      
    mov [si+28],0bec0h
    
    
    mov [si+30],0bdc0h
    
    
    mov [si+32],0bbc0h
    
    
    mov [si+34],0b7c0h
    
    
    mov [si+36],07f80h
   
    
    mov [si+38],07f40h
    
    
    mov [si+40],07ec0h
    
   
    mov [si+42],07dc0h
    
    
    mov [si+44],07bc0h
    
    
    mov [si+46],077c0h

    
    ret
    
    
endp 



display_transact_confirm proc
    call clear_display   
    
    mov al,01h
    out 02,al
    
    mov al,8fh
    out 00,al
    
    mov al,00h
    out 02,al 
    
    call delay_30ms
    mov al,01h
    out 02,al
    
    mov al,07h
    out 00,al
    
    mov al,00h
    out 02,al
    
    lea si,transact_confirm
    trans:
        call display_char
        
        inc si
        
        cmp [si],'$'
        jnz trans
       
    ret
endp


transaction proc
  
  
  look_for_item_no_key: 
  
   
    call get_input
    cmp dx,0bdc0h
    
    jnz look_for_item_no_key
    
    call clear_display
    
    call initialize_LCD
    
    call input_item_number
    
    
    ret
    
endp 


program proc
    
    
    
    ret
    
endp



backspace proc
    
    lea di,last_string_count 
    
    cmp [di],0
    je return 
    
    dec si
    mov [si],0
    
    call delay_30ms
    mov al,01h
    out 02h,al
    
    mov al,10h
    out 00h,al
    
    mov al,00h
    out 02,al
    
    call delay_30ms
    
    mov di,si        ;;Hold si value in order to resrore it before return from the method
    
    dec last_string_count
    lea si, dummy
    
    mov [si],' '
    call display_char
    
    call delay_30ms
    mov al,01h
    out 02h,al
    
    mov al,10h
    out 00h,al
    
    mov al,00h
    out 02,al  
    
    mov si,di
        
    return: ret
endp 

input_item_number proc
    
    lea si,last_string   
    
    mov last_string_count,0
    
    inp:
    
    
    call get_input 
    
    
    check_item_no_backspace:
        cmp dx,0bf40h
        jnz  take
        
        
        
        call backspace 
        
        
    
        jmp inp 
        
    take:  
    cmp dx,0ddc0h
    jb inp
    
    lea di,decode_table
    mov cl,0
    
    check2: 
    
        cmp dx,[di]
        jz print_last_string
        
        inc cl
        add di,2        
        
        jmp check2
        
    print_last_string: 
    
        add cl,'0'
        mov [si],cl
     
        call display_char
     
        inc last_string_count
       
        inc si   
        
    cmp last_string_count,8
    jb inp
    
    look_for_enter:
        call get_input
        
        cmp dx,0bf80h
        jnz look_for_enter 
    
      
    
    ret
endp


convert_string_to_number proc   ;Convert a string to number with length in CX
    
     
    ret
    
endp