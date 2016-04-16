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
 dummy dw 0
 mul_factor dw 0  
 dummy2 dw 0
 price_if_equal dw 0 
 ddd db 0
 aldummy db 0
 
 total_transaction dw 2 dup(0)
 
last_string db 20 dup(0),'$'
last_string_count dw 0

number_of_items dw 0
item_code db 0

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
		 
		       mov ax,0           ;clearing
		 
		     call initialize_LCD 
		       
		      call initialize_8255_2                 
               
               call initialize_8259
         
		    call display_system_ready
		    
		  
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
                
                ja transact
                
            
                cmp dx,0d7c0h
                
                jb transact
                je mode
                
                call transaction
                jmp repeat_till_off 
	        
	        prog:
	        
	         call program
	         ;call display_select_mode
	        jmp mode
		 
		 
		 

 cancel_key:
            call clear_display
            
            call delay_30ms
            
            
            ;;mov al,00100000b
            ;out 10,al 
 
 
 
 iret
 

          
delay_40us proc
          push ax
  
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
		  pop ax
		  ret

endp 


delay_30ms proc
      push ax
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
    
    
           pop ax
    
    ret
    
endp




initialize_LCD proc    
    
    push ax             ;store original ax
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
		              
		 

       pop ax     ;store original ax
	ret

endp  




initialize_LCD_opp proc    
    
    push ax     ;store ax
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
		              
		 
          pop ax      ;store ax

	ret

endp
display_char proc
    
    
    push ax
    mov al,00000101b
    out 02,al
    call delay_30ms
    mov al,[si]
    out 00,al
    
    call delay_30ms
    
    mov al,00000100b
    out 02,al 
    pop ax
    ret
    
    
    
endp

initialize_8259 proc
	 
	push ax 
	mov al,13h  ; ICW1
	out 10h,al

	mov al,80h	; ICW2 vector starting at 80h
	out 12h,al
               
    
	mov al,03h	;ICW4
	out 12h,al

	mov al, 0feh	;OCW1 Only interrupt-0 is unmasked 
	out 12h,al
    sti
    pop ax
	ret
endp

initialize_8255_2 proc
     push ax
	mov al,10001001b
	out 1eh,al

	
	  pop ax
	ret

endp


clear_display proc
     
     ;push ax              ;;dummying
     call delay_30ms   
     
         mov al,01h
         out 02,al
         
         mov al,01h
         out 00,al
         
         mov al,00h
         out 02,al  
         
         
     call delay_30ms 
     ;pop ax
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
    
    push ax
    push bx
    push cx
    push si
                
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
    ;lea si,decode_table
    ;mov bl,0  
    
    pop si
    pop cx
    pop bx
    pop ax
    
         
    ret
    
    
endp  

display_select_mode proc
      
      push si
     call clear_display 
      
    call delay_30ms
    
    lea si,select_mode
    sel_mode:
    call display_char 
    inc si
    cmp [si],'$'
    jnz sel_mode
    
    push ax                ;;test
    push si
    mov ax,number_of_items
    lea si,ddd
    mov [si],al
    add [si],'0'
    call display_char
    
    pop si
    pop ax    
    
    pop si
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

    
    
    lea si,last_string_count 
    mov word ptr[si],0
    
    lea si,number_of_items 
    mov word ptr[si],0 
    
    lea si,dummy 
    mov word ptr[si],0 
    
    lea si,dummy2 
    mov word ptr[si],0  
    
    lea si,total_transaction 
    mov word ptr[si],0              
    
    lea si,mul_factor 
    mov word ptr[si],0 
    
    ret
    
    
endp 



display_transact_confirm proc
    
    push ax
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
   pop ax
        
    ret
endp


transaction proc
  push ax
  push si
  lea si,total_transaction
  mov word ptr[si],0
  mov word ptr[si+2],0
  look_for_item_no_key: 
  
    call clear_display
    
    
   total_or_itemno:
   
    call get_input
    cmp dx,0b7c0h
    
    jz show_total
    
    
    cmp dx,0bdc0h
    
    jnz total_or_itemno
    
    
    
    call initialize_LCD
    
    call input_item_number
    ;call clear_display
    call check_for_item_number  ;;bp will have address of itemNO + 8,
      
    
    
    
    
    
    cmp ax,0
    jz total_or_itemno
     
    call clear_display                         
    
    mov bx,price_if_equal
    
    press_quantity:
    
    call get_input
    cmp dx,0bec0h 
    jnz press_quantity
    
    push si           ;test
    lea si,ddd 
    mov [si],'v'
    call display_char
    pop si
    call get_price_or_quantity   ;last_string has quantity in string form  
    push si
    
    lea si,ddd 
     
     mov [si],'v'
    
    call display_char
    
    pop si
    
    call convert_string_to_number  ;convert to a 16-bit unsigned integer in ax 
    push si           ;test
    lea si,ddd 
    mov [si],'v'
    call display_char
    pop si
    
    call  add_to_total
    push si           ;test
    lea si,ddd 
    mov [si],'v'
    call display_char
    pop si
    
    jmp total_or_itemno
        
    show_total:
    push si           ;test
    lea si,ddd 
    mov [si],'v'
    call display_char
    pop si
    call convert_number_to_string
    call show_last_string
     
    push si
    push bx
    lea si,ddd 
     mov bx,last_string_count
     mov [si],bl
    add [si],'a'
    call display_char
    pop bx
    pop si
    ;lea di,last_string 

    ;lea si,last_string
    ;add si,last_string_count
    cmp si,0
    jz display_0
    
    
    
    print_total:  
        dec si
       call display_char
    
        
        cmp di,si
        
        jnz print_total
        jz return6
    
    display_0:
    
    lea si,last_string
    mov [si],'0'
    call display_char
    
    
    return6: pop si
    pop ax
    ret
    
endp 


program proc
    push ax
    push si
    inp3:
    
    call get_input
    
    cmp dx,07bc0h
    jz delete
    
    
    
    cmp dx,07dc0h
    jnz inp3
    
    call clear_display
    
    press_item_no:
    
        call get_input
        cmp dx,0bdc0h
        jnz press_item_no
        
        call  input_item_number
        
        
        call clear_display
        
        
        
        call check_for_item_number  
        
        
        
        
         
        cmp ax,0
        jz add_new
        
        call delete_item
        sub di,10 
        
        add_new:
        mov cx,8
        lea si,last_string 
        addd:
          mov bx,[si]
          mov [di],bx
          inc si
          inc di
          dec cx
          jnz addd
         
         
        mov dummy2,di  
        
        check_cost:
        call get_input
        cmp dx,077c0h
        
        jnz check_cost
        
        call clear_display
         
        call get_price_or_quantity
        
        
        call convert_string_to_number 
        
        mov di,dummy2
        
        mov word ptr[di],ax
        
        inc number_of_items
        
       
       jmp return5
       
       
       
    
    delete:
      press_item_no2:
        
        call clear_display
        
        call get_input 
        
        cmp dx,0bdc0h
        
        jnz press_item_no2
        
        call  input_item_number
        
        
        
        call clear_display
        
        call check_for_item_number 
        cmp ax,0
        jz return5
        
        call delete_item
        
    
    return5: pop si
    pop ax
    ret
    
endp



backspace proc
    push ax
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
        
    return:
    pop ax
     ret
endp 

input_item_number proc
    push dx
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
    
       pop dx
    
    ret
endp


convert_string_to_number proc   ;Convert a string to number with length in last_string_count
    
    push bx
    push cx
    mov bx,0
    
    lea di,last_string
    
    mov ax,0 
    mov cx,10
    partial_multiply:
        mul cx 
        sub byte ptr [di+bx],'0'
        add ax,[di+bx] 
        
        inc bx
        cmp bx,last_string_count
        jnz partial_multiply
    
    
    pop cx
    pop bx
            
    ret
    
endp  



check_for_item_number proc ;Find an item whos no is in the last_sting
    
   
    
    lea di,item_code  
    mov price_if_equal,0 
    mov bp,di
    
    
    lea si,last_string
    mov ax, number_of_items 
    
     
    cmp ax,0
    jz here 
    
    
    
    
    mov cx,4
    check_for_item:
          mov bx,[si]  
          cmp [bp+0],bx
          jnz not_equal
          add bp,2
           
          add si,2
          dec cx
          jz equal
          jmp check_for_item
        
    
     not_equal:
     
      add di,10 
     mov bp,di 
     lea si,last_string
     mov cx,4
     dec ax
     jnz check_for_item
     jmp here
     
      equal: 
      
     lea si,price_if_equal 
     mov cx,  word ptr[bp+0]
     mov [si], cx
      
     
    here:  
    
    
    
    ret
endp  


add_to_total proc   ;total_transaction +=  ax*bx
    
    lea di,total_transaction
    
    mul bx
    
    
    add [di],dx
    add [di+2],ax
    ret
    
endp



get_price_or_quantity proc
    
    push cx
    push si
    lea si,last_string
    mov last_string_count,0
    inp2:
    
   
   
    call get_input
    
    cmp dx,0bf40h
    jnz check_for_enter
    
    call backspace
    
    jmp inp2
    
    check_for_enter:  
    
    cmp dx,0bf80h
    jz return3
    
    cmp dx,0ddc0h
    jb inp2
    
    lea di,decode_table
    mov cl,0
    
    check3: 
    
        cmp dx,[di]
        jz print_last_string2
        
        inc cl
        add di,2        
        
        jmp check3
        
    print_last_string2: 
    
        add cl,'0'
        mov [si],cl
     
        call display_char
     
        inc last_string_count
       
        inc si 
        
     jmp inp2
     
           
    return3:pop si
    pop cx 
    ret
endp 



delete_item proc 
    
    push bx
    push cx
    push dx
    push di
    push bp
    sub bp,8   ;bp has the address after it is checked equal
    mov ax,number_of_items
    dec ax
    mov cx,10
    mul cx
    lea di,item_code
    add di,ax
    mov cx,10
    
    swap:
      mov bx,[di]
      mov [bp],bx
      inc bp
      inc di
      dec cx
      jnz swap
      
    dec number_of_items
    
    return4: pop bp
    pop di
    pop dx
    pop cx
    pop bx
    ret
    
endp




convert_number_to_string proc
    push cx
    push si
    lea si,last_string
    mov last_string_count,0
    
    
    mov cx,10  
    partial_divide:
    div cx
    mov [si],dx
    add [si],'0'
    inc si
    inc last_string_count
    cmp ax,0
    ja partial_divide 
    pop si
    pop cx
    ret
endp 


show_last_string proc
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push bp
   
   
   lea si,last_string
   mov cx,0
   
   call clear_display
   
   pr:
   
   call display_char
   inc si
   inc cx
   cmp cx,last_string_count
   jnz pr
    
    
    
    push bp
    push di
    push si
    push dx
    push cx
    push bx
    push ax
    ret
    
endp


