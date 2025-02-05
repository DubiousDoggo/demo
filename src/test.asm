
.export _text
_text: .asciiz "HELLO WORLD!"

.export _func1
_func1:
    clc
    adc #1
    rts