#include<stdio.h>
#include<stdlib.h>

typedef unsigned char uint8_t;

extern const char text[];

extern uint8_t func1(uint8_t in);

int main()
{
    puts(text);
    printf("%u", func1(7));
    return EXIT_SUCCESS;
}