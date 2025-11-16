
#include<stdlib.h>

extern void scroller_init(void);
extern void scroller_spin(void);

int main()
{
    scroller_init();
    while (1) { scroller_spin(); }
}