#include <stdio.h>

int main(void){
	int a, b, c;
	printf("Enter three integers: \n");
	scanf("%d", &a);
	scanf("%d", &b);
	scanf("%d", &c);
	
	int last, middle, first;
	last = a<b && a<c ? a: b<a && b<c ? b: c;
	middle = b>a && a>c ? a: b>c && c>a ? c: a>b && b>c ? b: a>c && c>b ? c: c>a && a>b ? a: c>b && b>a ? b:0;	
	first = a>b && a>c ? a: b>a && b>c ? b: c;

	printf("%d, %d, %d\n",last,middle,first);
	
	return(0);

}
