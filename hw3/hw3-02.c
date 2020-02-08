#include <stdio.h>

int gcd(int x, int y)
{
/*if y == 0
return x;
else
return gcd(y, x % y);
*/
return y==0 ? x: gcd(y, x % y);
 }


int main(void){
	int a,b,c;
	printf("Enter two non-zero, positive integer values: \n");
	scanf("%d", &a);
	scanf("%d", &b);
	/*int gcd(int x, int y)
	{
	if y == 0
	return x;
	else 
	return gcd(y, x % y);

	}*/


	printf("%d\n", gcd(a,b));
	return 0;

}
