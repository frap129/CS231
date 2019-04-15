/* Joseph Maples */

#include <stdio.h>

int main(int argc, char *argv[]) {
	double price;
	int bottles;
    printf("Please enter the cost of one bottle of Pepsi: $");
    scanf("%le", &price);
    printf("Please enter the number of bottles to purchase: ");
    scanf("%d", &bottles);

    double total = price * bottles;
    printf("For %d bottles of Pepsi at $%.2f per bottle, the total cost is $%.2f. \n", bottles, price, total);
    printf("Written by Joseph Maples\n");
}