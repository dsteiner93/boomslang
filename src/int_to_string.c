#include <stdio.h>
#include <stdlib.h>
char *int_to_string(int i)
{
    char *str = malloc(11 * sizeof(char)); /* max len for int is 10 */
    snprintf(str, sizeof(str), "%d", i);
    return str;
}
