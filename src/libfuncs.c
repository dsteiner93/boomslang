#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int println(char *s)
{
    printf("%s\n", s);
}

char *int_to_string(int i)
{
    char *str = malloc(11 * sizeof(char)); /* max len for int is 10 */
    snprintf(str, 11 * sizeof(char), "%d", i);
    return str;
}

char *long_to_string(long l)
{
    char *str = malloc(24 * sizeof(char));
    snprintf(str, 24 * sizeof(char), "%li", l);
    return str;
}

char *float_to_string(double f)
{
    char *str = malloc(24 * sizeof(char));
    snprintf(str, 24 * sizeof(char), "%.4f", f);
    return str;
}

char *char_to_string(char c)
{
    char *str = malloc(2 * sizeof(char));
    snprintf(str, 2 * sizeof(char), "%c", c);
    return str;
}

char *bool_to_string(int b)
{
    if (b) return "true"; else return "false";
}

long int_to_long(int i)
{
    return (long) i;
}

float int_to_float(int i)
{
    return (float) i;
}

// The following function was inspired by the stack ovrflow post
// https://stackoverflow.com/questions/8465006/how-do-i-concatenate-two-strings-in-c
char* concat_strings(const char *s1, const char *s2)
{
    char *result = malloc(strlen(s1) + strlen(s2) + 1);
    strcpy(result, s1);
    strcat(result, s2);
    return result;
}

int compare_strings(const char *s1, const char *s2)
{
    return (strcmp(s1, s2) == 0);
}

void check_int_not_zero(int i, char *message)
{
    if (i == 0)
    {
        fprintf(stderr, "%s\n", message);
        exit(1);
    }
}

void check_long_not_zero(long l, char *message)
{
    if (l == 0)
    {
        fprintf(stderr, "%s\n", message);
        exit(1);
    }
}

void check_float_not_zero(float f, char *message)
{
    if (f == 0.0)
    {
        fprintf(stderr, "%s\n", message);
        exit(1);
    }
}

void check_not_null(void *p)
{
    // Inspired by https://www.cs.cornell.edu/courses/cs6120/2019fa/blog/null-pointer-guards/
    if (p == NULL)
    {
        fprintf(stderr, "NullPointerException\n");
        exit(1);
    }
}
