
/* aca van los headers */
#include <stdio.h>
#include <string.h>

#include "Prelude.h"
#include "TestingStacks.h"

Bool match(char c1, char c2);

/* TESTING */
void testingStacks()
{
  Stack p = emptyStack();
  printf("p=emptyStack  = "); printStack(p); printf("\n");
  p = push('2',p);
  printf("p=push('2',p) = "); printStack(p); printf("\n");
  p = push('3',p);
  printf("p=push('3',p) = "); printStack(p); printf("\n");
  printf("top(p)        = "); printStackElemType(top(p)); printf("\n");
  p = pop(p);
  printf("p=pop(p)      = "); printStack(p); printf("\n");

  String msg = "Expresion";
  printf("El string '%s' tiene longitud %d\n", msg, strlen(msg));
  printf("El string '%s' esta balanceado? ", msg); printRespuesta(balance(msg)); printf("\n");

  msg = "{2+[3*(5+4)/2]*(1+1)}";
  printf("El string '%s' tiene longitud %d\n", msg, strlen(msg));
  printf("El string '%s' esta balanceado? ", msg); printRespuesta(balance(msg)); printf("\n");

  msg = "{2+[3*(5+4]/2)*(1+1)}";
  printf("El string '%s' tiene longitud %d\n", msg, strlen(msg));
  printf("El string '%s' esta balanceado? ", msg); printRespuesta(balance(msg)); printf("\n");
}

Bool balance(String str)
{
  Stack p = emptyStack();
  int n = strlen(str);
  int i = 0;
  Bool balanced = True;
  while (i<n && balanced)
   {
     if (str[i] == '(' || str[i] == '[' || str[i] == '{')
       p = push(str[i], p);
     if (str[i] == ')' || str[i] == ']' || str[i] == '}')
       { if (isEmptyStack(p)) balanced = False;
         else 
	  {
           if (not match(top(p), str[i])) balanced = False;
	   p = pop(p);
	  }
       }
     i++;
   }
  balanced = balanced && isEmptyStack(p);
  return balanced;
}

Bool match(char c1, char c2)
{
  if (c1 == '(') return (c2 == ')');
  if (c1 == '[') return (c2 == ']');
  if (c1 == '{') return (c2 == '}');
  return False;
}

