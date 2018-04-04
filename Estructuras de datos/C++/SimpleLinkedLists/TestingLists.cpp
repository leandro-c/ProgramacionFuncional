#include <stdio.h>

#include "..\Prelude\Prelude.h"
#include "TestingLists.h"

/************************************/
/* Algoritmos de listas, iterativos */
/************************************/

int ilength(List xs)
/*
    PROPOSITO: devolver el numero de elementos de la lista
    PRECOND: ninguna, es una operacion total
    OBSERVACIONES: la lista tiene que estar correctamente armada
    CODIGO FUNCIONAL:
        ilength xs = whileNotIsNil xs 0
         where whileNotIsNil []                        len = len
               whileNotIsNil (headCurrent:tailCurrent) len =
                     whileNotIsNil tailCurrent (len+1)
*/
{
  List current = xs;
  int len = 0;                // IniciarRecorrido()
  while (not isNil(current))  // while (not finRecorrido())
   {                          // {
     len++;                   //   ProcesarElemento()
     current = tail(current); //   PasarAlSiguienteElemento()
   }                          // }
  return len;                 // FinRecorrido()
}


int icount(ListInt xs, int y)
/*
    PROPOSITO: devolver la cantidad de veces que aparece un elemento en la lista
    PRECOND: ninguna, es una operacion total
    CODIGO FUNCIONAL:
        icount xs y = whileNotIsNil xs y 0
         where whileNotIsNil []                        y count = count
               whileNotIsNil (headCurrent:tailCurrent) y count =
                     whileNotIsNil tailCurrent (if headCurrent==y
                                                 then count+1
                                                 else count)
*/
{
   ListInt current = xs;
   int count = 0;
   while (not isNil(current))
   {
      if (head(current) == y) count++;
      current = tail(current);
   }
   return count;
}

int isum(ListInt xs)
/*
    PROPOSITO: devolver la suma de todos los elementos de la lista
    PRECOND: ninguna, es una operacion total
    CODIGO FUNCIONAL:
        isum xs = whileNotIsNil xs 0
         where whileNotIsNil []                        total = total
               whileNotIsNil (headCurrent:tailCurrent) total =
                     whileNotIsNil tailCurrent (total+headCurrent)
*/
{
   ListInt current = xs;
   int total = 0;
   while (not isNil(current))
   {
      total = total + head(current);
      current = tail(current);
   }
   return total;
}

int imaximum(ListInt xs)
/*
    PROPOSITO: devolver el maximo elemento de la lista
    PRECOND: la lista no es vacia
    CODIGO FUNCIONAL:
        imaximum [] = error "La lista vacia no tiene maximo.\n"
        imaximum (headXs:tailXs) = whileNotIsNil tailXs headXs
         where whileNotIsNil []                        currMax = currMax
               whileNotIsNil (headCurrent:tailCurrent) currMax =
                     whileNotIsNil tailCurrent (max currMax headCurrent)
*/
{
   int x;
   if (not isNil(xs))
    {
      int currMax = head(xs);
      ListInt current = tail(xs);
      while (not isNil(current))
      {
        currMax = max(currMax, head(current));
        current = tail(current);
      }
      return currMax;
    }
    else
    { BOOM("La lista vacia no tiene maximo.\n"); }
}


List ireverse(List xs)
/*
    PROPOSITO: dar vuelta los elementos de una lista
    PRECOND: ninguna, es una operacion total
    CODIGO FUNCIONAL:
        ireverse xs = whileNotIsNil xs []
         where whileNotIsNil []                        newList = newList
               whileNotIsNil (headCurrent:tailCurrent) newList =
                     whileNotIsNil tailCurrent (headCurrent : newList)
*/
{
   List current = xs;
   List newList = Nil();
   while (not isNil(current))
   {
      newList = Cons(head(current), newList);
      current = tail(current);
   }
   return newList;
}


ListInt isuccl(ListInt xs)
/*
    PROPOSITO: sumarle 1 a todos los elementos de la lista
    PRECOND: ninguna, es una operacion total
    CODIGO FUNCIONAL:
        isuccl xs = whileNotIsNil (ireverse xs) []
         where whileNotIsNil []                        newList = newList
               whileNotIsNil (headCurrent:tailCurrent) newList =
                     whileNotIsNil tailCurrent (headCurrent+1 : newList)
*/
{
   ListInt current = ireverse(xs);
   ListInt newList = Nil();
   while (not isNil(current))
   {
      newList = Cons(head(current)+1, newList);
      current = tail(current);
   }
   return newList;
}

ListInt ipares(ListInt xs)
/*
    PROPOSITO: quedarse con los elementos pares de la lista
    PRECOND: ninguna, es una operacion total
    CODIGO FUNCIONAL:
        ipares xs = whileNotIsNil (ireverse xs) []
         where whileNotIsNil []                        newList = newList
               whileNotIsNil (headCurrent:tailCurrent) newList =
                     whileNotIsNil tailCurrent (if esPar x
                                                 then headCurrent : newList
                                                 else newList
                                               )
*/
{
   ListInt current = ireverse(xs);
   ListInt newList = Nil();
   int x;
   while (not isNil(current))
   {
      x = head(current);
      if (esPar(x)) newList = Cons(x, newList);
      current = tail(current);
   }
   return newList;
}


List iappend(List xs, List ys)
/*
    PROPOSITO: arma una nueva lista con los elementos de las dos listas
    PRECOND: ninguna, es una operacion total
    CODIGO FUNCIONAL:
        iappend xs ys = whileNotIsNil (ireverse xs) ys
         where whileNotIsNil []                        finalList = finalList
               whileNotIsNil (headCurrent:tailCurrent) finalList =
                     whileNotIsNil tailCurrent (headCurrent : finalList)
*/
{
   List current = ireverse(xs);
   List finalList = ys;
   while (not isNil(current))
   {
      finalList = Cons(head(current), finalList);
      current = tail(current);
   }
   return finalList;
}

/************************************/
/* Algoritmos de listas, recursivos */
/************************************/
int length(List xs)
/*
    PROPOSITO: devolver el numero de elementos de la lista
    PRECOND: ninguna, es una operacion total
    CODIGO FUNCIONAL:
      length [] = 0
      length (y:ys) = length ys + 1
*/
{
  if (isNil(xs))
       return 0;                     // La lista vacia no tiene elementos
  else return length(tail(xs)) + 1;  // Cons agrega un elemento a tail(xs)

/* // Version larga, con un unico return
  int resultado;
  ELEM_TYPE y;
  List ys;
  if (isNil(current))
     resultado = 0;               // La lista vacia no tiene elementos
  else
   {
     y  = head(xs)                // Damos nombre al primer elemento
     ys = tail(xs)                // Damos nombre a los demas elementos
     resultado = length(ys) + 1;  // Cons agrega un elemento, y entonces se suma 1
   }
  return resultado;
*/
}

int count(ListInt xs, int x)
/*
    PROPOSITO: devolver la cantidad de veces que aparece un elemento en la lista
    PRECOND: ninguna, es una operacion total
    CODIGO FUNCIONAL:
      count []     x = 0
      count (y:ys) x = if y == x
                        then count ys x + 1
                        else count ys x
*/
{
  if (isNil(xs))
       return 0;                     // La lista vacia no tiene elementos iguales a x
  else if (head(xs) == x)
       return count(tail(xs),x) + 1; // Si head(xs) es x, hay uno mas que en tail(xs)
  else return count(tail(xs),x);     // Si no, hay la misma cantidad que en tail(xs)
}

int sum(ListInt xs)
/*
    PROPOSITO: devolver la suma de todos los elementos de la lista
    PRECOND: ninguna, es una operacion total
    CODIGO FUNCIONAL:
      sum []     = 0
      sum (y:ys) = y + sum ys
*/
{
  if (isNil(xs))
       return 0;                         // 0 es el neutro de la suma
  else return head(xs) + sum(tail(xs));  // agrega el primer elemento a la suma del resto
}

int maximum(ListInt xs)
/*
    PROPOSITO: devolver el maximo elemento de la lista
    PRECOND: la lista no es vacia
    OBSERVACIONES: es comun ver versiones de operaciones
                   como esta que en el caso lista vacia
                   devuelven un valor especial, pero no
                   es lo mejor desde el punto de vista de
                   la logica de programacion
    CODIGO FUNCIONAL:
      maximum []     = error "La lista vacia no tiene maximo.\n"
      maximum [y]    = y
      maximum (y:ys) = max y (maximum ys)
*/
{
   int y;
   ListInt ys;

   if (isNil(xs))
     BOOM("La lista vacia no tiene maximo.\n")
   else
     {
       y = head(xs);
       ys = tail(xs);
       if (isNil(ys))        // Si no hay elementos, no se invoca maximum
            return y;                  // Si hay un solo elemento, es el maximo
       else return max(y,maximum(ys)); // Si hay mas, es el mas grande de los dos
     }
}


List reverse(List xs)
/*
    PROPOSITO: dar vuelta los elementos de una lista
    PRECOND: ninguna, es una operacion total
    CODIGO FUNCIONAL:
      reverse []     = []
      reverse (y:ys) = snoc (reverse ys) y
       where snoc zs x = zs ++ [x]
*/
{
  if (isNil(xs))
       return Nil();                             // La lista vacia es su propio reverso
  else return Snoc(reverse(tail(xs)),head(xs));  // agrega el primer elemento al final del reverso del resto
}


ListInt succl(ListInt xs)
/*
    PROPOSITO: sumarle 1 a todos los elementos de la lista
    PRECOND: ninguna, es una operacion total
      succl []     = []
      succl (y:ys) = y+1 : succl ys
*/
{
  if (isNil(xs))
       return Nil();                              // No hay elementos para sumarle 1
  else return Cons(head(xs)+1, succl(tail(xs)));  // suma 1 al primer elemento y lo agrega a los demas ya sumados
}

ListInt pares(ListInt xs)
/*
    PROPOSITO: quedarse con los elementos pares de la lista
    PRECOND: ninguna, es una operacion total
    CODIGO FUNCIONAL:
      pares []     = []
      pares (y:ys) = if esPar y
                      then y : pares ys
                      else pares ys
*/
{
  if (isNil(xs))
       return Nil();                            // No hay elementos pares en la lista vacia
  else if (esPar(head(xs)))
       return Cons(head(xs), pares(tail(xs)));  // agrega el primer elemento si es par
  else return pares(tail(xs));                  // y si no, no
}


List append(List xs, List ys)
/*
    PROPOSITO: arma una nueva lista con los elementos de las dos listas
    PRECOND: ninguna, es una operacion total
    CODIGO FUNCIONAL:
      append []     ys = ys
      append (x:xs) ys = x : append xs ys
*/
{
  if (isNil(xs))
       return ys;                                   // No hay elementos para agregar a ys
  else return Cons(head(xs), append(tail(xs),ys));  // agrega el primer elemento al append de la cola e ys
}


/*****************************************/
/*  Mas operaciones sobre listas         */
/*****************************************/
ListInt zipAdd(ListInt xs, ListInt ys)
/*
   PROPOSITO: sumar dos listas elemento a elemento. Si sobran de alguna, descartarlos
   PRECONDICION: ninguna, es una operacion total
   CODIGO FUNCIONAL:
     zipAdd xs ys = while xs ys []
     where while (x:xs) (y:ys) result = while xs ys (x+y:result)
           while _      _      result = result
*/
{
  ListInt listaA = xs;
  ListInt listaB = ys;
  ListInt result = Nil();

  while (not isNil(listaA) && not isNil(listaB))
    {
       result = Cons(head(listaA)+head(listaB), result);
       listaA = tail(listaA);
       listaB = tail(listaB);
    }
  return ireverse(result);
}

ListInt inub(ListInt xs)
/*
   PROPOSITO: eliminar elementos consecutivos repetidos
   PRECONDICION: ninguna, es una operacion total
   CODIGO FUNCIONAL:
     inub [] = []
     inub xs = let (headRs:tailRs) = ireverse xs
                in whileNotIsNil tailRs headRs []
      where whileNotIsNil []     next finalList = next:finalList
            whileNotIsNil (x:xs) next finalList =
                if x == next then whileIsNotNil xs next finalList
                             else whileIsNotNil xs x (next:finalList)
*/
{
  ListInt rs, finalList, current;
  int next;

  if (isNil(xs))
    finalList = Nil();
  else
    {
      rs        = ireverse(xs);
      current   = tail(rs);
      next      = head(rs);
      finalList = Nil();
      while (not isNil(current))
        {
          if (not (head(current) == next))
            {
              finalList = Cons(next,finalList);
              next = head(current);
            }
          current = tail(current);
        }
      finalList = Cons(next, finalList);
    }
  return finalList;
}

ListInt iposPares(ListInt xs)
/*
   PROPOSITO: quedarse con los elementos de una lista que estan en indices pares
   PRECONDICION: ninguna, es una operacion total
   CODIGO FUNCIONAL:
    iposPares xs = whileIsNotNil xs []
     where whileIsNotNil []       result = reverse result
           whileIsNotNil [x]      result = reverse (x:result)
           whileIsNotNil (x:y:xs) result = whileIsNotNil xs (x:result)
*/
{
  ListInt current = xs;
  ListInt result = Nil();
  while (not isNil(current))
   {
     result = Cons(head(current),result);             // Conserva el elemento en la posicion 0
     current = tail(current);
     if (not isNil(current)) current = tail(current); // Descarta el elemento en la posicion 1
   }
  return (ireverse(result));  // La lista queda al reves, asi que hay que darla vuelta
}

bool verifSumAnteriores(ListInt xs)
/*
   PROPOSITO: Verificar si todo elemento es la suma de los anteriores
   PRECONDICION: ninguna, es una operacion total
   CODIGO FUNCIONAL:
    verifSumAnteriores xs = while xs 0 true
     where while _      currentSum False             = False
           while []     currentSum seCumpleCondicion = seCumpleCondicion
           while (x:xs) currentSum seCumpleCondicion = while xs (x+currentSum)
	                                                     ((x==currentSum) && seCumpleCondicion)
*/
{
  int x;
  bool seCumpleCondicion = true; // Implementa el short circuit de (&&)
  if (not isNil(xs))
    {
      int currentSum = head(xs);
      ListInt current = tail(xs);
      while (seCumpleCondicion && not isNil(current))
       {
         x       = head(current);
         current = tail(current);
         seCumpleCondicion = (x==currentSum) && seCumpleCondicion;
         currentSum = currentSum + x;
       }
    }
  return seCumpleCondicion;
}

bool verifOrdenada(ListInt xs)
/*
   PROPOSITO: Verificar si la lista esta ordenada
   PRECONDICION: ninguna, es una operacion total
   CODIGO FUNCIONAL:
    verifOrdenada [] = true
    verifOrdenada xs = while xs true
     where while _         False        = False
           while [x]       estaOrdenada = estaOrdenada
           while (x:x':xs) estaOrdenada = while (x':xs) ((x<=x') && estaOrdenada)
*/
{
  bool estaOrdenada;
  int x;
  ListInt current;

  estaOrdenada = true;
  current = xs;
  while (estaOrdenada && not isNil(current))
    {
      x       = head(current);
      current = tail(current);
      if (not isNil(current))
        estaOrdenada = (x <= head(current)) && estaOrdenada;
    }
  return estaOrdenada;
}

bool sePuedeExpandirA(ListInt xs, ListInt ys)
/*
   PROPOSITO: Verificar si xs puede expandirse a ys
   PRECONDICION: ninguna, es una operacion total
   CODIGO FUNCIONAL:
    sePuedeExpandirA xs ys = while xs ys true
     where while _      _      False   = False
           while []     _      expande = expande
           while _      []     expande = False
           while (x:xs) (y:ys) expande = while (if (x==y) then xs else x:xs) ys ((x==y) && expande)
*/
{
   ListInt parte = xs;
   ListInt todo = ys;
   while (not isNil(parte) && not isNil(todo))
     {
	if (head(parte) == head(todo))
          parte = tail(parte);
	todo = tail(todo);
     }
   return (isNil(parte));
}

/************************************
 * Versiones funcionales, recursivas

  zipAdd []     _      = []
  zipAdd _      []     = []
  zipAdd (x:xs) (y:ys) = x+y : zipAdd xs ys

  nub []       = []
  nub [x]      = [x]
  nub (x:y:xs) = if (x==y)
                  then nub (y:xs)
                  else x : nub (y:xs)

  posPares []       = []
  posPares [x]      = [x]
  posPares (x:y:xs) = x : posPares xs

  verifSumAnt xs    = null (soloNoSA xs 0)
  soloNoSA [] n     = []
  soloNoSA (x:xs) n = let ys = soloNoSA xs (n+x)
                       in if x==n then x:ys else ys

  verifOrdenada []       = true
  verifOrdenada [x]      = true
  verifOrdenada (x:y:xs) = x<=y && verifOrdenada(y:xs)

  sePuedeExpandirA [] ys = true
  sePuedeExpandirA _  [] = False
  sePuedeExpandirA (x:xs) (y:ys) = if x==y
                                    then sePuedeExpandirA xs ys
                                    else sePuedeExpandirA (x:xs) ys

// Merge de listas ordenadas
// Insert ordenado
*/

void testingLists(ListInt xs)
{
  cout << ("\n");
  cout << ("verifSumAnteriores([2,2,4,8,16])  = ");
     printBool(verifSumAnteriores(Cons(2,Cons(2,Cons(4,Cons(8,Cons(16,Nil())
                                                           )
                                                    )
                                             )
                                      )
                                 )); cout << ("\n");
  cout << ("\n");
  cout << ("xs             = "); printList(xs); cout << ("\n");
  cout << ("ilength(xs)    = ") << ( ilength(xs)) << endl;
  cout << ("icount(xs, 42) = ") << ( icount(xs, 42)) << endl;
  cout << ("isum(xs)       = ") << ( isum(xs)) << endl;
  cout << ("imaximum(xs)   = ") << ( imaximum(xs)) << endl;
  cout << ("ireverse(xs)   = "); printList(ireverse(xs)); cout << ("\n");
  cout << ("isuccl(xs)     = "); printList(isuccl(xs)); cout << ("\n");
  cout << ("ipares(xs)     = "); printList(ipares(xs)); cout << ("\n");
  cout << ("\n");
  cout << ("length(xs)     = ") << ( length(xs)) << endl;
  cout << ("count(xs, 42)  = ") << ( count(xs, 42)) << endl;
  cout << ("sum(xs)        = ") << ( sum(xs)) << endl;
  cout << ("maximum(xs)    = ") << ( maximum(xs)) << endl;
  cout << ("reverse(xs)    = "); printList(reverse(xs)); cout << ("\n");
  cout << ("succl(xs)      = "); printList(succl(xs)); cout << ("\n");
  cout << ("pares(xs)      = "); printList(pares(xs)); cout << ("\n");
  cout << ("\n");
  cout << ("zipAdd(xs,Cons(0,xs))           = "); printList(zipAdd(xs,Cons(0,xs))); cout << ("\n");
  cout << ("sePuedeExpandirA(ipares(xs),xs) = "); printBool(sePuedeExpandirA(ipares(xs),xs)); cout << ("\n");
  cout << ("inub(xs)                        = "); printList(inub(xs)); cout << ("\n");
  cout << ("iposPares(xs)                   = "); printList(iposPares(xs)); cout << ("\n");
  cout << ("verifSumAnteriores(xs)          = "); printBool(verifSumAnteriores(xs)); cout << ("\n");
  cout << ("verifOrdenada(xs)               = "); printBool(verifOrdenada(xs)); cout << ("\n");
  cout << ("\n");

  ListInt ys = iappend(xs, ipares(xs));
  cout << ("ys=append(xs,pares(xs)) = "); printList(ys); cout << ("\n");
  cout << ("ilength(ys)             = ") << ( ilength(ys)) << endl;
  cout << ("icount(ys, 42)          = ") << ( icount(ys, 42)) << endl;
  cout << ("isum(ys)                = ") << ( isum(ys)) << endl;
  cout << ("imaximum(ys)            = ") << ( imaximum(ys)) << endl;
  cout << ("ireverse(ys)            = "); printList(ireverse(ys)); cout << ("\n");
  cout << ("isuccl(ys)              = "); printList(isuccl(ys)); cout << ("\n");
  cout << ("ipares(ys)              = "); printList(ipares(ys)); cout << ("\n");
  cout << ("\n");
  cout << ("length(ys)              = ") << ( length(ys)) << endl;
  cout << ("count(ys, 42)           = ") << ( count(ys, 42)) << endl;
  cout << ("sum(ys)                 = ") << ( sum(ys)) << endl;
  cout << ("maximum(ys)             = ") << ( maximum(ys)) << endl;
  cout << ("reverse(ys)             = "); printList(reverse(ys)); cout << ("\n");
  cout << ("succl(ys)               = "); printList(succl(ys)); cout << ("\n");
  cout << ("pares(ys)               = "); printList(pares(ys)); cout << ("\n");
  cout << ("\n");
  cout << ("zipAdd(ys,Cons(0,ys))           = "); printList(zipAdd(ys,Cons(0,ys))); cout << ("\n");
  cout << ("sePuedeExpandirA(ipares(ys),ys) = "); printBool(sePuedeExpandirA(ipares(ys),ys)); cout << ("\n");
  cout << ("nub(ys)                         = "); printList(inub(ys)); cout << ("\n");
  cout << ("iposPares(ys)                   = "); printList(iposPares(ys)); cout << ("\n");
  cout << ("verifSumAnteriores(ys)          = "); printBool(verifSumAnteriores(ys)); cout << ("\n");
  cout << ("verifOrdenada(ys)               = "); printBool(verifOrdenada(ys)); cout << ("\n");
  cout << ("\n");

//  cout << ("maximum(Nil()) = ");
//  cout << ("%d\n", imaximum(Nil()));
}
