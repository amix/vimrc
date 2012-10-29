
/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  calloc_int_matrix
 *  Description:  Allocate a dynamic int-matrix of size rows*columns; return a pointer.
 * =====================================================================================
 */
int**
calloc_int_matrix ( int rows, int columns )
{
  int   i;
  int **m;
  m     = calloc ( rows, sizeof(int*) );        /* allocate pointer array     */
  assert( m != NULL );                          /* abort if allocation failed */
  *m    = calloc ( rows*columns, sizeof(int) ); /* allocate data array        */
  assert(*m != NULL );                          /* abort if allocation failed */
  for ( i=1; i<rows; i+=1 )                     /* set pointers               */
    m[i]  = m[i-1] + columns;
  return m;
}  /* ----------  end of function calloc_int_matrix  ---------- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  free_int_matrix
 *  Description:  Free a dynamic int-matrix.
 * =====================================================================================
 */
void
free_int_matrix ( int **m )
{
  free(*m);                                     /* free data array            */
  free( m);                                     /* free pointer array         */
  return ;
}  /* ----------  end of function free_int_matrix  ---------- */

