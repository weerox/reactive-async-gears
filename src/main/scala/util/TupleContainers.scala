package rasync.util

private[rasync] type ContainerMap[T <: Tuple, A[_], B[_]] <: Tuple = T match
  case A[x] *: t  => B[x] *: ContainerMap[t, A, B]
  case EmptyTuple => EmptyTuple

private[rasync] type Container[C[_]] = [X <: Tuple] =>> X =:= Tuple.Map[Tuple.InverseMap[X, C], C]
