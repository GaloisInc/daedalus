module Possibly where
  
---- Possibly ----------------------------------------------------------------

type ErrorMsg = [String]
type Possibly a = Either ErrorMsg a

failP :: [String] -> Possibly a
failP = Left
  
-- the bind for the IO(Possibly -) monad
-- (actually has more general type)
bind_IOPossibly ::
  IO (Possibly a) -> (a -> IO (Possibly b)) -> IO (Possibly b)
bind_IOPossibly ioA ioB =   
  do
  r1 <- ioA
  case r1 of
    Left s   -> return (Left s)
    Right v1 -> ioB v1

                  
