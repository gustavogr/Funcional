module Buffer where

type Buffer = (String,String)

empty :: Buffer                   -- Buffer nuevo
empty = ("","")

cursor :: Buffer -> Maybe Char     -- Leer bajo el cursor
cursor (_,[])   = Nothing
cursor (_,c:_)  = Just c

insert :: Char -> Buffer -> Buffer -- ...antes del cursor
insert c (as,ds) = (c:as, ds)

delete :: Buffer -> Buffer         -- ...anterior al cursor
delete ([],ds)   = ([],ds)
delete (_:as,ds) = (as,ds)

remove :: Buffer -> Buffer         -- ...bajo al cursor
remove (as,[])   = (as,[])
remove (as,d:ds) = (as,ds)

left :: Buffer -> Buffer         -- Cursor a la izquierda
left ([],ds)   = ([],ds)
left (a:as,ds) = (as,a:ds)

right :: Buffer -> Buffer         -- Cursor a la derecha
right (as,[])   = (as,[])
right (as,d:ds) = (d:as,ds)

atLeft :: Buffer -> Bool           -- Extremo izquierdo?
atLeft (as,_) = null as

atRight :: Buffer -> Bool           -- Extremo derecho?
atRight (_,ds) = null ds