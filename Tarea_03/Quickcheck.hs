{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Data.Maybe (fromJust)

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

prop_left_and_remove bf =
  not (atLeft bf) ==> delete bf == (remove . left) bf

prop_right_and_left bf =
  not (atRight bf) ==> bf == (left . right) bf  

prop_remove_at_right str =
  bf == remove bf
    where bf = (str,"")

prop_delete_at_left str = 
  bf == delete bf
    where bf = ("",str)

prop_empty_buffer =
  bf == left bf && bf == right bf && cursor bf == Nothing
    where bf = empty

prop_insert_and_check c bf =
  c == fromJust bf'
    where bf' = (cursor . left . insert c) bf

prop_insert_and_delete c bf =
  bf == (delete . insert c) bf

return []
runTests = $quickCheckAll

main = runTests