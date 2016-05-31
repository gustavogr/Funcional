{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Data.Maybe (fromJust)
import Buffer

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