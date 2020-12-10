{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- parseMessage "E 2 562 help help"
-- == LogMessage (Error 2) 562 "help help"
parseMessage :: String -> LogMessage
parseMessage s =
  case wordsList of
    ("I" : x : xs) -> LogMessage Info (read x) (unwords xs)
    ("W" : x : xs) -> LogMessage Warning (read x) (unwords xs)
    ("E" : x : y : xs) -> LogMessage (Error (read x)) (read y) (unwords xs)
    _ -> Unknown s
  where
    wordsList = words s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- data MessageTree
--   = Leaf
--   | Node MessageTree LogMessage MessageTree

-- Assume a sorted MessageTree and produce new sorted MessageTree with
-- LogMessage inserted.
-- timestamps in the left subtree are less, timestamps in the right subtree are
-- greater than the timestamp in the LogMessage of the Node
-- If LogMessage is of type Unknown return unchanged MessageTree

insert :: LogMessage -> MessageTree -> MessageTree
insert lmsg@LogMessage {} Leaf = Node Leaf lmsg Leaf -- suggested by hlint
{-insert lmsg@(LogMessage _ _ _) Leaf = Node Leaf lmsg Leaf-}
insert lmsg1@(LogMessage _ ts1 _) (Node left lmsg2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left lmsg2 (insert lmsg1 right)
  | otherwise = Node (insert lmsg1 left) lmsg2 right
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map getMsg (filter (filterMsg 50) xs)

filterMsg :: Int -> LogMessage -> Bool
filterMsg lvl (LogMessage (Error level) _ _) = level > lvl
filterMsg _ _ = False

getMsg :: LogMessage -> String
getMsg (LogMessage _ _ msg) = msg
getMsg _ = ""

main = do
  msgs <- testParse parse 10 "sample.log"
  let err = whatWentWrong msgs
  print err