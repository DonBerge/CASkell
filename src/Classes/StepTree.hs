module Classes.StepTree where


type Name = String
type Original = String
type Resultant = String

data StepTree = E | Step Name Original Resultant [StepTree] -- Step Name Original Resultant Children

data Tree a = Node a [Tree a] deriving (Show)

-- Función para imprimir el árbol en forma de texto bonito
drawTree :: StepTree -> String
drawTree = unlines . draw

draw :: StepTree -> [String]
draw E = []
draw (Step _ o r ts) = 
    let subTrees = concatMap drawSubTree ts
    in (o ++ " => " ++ r) : subTrees

drawSubTree :: StepTree -> [String]
drawSubTree E = []
drawSubTree (Step _ o r ts) = 
    let firstLine = "├─ " ++ show o ++ " => " ++ show r
        subTrees  = map ("│  " ++) (concatMap drawSubTree ts)
    in firstLine : subTrees

instance Show StepTree where
    show = drawTree