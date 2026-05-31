{-# LANGUAGE OverloadedStrings #-}

module Web.AstHtml
  ( renderAstTree
  ) where

import Data.List (find)
import Data.Maybe (mapMaybe)
import Syntax (BinOp(..), Exp(..), Lit(..), Stmt(..))
import Text.Blaze.Html (Html, preEscapedToHtml)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data AstNode = AstNode
  { anId :: Int
  , anLabel :: String
  , anDepth :: Int
  , anX :: Double
  , anChildren :: [Int]
  }

data LayoutState = LayoutState
  { lsNextId :: Int
  , lsLeafX :: Double
  , lsNodes :: [AstNode]
  }

colWidth :: Double
colWidth = 90

rowHeight :: Double
rowHeight = 72

nodeH :: Double
nodeH = 28

charW :: Double
charW = 7.5

minNodeW :: Double
minNodeW = 44

padX :: Double
padX = 24

padY :: Double
padY = 20

renderAstTree :: Exp -> Html
renderAstTree expr =
  let nodes = layoutAst expr
      bounds = svgBounds nodes
      (_, _, _, h) = bounds
      svg =
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100%\" height=\""
          ++ show (round h :: Int)
          ++ "px\" viewBox=\""
          ++ viewBoxStr bounds
          ++ "\">"
          ++ renderAllEdges nodes
          ++ concatMap renderNodeSvg nodes
          ++ "</svg>"
   in H.div ! A.class_ "ast-panel" $ preEscapedToHtml svg
  where
    viewBoxStr (x, y, w', h') =
      show (floor x :: Int)
        ++ " "
        ++ show (floor y :: Int)
        ++ " "
        ++ show (ceiling w' :: Int)
        ++ " "
        ++ show (ceiling h' :: Int)

layoutAst :: Exp -> [AstNode]
layoutAst expr =
  reverse (lsNodes (snd (layoutExp 0 expr (LayoutState 0 0 []))))

layoutExp :: Int -> Exp -> LayoutState -> (AstNode, LayoutState)
layoutExp depth expr state =
  case expr of
    EVar name ->
      placeLeaf depth ("Var: " ++ name) state
    ELit lit ->
      placeLeaf depth ("Lit: " ++ showLit lit) state
    EApp fun arg ->
      let (funNode, state1) = layoutExp (depth + 1) fun state
          (argNode, state2) = layoutExp (depth + 1) arg state1
       in placeInternal depth "App" [funNode, argNode] state2
    EAbs name body ->
      let (bodyNode, state1) = layoutExp (depth + 1) body state
       in placeInternal depth ("λ " ++ name) [bodyNode] state1
    ELet name value body ->
      let (valueNode, state1) = layoutExp (depth + 1) value state
          (bodyNode, state2) = layoutExp (depth + 1) body state1
       in placeInternal depth ("let " ++ name) [valueNode, bodyNode] state2
    EIf cond yes no ->
      let (condNode, state1) = layoutExp (depth + 1) cond state
          (yesNode, state2) = layoutExp (depth + 1) yes state1
          (noNode, state3) = layoutExp (depth + 1) no state2
       in placeInternal depth "if" [condNode, yesNode, noNode] state3
    EBin op left right ->
      let (leftNode, state1) = layoutExp (depth + 1) left state
          (rightNode, state2) = layoutExp (depth + 1) right state1
       in placeInternal depth ("Bin: " ++ showBinOp op) [leftNode, rightNode] state2
    EList items ->
      let (itemNodes, state1) = layoutManyExp (depth + 1) items state
       in placeInternal depth "List" itemNodes state1
    EBlock stmts outputName ->
      let (stmtNodes, state1) = layoutManyStmt (depth + 1) stmts state
       in placeInternal depth ("block -> " ++ outputName) stmtNodes state1

layoutManyExp :: Int -> [Exp] -> LayoutState -> ([AstNode], LayoutState)
layoutManyExp _ [] state = ([], state)
layoutManyExp depth (expr:exprs) state =
  let (node, state1) = layoutExp depth expr state
      (nodes, state2) = layoutManyExp depth exprs state1
   in (node : nodes, state2)

layoutManyStmt :: Int -> [Stmt] -> LayoutState -> ([AstNode], LayoutState)
layoutManyStmt _ [] state = ([], state)
layoutManyStmt depth (stmt:stmts) state =
  let (node, state1) = layoutStmt depth stmt state
      (nodes, state2) = layoutManyStmt depth stmts state1
   in (node : nodes, state2)

layoutStmt :: Int -> Stmt -> LayoutState -> (AstNode, LayoutState)
layoutStmt depth stmt state =
  case stmt of
    SAssign name expr ->
      let (exprNode, state1) = layoutExp (depth + 1) expr state
       in placeInternal depth ("assign " ++ name) [exprNode] state1
    SIfStmt cond yes no ->
      let (condNode, state1) = layoutExp (depth + 1) cond state
          (yesNodes, state2) = layoutManyStmt (depth + 1) yes state1
          (noNodes, state3) = layoutManyStmt (depth + 1) no state2
       in placeInternal depth "stmt if" (condNode : yesNodes ++ noNodes) state3
    SSwitchStmt subject cases otherwiseBranch ->
      let (subjectNode, state1) = layoutExp (depth + 1) subject state
          (caseNodes, state2) = layoutManyCase (depth + 1) cases state1
          (otherwiseNodes, state3) = layoutManyStmt (depth + 1) otherwiseBranch state2
       in placeInternal depth "switch" (subjectNode : caseNodes ++ otherwiseNodes) state3
    SFor name items body ->
      let (itemsNode, state1) = layoutExp (depth + 1) items state
          (bodyNodes, state2) = layoutManyStmt (depth + 1) body state1
       in placeInternal depth ("for " ++ name) (itemsNode : bodyNodes) state2
    SWhile cond body ->
      let (condNode, state1) = layoutExp (depth + 1) cond state
          (bodyNodes, state2) = layoutManyStmt (depth + 1) body state1
       in placeInternal depth "while" (condNode : bodyNodes) state2

layoutManyCase :: Int -> [(Exp, [Stmt])] -> LayoutState -> ([AstNode], LayoutState)
layoutManyCase _ [] state = ([], state)
layoutManyCase depth ((caseExpr, body):cases) state =
  let (caseExprNode, state1) = layoutExp (depth + 1) caseExpr state
      (bodyNodes, state2) = layoutManyStmt (depth + 1) body state1
      (caseNode, state3) = placeInternal depth "case" (caseExprNode : bodyNodes) state2
      (caseNodes, state4) = layoutManyCase depth cases state3
   in (caseNode : caseNodes, state4)

placeLeaf :: Int -> String -> LayoutState -> (AstNode, LayoutState)
placeLeaf depth label state =
  let nodeId = lsNextId state
      x = lsLeafX state
      node = AstNode nodeId label depth x []
      nextState =
        state
          { lsNextId = nodeId + 1
          , lsLeafX = x + 1
          , lsNodes = node : lsNodes state
          }
   in (node, nextState)

placeInternal :: Int -> String -> [AstNode] -> LayoutState -> (AstNode, LayoutState)
placeInternal depth label children state =
  let childXs = map anX children
      x =
        if null childXs
          then lsLeafX state
          else sum childXs / fromIntegral (length childXs)
      nodeId = lsNextId state
      childIds = map anId children
      node = AstNode nodeId label depth x childIds
      nextState =
        state
          { lsNextId = nodeId + 1
          , lsNodes = node : lsNodes state
          }
   in (node, nextState)

nodeWidth :: String -> Double
nodeWidth label =
  max minNodeW (fromIntegral (length label) * charW + 16)

nodeCenter :: AstNode -> (Double, Double)
nodeCenter node =
  ( padX + anX node * colWidth + colWidth / 2
  , padY + fromIntegral (anDepth node) * rowHeight
  )

nodeRect :: AstNode -> (Double, Double, Double, Double)
nodeRect node =
  let w = nodeWidth (anLabel node)
      (cx, cy) = nodeCenter node
   in (cx - w / 2, cy - nodeH / 2, w, nodeH)

svgBounds :: [AstNode] -> (Double, Double, Double, Double)
svgBounds nodes =
  case map nodeRect nodes of
    [] -> (0, 0, 200, 100)
    rects ->
      let minRx = minimum (map (\(x, _, _, _) -> x) rects)
          minRy = minimum (map (\(_, y, _, _) -> y) rects)
          maxRx = maximum (map (\(x, _, w, _) -> x + w) rects)
          maxRy = maximum (map (\(_, y, _, h) -> y + h) rects)
          margin = 16
       in ( minRx - margin
          , minRy - margin
          , maxRx - minRx + 2 * margin
          , maxRy - minRy + 2 * margin
          )

renderAllEdges :: [AstNode] -> String
renderAllEdges nodes =
  concatMap (renderParentEdges nodes) nodes
  where
    renderParentEdges allNodes parent =
      concatMap (renderEdgeTo parent) (mapMaybe (`lookupNode` allNodes) (anChildren parent))

renderEdgeTo :: AstNode -> AstNode -> String
renderEdgeTo parent child =
  let (pcx, pcy) = nodeCenter parent
      (_, _, _, ph) = nodeRect parent
      (ccx, _) = nodeCenter child
      (_, cy, _, _) = nodeRect child
   in "<line class=\"ast-edge\" x1=\""
        ++ showPx pcx
        ++ "\" y1=\""
        ++ showPx (pcy + ph / 2)
        ++ "\" x2=\""
        ++ showPx ccx
        ++ "\" y2=\""
        ++ showPx cy
        ++ "\"/>"

renderNodeSvg :: AstNode -> String
renderNodeSvg node =
  let (x, y, w, h) = nodeRect node
      (cx, cy) = nodeCenter node
   in "<rect class=\"ast-node\" x=\""
        ++ showPx x
        ++ "\" y=\""
        ++ showPx y
        ++ "\" width=\""
        ++ showPx w
        ++ "\" height=\""
        ++ showPx h
        ++ "\" rx=\"4\"/><text class=\"ast-label\" x=\""
        ++ showPx cx
        ++ "\" y=\""
        ++ showPx (cy + 4)
        ++ "\" text-anchor=\"middle\">"
        ++ escapeXml (anLabel node)
        ++ "</text>"

showPx :: Double -> String
showPx n =
  if n == fromIntegral (round n :: Int)
    then show (round n :: Int)
    else show n

escapeXml :: String -> String
escapeXml = concatMap esc
  where
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '&' = "&amp;"
    esc '"' = "&quot;"
    esc c = [c]

lookupNode :: Int -> [AstNode] -> Maybe AstNode
lookupNode nid nodes = find (\n -> anId n == nid) nodes

showLit :: Lit -> String
showLit (LInt n) = show n
showLit (LBool True) = "True"
showLit (LBool False) = "False"

showBinOp :: BinOp -> String
showBinOp Add = "+"
showBinOp Sub = "-"
showBinOp Mul = "*"
showBinOp Div = "/"
showBinOp Eq = "=="
showBinOp Ne = "~="
showBinOp Lt = "<"
showBinOp Le = "<="
showBinOp Gt = ">"
showBinOp Ge = ">="
showBinOp And = "&&"
showBinOp Or = "||"
