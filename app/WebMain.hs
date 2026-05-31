{-# LANGUAGE OverloadedStrings #-}

module Main where

import Derivation.Trace (inferSource)
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort) -- 导入Warp模块，defaultSettings是默认设置，setHost是设置主机，setPort是设置端口。
import System.Environment (lookupEnv)         -- 导入Environment模块，lookupEnv是获取环境变量。   
import Text.Blaze.Html (Html)               -- 导入Html模块，Html是HTML类型。Text.Blaze.Html是Blaze库中的Html类型。
import Text.Blaze.Html.Renderer.Utf8 (renderHtml) -- 导入Utf8模块，renderHtml是渲染HTML。
import Web.Page (renderPage)                      -- 导入Page模块，renderPage是渲染页面。

import qualified Data.Text.Lazy as TL -- qualified 表示引入模块，as 表示别名。
import qualified Web.Scotty as S -- 导入Scotty模块，S是Scotty类型。

defaultPort :: Int
defaultPort = 8080

main :: IO ()
main = do
  port <- readPort
  putStrLn $
    "Algorithm W web UI — open http://127.0.0.1:"
      ++ show port
      ++ " in your browser"
  let opts =
        S.defaultOptions
          { S.settings = setPort port $ setHost "127.0.0.1" defaultSettings -- 更新记录，修改settings字段。服务器在本机上(localhost)
          }
  S.scottyOpts opts app  -- 路由由app定义

readPort :: IO Int
readPort =
  maybe defaultPort read <$> lookupEnv "PORT"

app :: S.ScottyM ()   -- 一个路由表
app = do
  S.get "/" $         -- 处理GET请求，打开首页。第一次打开网站是空表单
    sendHtml (renderPage "" Nothing)  -- 渲染首页，空字符串表示没有输入，Nothing表示没有推断结果。sendHtml把整页HTML返回给浏览器

  S.post "/infer" $ do   -- 输入表达式后提交，使用post方法，向/infer路由发送POST请求，表单字段source的值作为source参数传递给inferSource函数。
    source <- TL.unpack <$> S.formParam "source"  -- 从表单中获取source字段的值，并转换为字符串。
    let outcome = inferSource source  -- 调用inferSource函数，推断source的类型。outcome是推断结果。
    sendHtml (renderPage source (Just outcome))  -- 渲染推断结果，source是输入的表达式，Just outcome是推断结果。sendHtml把整页HTML返回给浏览器

sendHtml :: Html -> S.ActionM ()
sendHtml page = do
  S.setHeader "Content-Type" "text/html; charset=utf-8"
  S.raw (renderHtml page)
