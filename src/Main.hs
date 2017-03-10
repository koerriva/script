module Main where

{-
本程序主要完成的任务:
   1,解析thrift文件,并自动生成服务
   2,解析script文件(thrift实现脚本),提供服务
   备注:服务的协议和传输部分暂时用http服务模拟
-}

main = do
    putStrLn "Thrift-like srcipt engine v0.1"