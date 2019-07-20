Refの問題点

IORef (Maybe Type)のShowインスタンスが無くて、型変数の区別が点かない
型変数の値がNothingかJust Typeかを毎回readIORefで読まないといけない

--> 代入をデータとして扱うと、型変数をただのユニークな文字列にできて楽になる

src/Subst/で作る

Substの問題点

制約の解決と式のトラバースを同時にやっていてコードがぐちゃぐちゃしてる。

--> Constraint Generationを使うとRWSTでパッと書ける

src/Constraints/で作る
