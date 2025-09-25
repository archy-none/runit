# Transrustic
> 読み：トランスラスティーク。
> 多分あの柄谷行人の著作が由来

Rustにトランスパイルする言語です。
所有権問題 `.clone()`メソッドの付与を、静的解析で全て自動的に解決します。

## 概要

以下のコードでは、変数`name`が2回使用されている。\
このままだと、Rustでは2行目でムーブしてしまい、後の3行目では使えなくなってしまう。
```fsharp
let name = "Archy None" in
let msg = name + ", Hey" in
name
```

コンパイルすると、最初のムーブである2行目に`.clone()`メソッドの呼び出しが自動的に付与される。
```rust
let r#_name = String::from("Archy None");
let r#_msg = r#_name.clone() + &String::from(", Hey");
r#_name
```

----
```fsharp
let name = "Archy None" in
let msg = name + ", Hey" in
msg
```

```rust
let r#_name = String::from("Archy None");
let r#_msg = r#_name + &String::from(", Hey");
r#_msg
```

## ライセンス
んなもんはねえ。アナキストに著作権が通用するかってんだ、好きに使え！
