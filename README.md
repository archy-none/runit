# Transrustic
> 読み：トランスラスティーク。
> 多分あの柄谷行人の著作が由来

Rustにトランスパイルする言語です。
所有権問題や.clone()メソッドの付与を、静的解析で全て自動的に解決します。

## 概要

```fsharp
let name = "Archy None" in
let msg = "I am " + name in
name
```

```rust
let r#_name = String::from("Archy None");
let r#_msg = String::from("I am ") + &r#_name.clone();
r#_name
```
----
```fsharp
let name = "Archy None" in
let msg = "I am " + name in
msg
```

```rust
let r#_name = String::from("Archy None");
let r#_msg = String::from("I am ") + &r#_name;
r#_msg
```

## ライセンス
んなもんはねえ。好きに使え。
