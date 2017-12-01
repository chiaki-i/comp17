# comp17

## task1
* [OCamllex, OCamlyacc](https://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html)

## task7
* レジスタ割り当て
    * Alloc.g が呼ばれるたびに レジスタの番号を1減らすのをやめる。もし target に含まれるもののうち現在の条件に当てはまるものがあればそれを採用し、そうでなければ interfere に含まれないレジスタのうち最も番号が大きいものを選ぶ。

## task8
* 末尾呼び出し最適化
    * Code.g_def で tail 関数を必要としているので tail.ml と code.ml を統合
    * アセンブラ用の道具：[Linuxでx86アセンブラ（道具編）](https://qiita.com/MoriokaReimen/items/b316a68d76c1eafa18f8)
    * `nasm -g -f macho64 -o <output> <input>`
