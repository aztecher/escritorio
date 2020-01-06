{-# LANGUAGE ExistentialQuantification #-}
--
-- **********
-- WARNING
-- **********
-- 以降数学記号を使う場合は以下で代用する.
--
-- 存在する/Eの左右反転したやつ -> e'
-- 任意の~ /Aの上下反転したやつ -> a'
--
-- 存在量化された型
--
-- 存在型(Existential types / existentials)は
-- 型の集合をひとつの型へと圧縮する方法である.
--
-- 言語拡張 : {-# LANGUAGE ExistentialQuantification #-}
--
-- =================
-- forallキーワード
-- =================
--
-- `forall` キーワードは明示的に型変数をスコープに持ってくるのに使われる.
-- 今, 一つよく見る例を示す.
--
--
-- Example: 多相関数
--
-- > map :: (a -> b) -> [a] -> [b]
--
-- これらの `a`, `b` は一般に 「型変数」である.
-- しかしこれらは, 「全称量化された (universally quantified)」
-- とする方法もある.
-- これはつまり, 「任意の ~ について」(for all) と 「 ~ が存在する」 (exists) であり, これらの「量化」これらの後に現れる.
-- 例えば, e'x は, これに続く少なくともひとつの値xについて, 続く命題が真であることを意味する
-- a'x は, 想定されうる全てのxについて続く命題が真であることを意味する.
--
-- a'x, x^2 >= 0 (任意のxについて, x^2は0以上)
-- e'x, x^3 = 27 (xを3乗すると27となる値xが少なくとも一つ存在する)
--
-- 
-- なぜこんな話をしたかというと, `forall` キーワードは `型` を同様に量子化するためである.
-- 先程見た `map` の型を次のように書き直すことができる.
--
--
-- Example: 明示的な型変数の量化
--
-- > map :: forall a b. (a -> b) -> [a] -> [b]
--
-- これはつまり, 「我々が考えるどんな `a` や `b` についても, `map` は `(a -> b) -> [a] -> [b]` という型をとる」ということである
-- 例えば, `a = Int` と `b = String` という型を選ぶかもしれない.
-- そして, `map` は型 `(int -> String) -> [Int] -> [String]` を持つ.
-- `map` の一般化された型をより具体的な型へと「インスタンス化」しているのだ.
--
-- しかし, Haskellでは知っての通り,
-- 小文字で始まる型は暗黙的に `forall` を持つことになっており,
-- 次の2つの宣言は同じであるの同様に, `map` の2つの宣言は同じである.
--
--
-- Example: ふたつの同じ型宣言
--
-- > id :: a -> a
-- > id :: forall a. a -> a
--
-- 何が言いたいかというと, `forall` キーワードを置く場所によって
-- 明示的にHaskellに伝え, 規定の振る舞いを上書きすることができるのだ.
-- 使い方のひとつは, 存在型(existential types) や単に存在(existentials) ともいう 「存在量化された型 (existential quantified types)」を構築することである.
--
-- しかしここで疑問が浮かぶわけだ
-- 「forall は全称量子化じゃねーのかよ!? どうやってそこから抜け出して存在型にするんだよ!?」
-- 以降実際に存在型の力の例を見て, 深淵に飛び込もう
--

-- ========================================
-- 例: 異なる方が混合されたリスト
-- ========================================
--
-- もしあるクラスのインスタンスである様々な型の値を,
-- 一つのリストに入れる事ができれば便利だろう.
--
-- しかし, リストの要素は全て同じ型でなければ行けないので,
-- 通常このようなことはできない.
--
-- このような場合に, 存在型を使って `type hider` や `type box`
-- と呼ばれるものを定義することで, この条件を緩めることができる.
--
--
-- Example: 統一されていない方を格納するリスト (heterogeneous list)

data ShowBox = forall s. Show s => SB s

heteroList :: [ShowBox]
heteroList = [SB (), SB 5, SB True]

-- 詳しくは説明しないが, 直感的に明らかだろう
-- 重要なのは, 3つの異なる型の値に対して構築子を呼び出していて,
-- 全てを一つのリストの中に置いているということだ.
-- つまり, これはそれぞれの値が結果的に同じ型になったということを示している.
-- `forall` キーワードを使い, この構築子に
-- `SB :: forall s. Show s => s -> ShowBox`
-- という方を与えたからである.
--
-- そして, これらの値それぞれについて分かっていることとして,
-- `show` を通じて文字列に変換できるということが分かっているわけである.
--
-- さてもう少し踏み込むと, `show` の定義に置いて, `s` の型は分からない.
-- しかしながら, SBの構築しの制約によって, その方がShowのインスタンス
-- であることならわかる.
-- それ故, `s` に対して関数 `show` を使うことは妥当である.
--
-- Example: 統一されていない方を格納するリストの利用
instance Show ShowBox where
  show (SB s) = show s

-- print :: Show s => s -> IO ()
-- print x = putStrLn (show x)
--
-- mapM_ :: (a -> m b) -> [a] -> m ()
--
-- つまり,
-- mapM_ print :: Show s => [s] -> IO ()
--
-- この仕組みで, `ShowBox` は `Show` のインスタンスであると定義した
-- だけで, リストの値を出力できる.
f :: [ShowBox] -> IO ()
f xs = mapM_ print xs

main = f heteroList


-- ==================================
-- 「存在」型における「存在」の定義
-- ==================================
--
-- 先程の疑問に戻る.
-- すなわち, `forall` が全称量子化ならば, なぜ存在型と呼ぶのだろう.
--
-- `forall a. a` はすべての型の共通部分であり, すなわちボトムのはずである.
-- これは値(つまり要素)がボトムだけであるような型(つまり集合)である.
--
-- いくつか例を挙げるa
-- 1. [forall a. a] はすべての型 `forall a. a` を持つ要素のリスト, つまりボトムのリストの型である
-- 2. [forall a. Show a => a] は全ての要素が型 `forall a . Show a => a` を持つようなリストの型である. Showクラス制約は集合を制限するが, まだこれらすべてに共通する値はボトムだけである.
-- 3. [forall a. Num a => a] 再び, それぞれの要素がすべてNumのインスタンスであるような型の要素のリストである. これが含めるのは型 `forall a. Num a => a` を持つような数値リテラル, つまりまたボトムだけである.
-- 4. `forall a. [a]` は, とにかく呼び出し側からみなされうる, なんらかの(同じ)型 a が要素であるリストの型である.
--
-- 型は, 多くの値を共通に持つわけではなく, いくつかの方法で大体の型の共通集合が結局はボトムの組み合わせになることがわかった.
--
--
-- さきほどは, 'type box' を使って異なる型を格納するリストを作った.
-- 理想的には, 異なる型を格納するリストは `[exists a. a]` という型,
-- すなわち「すべての要素が型 `exist a. a` を持つようなリスト」
-- であると良い.
-- この `exist` キーワード(これはHaskellには存在しない)は推測されるように
-- 型の和集合であり, そして `[exists a. a]` は
-- すべての要素がどんな型も取れる
-- (かつ異なる要素は同じ型である必要はない)リストの型なのである.
--
-- しかし, データ型を使ってほとんど同じ振る舞いを得たいのだった,
-- これを定義してみる.
--
--
-- Example: 存在データ型 (MkTは言語拡張により利用可能)
data T = forall a. MkT a

-- これは次のようなものを意味する
-- MkT :: forall a. a -> T
--
-- `MkT` に任意の値を渡すことができ, それは `T` へ変換される.
-- では, `MkT` の値を分解(deconstruct0h)するとき, 何が起きるのだろう.
--
--
-- Example: 存在型コンストラクタにおけるパターンマッチ
--
-- foo (MkT x) = ... -- <!-- what is the type of x? --> xの型は何?
--
-- 示した様に, `x` はどんな値でもとれる.
-- これは, それが何らかの任意の型の要素であることを意味し,
-- 型 `x :: exists a. a` を持つ.
-- 言い換えれば, この `T` の定義は次と同型(isomorphic) なのである
--
--
-- Example: この存在型データ型と等価なバージョン(疑似Haskell)
--
-- data T = MkT (exists a. a)
--
-- そして突然, 存在型が現れた.
-- 今, 不統一(heterogeneous)リストを作ることができる.
--
--
-- Example: 不純一 (heterogeneous) リストの構築
--
-- heteroList = [MkT 5, MkT (), MkT True, MkT map]
--
-- もちろん, `heteroList` をパターンマッチしたとき,
-- 知っているのはそれが何らかの任意の型であることだけなので,
-- その要素に対して何もすることはできない.
-- しかしながら, もしクラス制約を導入すれば,
--
--
-- Example: クラス制約を伴う新しい存在型データ型
--
-- data T' = forall a. Show a -> MkT' a
--
-- これは, 統一された型 (isomorphic)である.
--
--
-- Example: '真'の存在型へ変換された新しいデータ型
--
-- data T' = MkT' (exists a. Show a => a)
--
-- 再び和集合をとる型を制限するため, クラス制約を提供する.
-- `MkT'` の中にある値は, Showのインスタンスである何らかの任意の型の値
-- であることが分かる.
-- これが意味しているのは, 型 `exists a. Show a => a` の値に対して
-- `show` を適用できるということだ.
-- どの型なのか分かってもまったく問題はない.
--
--
-- Example: 新しい非統一機構の利用
--
-- heteroList' = [MkT' 5, MkT' (), MkT' True, MkT' "Sartre"]
--
-- main = mapM_ (\(MkT' x) -> print x) heteroList'
--
--
-- さて以降, まとめる.
-- データ型を全称量子化子の相互作用は存在型を生み出す.
-- `forall` を含む型の多くの興味深いのが,
-- この相互作用を使用することにより, それらの型を存在型とするのである.
--
-- 存在型が欲しいときはいつでもデータ構築子でそれをラップしなければならず,
-- [exists a. a] のように公然と現れることはできない
--
--
-- ==========
-- runST
-- ==========
--
-- STモナドは `State` モナドを強化したものであり,
-- より複雑な構造を持ち, より高度な話題を含んでいる.
-- これは本来Haskellに IO を提供するために書かれたものだ.
-- IO は基本的には現実世界の情報すべての環境付きのただの State モナドだ.
-- 実際, GHC内部では少なくともSTが使われており,
-- 環境は `RealWorld` と呼ばれる型である.
--
-- Stateモナドから外へ出るには, `runState` を使うことができる.
-- STにおける類似した関数は `runST` と呼ばれており, 以下のような型を持つ.
--
--
-- Example: runST関数
--
-- runST :: forall a. (forall s. ST s a) -> a
--
-- これはより複雑な「rank-2 多相 (polymorphism)」と呼ばれる言語機能の
-- 実例となっているが, ここで詳細には立ち入らない
-- 重要なのは, 「初期状態を与える引数は存在しない」ことに気づくことである.
-- 代わりに, STは State に対して異なる状態の記法を使用する.
-- State は現在の状態を取得 (get) と設定 (put) することを可能にするのに加え, ST参照のインターフェスを提供する.
-- `newSTRef` :: a -> ST s (STRef s a)` によって初期値を与え
-- `STRef` という型を持つ参照を作ると, これを操作する
-- `readSTRef :: STRef s a -> ST s a`
-- `writeSTRef :: STRef s a -> a -> ST s ()`
-- を使うことができる.
-- ST計算の内部環境はある特定のものではなく, それ自体は参照から値への
-- 対応付である.
-- それ故, 初期状態は単に参照を含まない空の対応付けなので,
-- runSTに初期状態を提供する必要はない.
--
-- しかしながら, 事はそれほど単純ではない.
-- ひとつのST計算において参照を作り, それが他で使われることを止めるにはどうすればよいだろうか(これはスレッド安全性などに関わってくる議題である)
-- ST計算は「初期内部環境はいかなる特定の参照を含む」という仮定をも許容スべきではないので, これを許容したくはない.
-- より具体的に言うならば, 次のようなコードは不正としたいわけである.
--
-- Example: 良くないSTコード
--
-- let v = runST (newSTRef True)
-- in runST (readSTRef v)
--
--
-- これを防ぐにはどうすればいいだろうか,
-- `runST` の型においての rank-2 多相の効果は
-- 「最初の引数のなかだけに `s` のスコープを制約すること」である.
--
-- 言い換えれば, この型変数 `s` は２つ目の引数には現れないが, 最初の引数に現れる
-- どうやってこれをうまくやるのか見ていく.
--
--
-- Example: より簡潔な悪いSTコード
-- ... runST (newSTRef True) ...
--
-- コンパイラはこの型を一致させようと試みる.
--
--
-- Example: コンパイラの型チェック段階
--
-- newSTRef True :: forall s. ST s (STRef s Bool)
-- runST :: forall a. (forall s. ST s a) -> a
-- together, forall a. (forall s. ST s (STRef s Bool)) -> STRef s Bool
--
-- 最初の括弧の `forall` の重要性は, その名前 `s` を変更することができることだ. これは次のように書ける.
--
--
-- Example: 型の不一致!
--
-- together, forall a. (forall s'. ST s' (STRef s' Bool) -> STRef s Bool
--
-- a'x. x > 5 というのは, a'y. y > 5 というのとちょうど同じ,
-- というのは数学的に理にかなっている.
-- 変数に別のラベルを与えているだけである.
-- しかし, 先程のコードには問題がある.
-- `runST` の返り値の型に対しては `forall` はスコープに含めないので,
-- そこでは `s` の名前を買えないことに注意すべきである.
-- しかし, 突如として型の不一致が起こる!
-- 最初の引数に置いて, ST計算の返り値の型は `runST` の帰り値の型と
-- 一致しなければならないが, そうなっていない!
--
-- この存在型の重要な機能は,
-- 「最初の引数でコンパイラに状態の型を一般化することを可能にし,
--   帰り値の型はそれに依存することはできないこと」
-- である.
-- これは, この存在型の問題をうまく回避し,
-- 異なる呼び出しの間で参照が共有されることができないことにより,
-- それぞれの `runST` 呼び出しを
-- それ自身の小さなヒープ内に '区切る' (compartmentalise) のだ.
--

-- ===========================
-- 組み込み要素としての量化
-- ===========================
--
-- 全称量化はまだ定義されていないデータ型を定義するのに便利である.
-- Haskellに組みのような型がなかったとしよう.
-- 量化は次のような定義を可能にする.
--
-- Example:
--
-- newtype Pair a b = Pair (forall c.(a -> b -> c) -> c)
--

-- ==========================================================
--
-- とまあここまで書いたけどさっぱり分からん
-- もうちょい分かりやすくかけんのかねまったく.
--
-- ということで独自の解釈を書いていく.
-- その後にこれらは消す.
--
-- まずいまいちつかめていないのは,
--
-- * 量化 : 
-- * 全称量化 :
-- * 存在型 :
-- * 型のランク :
--
-- であろう. ここらへんを具体的に知るところから始まる.
