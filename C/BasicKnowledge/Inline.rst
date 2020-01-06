==============
inline
==============

ここでは, ``inline`` に関して記述していく.

inline
============

inline(インライン)指定子は, コンパイラに対して特定の関数をインライン展開するようヒントを与える. インライン展開とは, 分かってしまえば読んで時のごとくなのだが, コードを展開することを意味する. インライン関数で定義される関数が別のソース内で呼ばれた際, その位置に関数の内容を展開する. これはマクロによる関数定義に動作としては似通っており, 効果としても関数呼び出しのオーバーヘッドの減少などが目的であるためその点でも同じだろう. しかし, インライン展開の場合は先に述べたとおり, コンパイラに対してのヒントなので, この展開処理はコンパイラによって行われる(どうも行われない場合もあるらしい. その辺はコンパイラ先生が勝手にやるみたい). これは, マクロの展開をプリプロセッサが行なうことに比べるとおおきな違いである.

もちろん関数内部のコードを展開するので, むやみに長い関数に適用するといたずらにコードの総量を増やすことになり, 逆に関数呼び出しのオーバーヘッドよりも処理速度を要求されることになる(という話もある. 実際に体験したことはないが). この話は, インクルードディレクティブの話と同じである.

また, inlineを指定すれば必ずインライン関数になるわけではなく, 必ず *コンパイラの宣言に従う* 必要がある. インライン展開できなかった場合には, 通常の関数として扱われる.

inline vs マクロ
------------------

MSDNによると, inlineとマクロに関しては次のような言及がある.

.. note::

    インライン関数はマクロに似ていますが, インライン関数がコンパイラによって解析されるのに対し(関数コードはコンパイル時に呼び出しの時点で展開されるため), マクロはプリプロセッサによって展開されれます. 

    そのため, これらにはいくつかの重要な違いがあります.

    インライン関数は, 通常の関数に適用されるタイプセーフの全てのプロトコルに従う.

    インライン関数は他の関数と同じ構造を使用して指定されますが, 関数宣言に ``inline`` キーワードを指定する点が異なる

    インライン展開に引数として渡された式は1回だけ評価される. マクロでは, 引数として渡された式が複数回評価される可能性があります.

なお, MSDNでも, インライン関数は小さい関数で使用することを推奨しており, オーバーヘッド削減効果がある関数として紹介されている.


Disassemble
------------

話としてはわかったが, やっぱり実際に見てみたいというのが性だろう. というわけで, プリプロセス終了段階と, バイナリ段階で展開の様子を確認してみる.

以下のようなコードを用意する.

.. code-block:: c


    #include <stdio.h>
    #define SQUARE(x) ((x)*(x))
    
    // Clang では, static が付いているか,
    // 最適化オプションがないと
    // inlineはエラーになるらしい
    inline static void hello() {
      printf ("hello, world\n");
    }
    
    int main() {
      printf ("SQUARE = %d\n", SQUARE(2));
      hello();
      return 0;
    }

以下のようなmakefileでプリプロセス段階を.ppというファイルで書き出し, 通常のコンパイルは通常通り行なう.

.. code-block:: make

    ##################################################################
    # Makefile Template
    #
    # please replace the name 'filename' to your program name,
    # and add the FLAGS or libraries you need to compile the program
    #
    # Date:  2018.03.04
    # author: Mikiya Michishita
    ###################################################################
    
    CFLAGS := -I/usr/local/include -g -O2 -Wall
    
    CC := CC
    
    DEFAULT_TARGET := help
    
    # list up all targets
    ALL_TARGET := inline-after-preprocess \
    	inline
    
    CLEAN_TARGET := inline
    
    target: $(DEFAULT_TARGET) ## help
    
    all: $(ALL_TARGET) ## Make all files
    
    
    inline-after-preprocess: inline.c ## create preprocessed code
    	${CC} $(CFLAGS) -E inline.c >> inline.pp
    
    inline: inline.o ## Make `inline` executable file by `inline.c` source code
    	${CC} $(CFLAGS) inline.o -o inline
    
    clean: ## Remove object file and others
    	rm *.o
    	rm *.pp
    	rm $(CLEAN_TARGET)
    
    clean_tags: ## Remove tags created by ctags and gtags
    	rm tags
    	rm GPATH GTAGS GRTAGS
    
    # compile option
    .c.o:
    	${CC} ${CFLAGS} -c $<
    
    .C.o:
    	${CC} ${CFLAGS} -c $<
    
    .PHONT: help
    help:
    	@grep -E '^[a-zA-Z_-]+:.*##.*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*##"}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

プリプロセッサが解釈したあとのコードを確認してみる.
ヘッダーの展開の影響でコード量が膨大になっているため, 必要な部分のみ抜き出すことにする.

.. code-block:: text

    inline static void hello() {
      printf ("hello, world\n");
    }
    
    int main() {
      printf ("SQUARE = %d\n", ((2)*(2)));
      hello();
      return 0;
    }

前述の説明の通り, プリプロセッサが翻訳した段階では, マクロは展開されているが, インライン関数自体の展開は行われていない.

さて, 作成されたバイナリをgdbで解析してみる.

以下に示すのは, 最適化オプションを切ったもの ``-O0`` である(インライン展開は期待できない). ちょうど, つぎにコールされるのが ``hello()`` 関数である.

.. code-block:: sh

    [-------------------------------------code-------------------------------------]
       0x100000f24 <main+20>:       mov    DWORD PTR [rbp-0x4],0x0
       0x100000f2b <main+27>:       mov    al,0x0
       0x100000f2d <main+29>:       call   0x100000f70
    => 0x100000f32 <main+34>:       mov    DWORD PTR [rbp-0x8],eax
       0x100000f35 <main+37>:       call   0x100000f50 <hello>
       0x100000f3a <main+42>:       xor    eax,eax
       0x100000f3c <main+44>:       add    rsp,0x10
       0x100000f40 <main+48>:       pop    rbp


以下は, 最適化オプション ``-O2`` をつけたものである(インライン展開が期待できる). ちょうど, つぎにコールされるのが ``hello()`` 関数である.

.. code-block:: sh

    [-------------------------------------code-------------------------------------]
       0x100000f4b <main+11>:       mov    esi,0x4
       0x100000f50 <main+16>:       xor    eax,eax
       0x100000f52 <main+18>:       call   0x100000f68
    => 0x100000f57 <main+23>:       lea    rdi,[rip+0x47]        # 0x100000fa5
       0x100000f5e <main+30>:       call   0x100000f6e
       0x100000f63 <main+35>:       xor    eax,eax
       0x100000f65 <main+37>:       pop    rbp
       0x100000f66 <main+38>:       ret

見てわかるとおり, シンボルテーブルから helloが消えていることがわかる.

メンバ関数のインライン化
==========================

メンバ関数も通常の関数同様にインライン関数として定義することができる.
メンバ関数のインライン化の場合, 通常はクラス内部で内容を定義してしまう.
(つまり, ヘッダーで定義し, 実態をソースで書くような形でメンバ関数をインライン化すると, ヘッダーの方のクラス内部に内容を定義するということ...だろうか / 定義と実装は分離した上で実態としてはクラス内部に展開するという形になるということだろうか)


さらに, クラス内で定義したメンバ関数の場合は, inlineを指定しなくても, *自動的にインライン化* される. もちろん, ``inline`` キーワードを指定しても良い. インライン化できなかった場合は通常の関数として扱われる.
