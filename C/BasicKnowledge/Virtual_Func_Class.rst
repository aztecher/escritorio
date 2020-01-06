==========================
Virtual (Function/Class)
==========================

ここでは主に, ``仮想関数`` と ``仮想基本クラス`` に関して記述していく. もっと単順には, ``virtual`` の動作について説明する.

その前段階兼復習として, ``継承とポインタ`` に関しても少し議論しておく. この議論は特に, ``仮想関数`` で例示するコードサンプルを読むときに必要になるだろう.

継承とポインタ
===============

派生クラスのポインタ
----------------------

C++ では, クラス型の変数のポインタを生成することができる.

さらに, なんとクラス型のポインタ変数には 「派生クラスのアドレスを代入」できる. これは, オブジェクト指向未経験の方であればかなり斬新なものだろう.

派生クラスは, 基本クラスの情報を保有している. つまり, 基本クラス型に互換性があるため, ポインターに代入できる.

.. code-block:: cpp

    #include <iostream>
    using namespace std;
    
    class Base {
      public:
        char *str;
    };
    
    class Derived : public Base {
      public:
        int i;
    } derived;
    
    int main() {
      derived.str = "Kitty on your lap";
    
      // 派生クラスのインスタンスのアドレスを基本クラスのポインタ変数に代入
      // 派生クラスDerivedは, Baseクラスのメンバーを持っているためこれは有効
      // 基本クラスのポインタは, 派生クラスのオブジェクトを指すことが可能.
      Base *poly = &derived;
      cout << poly->str; // ポインタで参照
    
    
      return 0;
    }


上記でも書いたが, 派生クラスは基本クラスのメンバーを持っているため, 基本クラスのポインターでそれを指すことができた. しかし, 基本クラスのポインタは基本クラスの情報しか持たない. そのため, 基本クラスのポインタで派生クラスのオブジェクトを指していても, 派生クラスのメンバーを呼び出すことはできない. つまり, 以下のようなコードはコンパイルエラーとなる.

.. code-block:: cpp

    #include <iostream>
    using namespace std;
    
    class Base {
      public:
        char *str;
    };
    
    class Derived : public Base {
      public:
        int i;
    } derived;
    
    int main() {
      derived.str = "Kitty on your lap";
      derived.i = 1;
    
      Derived *d = &derived;
      cout << d->str << endl;
      cout << d->i << endl;
    
      Base *b = &derived; // OK
      cout << b->str << endl; // OK
      cout << b->i << endl; // ERROR: no membaer named 'i' in 'Base'
    
      return 0;
    }


ポインタで上記のようなことができたのだから, 参照でも可能とかんがえるのは妥当だろう. 参照も, 派生クラスのオブジェクト型を受け取ることができる.

.. code-block:: cpp

    #include <iostream>
    using namespace std;
    
    class Base {
      public:
        char *str;
    };
    
    class Derived : public Base {
      public:
        int i;
    } derive;
    
    // Base型のオブジェクトの参照を受け取る.
    // ポインタ同様に, 派生クラスの参照を渡すことができる.
    void getRef(Base &d) {
      cout << d.str;
    }
    
    int main() {
      derive.str = "Kitty on your lap";
      // Base型のオブジェクトの参照を受け取る関数に対し,
      // Derived型のオブジェクトの参照を渡している.
      getRef(derive);
      return 0;
    }


上記の機能は, 同名のメンバ名を持つクラス間で面白い現象を生じる. メンバ名が基本クラスと派生クラスで衝突している時, クラスを明示しなかった場合は, 常に自分のクラスのメンバを郵政んした.


しかし, ポインタが指すオブジェクトは基本クラスの情報しかない. そのため  基本クラスのメンバを呼び出すという現象が生じる.

.. code-block:: cpp

    #include <iostream>
    using namespace std;
    
    class Base {
      public:
        void paint() { cout << "Kitty on your lap\n"; }
    };
    
    class Derived : public Base {
      public:
        void print() { cout << "Di Gi Gharat\n"; }
    } derived;
    
    int main () {
      Base *b = &derived;
      derived.paint();
      b->paint();
    
      return 0;
    }


これをより発展させると, 動的なポリモーフィズムが実現できる. このことを理解した上で次のセクションからは, 高度なポリモーフィズムの関数のオーバーライドについて説明していく.

仮想関数
==========

基本クラスにおける仮想関数について取り上げ, 派生クラスによる関数オーバーライドと ``virtual`` に関して説明する.

関数のオーバーライド
----------------------

一般に基本クラスは汎用的な情報しか含まないため, 基本クラスの機能を拡張していく形で派生クラスを構築していく. このとき, 基本クラスのメンバー関数を `再定義` することができる. このような関数を ``仮想関数`` という. 仮想関数は `基本クラスで再定義可能であるということを明示する必要がある`. そのときに, ``virtual`` 宣言を行ない, これは基本クラスのみに記述すればよい(派生クラスでの再定義時には必要がない). 仮想関数の呼び出しは, 通常のメンバー関数と変わりなく行う.

ポリモーフィズム
------------------

この仮想関数を利用することで, ポインタが仮想関数を呼び出した場合に, `オブジェクトの型によって呼び出す関数が決定する` ことになり, いわゆる動的なポリモーフィズムを表現することができる. 例を以下に示す.

.. code-block:: cpp

    #include <iostream>
    using namespace std;
    
    // 基本クラス
    class Kitty {
      public:
        // 仮想関数 (これをオーバーライドする)
        virtual void paint() { cout << "Kitty on your lap\n"; }
    } kitty;
    
    // 派生クラス
    class Chobits : public Kitty {
      public:
        // 基本クラスの仮想関数を再定義
        void paint() { cout << "Chobits\n"; }
    } chobits;
    
    // 派生クラス
    class Di_Gi_Gharat : public Chobits {
      public:
        // 基本クラスの仮想関数を再定義
        void paint() { cout << "Di Gi Gharat\n"; }
    } di_gi_gharat;
    
    int main() {
      // TODO
      Kitty *ki = &kitty;
      Kitty *cho = &chobits;
      Kitty *digi = &di_gi_gharat;
    
      // polymorphism
      // ポインタから呼び出された時, オブジェクトの型に合わせて呼び出す.
      // この判断は, 「コンパイル時ではなく実行時にしている」
      ki->paint();
      cho->paint();
      digi->paint();
    
      return 0;
    }


上記の例を少し拡張したものを以下に示す. 実行されるメンバー関数(仮想関数とそのオーバーライド関数)が動的に決定される(実行時に決定される)ことをより直接的に表現したプログラムである. ユーザー入力を受け取り, その結果によって呼び出す関数が変化している事がわかる.

.. code-block:: cpp

    #include <iostream>
    using namespace std;
    
    class Chobits {
      public:
        virtual void paint() { cout << "Chobits\n"; }
    } chobits;
    
    class Di_Gi_Gharat : public Chobits {
      public:
        void paint() { cout << "Di Gi Gharat\n"; }
    } di_gi_gharat;
    
    int main() {
      Chobits *poly;
      unsigned char ch;
    
      cout << "ちぃ? y/n >";
      cin >> ch;
    
      // ユーザー入力によって, どちらのクラスのポインタか変更する
      if (ch == 'y') poly = &chobits;
      else poly = &di_gi_gharat;
    
      poly->paint();
      return 0;
    }


仮想関数を再定義しない場合の例. 仮想関数は必要がなければ再定義しなくてももちろんいい. その場合, 再定義していないオブジェクト型は最後に再定義したクラスのメンバ関数を参照する. これまで継承したクラスの中で, 再定義しているクラスがなければ, virtualを宣言した基本クラスの最初の仮想メンバー関数を参照する.

.. code-block:: cpp

    #include <iostream>
    using namespace std;
    
    class Kitty {
      public:
        virtual void paint() { cout << "Kitty on your lap\n"; }
    };
    
    // 派生クラス. 関数のオーバーライドをしている
    class Chobits : public Kitty {
      public:
        // この関数が, Di_Gi_Gharat の関数を呼び出す
        void paint() { cout << "Chobits\n"; }
    } chobits;
    
    // 派生クラスだが, 関数のオーバーライドをしていない
    class Di_Gi_Gharat : public Chobits {
    } di_gi_gharat;
    
    int main() {
      Kitty *poly = &di_gi_gharat;
    
      poly->paint();
      return 0;
    }



最後に, 少しネイティブな動作について説明する. 通常の関数やフレンド関数, メンバ関数などはコンパイル時にアドレスが判明する. これらは, コンパイル時点でそれぞれの関数を呼ぶアドレス情報が確定されるため, 関数の呼び出しにかかるオーバーヘッドが少なく, 非常に効率がいい. このように, コンパイル時点で確定している情報を「コンパイル時バインディング」と呼ぶ.


コンパイル時バインディングは, 高速に動作するが柔軟性に欠ける. 似たような動作でも, 異なるイベントとしてソースコードを記述しなければいけない.


これに対して, オーバーライドのようなオブジェクト指向における実行時に決定される情報を「実行寺バインディング」と呼ぶ. オーバーヘッドは大きくなるが, 非常に高い柔軟性がある.


仮想関数は実行時バインディングになる. どのオブジェクト型を実行するかは, 実行するまでわからないからである. これは非常に柔軟なプログラムを書くことができる反面, オーバーヘッドが大きいことを忘れないように.


仮想関数とデストラクタ
--------------------------

仮想関数が唯一思うように動いてくれない場合がある. これを知らないと, 設計時に思いがけない障害に突き当たることになる.


デストラクタは, 派生クラスから基本クラスに向かって順にコールされる. すなわち, 派生クラスから順にデータを開放していかないといけない. もしデストラクタが, 仮想関数を呼び出した場合, 派生クラスの情報はすでに崩壊している可能性がある.

そのため, C++のデストラクタはオーバーライドを行わない. デストラクタで仮想関数を呼び出しても, 実態関数にアクセスせずにデストラクタが発生しているクラスの仮想関数を呼び出す. これは, 参照回数などを管理する特殊な構造を設計するときに注意する必要がある.

.. code-block:: cpp

    #include <iostream>
    using namespace std;
    
    class Test1 {
      public:
        // 仮想関数
        virtual void func() { cout << "Kitty on your lap\n"; }
        ~Test1 () { this->func(); }
    };
    
    // Test1クラスを継承するTest2クラス
    // インスタンスtest2が破壊されると, 最終的に
    // ~Test1() が呼び出され, この時, 仮想関数 func() が呼び出されている.
    //
    // Test2クラスのインスタンスが~Test1() を呼び出しているため
    // (Test1に関してはインスタンスの作成が行われていないからそうなるよね)
    // 一見すると, Test2::func() が呼び出されることを期待するが,
    // そうはならず, Test1::func() が呼び出されている.
    class Test2 : public Test1 {
      public:
        void func() {
          cout << "Tokyo mew mew\n";
          Test1::func();
        }
    } test2;
    
    int main() { return 0; }


仮想クラス
============

仮想クラスについて取り上げ, その継承時の問題点と, その解決策としての ``virtual`` に関して説明する.

クラスの継承と問題点
-----------------------

C++の継承は非常に柔軟だが, 継承が複雑になるに連れ, 同じクラスを二度継承するという自体が発生してしまうという問題が生じる.

.. code-block:: text

    基本クラス -------> 派生クラス1 --------> 派生クラス3
                 |                             ^
                 |                             |
                 \----> 派生クラス2 -----------/

上の図を見て分かる通り, 派生クラス3は基本クラスを2度継承していしまっている. これは派生クラス1, 2がどちらも基本クラスを継承していることに起因する.

.. code-block:: cpp

    #include <iostream>
    using namespace std;
    
    class Base {
      public:
        char *name;
    };
    
    class Derived1 : public Base {
      public:
        int age;
    };
    
    class Derived2 : public Base {
      public:
        char *sex;
    };
    
    // Derive1, 2の基本クラスが同一なので,
    // Baseを2解継承することになり, コンパイルエラーとなる.
    class Derived3 : public Derived1, public Derived2 {
      public:
        void print() {
          cout << "名前: " << name;
          cout << "\t年齢: " << age;
          cout << "\t性別: " << sex << endl;
        }
    }; derive;
    
    int main() {
      derive.name = "Mikiyaf"; // 非常に曖昧
      derive.age = 24;
      derive.sex = "男";
      derive.print();
      return 0;
    }


上記のような問題を解決する方法として ``仮想基本クラス`` がある. Baseクラスを仮想基本クラスと宣言すると, 派生クラスで2度継承する自体を避けることができる. これを宣言する際に, ``virtual`` キーワードを利用する.

``virtual`` とアクセス指定子(public, private, protected)の順番は任意.
(おそらく, virtualが頭につくことが多い)

.. code-block:: cpp

    #include <iostream>
    using namespace std;
    
    class Base {
      public:
        char *name;
    };
    
    class Derived1 : virtual public Base {
      public:
        int age;
    };
    
    class Derived2 : virtual public Base {
      public:
        char *sex;
    };
    
    class Derived3 : public Derived1, public Derived2 {
      public:
        void print() {
          cout << "名前: " << name;
          cout << "\t年齢: " << age;
          cout << "\t性別: " << sex << endl;
        }
    } derive;
    
    int main() {
      derive.name = "Mikiyaf";
      derive.age = 24;
      derive.sex = "男";
      derive.print();
      return 0;
    }


Derived3 が継承した基本クラスは, 仮想基本クラスとして Base を継承する. そのため, Derived3 オブジェクトは, Baseのコピーを一つしか持たない. こうすることで, 並列的多重継承を保証することができる.


virtual キーワードは, 仮想基本クラスを持つ ``派生クラスからしか意味を持たない`` つまり, 上のプログラムのDerived1, Derived2からは継承した, Baseクラスは通常の基本クラスとして機能する.

基本仮想クラスのアクセス制御
------------------------------

仮想基本クラスを持つ, 二つの基本クラスがそれぞれ別のアクセス指定子で継承している場合, 派生クラスからアクセス制御は, 最もオープンなアクセス制御を取る.

.. code-block:: cpp

    #include <iostream>
    using namespace std;
    
    class Base {
      public:
        char *name;
    };
    
    class Derived1 : virtual public Base {
      public:
        int age;
    };
    
    class Derived2 : virtual private Base {
      public:
        char *sex;
    };
    
    // そもそも, Derive3が継承時にpublicで継承していないと,
    // printで各変数名でアクセスすることはできない.
    class Derived3 : public Derived1, public Derived2 {
      public:
        void print() {
          cout << "名前; " << name; // ここのアクセスは, publicになる
          cout << "\t年齢: " << age;
          cout << "\t性別: " << sex << endl;
        }
    } derive;
    
    int main() {
      derive.name = "Mikiyaf";
      derive.age = 24;
      derive.sex = "male";
      derive.print();
      return 0;
    }

