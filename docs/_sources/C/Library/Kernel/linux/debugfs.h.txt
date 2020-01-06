=========================
debugfs.h
=========================

'linux/debugfs.h' の情報を記述する.


Functions
============

debugfs_create_dir
----------------------

::

    struct dentry *debugfs_create_dir (
        const char *name, struct dentry *parent);

debugfsファイルシステムにディレクトリを作成する.
第一引数には, 作成するディレクトリの名前の文字列が入っているポインタ, 第二引数には親となるdentry(NULLセットされている場合, debugfsファイルシステムのルートに作成される).

恐らくこれの返り値がdentryなので, この帰り値を `debugfs_create_dir` や `debugfs_create_file` のparent引数にすることで, ディレクトリツリー構造になっていくのだろう.

debugfs_create_file
----------------------

::

    struct dentry *debugfs_create_file(
        const char *name,
        mode_t *mode,
        struct dentry *parent,
        void *data,
        const dtruct file_operations *fops);

debugfsファイルシステムにファイルを作成する.
第一引数には, 作成するファイルの名前の文字列が入っているポインタ, 第二引数には当該ファイルのパーミション, 第三引数には親となるdentry(NULLセットされている場合, debugfsファイルシステムのルートに作成される), 第四引数にはよくわからん(inode.i_privateのポインタが, open時に指す先になるらしいがよくわからん), 第五引数にはハンドラテーブルのポインタを渡す.

ここで返り値として返される値を `debugfs_remove` に与えることで削除することができる. `debugfs_remove_recursive` の場合には, `debugfs_create_dir` の返り値を与えることで再帰的に削除されるのでこの返り値は必要ないようだ.

.. _debugfs_create_u32_tag:
debugfs_create_u32
--------------------

::

    struct dentry *debugfs_create_u32 (
        const char *name,
        mode_t mode,
        struct dentry *parent,
        u32 *value);

unsigned 32-bit 値を read/write するためのdebugfsファイルを作成する.
第一引数には, 作成するファイルの名前の文字列が入っているポインタ, 第二引数には当該ファイルのパーミション, 第三引数には親となるdentry(NULLセットされている場合, debugfsファイルシステムのルートに作成される), 第四引数にはこのファイルがread/writeする値へのポインタ.

debugfs_create_x32
---------------------

::

    struct dentry *debugfs_create_x32(
        const char *name,
        umode_t mode,
        struct dentry *parent,
        u32 *value);

:ref:`debugfs_create_u32_tag` のx32バージョン

debugfs_remove_recursive
-----------------------------

::

    void debugfs_remove_recursive( struct dentry *dentry );

ディレクトリを再帰的に削除する.
第一引数に削除したいディレクトリのdentryポインタを渡す.
再帰的にdebugfsのディレクトリツリーを削除する.
