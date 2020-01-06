=========================
fcntl.h
=========================

'fcntl.h' の情報を記述する.

Functions
===========

open
------

::

    void open(const char *pathname, int flags);
    void open(const char *pathname, int flags, mode_t mode);

ファイルのパス名を与えると, ファイルディスクリプタ(fd)を返す. fdは, この後に続くシステムコール(read, write, lseek, fcntlなど)で使用される小さな非負の整数である. 引数flagsには, アクセスモード `O_RDONLY`, `O_WRONLY`, `O_RDWR` のどれか一つが入る. それぞれ, 「読み込み専用」, 「書き込み専用」,「読み書き用」にファイルをオープンすることを要求するものである.

他にもflagsには, ファイル生成フラグとファイル状態フラグを0個以上の「ビット単位のOR」で指定できるが一旦省略(必要なら追記).

戻り値には新しいファイルディスクリプタを返す.
エラーが発生した場合は-1を返し, errnoが適切に設定される.
