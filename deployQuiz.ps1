stop-process -name Quiz.Console
remove-item c:\run\Quiz\*
copy-item c:\Quiz\Quiz.Console\bin\Debug\* c:\run\Quiz\
c:\run\Quiz\Quiz.Console.exe

