# creating-languages
So far what we have are our Python 3 fixes for Norvig's small lispy interpreter in lispy.py (the original pun of lis.py being lost, I know) and our PangolinV1 intepreter Pangolin1Lispy.py

Reading the code for both of these will be a useful guide for writing very small compact interpreters, but even more complex & robust builds will be coming over the next month to supplement your experiments in language design.

Now what's coming is
+ a Python interpreter that builds a more traditional object-based AST before evaluating the code
+ a Haskell interpreter that shows why functional languages like Haskell are often used by computer scientists to experiment with language design
+ a second Python interpreter that will show how to implement call-by-reference instead of call-by-value and will talk more about the idea of pointers

If you have any questions or find any problems in this code, please open an issue in Github (https://github.com/clarissalittler/creating-languages/issues) and let me know! 