;;; Compiled snippets and support files for `java-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'java-mode
										 '(("while" "while (${1:true}) {\n      $0\n}" "while loop"
												(not
												 (sp-point-in-string-or-comment))
												nil nil "/home/arjen/.config/emacs/snippets/java-mode/while" nil "while")
											 ("v" "void $0" "void" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/void" nil "v")
											 ("var=" "${1:int} ${2:variable} = `%`$0;" "variable declaration & assignment"
												(not
												 (sp-point-in-string-or-comment))
												nil nil "/home/arjen/.config/emacs/snippets/java-mode/var=" nil "var=")
											 ("var" "${1:int} ${2:variable}" "variable declaration"
												(not
												 (sp-point-in-string-or-comment))
												nil nil "/home/arjen/.config/emacs/snippets/java-mode/var" nil "var")
											 ("try" "try {\n    `%`$0\n} catch (${1:Throwable} e) {\n      ${2:System.out.println(\"Error \" + e.getMessage());\n      e.printStackTrace();}\n}" "try"
												(not
												 (sp-point-in-string-or-comment))
												nil nil "/home/arjen/.config/emacs/snippets/java-mode/try" nil "try")
											 ("toStr" "public String toString() {\n       $0\n}" "toString" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/toString" nil "toStr")
											 ("." "this.$0" "this" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/this" nil ".")
											 ("tc" "import junit.framework.*;\nimport junit.textui.*;\n\npublic class Test${1:Class} extends TestCase {\n       protected void setUp() {\n                 $0\n       }\n}" "testClass" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/testClass" nil "tc")
											 ("test" "@Test\npublic void test_${1:Case}() {\n       $0\n}" "test" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/test" nil "test")
											 ("ret" "return `%`$0;" "return"
												(not
												 (sp-point-in-string-or-comment))
												nil nil "/home/arjen/.config/emacs/snippets/java-mode/return" nil "ret")
											 ("p" "public $0" "public" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/public" nil "p")
											 ("pr" "protected $0" "protected" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/protected" nil "pr")
											 ("pri" "private $0" "private" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/private" nil "pri")
											 ("println" "System.out.println(\"`%`$0\");" "println" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/println" nil "println")
											 ("printf" "System.out.printf(\"`%`$0%n\");" "printf" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/printf" nil "printf")
											 ("paintComponent" "@Override public void paintComponent(Graphics g) {\n          `%`$0\n}" "paintComponent (Swing)"
												(not
												 (sp-point-in-string-or-comment))
												nil nil "/home/arjen/.config/emacs/snippets/java-mode/paintComponent" nil "paintComponent")
											 ("new" "${1:Type} ${2:obj} = new ${3:$1}($4);$0" "new" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/new" nil "new")
											 ("method@" "@Override ${1:public} ${2:void} ${3:methodName}($4) {\n          $0\n}" "@Override method"
												(not
												 (sp-point-in-string-or-comment))
												nil nil "/home/arjen/.config/emacs/snippets/java-mode/method@" nil "method@")
											 ("method" "${1:void} ${2:name}($3) {\n    $0\n}" "method"
												(not
												 (sp-point-in-string-or-comment))
												nil nil "/home/arjen/.config/emacs/snippets/java-mode/method" nil "method")
											 ("main" "public static void main(String[] args) {\n       $0\n}" "main" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/main" nil "main")
											 ("doc" "/**\n * $0\n *\n */" "javadoc" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/javadoc" nil "doc")
											 ("iterator" "public Iterator<${1:type}> iterator() {\n       $0\n}\n" "iterator" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/iterator" nil "iterator")
											 ("interface" "interface ${1:`(f-base buffer-file-name)`} {\n          $0\n}" "interface" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/interface" nil "interface")
											 ("import" "import ${1:System.};\n$0" "import" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/import" nil nil)
											 ("ife" "if (${1:true}) {\n    `%`$2\n} else {\n    $0\n}" "ife" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/ife" nil "ife")
											 ("if" "if (${1:true}) {\n   $0\n}" "if"
												(not
												 (sp-point-in-string-or-comment))
												nil nil "/home/arjen/.config/emacs/snippets/java-mode/if" nil "if")
											 ("fore" "for (${1:Object} ${2:var} : ${3:iterator}) {\n    $0\n}\n" "foreach" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/foreach" nil "fore")
											 ("for" "for (${1:int i = 0}; ${2:i < N}; ${3:i++}) {\n    `%`$0\n}" "for" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/for" nil "for")
											 ("file" "public class ${1:`(file-name-base\n                    (or (buffer-file-name)\n                        (buffer-name)))`} {\n  $0\n}\n" "file_class" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/file_class" nil "file")
											 ("eq" "public boolean equals(${1:Class} other) {\n       $0\n}" "equals" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/equals" nil "eq")
											 ("/*" "/**\n * $0\n */" "doc"
												(not
												 (use-region-p))
												nil nil "/home/arjen/.config/emacs/snippets/java-mode/doc" nil "/*")
											 ("__init__" "public ${1:`(f-base buffer-file-name)`}($2) {\n       $0\n}" "constructor" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/constructor" nil "__init__")
											 ("class" "${1:public }class ${2:`(f-base buffer-file-name)`} {\n           $0\n}" "class" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/class" nil "class")
											 ("apr_assert" "if (Globals.useAssertions) {\n   ${1:assert ..};\n}\n" "apr_assert" nil nil nil "/home/arjen/.config/emacs/snippets/java-mode/apr_assert" nil "apr_assert")
											 ("@return" "@return ${1:description}" "return"
												(sp-point-in-comment)
												nil nil "/home/arjen/.config/emacs/snippets/java-mode/@return" nil "@return")
											 ("@param" "@param ${1:paramater} $0" "param"
												(sp-point-in-comment)
												nil nil "/home/arjen/.config/emacs/snippets/java-mode/@param" nil "@param")))


;;; Do not edit! File generated at Tue Dec 26 10:44:48 2023
