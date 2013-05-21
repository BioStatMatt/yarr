<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
</head>
<body>
<div style="width: 700px;">
<tt>R</tt> output often contains characters that must be escaped when embedded into HTML documents. The <tt>yarr</tt> package will automatically escape special HTML characters in output when <tt>&lt;&lt;/=</tt> or <tt>&lt;&lt;/@</tt> are used in place of <tt>&lt;&lt;=</tt> or <tt>&lt;&lt;@</tt>. The following example was last evaluated <</= cat(format(Sys.time(), "%m/%d/%y")) >>.
<pre>
<</@
f <- function(x) {
    x + 1
}
f(1)
>>
</pre>
In this example, the less-than and greater-than characters, <tt>&lt;</tt> and <tt>&gt;</tt> are automatically escaped.
</div>
</body>
</html>
