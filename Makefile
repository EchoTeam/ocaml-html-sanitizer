# If nothing works, try `ocamlmklib -o $(PACKAGE) *.ml`

PACKAGE=ocaml-html-sanitizer

REQUIRES="str,netstring"

OCAMLOPT = ocamlfind ocamlopt -package $(REQUIRES)
OCAMLC = ocamlfind ocamlc -package $(REQUIRES)

PARSERS=CSS_EscapeParser.mly CSS_Parser.mly HE_Parser.mly
LEXERS=CSS_EscapeLexer.mll CSS_Lexer.mll HE_Lexer.mll
LEXCMXS=${LEXERS:.mll=.cmx}

LIBCMXS=CSS_Types.cmx CSS_EscapeParser.cmx CSS_EscapeLexer.cmx CSS_Parser.cmx CSS_Lexer.cmx HE_Parser.cmx HE_Lexer.cmx CSS.cmx Wordbreak.cmx Sanitizer.cmx
LIBCMOS=${LIBCMXS:.cmx=.cmo}
LIBCMIS=Sanitizer.cmi

LIBS=$(PACKAGE).cma $(PACKAGE).cmxa $(PACKAGE).a lib$(PACKAGE).a

all: compile-parsers compile-lexers $(LIBS)

html_sanitizer: compile-parsers compile-lexers $(LIBS) $(LIBCMIS) html_sanitizer.cmx
	$(OCAMLOPT)  -linkpkg -o html_sanitizer $(LIBCMXS) html_sanitizer.cmx

install:
	@echo "Use install-package if you want to do a system-wide install"

install-package: uninstall $(LIBCMIS) $(LIBS)
	ocamlfind install $(PACKAGE) $(LIBS) $(LIBCMIS) META

compile-parsers: ${PARSERS:.mly=.ml} ${PARSERS:.mly=.mli} ${PARSERS:.mly=.cmi}

compile-lexers: ${LEXERS:.mll=.ml}

uninstall:
	ocamlfind remove $(PACKAGE)

$(PACKAGE).cma: $(LIBCMIS) $(LIBCMOS)
	ocamlmklib -o $(PACKAGE) $(LIBCMOS)

$(PACKAGE).cmxa: $(LIBCMIS) $(LIBCMXS)
	ocamlmklib -o $(PACKAGE) $(LIBCMXS)

lib$(PACKAGE).a: $(LIBCMIS) $(LIBCMXS)
	ocamlmklib -o lib$(PACKAGE) $(LIBCMXS)

check: html_sanitizer
	./html_sanitizer --string "<SCRIPT>script</script>" --verify "script"
	echo "unclosed</script>" | ./html_sanitizer --file - --verify "unclosed"
	./html_sanitizer --test ./tests/*.ht
	./html_sanitizer --string "замок" --verify "замок"
	./html_sanitizer --string "<script>foo</script>" --verify '<script>foo</script>' --level transparent
	./html_sanitizer --string "<p >foo</p >" --verify "<p>foo</p>" --level transparent
	./html_sanitizer --string "<p unknown=value>foo" --verify '<p unknown="value">foo</p>' --level transparent
	./html_sanitizer --string "<unknown-tag bar>foo<p>" --verify '<unknown-tag bar="bar">foo<p></p></unknown-tag>' --level transparent
	./html_sanitizer --string "<unknown-tag bar='<script>' OnClick='alert(1)' a=b>foo" --level permissive --verify '<unknown-tag bar="<script>" a="b">foo</unknown-tag>'
	./html_sanitizer --string "<tag>0123456789abcdef<foo></tag>" --permitted-tags "tag" --verify "<tag>0123456789abcdef</tag>"
	./html_sanitizer --string "<tag>0123456789abcdef<foo></tag>" --permitted-tags "tag foo" --verify "<tag>0123456789abcdef<foo></foo></tag>"
	./html_sanitizer --string "<tag>0123456789abcdef</tag>" --permitted-tags "tag" --break-long-words true --verify "<tag>0123456789<wbr></wbr>abcdef<wbr></wbr></tag>"
	./html_sanitizer --break-long-words true --string "http://example.com/somelongexpression, and then some" --verify "http://example.com/somelongexpression<wbr></wbr>, and then some<wbr></wbr>"
	./html_sanitizer --string "foo<p>bar<br>baz</p>zab" --level nanohtml --verify "foo<br>bar<br>baz<br>zab"

CSS_Parser.cmi: CSS_Types.cmo

Wordbreak.cmx: Wordbreak.ml
	$(OCAMLOPT) -pp camlp4o -c $<

Wordbreak.cmo: Wordbreak.ml
	$(OCAMLC) -pp camlp4o -c $<

.SUFFIXES: .ml .mli .mly .mll .cmx .cmo .cmi

.mly.ml:
	ocamlyacc -v $<

.mly.mli:
	ocamlyacc $<

.mll.ml:
	ocamllex $<

.ml.cmx:
	$(OCAMLOPT) -o $@ -c $<

.ml.cmo:
	$(OCAMLC) -o $@ -c $<

.mli.cmi:
	$(OCAMLC) -o $@ $<

clean:
	rm -f *.cm* *.[ao] *.output
	rm -f ${PARSERS:.mly=.ml} ${PARSERS:.mly=.mli}
	rm -f ${LEXERS:.mll=.ml}
	rm -f html_sanitizer
