# build jaq and get the path of jaq executable
JAQ=$(shell cargo --quiet --config "target.'cfg(true)'.runner = 'echo'" run --release)
JOTDOWN=jotdown
FILES=intro.dj cli.dj corelang.dj stdlib.dj advanced.dj formats.dj examples.dj

all: MANUAL.xhtml jaq.1 jq-tests shell-tests broken-links
clean:
	rm `cat .gitignore`

body.xhtml: header.html $(FILES)
	(echo '<body>' && cat header.html && cat $(FILES) | $(JOTDOWN) && echo '</body>') > $@

%.xhtml: %.dj
	(echo "<body>" && $(JOTDOWN) $< && echo "</body>") > $@

MANUAL.xhtml: manual.jq body.xhtml style.css hl.json template.xhtml hl.json
	$(JAQ) -L . -f manual.jq --rawfile body body.xhtml --rawfile style style.css template.xhtml --to xml > $@

%.man: %.xhtml man.jq
	$(JAQ) -f man.jq $< -j > $@

jaq.1: header.1 man-prologue.man body.man man-epilogue.man
	cat $^ > $@

snippets.json: body.xhtml snippets.jq
	$(JAQ) -f snippets.jq $< > $@

hl-shell.json: snippets.json shelltest
	$(JAQ) --raw-output0 '.shell // empty' $< | ./shelltest --highlight > $@

hl-%.json: snippets.json
	$(JAQ) --raw-output0 '.$* // empty' $< | cargo run --example $*-syn-highlight > $@

hl.json: snippets.json hl-jq.json hl-xjon.json hl-shell.json
	$(JAQ) -n -L . -f hl.jq $< > $@

jq-tests: snippets.json
	$(JAQ) --to raw -f jq-tests.jq $< | $(JAQ) --run-tests

# substitute `jaq` in command lines (starting with `$ `) by $(JAQ)
shell-tests: snippets.json shelltest
	$(JAQ) --raw-output0 '.shell // empty' $< | sed '/^$ /s|jaq|$(JAQ)|g' | (cd .. && docs/shelltest)

broken-links: MANUAL.xhtml
	$(JAQ) -s -f ../examples/broken-links.jq $<

shelltest: shelltest.rs
	rustc $<
