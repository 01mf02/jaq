# build jaq and get the path of jaq executable
JAQ=$(shell cargo --quiet --config "target.'cfg(true)'.runner = 'echo'" run --release)
JOTDOWN=jotdown
FILES=intro.dj cli.dj corelang.dj stdlib.dj advanced.dj formats.dj examples.dj

all: MANUAL.xhtml MANUAL.shl.xhtml jaq.1 tests cli-tests broken-links
clean:
	rm `cat .gitignore`

body.xhtml: header.html $(FILES)
	(echo '<body>' && cat header.html && cat $(FILES) | $(JOTDOWN) && echo '</body>') > $@

%.xhtml: %.dj
	(echo "<body>" && $(JOTDOWN) $< && echo "</body>") > $@

MANUAL.xhtml: manual.jq body.xhtml style.css template.xhtml
	$(JAQ) -f manual.jq --rawfile body body.xhtml --rawfile style style.css template.xhtml --to xml > $@

jq-filters: MANUAL.xhtml
	$(JAQ) --raw-output0 '.. | select(.t? == "code" and .a.lang == "jq").c[] | @htmld' $< > $@

jq-asts: jq-filters
	cargo run --example syn-highlight < $< > $@

MANUAL.shl.xhtml: syn-highlight.jq MANUAL.xhtml jq-asts
	$(JAQ) -f syn-highlight.jq --rawfile manual MANUAL.xhtml jq-asts --to xml > $@

%.man: %.xhtml man.jq
	$(JAQ) -f man.jq $< -j > $@

jaq.1: header.1 man-prologue.man body.man man-epilogue.man
	cat $^ > $@

tests: body.xhtml
	$(JAQ) --to raw -f tests.jq $< | $(JAQ) --run-tests

# substitute `jaq` in command lines (starting with `# `) by $(JAQ)
cli-tests: body.xhtml shelltest
	$(JAQ) -j -f cli-tests.jq $< | sed '/^$ /s|jaq|$(JAQ)|g' | (cd .. && docs/shelltest)

broken-links: MANUAL.xhtml
	$(JAQ) -s -f ../examples/broken-links.jq $<

shelltest: shelltest.rs
	rustc $<
