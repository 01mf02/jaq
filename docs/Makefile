# build jaq and get the path of jaq executable
JAQ=$(shell cargo --quiet --config "target.'cfg(true)'.runner = 'echo'" run --release)
JOTDOWN=jotdown
FILES=intro.dj cli.dj corelang.dj stdlib.dj advanced.dj formats.dj examples.dj

all: MANUAL.xhtml jaq.1 tests cli-tests broken-links
clean:
	rm `cat .gitignore`

body.xhtml: header.html $(FILES)
	(echo '<body>' && cat header.html && cat $(FILES) | $(JOTDOWN) && echo '</body>') > $@

%.xhtml: %.dj
	(echo "<body>" && $(JOTDOWN) $< && echo "</body>") > $@

MANUAL.xhtml: manual.jq body.xhtml style.css hl.json template.xhtml
	$(JAQ) -f manual.jq --rawfile body body.xhtml --rawfile style style.css --slurpfile hl hl.json template.xhtml --to xml > $@

hl.json: hl-jq.json hl-xjon.json
	$(JAQ) -n --slurpfile hl_jq hl-jq.json --slurpfile hl_xjon hl-xjon.json \
	   	'{"jq": $$hl_jq | add, "xjon": $$hl_xjon | add}' > $@

hl-jq.json: tests.json
	$(JAQ) --raw-output0 '.filter' $< | cargo run --example syn-highlight | \
		$(JAQ) -n --slurpfile tests $< '$$tests[].filter | {(.): first(inputs)}' > $@

hl-xjon.json: tests.json
	$(JAQ) --raw-output0 '.output' $< | cargo run --example xjon-syn-highlight | \
		$(JAQ) -n --slurpfile tests $< '$$tests[].output | {(.): first(inputs)}' > $@

%.man: %.xhtml man.jq
	$(JAQ) -f man.jq $< -j > $@

jaq.1: header.1 man-prologue.man body.man man-epilogue.man
	cat $^ > $@

tests.json: body.xhtml tests.jq
	$(JAQ) -f tests.jq $< > $@

tests: tests.json
	$(JAQ) --to raw -f jq-tests.jq $< | $(JAQ) --run-tests

# substitute `jaq` in command lines (starting with `# `) by $(JAQ)
cli-tests: body.xhtml shelltest
	$(JAQ) -j -f cli-tests.jq $< | sed '/^$ /s|jaq|$(JAQ)|g' | (cd .. && docs/shelltest)

broken-links: MANUAL.xhtml
	$(JAQ) -s -f ../examples/broken-links.jq $<

shelltest: shelltest.rs
	rustc $<
