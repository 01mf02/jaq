# build jaq and get the path of jaq executable
JAQ=$(shell cargo --quiet --config "target.'cfg(true)'.runner = 'echo'" run --release)
FILES=intro.dj cli.dj corelang.dj stdlib.dj advanced.dj formats.dj examples.dj

all: MANUAL.xhtml jaq.1 tests cli-tests broken-links
clean:
	rm body.xhtml MANUAL.xhtml jaq.1 shelltest

body.xhtml: header.html $(FILES)
	(echo '<body>' && cat header.html && cat $(FILES) | jotdown && echo '</body>') > $@

MANUAL.xhtml: manual.jq body.xhtml style.css template.xhtml
	$(JAQ) -f manual.jq --rawfile body body.xhtml --rawfile style style.css template.xhtml --to xml > $@

jaq.1: filter.lua man-prologue.dj $(FILES) man-epilogue.dj
	pandoc -V title=JAQ -V section=1 --lua-filter filter.lua --from djot man-prologue.dj $(FILES) man-epilogue.dj -s -o $@

tests: body.xhtml
	$(JAQ) --to raw -f tests.jq $< | $(JAQ) --run-tests

# substitute `jaq` in command lines (starting with `# `) by $(JAQ)
cli-tests: body.xhtml shelltest
	$(JAQ) -j -f cli-tests.jq $< | sed '/^$ /s|jaq|$(JAQ)|g' | (cd .. && docs/shelltest)

broken-links: MANUAL.xhtml
	$(JAQ) -s -f ../examples/broken-links.jq $<

shelltest: shelltest.rs
	rustc $<
