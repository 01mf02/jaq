JAQ=cargo run --release --
FILES=intro.md cli.md corelang.md stdlib.md advanced.md formats.md examples.md

all: MANUAL.xhtml jaq.1 tests broken-links
clean:
	rm body.xhtml MANUAL.xhtml jaq.1

body.xhtml: header.html $(FILES)
	(echo '<body>' && cat header.html && cat $(FILES) | jotdown && echo '</body>') > $@

MANUAL.xhtml: manual.jq body.xhtml style.css template.xhtml
	$(JAQ) -f manual.jq --rawfile body body.xhtml --rawfile style style.css template.xhtml --to xml > $@

jaq.1: filter.lua man-prologue.md $(FILES) man-epilogue.md
	pandoc -V title="JAQ(1)" --lua-filter filter.lua --from djot man-prologue.md $(FILES) man-epilogue.md -s -o $@

tests: body.xhtml
	$(JAQ) --to raw -f tests.jq $< | $(JAQ) --run-tests

broken-links: MANUAL.xhtml
	$(JAQ) -s -f ../examples/broken-links.jq $<
