BIN=fortran-calendar-jp
MKFILE:=-C .. -f makefile

run: clean release
	../build/app/$(BIN) \
  --dir ~/2_tools/fortran-calendar-jp/data \
  --date_fr "2000-01-01" \
  --date_to "2024-12-31"

release:
	make release $(MKFILE)

test:
	make test $(MKFILE)

install:
	make install $(MKFILE)

uninstall:
	make uninstall $(MKFILE)

clean:
	make clean $(MKFILE)
