TARGET_NAMES = decrypt encrypt key_decrypt key_encrypt

ifeq ($(shell uname -o),Cygwin)
BIN_EXT=.exe
else
BIN_EXT=
endif

TARGETS = $(addsuffix $(BIN_EXT),$(TARGET_NAMES))

CC = gnatmake

.PHONY: all
all: $(TARGETS)

# Yeah, but it's a small project.
ALL_ADA = $(wildcard *.ads) $(wildcard *.adb)

$(TARGETS): $(ALL_ADA)
	$(CC) $(basename $@)

.PHONY: test
test: $(TARGETS)
	cat message.plain | ./encrypt > test.enc.out
	(head -1 message.plain; cat test.enc.out) | ./decrypt > test.dec.out
	echo YOURMOTHERWASAHAMPSTERANDYOURFATHERSMELTOFELDERBERRIESX | diff - test.dec.out
	cat null_test.plain | ./encrypt > test.enc.out
	(head -1 null_test.plain; cat test.enc.out) | ./decrypt > test.dec.out
	echo AAAAAAAAAAAAAAA | diff - test.dec.out
	cat foo_key.plain | ./key_encrypt > test.enc.out
	(head -1 foo_key.plain; cat test.enc.out) | ./key_decrypt > test.dec.out
	echo AAAAAAAAAAAAAAA | diff - test.dec.out
	@echo ok

ZIP_FILE = solitaire.zip
.PHONY: zip
zip: $(ZIP_FILE)

FILES_TO_ZIP = README.md LICENSE $(wildcard *.ads *.adb *.cipher *.plain)
$(ZIP_FILE): $(FILES_TO_ZIP)
	rm -f $@
	zip $@ $(FILES_TO_ZIP)

.PHONY: clean
clean:
	rm -f $(TARGET_NAMES) *.exe solitaire.zip
	rm -f *.o *.ali *~ *.bak *.out
