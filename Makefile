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

.PHONY: clean
clean:
	rm -f $(TARGET_NAMES) *.exe
	rm -f *.o *.ali *~ *.bak *.out

