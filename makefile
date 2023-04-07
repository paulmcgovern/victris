LD = /usr/bin/cl65
EXECUTABLE = victris
MEM_CFG    = vic20.cfg
LABEL_FILE = $(EXECUTABLE).lbl
FLATPAK    = /usr/bin/flatpak run --branch=stable --arch=x86_64
XVIC_CMD   = $(FLATPAK) --command=xvic net.sf.VICE
C1541_CMD  = $(FLATPAK) --command=c1541 net.sf.VICE

all: clean compile disk

# Run the executable in the emilator
run: clean compile
	$(XVIC_CMD) --autostart $(EXECUTABLE)

# Create a disk image that can be attached
# to the emulator
disk: compile
	$(C1541_CMD) -format $(EXECUTABLE),01 d64 $(EXECUTABLE).D64 -attach $(EXECUTABLE).D64 -write $(EXECUTABLE) 

compile:
	$(LD) victris.asm -Ln $(LABEL_FILE) -t vic20 -C $(MEM_CFG) -o $(EXECUTABLE)

.PHONY: clean
clean:
	rm -f *.o *.lbl *.D64 $(EXECUTABLE)

