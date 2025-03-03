TARGET = flp-fun
SRC_FILES = $(wildcard src/*.hs)

GHC = ghc
GHC_FLAGS = -Wall

all: $(TARGET)

$(TARGET): $(SRC_FILES)
	$(GHC) $(GHC_FLAGS) -o $(TARGET) $(SRC_FILES)

clean:
	rm -f $(TARGET) src/*.hi src/*.o

run: $(TARGET)
	./$(TARGET)

archive: clean
	zip -r xkrato67.zip Makefile README.md ./src