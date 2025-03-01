TARGET = flp-fun
SRC_FILES = $(wildcard *.hs)

GHC = ghc
GHC_FLAGS = -Wall

all: $(TARGET)

$(TARGET): $(SRC_FILES)
	$(GHC) $(GHC_FLAGS) -o $(TARGET) $(SRC_FILES)

clean:
	rm -f $(TARGET) *.hi *.o

run: $(TARGET)
	./$(TARGET)
