CXX := g++
CC := gcc

CFLAGS := -Wl,--subsystem,windows -municode -O2

BIN_PATH := bin
OBJ_PATH := obj
SRC_PATH := src

TARGET_BINARY := main

ifeq ($(OS),Windows_NT)
	TARGET_BINARY := $(addsuffix.exe,$(TARGET))
endif

TARGET := $(BIN_PATH)/$(TARGET_BINARY)

SRC := $(foreach x, $(SRC_PATH), $(wildcard $(addprefix $(x)/*,.c*)))
OBJ := $(addprefix $(OBJ_PATH)/, $(addsuffix .o, $(notdir $(basename $(SRC)))))

DISTCLEAN_LIST := $(OBJ) \
                  $(OBJ_DEBUG)
CLEAN_LIST := $(TARGET) \
			  $(TARGET_DEBUG) \
			  $(DISTCLEAN_LIST)

default: all

$(TARGET): $(OBJ)
	$(CC) $(CFLAGS) -o $@ $(OBJ)

$(OBJ_PATH)/%.o: $(SRC_PATH)/%.c
	$(CC) $(CCOBJFLAGS) -o $@ $<

$(OBJ_PATH)/%.o: $(SRC_PATH)/%.cpp
	$(CXX) $(CCOBJFLAGS) -o $@ $<

.PHONY: all
all: $(TARGET)

.PHONY: debug
debug: $(TARGET_DEBUG)

.PHONY: clean
clean:
	@echo CLEAN $(CLEAN_LIST)
	@rm -f $(CLEAN_LIST)

.PHONY: distclean
distclean:
	@echo CLEAN $(CLEAN_LIST)
	@rm -f $(DISTCLEAN_LIST)