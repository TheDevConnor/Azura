CXX := g++
CC := gcc

CFLAGS := -O2 -std=c99 -Wall -Wextra

BIN_PATH := bin
OBJ_PATH := obj
SRC_PATH := src

TARGET := $(BIN_PATH)/main

ifeq ($(OS),Windows_NT)
	TARGET := $(BIN_PATH)/main.exe
endif

SOURCE_FILES := $(wildcard $(SRC_PATH)/*.c*)

all:
	
	gcc -o $(TARGET) $(SOURCE_FILES) $(CFLAGS)