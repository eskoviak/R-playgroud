CXX := clang++
CXX_STD := -std=c++2b
CXX_STD_BETA := -std=c++2c
CXX_FLAGS :=
INCLUDES := 

PROJECT_HOME := /Users/edmundlskoviak/Documents/repos/activity_playgroud
LIBS := -L/usr/local/lib -L/opt/homebrew/Cellar/libpq/16.2/lib -lpqxx -lpq

SRC := src
BUILD := build

eigen-sample: $(SRC)/eigen-sample.cpp
	$(CXX) $(CXX_STD) $(CXX_FLAGS) -o $(BUILD)/$@ $<
 