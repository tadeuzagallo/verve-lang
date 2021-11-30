CXX=clang++
LDFLAGS=
CXXFLAGS=-std=c++17

SRCS=$(wildcard src/*.cpp)
HEADERS=$(wildcard src/*.h)
OBJS=$(patsubst %.cpp,%.o,$(SRCS))

build/verve: $(OBJS)
	$(CXX) $(LDFLAGS) -o $@ $^

src/%.o: build/%.cpp $(HEADERS)
	$(CXX) $(CXXFLAGS) -c $< -o $@
