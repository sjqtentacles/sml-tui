MLTON      ?= mlton
BIN        := bin
LIBDIR     := lib/github.com/sjqtentacles/sml-tui
TEST_MLB   := test/sources.mlb
SRCS       := $(wildcard $(LIBDIR)/*.sml $(LIBDIR)/*.sig) $(wildcard test/*.sml) \
              $(TEST_MLB) $(LIBDIR)/sources.mlb

.PHONY: all test poly test-poly all-tests example screenshot clean

all: $(BIN)/test-mlton

$(BIN)/test-mlton: $(SRCS) | $(BIN)
	$(MLTON) -output $@ $(TEST_MLB)

example: $(BIN)/counter

$(BIN)/counter: $(SRCS) examples/counter.sml examples/sources.mlb | $(BIN)
	$(MLTON) -output $@ examples/sources.mlb

# Render a styled dashboard frame straight to a PNG (no terminal needed).
screenshot: $(BIN)/dashboard
	mkdir -p assets
	./$(BIN)/dashboard

$(BIN)/dashboard: $(SRCS) examples/dashboard.sml examples/render.sml examples/dashboard.mlb | $(BIN)
	$(MLTON) -output $@ examples/dashboard.mlb

test: $(BIN)/test-mlton
	$(BIN)/test-mlton

poly: $(BIN)/test-poly

$(BIN)/test-poly: $(SRCS) tools/polybuild | $(BIN)
	sh tools/polybuild -o $@ $(TEST_MLB)

test-poly: $(BIN)/test-poly
	$(BIN)/test-poly

all-tests: test test-poly

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -rf $(BIN)
