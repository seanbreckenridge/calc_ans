TARGET_BIN="${HOME}/.local/bin"
TARGET_SHARE="${HOME}/.local/share"
TARGET_EXE=_build/default/bin/main.exe

$(TARGET_EXE): bin/dune bin/main.ml lib/calc_ans.ml lib/dune calc_ans.opam dune-project tokens build
	@ bash ./build

install: $(TARGET_EXE) files
	@cp -v $(TARGET_EXE) $(TARGET_BIN)/calc_ans
	@chmod +x $(TARGET_BIN)/calc_ans

files:
	@mkdir -pv $(TARGET_BIN)
	@rm -fv $(TARGET_BIN)/calc_ans
	@mkdir -p $(TARGET_SHARE)/calc_ans
	@cp -v ./tokens $(TARGET_SHARE)/calc_ans/tokens
	@cp -v ./calc_ans_rlwrap $(TARGET_BIN)

clean:
	rm -rf _build
