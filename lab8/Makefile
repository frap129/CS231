NAME := hangman

PHONY: clean build post-clean
clean:
	@rm -f $(NAME) $(NAME).hi $(NAME).o $<
	@echo "Build files removed!" $<
build:
	@echo "Compiling..." $<
	@ghc $(NAME).hs $<
	@echo "Done! Final Binary: ./$(NAME)" $<
post-clean:
	@rm -f $(NAME).hi $(NAME).o $<
	@echo "Build files removed!" $<

