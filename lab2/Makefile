NAME := lab2

PHONY: clean build
clean:
	@rm -f $(NAME) $<
	@echo "Build files removed!" $<
build:
	@echo "Compiling..." $<
	@gcc $(NAME).c -g -o $(NAME) $<
	@echo "Done! Final Binary: ./$(NAME)" $<

