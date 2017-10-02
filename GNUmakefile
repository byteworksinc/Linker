SRC = exp.asm file.asm linker.asm out.asm pass1.asm pass2.asm \
	seg.asm symbol.asm util.asm

OBJECTS = $(SRC:%.asm=obj/%.A) $(SRC:%.asm=obj/%.ROOT)


# link order is important.
linker: $(OBJECTS)
	iix link \
	obj/linker obj/util obj/file obj/pass1 obj/pass2 obj/seg \
	obj/symbol obj/exp obj/out \
	keep=$@
	iix chtyp -a 0xdb01 $@

.PHONY: clean
clean:
	$(RM) obj/*.A obj/*.ROOT obj/*.a obj.*.root
	$(RM) linker


# fix the filetype on these headers
# since they have no extension.
ftype:
	iix chtyp -l asm directpage

obj/%.A : %.asm %.mac directpage
	iix compile $< keep=obj/$*

