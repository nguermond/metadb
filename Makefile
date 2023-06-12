

docs:
	dune build @doc
	mkdir -p docs
	cp -r _build/default/_doc/_html/* docs/

clean:
	rm -rf _build
	rm -r *~
