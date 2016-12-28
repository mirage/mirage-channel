all:
	ocaml pkg/pkg.ml build -n mirage-channel -q
	ocaml pkg/pkg.ml build -n mirage-channel-lwt -q --tests true
	ocaml pkg/pkg.ml test

clean:
	ocaml pkg/pkg.ml clean -n mirage-channel -q
	ocaml pkg/pkg.ml clean -n mirage-channel-lwt -q
