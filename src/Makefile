# Dependancies: Lua, libredblack, pthreads...

SOURCES = util.ml config.ml resources.ml audio.ml vector.ml input.ml drawing.ml line.ml area.ml gameobj.ml shot.ml particles.ml background.ml level.ml player.ml power.ml text.ml timer.ml upgrade.ml driver.ml main.ml
OBJS = util.cmx config.cmx resources.cmx audio.cmx vector.cmx input.cmx drawing.cmx line.cmx area.cmx gameobj.cmx shot.cmx particles.cmx background.cmx level.cmx player.cmx power.cmx text.cmx timer.cmx upgrade.cmx driver.cmx main.cmx

INCLUDE = `ocamlfind query sdl`

LIBS = str.cmxa unix.cmxa nums.cmxa bigarray.cmxa sdl.cmxa sdlloader.cmxa sdlttf.cmxa sdlmixer.cmxa

OCAMLOPT = ocamlopt

FLAGS = -I $(INCLUDE) $(LIBS)

LDFLAGS = -I $(INCLUDE) $(LIBS)

PROGRAM = sm

.PHONY: clean

all: $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(OCAMLOPT) $(LDFLAGS) $(OBJS) -o $(PROGRAM)

$(OBJS): $(SOURCES)
	$(OCAMLOPT) $(FLAGS) -c $(SOURCES)

clean:
	rm -rf *.cmx *.cmi *.o *~

