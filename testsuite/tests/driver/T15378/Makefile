TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -f *.o *.hi *.dyn_o *.dyn_hi
	rm -f *.iface *.usage
	rm -f A.hi.initial B.hi.initial B.j1.hi B.j2.hi
	rm -f rebuild.log

T15378: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) --make -O -j2 -fforce-recomp B.hs
	"$(TEST_HC)" --show-iface B.hi > B.initial.iface
	sed -n 's/.*main:A \([0-9a-f][0-9a-f]*\).*/\1/p' B.initial.iface | head -n1 > B.initial.usage
	cp A.hi A.hi.initial
	cp B.hi B.hi.initial

	"$(TEST_HC)" $(TEST_HC_OPTS) --make -O -j2 -v1 B.hs > rebuild.log 2>&1
	if grep -F "Compiling B [A changed]" rebuild.log; then \
	  echo "unexpected recompilation: B was rebuilt due to [A changed]"; \
	  exit 1; \
	fi
	cmp A.hi.initial A.hi
	cmp B.hi.initial B.hi

	"$(TEST_HC)" $(TEST_HC_OPTS) --make -O -j1 -fforce-recomp B.hs
	cp B.hi B.j1.hi
	"$(TEST_HC)" --show-iface B.j1.hi > B.j1.iface
	sed -n 's/.*main:A \([0-9a-f][0-9a-f]*\).*/\1/p' B.j1.iface | head -n1 > B.j1.usage

	"$(TEST_HC)" $(TEST_HC_OPTS) --make -O -j2 -fforce-recomp B.hs
	cp B.hi B.j2.hi
	"$(TEST_HC)" --show-iface B.j2.hi > B.j2.iface
	sed -n 's/.*main:A \([0-9a-f][0-9a-f]*\).*/\1/p' B.j2.iface | head -n1 > B.j2.usage

	cmp B.j1.hi B.j2.hi
	cmp B.j1.usage B.j2.usage
