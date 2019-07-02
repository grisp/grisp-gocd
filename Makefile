#.PHONY : all
all:
	rebar3 escriptize
	_build/default/bin/generate_pipelines
clean:
	git -C grisp-gocd-config/ reset HEAD
	rm -vf grisp-gocd-config/*.json
	git -C grisp-gocd-config/ checkout -- \*
