#.PHONY : all
all:
	pipelines/create_pipelines.sh
clean:
	git -C ../grisp-gocd-config reset HEAD
	rm -vf ../grisp-gocd-config/*.json
