
from github import Github

# using an access token
g = Github("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")

# repositories = g.search_repositories(query='iot')

#example
repositories = g.search_repositories(query='kevinwlu/iot')

for repo in repositories:
	print(repo)
	# print(repo.get_topics())
	
	contents = repo.get_contents("")
	while contents:
		file_content = contents.pop(0)
		if file_content.type == "dir":
			contents.extend(repo.get_contents(file_content.path))
		else:
			if '.py' not in file_content.path:
				continue
			print(file_content)
			print(file_content.path)

			# real code in here
			print(file_content.decoded_content.decode())
	break