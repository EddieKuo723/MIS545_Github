import csv
from github import Github

# using an access token
g = Github("access_token")


# Github Enterprise with custom hostname
# g = Github(base_url="https://{hostname}/api/v3", login_or_token="access_token")

keyword = 'Github'
keyword_list = ['Python','Java','C','C++','PHP','Go']


with open(f'{keyword}_most_stars.csv', mode='w', encoding='utf-8-sig', newline='') as csv_file:
	w = csv.writer(csv_file)

	w.writerow(['keyword', 'owner', 'repo_name','stars','forks_count','issue_count','network_count','subscribers_count','watchers_count','description','topics'])

	
	for keyword in keyword_list:
		repositories = g.search_repositories(query=f'language:{keyword}',sort= 'stars')

		for q,repo in enumerate(repositories):
			# print(repo)
			# print(repo.owner.login)	
			print(repo.name)			
			print(repo.stargazers_count)
			
			# print(repo.forks_count)
			# print(repo.open_issues_count)
			# print(repo.network_count)
			# print(repo.subscribers_count)
			# print(repo.watchers_count)

			# print(repo.description)
			# print(','.join(repo.get_topics()))
			w.writerow([keyword, repo.owner.login, repo.name,repo.stargazers_count,repo.forks_count,repo.open_issues_count,repo.network_count,repo.subscribers_count,repo.watchers_count,repo.description,','.join(repo.get_topics())])
			if q == 99:
				break

