package org.skyve.wildcat.content;

import java.util.ArrayList;
import java.util.List;

public final class SearchResults {
	private String searchTimeInSecs;
	private String suggestion;

	private List<SearchResult> results = new ArrayList<>();

	public String getSearchTimeInSecs() {
		return searchTimeInSecs;
	}

	public void setSearchTimeInSecs(String searchTimeInSecs) {
		this.searchTimeInSecs = searchTimeInSecs;
	}

	public String getSuggestion() {
		return suggestion;
	}

	public void setSuggestion(String suggestion) {
		this.suggestion = suggestion;
	}

	public List<SearchResult> getResults() {
		return results;
	}

	public void setResults(List<SearchResult> results) {
		this.results = results;
	}
}
