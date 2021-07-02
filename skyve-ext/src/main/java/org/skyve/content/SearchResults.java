package org.skyve.content;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * A mix of search results for both bean and attachment content.
 * @author mike
 */
public final class SearchResults implements Serializable {
	private static final long serialVersionUID = -3737028074877187674L;

	private String searchTimeInSecs;
	private String suggestion;

	private List<SearchResult> results = new ArrayList<>();

	/**
	 * Amount of time in seconds that the search operation took to execute.
	 * @return
	 */
	public String getSearchTimeInSecs() {
		return searchTimeInSecs;
	}

	/**
	 * Amount of time in seconds that the search operation took to execute.
	 * @param searchTimeInSecs
	 */
	public void setSearchTimeInSecs(String searchTimeInSecs) {
		this.searchTimeInSecs = searchTimeInSecs;
	}

	/**
	 * A suggestion/correction of a search term based on the existing content.
	 * @return
	 */
	public String getSuggestion() {
		return suggestion;
	}

	/**
	 * A suggestion/correction of a search term based on the existing content.
	 * @param suggestion
	 */
	public void setSuggestion(String suggestion) {
		this.suggestion = suggestion;
	}

	/**
	 * The search results.
	 * @return
	 */
	public List<SearchResult> getResults() {
		return results;
	}

	/**
	 * The search results.
	 * @param results
	 */
	public void setResults(List<SearchResult> results) {
		this.results = results;
	}
}
