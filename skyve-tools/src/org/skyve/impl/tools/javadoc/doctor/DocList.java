package org.skyve.impl.tools.javadoc.doctor;

import java.util.ArrayList;
import java.util.List;

/**
 * Renders ordered or unordered HTML lists.
 */
public class DocList {
	private List<String> items = new ArrayList<>();
	
	private boolean numbered;
	
	/**
	 * Returns list items to render.
	 *
	 * @return mutable list of item HTML/text values
	 */
	public List<String> getItems(){
		return items;
	}
	
	/**
	 * Creates a list renderer.
	 *
	 * @param numbered {@code true} for ordered list, {@code false} for unordered
	 */
	public DocList(boolean numbered){
		super();
		this.numbered = numbered;
	}
	
	/**
	 * Renders this list as HTML markup.
	 *
	 * @return HTML string representation
	 */
	public String toHTML(){
		StringBuilder html = new StringBuilder();
		html.append((this.numbered?"<ol>":"<ul>"));
		for(String s : items){
			html.append("<li>" + s + "</li>");
		}
		html.append((this.numbered?"</ol>":"</ul>"));
		
		return html.toString();
	}
	
}
