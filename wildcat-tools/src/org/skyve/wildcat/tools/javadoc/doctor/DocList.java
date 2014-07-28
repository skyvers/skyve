package org.skyve.wildcat.tools.javadoc.doctor;

import java.util.ArrayList;
import java.util.List;

public class DocList {

	private List<String> items = new ArrayList<>();
	
	private boolean numbered;
	
	public List<String> getItems(){
		return items;
	}
	
	public DocList(boolean numbered){
		super();
		this.numbered = numbered;
	}
	
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
