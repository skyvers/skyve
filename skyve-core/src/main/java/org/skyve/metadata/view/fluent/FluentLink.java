package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.Link;

public class FluentLink extends FluentWidget {
	private Link link = null;
	
	public FluentLink() {
		link = new Link();
	}
	
	public FluentLink(Link link) {
		this.link = link;
	}
	
	public FluentLink from(@SuppressWarnings("hiding") Link link) {
		return this;
	}
	
	@Override
	public Link get() {
		return link;
	}
}
