package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;

public class FluentContentLink extends FluentWidget {
	private ContentLink link = null;
	
	public FluentContentLink() {
		link = new ContentLink();
	}

	public FluentContentLink(ContentLink link) {
		this.link = link;
	}

	public FluentContentLink from(@SuppressWarnings("hiding") ContentLink link) {
		return this;
	}

	@Override
	public ContentLink get() {
		return link;
	}
}
