package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.LinkItem;

public class FluentLinkItem extends FluentMenuItem<FluentLinkItem> {
	private LinkItem item = null;
	
	public FluentLinkItem() {
		item = new LinkItem();
	}

	public FluentLinkItem(LinkItem item) {
		this.item = item;
	}

	public FluentLinkItem from(@SuppressWarnings("hiding") org.skyve.impl.metadata.module.menu.LinkItem item) {
		super.from(item);
		href(item.getHref());
		return this;
	}
	
	public FluentLinkItem href(String href) {
		item.setHref(href);
		return this;
	}
	
	@Override
	public LinkItem get() {
		return item;
	}
}
