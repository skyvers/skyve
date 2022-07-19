package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.LinkItemMetaData;

public class FluentLinkItem extends FluentMenuItem<FluentLinkItem> {
	private LinkItemMetaData item = null;
	
	public FluentLinkItem() {
		item = new LinkItemMetaData();
	}

	public FluentLinkItem(LinkItemMetaData item) {
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
	public LinkItemMetaData get() {
		return item;
	}
}
