package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.LinkItem;

public class FluentLinkItem extends FluentMenuItem<FluentLinkItem> {
	private LinkItem item = new LinkItem();
	
	public FluentLinkItem() {
		// nothing to see
	}

	public FluentLinkItem(org.skyve.impl.metadata.module.menu.LinkItem item) {
		super(item);
		href(item.getHref());
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
