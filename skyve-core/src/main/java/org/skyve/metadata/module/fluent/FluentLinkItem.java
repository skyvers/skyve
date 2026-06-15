package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.LinkItemMetaData;

/**
 * Builds external link menu item metadata.
 */
public class FluentLinkItem extends FluentMenuItem<FluentLinkItem> {
	private LinkItemMetaData item = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentLinkItem() {
		item = new LinkItemMetaData();
	}

	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param item The metadata to mutate.
	 */
	public FluentLinkItem(LinkItemMetaData item) {
		this.item = item;
	}

	/**
	 * Copies link menu item state from an existing menu item.
	 *
	 * @param item The source link menu item.
	 * @return this fluent instance.
	 */
	public FluentLinkItem from(@SuppressWarnings("hiding") org.skyve.impl.metadata.module.menu.LinkItem item) {
		super.from(item);
		href(item.getHref());
		return this;
	}
	
	/**
	 * Sets the destination URL for the menu link.
	 *
	 * @param href The URL or relative link target.
	 * @return this fluent instance.
	 */
	public FluentLinkItem href(String href) {
		item.setHref(href);
		return this;
	}
	
	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The link item metadata instance.
	 */
	@Override
	public LinkItemMetaData get() {
		return item;
	}
}
