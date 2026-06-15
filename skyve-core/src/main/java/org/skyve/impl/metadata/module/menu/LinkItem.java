package org.skyve.impl.metadata.module.menu;

/**
 * Menu item that navigates to an arbitrary hyperlink.
 *
 * <p>Unlike document-oriented menu items, a {@code LinkItem} holds a raw
 * {@code href} URL that the browser navigates to when the item is clicked.
 * The URL may be relative (within the application) or absolute.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see AbstractMenuItem
 */
public final class LinkItem extends AbstractMenuItem {
	private static final long serialVersionUID = 3927344366251975756L;

	private String href;

	public String getHref() {
		return href;
	}

	public void setHref(String href) {
		this.href = href;
	}
}
