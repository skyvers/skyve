package org.skyve.impl.metadata.module.menu;

/**
 * Menu item that opens the edit view of a specific document instance.
 *
 * <p>Navigates directly to the default edit view for the document identified by
 * {@link AbstractDocumentMenuItem#getDocumentName()}.  Used to provide a shortcut
 * to a singleton or well-known instance rather than navigating through a list.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see AbstractDocumentMenuItem
 */
public final class EditItem extends AbstractDocumentMenuItem {
	private static final long serialVersionUID = 3953129114755669755L;
}
