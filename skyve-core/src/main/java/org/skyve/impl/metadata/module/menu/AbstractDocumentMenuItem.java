package org.skyve.impl.metadata.module.menu;

/**
 * Abstract base for menu items that navigate to a specific document.
 *
 * <p>Adds a {@code documentName} property to {@link AbstractMenuItem} to identify
 * the target document.  Concrete subclasses further specialise the navigation
 * target (e.g., list view, edit view, calendar, map, or tree).
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see AbstractDocumentOrQueryOrModelMenuItem
 * @see AbstractMenuItem
 */
public abstract class AbstractDocumentMenuItem extends AbstractMenuItem {
	private static final long serialVersionUID = 2956568471144635374L;

	private String documentName;

	public String getDocumentName() {
		return documentName;
	}
	public void setDocumentName(String documentName) {
		this.documentName = documentName;
	}
}
