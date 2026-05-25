package org.skyve.impl.metadata.module.menu;

/**
 * Menu item that opens the list view of a document, query, or model.
 *
 * <p>Extends {@link AbstractDocumentOrQueryOrModelMenuItem} with an
 * {@code autoPopulate} flag (default {@code true}) that controls whether the
 * list grid is automatically populated with data on first load or waits for the
 * user to supply search criteria.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see AbstractDocumentOrQueryOrModelMenuItem
 * @see TreeItem
 */
public class ListItem extends AbstractDocumentOrQueryOrModelMenuItem {
	private static final long serialVersionUID = -7998788239200957754L;
	
	private boolean autoPopulate = true;

	public boolean isAutoPopulate() {
		return autoPopulate;
	}

	public void setAutoPopulate(boolean autoPopulate) {
		this.autoPopulate = autoPopulate;
	}
}
