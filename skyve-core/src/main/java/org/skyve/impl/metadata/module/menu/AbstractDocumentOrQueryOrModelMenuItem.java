package org.skyve.impl.metadata.module.menu;

/**
 * Abstract base for menu items that may target a document, a named query, or a
 * model instead of a raw document.
 *
 * <p>Extends {@link AbstractDocumentMenuItem} with optional {@code queryName} and
 * {@code modelName} properties.  At render time the framework resolves which of
 * the three targets ({@code documentName}, {@code queryName}, {@code modelName})
 * is non-null and opens the corresponding view.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see ListItem
 * @see CalendarItem
 * @see MapItem
 */
public abstract class AbstractDocumentOrQueryOrModelMenuItem extends AbstractDocumentMenuItem {
	private static final long serialVersionUID = 840971349939181558L;

	private String queryName;
	private String modelName;

	public String getQueryName() {
		return queryName;
	}
	public void setQueryName(String queryName) {
		this.queryName = queryName;
	}

	public String getModelName() {
		return modelName;
	}
	public void setModelName(String modelName) {
		this.modelName = modelName;
	}
}
