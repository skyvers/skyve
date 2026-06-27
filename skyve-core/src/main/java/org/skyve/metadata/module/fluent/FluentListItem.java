package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ListItemMetaData;

/**
 * Builds list menu item metadata.
 */
public class FluentListItem extends FluentMenuItem<FluentListItem> {
	private ListItemMetaData item = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentListItem() {
		item = new ListItemMetaData();
	}

	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param item The metadata to mutate.
	 */
	public FluentListItem(ListItemMetaData item) {
		this.item = item;
	}

	/**
	 * Copies list menu item state from an existing menu item.
	 *
	 * @param item The source list menu item.
	 * @return this fluent instance.
	 */
	public FluentListItem from(@SuppressWarnings("hiding") org.skyve.impl.metadata.module.menu.ListItem item) {
		super.from(item);
		documentName(item.getDocumentName());
		queryName(item.getQueryName());
		modelName(item.getModelName());
		autoPopulate(item.isAutoPopulate());
		return this;
	}

	/**
	 * Sets the target document for the list item.
	 *
	 * @param documentName The document name.
	 * @return this fluent instance.
	 */
	public FluentListItem documentName(String documentName) {
		item.setDocumentName(documentName);
		return this;
	}
	
	/**
	 * Sets the query used to populate list rows.
	 *
	 * @param queryName The query name.
	 * @return this fluent instance.
	 */
	public FluentListItem queryName(String queryName) {
		item.setQueryName(queryName);
		return this;
	}

	/**
	 * Sets the model used to render list rows.
	 *
	 * @param modelName The model name.
	 * @return this fluent instance.
	 */
	public FluentListItem modelName(String modelName) {
		item.setModelName(modelName);
		return this;
	}
	
	/**
	 * Sets whether the list should auto-populate on initial render.
	 *
	 * @param autoPopulate {@code true} to auto-populate.
	 * @return this fluent instance.
	 */
	public FluentListItem autoPopulate(boolean autoPopulate) {
		item.setAutoPopulate(autoPopulate ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The list item metadata instance.
	 */
	@Override
	public ListItemMetaData get() {
		return item;
	}
}
