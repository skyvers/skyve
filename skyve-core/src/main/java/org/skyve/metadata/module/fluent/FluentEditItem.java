package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.EditItemMetaData;

/**
 * Builds edit menu item metadata.
 */
public class FluentEditItem extends FluentMenuItem<FluentEditItem> {
	private EditItemMetaData item = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentEditItem() {
		item = new EditItemMetaData();
	}

	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param item The metadata to mutate.
	 */
	public FluentEditItem(EditItemMetaData item) {
		this.item = item;
	}

	/**
	 * Copies edit menu item state from an existing menu item.
	 *
	 * @param item The source edit menu item.
	 * @return this fluent instance.
	 */
	public FluentEditItem from(@SuppressWarnings("hiding") org.skyve.impl.metadata.module.menu.EditItem item) {
		super.from(item);
		documentName(item.getDocumentName());
		return this;
	}
	
	/**
	 * Sets the target document for the edit menu item.
	 *
	 * @param documentName The document name.
	 * @return this fluent instance.
	 */
	public FluentEditItem documentName(String documentName) {
		item.setDocumentName(documentName);
		return this;
	}
	
	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The edit item metadata instance.
	 */
	@Override
	public EditItemMetaData get() {
		return item;
	}
}
