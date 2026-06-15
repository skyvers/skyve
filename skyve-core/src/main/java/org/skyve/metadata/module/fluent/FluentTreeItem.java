package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.TreeItemMetaData;

/**
 * Builds tree menu item metadata.
 */
public class FluentTreeItem extends FluentMenuItem<FluentTreeItem> {
	private TreeItemMetaData item = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentTreeItem() {
		item = new TreeItemMetaData();
	}

	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param item The metadata to mutate.
	 */
	public FluentTreeItem(TreeItemMetaData item) {
		this.item = item;
	}

	/**
	 * Copies tree menu item state from an existing menu item.
	 *
	 * @param item The source tree menu item.
	 * @return this fluent instance.
	 */
	public FluentTreeItem from(@SuppressWarnings("hiding") org.skyve.impl.metadata.module.menu.TreeItem item) {
		super.from(item);
		documentName(item.getDocumentName());
		queryName(item.getQueryName());
		modelName(item.getModelName());
		autoPopulate(item.isAutoPopulate());
		return this;
	}
	
	/**
	 * Sets the target document for the tree item.
	 *
	 * @param documentName The document name.
	 * @return this fluent instance.
	 */
	public FluentTreeItem documentName(String documentName) {
		item.setDocumentName(documentName);
		return this;
	}
	
	/**
	 * Sets the query used to load tree nodes.
	 *
	 * @param queryName The query name.
	 * @return this fluent instance.
	 */
	public FluentTreeItem queryName(String queryName) {
		item.setQueryName(queryName);
		return this;
	}

	/**
	 * Sets the model used to render tree nodes.
	 *
	 * @param modelName The model name.
	 * @return this fluent instance.
	 */
	public FluentTreeItem modelName(String modelName) {
		item.setModelName(modelName);
		return this;
	}
	
	/**
	 * Sets whether the tree auto-populates on first render.
	 *
	 * @param autoPopulate {@code true} to auto-populate.
	 * @return this fluent instance.
	 */
	public FluentTreeItem autoPopulate(boolean autoPopulate) {
		item.setAutoPopulate(autoPopulate ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The tree item metadata instance.
	 */
	@Override
	public TreeItemMetaData get() {
		return item;
	}
}
