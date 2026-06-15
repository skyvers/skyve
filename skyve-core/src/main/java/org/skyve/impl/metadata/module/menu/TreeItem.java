package org.skyve.impl.metadata.module.menu;

/**
 * Menu item that opens the tree view of a document or query.
 *
 * <p>Inherits all properties from {@link ListItem} (auto-populate, document name,
 * query name, model name) and opens the tree-grid variant of the list view instead
 * of the flat-grid variant.
 *
 * <p>Threading: not thread-safe.  Instances are populated during metadata loading
 * and are read-only once placed in the repository cache.
 *
 * @see ListItem
 */
public class TreeItem extends ListItem {
	private static final long serialVersionUID = -7998788239200957754L;
}
