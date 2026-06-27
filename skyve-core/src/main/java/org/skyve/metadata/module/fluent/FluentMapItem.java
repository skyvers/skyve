package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.MapItemMetaData;

/**
 * Builds map menu item metadata.
 */
public class FluentMapItem extends FluentMenuItem<FluentMapItem> {
	private MapItemMetaData item = null;
	
	/**
	 * Creates a fluent wrapper with a new metadata instance.
	 */
	public FluentMapItem() {
		item = new MapItemMetaData();
	}

	/**
	 * Creates a fluent wrapper around an existing metadata instance.
	 *
	 * @param item The metadata to mutate.
	 */
	public FluentMapItem(MapItemMetaData item) {
		this.item = item;
	}

	/**
	 * Copies map menu item state from an existing menu item.
	 *
	 * @param item The source map menu item.
	 * @return this fluent instance.
	 */
	public FluentMapItem from(@SuppressWarnings("hiding") org.skyve.impl.metadata.module.menu.MapItem item) {
		super.from(item);
		documentName(item.getDocumentName());
		queryName(item.getQueryName());
		modelName(item.getModelName());
		geometryBinding(item.getGeometryBinding());
		Integer i = item.getRefreshTimeInSeconds();
		if (i != null ) {
			refreshTimeInSeconds(i.intValue());
		}
		Boolean b = item.getShowRefreshControls();
		if (b != null) {
			showRefreshControls(b.booleanValue());
		}
		return this;
	}
	
	/**
	 * Sets the target document for the map item.
	 *
	 * @param documentName The document name.
	 * @return this fluent instance.
	 */
	public FluentMapItem documentName(String documentName) {
		item.setDocumentName(documentName);
		return this;
	}
	
	/**
	 * Sets the query used to load map data.
	 *
	 * @param queryName The query name.
	 * @return this fluent instance.
	 */
	public FluentMapItem queryName(String queryName) {
		item.setQueryName(queryName);
		return this;
	}

	/**
	 * Sets the model used to render map features.
	 *
	 * @param modelName The model name.
	 * @return this fluent instance.
	 */
	public FluentMapItem modelName(String modelName) {
		item.setModelName(modelName);
		return this;
	}
	
	/**
	 * Sets the geometry binding used for feature coordinates.
	 *
	 * @param geometryBinding The geometry binding expression.
	 * @return this fluent instance.
	 */
	public FluentMapItem geometryBinding(String geometryBinding) {
		item.setGeometryBinding(geometryBinding);
		return this;
	}

	/**
	 * Sets the auto-refresh interval in seconds.
	 *
	 * @param refreshTimeInSeconds The refresh interval.
	 * @return this fluent instance.
	 */
	public FluentMapItem refreshTimeInSeconds(int refreshTimeInSeconds) {
		item.setRefreshTimeInSeconds(Integer.valueOf(refreshTimeInSeconds));
		return this;
	}
	
	/**
	 * Sets whether map refresh controls are shown.
	 *
	 * @param showRefreshControls {@code true} to show refresh controls.
	 * @return this fluent instance.
	 */
	public FluentMapItem showRefreshControls(boolean showRefreshControls) {
		item.setShowRefreshControls(showRefreshControls ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	/**
	 * Returns the mutable metadata backing this fluent wrapper.
	 *
	 * @return The map item metadata instance.
	 */
	@Override
	public MapItemMetaData get() {
		return item;
	}
}
