package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.MapItemMetaData;

public class FluentMapItem extends FluentMenuItem<FluentMapItem> {
	private MapItemMetaData item = null;
	
	public FluentMapItem() {
		item = new MapItemMetaData();
	}

	public FluentMapItem(MapItemMetaData item) {
		this.item = item;
	}

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
	
	public FluentMapItem documentName(String documentName) {
		item.setDocumentName(documentName);
		return this;
	}
	
	public FluentMapItem queryName(String queryName) {
		item.setQueryName(queryName);
		return this;
	}

	public FluentMapItem modelName(String modelName) {
		item.setModelName(modelName);
		return this;
	}
	
	public FluentMapItem geometryBinding(String geometryBinding) {
		item.setGeometryBinding(geometryBinding);
		return this;
	}

	public FluentMapItem refreshTimeInSeconds(int refreshTimeInSeconds) {
		item.setRefreshTimeInSeconds(Integer.valueOf(refreshTimeInSeconds));
		return this;
	}
	
	public FluentMapItem showRefreshControls(boolean showRefreshControls) {
		item.setShowRefreshControls(showRefreshControls ? Boolean.TRUE : Boolean.FALSE);
		return this;
	}

	@Override
	public MapItemMetaData get() {
		return item;
	}
}
