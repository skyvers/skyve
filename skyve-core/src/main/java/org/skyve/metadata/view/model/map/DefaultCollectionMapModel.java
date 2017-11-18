package org.skyve.metadata.view.model.map;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.util.Binder;

import com.vividsolutions.jts.geom.Envelope;

public class DefaultCollectionMapModel<T extends Bean> extends DefaultMapModel<T> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -6315233078102862916L;

	private String collectionBinding;

	public DefaultCollectionMapModel(String collectionBinding) {
		this.collectionBinding = collectionBinding;
	}

	@Override
	public MapResult getResult(Envelope mapExtents) throws Exception {
		@SuppressWarnings("unchecked")
		List<Bean> collection = (List<Bean>) Binder.get(getBean(), collectionBinding);
		List<MapItem> items = new ArrayList<>(collection.size());
		
		for (Bean element : collection) {
			addItem(element, items, mapExtents);
		}
		
		return new MapResult(items, null);
	}
}
