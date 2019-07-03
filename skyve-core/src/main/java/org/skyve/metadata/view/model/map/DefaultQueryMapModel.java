package org.skyve.metadata.view.model.map;

import java.util.ArrayList;
import java.util.List;

import org.locationtech.jts.geom.Envelope;
import org.skyve.domain.Bean;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.persistence.DocumentQuery;

public class DefaultQueryMapModel<T extends Bean> extends DefaultMapModel<T> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 5182580858481923068L;

	private MetaDataQueryDefinition query;
	private DocumentQuery documentQuery; // from query

	public DefaultQueryMapModel(MetaDataQueryDefinition query) {
		this.query = query;
	}

	@Override
	public MapResult getResult(Envelope mapExtents) throws Exception {
		if (documentQuery == null) {
			documentQuery = query.constructDocumentQuery(null, null);
		}
		
		List<MapItem> items = new ArrayList<>(256);
		for (Bean bean : documentQuery.projectedIterable()) {
			addItem(bean, items, mapExtents);
		}
		
		return new MapResult(items, null);
	}
}
