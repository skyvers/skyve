package org.skyve.metadata.view.model.map;

import java.util.ArrayList;
import java.util.List;

import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;

public class DocumentQueryMapModel<T extends Bean> extends DefaultMapModel<T> {
	private static final long serialVersionUID = 5182580858481923068L;

	private MetaDataQueryDefinition query;
	private DocumentQuery documentQuery; // from query

	public DocumentQueryMapModel(MetaDataQueryDefinition query) {
		this.query = query;
	}

	@Override
	public MapResult getResult(Geometry mapBounds) throws Exception {
		if (documentQuery == null) {
			documentQuery = query.constructDocumentQuery(null, null);
		}
		
		List<MapItem> items = new ArrayList<>(256);
		Envelope mapEnvelope = mapBounds.getEnvelopeInternal();
		try (AutoClosingIterable<Bean> i = documentQuery.projectedIterable()) {
			for (Bean bean : i) {
				addItem(bean, items, mapEnvelope);
			}
		}
		
		return new MapResult(items, null);
	}
}
