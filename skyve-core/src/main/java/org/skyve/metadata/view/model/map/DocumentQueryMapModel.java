package org.skyve.metadata.view.model.map;

import java.util.ArrayList;
import java.util.List;

import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;

/**
 * An implementation of MapModel that is driven by a meta data query.
 * NB Skyve cannot determine the access control required to zoom in on features
 * since they are programmatically defined and potentially hetrogeneous, so you may need to add
 * some "singular" accesses to either the encapsulating view or to various module roles. 
 * 
 * @author mike
 *
 * @param <T>	The encapsulating bean.
 */
public class DocumentQueryMapModel<T extends Bean> extends DefaultMapModel<T> {
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
