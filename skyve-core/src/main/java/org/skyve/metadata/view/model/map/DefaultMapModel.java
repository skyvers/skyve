package org.skyve.metadata.view.model.map;

import java.util.List;

import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.geom.Geometry;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.util.Binder;

/**
 * Extend to add map features to a map.
 * NB Skyve cannot determine the access control required to zoom in on features
 * since they are programmatically defined and hetrogeneous, so you may need to add
 * some "singular" accesses to either the encapsulating view or to various module roles. 
 * 
 * @author mike
 *
 * @param <T>	The encapsulating bean.
 */
public abstract class DefaultMapModel<T extends Bean> extends MapModel<T> {
	private String geometryBinding;
	public final String getGeometryBinding() {
		return geometryBinding;
	}
	public final void setGeometryBinding(String geometryBinding) {
		this.geometryBinding = geometryBinding;
	}

	protected void addItem(Bean beanContainingGeometry, 
							List<MapItem> itemsToAddTo,
							Envelope mapExtents)
	throws Exception {
		Geometry geometry = (Geometry) Binder.get(beanContainingGeometry, geometryBinding);
		if ((geometry != null) && mapExtents.intersects(geometry.getEnvelopeInternal())) {
			MapItem item = new MapItem();
			item.setBizId((String) Binder.get(beanContainingGeometry, Bean.DOCUMENT_ID));
			item.setModuleName((String) Binder.get(beanContainingGeometry, Bean.MODULE_KEY));
			item.setDocumentName((String) Binder.get(beanContainingGeometry, Bean.DOCUMENT_KEY));
			item.setInfoMarkup(Binder.getDisplay(CORE.getUser().getCustomer(), beanContainingGeometry, Bean.BIZ_KEY));

			MapFeature feature = new MapFeature();
			feature.setGeometry((Geometry) Binder.get(beanContainingGeometry, geometryBinding));
			item.getFeatures().add(feature);

			itemsToAddTo.add(item);
		}
	}
}
