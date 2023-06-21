package org.skyve.metadata.view.model.map;

import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;
import org.skyve.metadata.MetaData;

/**
 * Extend to add map features to a map.
 * Consider extending DefaultMapModel, DocumentQueryMapModel or ReferenceMapModel.
 * NB Skyve cannot determine the access control required to zoom in on features
 * since they are programmatically defined and hetrogeneous, so you may need to add
 * some "singular" accesses to either the encapsulating view or to various module roles. 
 * 
 * @author mike
 *
 * @param <T>	The encapsulating bean.
 */
public abstract class MapModel<T extends Bean> implements MetaData {
	private T bean;
	public T getBean() {
		return bean;
	}
	public void setBean(T bean) {
		this.bean = bean;
	}
	
	public abstract MapResult getResult(Geometry mapBounds) throws Exception;
}
