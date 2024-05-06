package org.skyve.metadata.view.model.map;

import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.view.model.ViewModel;

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
public abstract class MapModel<T extends Bean> implements ViewModel {
	private T bean;
	public T getBean() {
		return bean;
	}
	public void setBean(T bean) {
		this.bean = bean;
	}
	
	@Override
	public void postConstruct(Customer customer, boolean runtime) {
		// nothing to see here
	}
	
	public abstract MapResult getResult(Geometry mapBounds) throws Exception;
}
