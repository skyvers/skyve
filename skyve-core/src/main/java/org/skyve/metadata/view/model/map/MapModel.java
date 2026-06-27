package org.skyve.metadata.view.model.map;

import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.view.model.ViewModel;

/**
 * Abstract base for custom map view models that supply geographic features to Skyve map widgets.
 *
 * <p>Subclass {@code MapModel} and reference the class from a view's {@code modelName} attribute
 * to drive a Skyve {@code map} widget with application-defined geometry features.
 * Implement {@link #getResult(Geometry)} to return a {@link MapResult} containing the
 * {@link MapItem} instances to display for the current map viewport.
 *
 * <p>Consider extending one of the concrete subclasses instead:
 * <ul>
 *   <li>{@link DefaultMapModel} — queries beans via a document query and converts geometry
 *       attributes to map items automatically.</li>
 *   <li>{@link DocumentQueryMapModel} — drives items from a named metadata query.</li>
 *   <li>{@link ReferenceMapModel} — builds items from a collection attribute on the driving bean.</li>
 * </ul>
 *
 * <p><b>Note:</b> Skyve cannot determine the access control required to zoom in on features
 * since they are programmatically defined and heterogeneous. If features span multiple modules,
 * add "singular" accesses to the encapsulating view or to the relevant module roles.
 *
 * <p>Threading: one instance is created per render cycle and is not shared across threads.
 *
 * @param <T> the driving document bean type
 * @see MapResult
 * @see MapItem
 * @see MapFeature
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
