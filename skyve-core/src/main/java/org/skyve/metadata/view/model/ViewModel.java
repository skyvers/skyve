package org.skyve.metadata.view.model;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.customer.Customer;

/**
 * Base interface for all programmatic view models that supply data or structure to a
 * Skyve view at runtime or during domain generation.
 *
 * <p>A {@code ViewModel} is registered in document view metadata and is instantiated
 * by the Skyve runtime when the view is rendered. After instantiation,
 * {@link #postConstruct(Customer, boolean)} is called to initialise the model with its
 * customer context and to distinguish runtime (full services available) from
 * generate-time (validation only, no persistence).
 *
 * <p>Concrete model types (e.g. {@code ListModel}, {@code ChartModel}, {@code MapModel},
 * {@code ComparisonModel}) extend this interface to add model-specific query and
 * rendering contracts.
 *
 * @see org.skyve.metadata.view.model.list.ListModel
 */
public interface ViewModel extends MetaData {
	/**
	 * Initialises this model after instantiation.
	 *
	 * <p>Skyve calls this method exactly once, immediately after constructing the model
	 * instance. Implementations should use the {@code runtime} flag to guard operations
	 * that require live Skyve services (e.g. persistence access):
	 * <ul>
	 *   <li>{@code runtime == true} &mdash; called from a live HTTP or job context;
	 *       all Skyve services are available.</li>
	 *   <li>{@code runtime == false} &mdash; called from the domain generator;
	 *       persistence and web context are not available.</li>
	 * </ul>
	 *
	 * @param customer  the customer for whom the model is being initialised;
	 *                  never {@code null}
	 * @param runtime   {@code true} when running in a live application context;
	 *                  {@code false} when running at generate time
	 */
	void postConstruct(Customer customer, boolean runtime);
}
