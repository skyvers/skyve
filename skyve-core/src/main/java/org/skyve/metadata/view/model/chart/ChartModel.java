package org.skyve.metadata.view.model.chart;

import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.view.model.ViewModel;

/**
 * Abstract base for custom chart view models that supply data to Skyve chart widgets.
 *
 * <p>Subclass {@code ChartModel} and implement {@link #getChartData()} to drive a
 * Skyve {@code chart} widget with application-defined aggregated data.
 * Reference the subclass from a view's {@code modelName} attribute.
 *
 * <p>For query-backed charts built without writing Java, use a
 * {@link MetaDataChartModel} or the {@link ChartBuilder} / {@link ProjectedChartBuilder}
 * builder APIs instead.
 *
 * <p>Threading: one instance is created per render cycle and is not shared across threads.
 *
 * @param <T> the driving document bean type
 * @see ChartData
 * @see ChartBuilder
 */
public abstract class ChartModel<T extends Bean> implements ViewModel {
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

	/**
	 * Returns the aggregated chart data for the current bean state.
	 *
	 * @return the chart data; must not be {@code null}
	 */
	public abstract ChartData getChartData();
}
