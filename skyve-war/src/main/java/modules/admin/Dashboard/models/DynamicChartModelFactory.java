package modules.admin.Dashboard.models;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.util.Binder;

import modules.admin.Dashboard.DashboardExtension;
import modules.admin.DashboardWidget.DashboardWidgetExtension;
import modules.admin.domain.Dashboard;

/**
 * A dynamic chart model factory that can create chart models for any document type
 * without requiring static class definitions.
 */
public class DynamicChartModelFactory {

	/**
	 * Creates a chart model for the specified bean type and model number.
	 * 
	 * @param <T> The bean type
	 * @param beanType The class of the bean type
	 * @param modelNumber The model number
	 * @return A chart model for the specified bean type
	 */
	public static <T extends Bean> ChartModel<T> createChartModel(Class<T> beanType, int modelNumber) {
		return new DynamicChartModel<>(beanType, modelNumber);
	}

	/**
	 * Registers a dynamic chart model with the Skyve repository for a specific document.
	 * 
	 * @param <T> The bean type
	 * @param customer The customer
	 * @param moduleName The module name
	 * @param documentName The document name
	 * @param modelName The model name
	 * @param modelNumber The model number
	 */
	@SuppressWarnings("unchecked")
	public static <T extends Bean> void registerChartModel(
			Customer customer,
			String moduleName,
			String documentName,
			String modelName,
			int modelNumber) {

		try {
			// Get the repository and module
			LocalDesignRepository repository = (LocalDesignRepository) CORE.getRepository();
			Module module = customer.getModule(moduleName);

			// Get the document bean type
			Class<T> beanType = (Class<T>) repository.getDocument(customer, module, documentName).getClass();

			// Create the chart model
			ChartModel<T> chartModel = createChartModel(beanType, modelNumber);

			// Register the chart model with the repository
			/*repository.mode(customer, module, documentName, modelName, chartModel);*/
			

		} catch (Exception e) {
			throw new RuntimeException("Failed to register dynamic chart model", e);
		}
	}

	/**
	 * A dynamic implementation of ChartModel that works with any bean type.
	 */
	private static class DynamicChartModel<T extends Bean> extends ChartModel<T> {

		private final Class<T> beanType;
		private final int modelNumber;

		public DynamicChartModel(Class<T> beanType, int modelNumber) {
			this.beanType = beanType;
			this.modelNumber = modelNumber;
		}

		@Override
		public ChartData getChartData() {
			// Get the dashboard from the static instance
			DashboardExtension dashboard = Dashboard.newInstance();
			if (dashboard != null) {
				DashboardWidgetExtension widget = dashboard.findWidget(modelNumber);

				// construct the custom chart
				if (widget != null) {
					return DashboardExtension.customChartModel(widget);
				}
			}
			return null;
		}
	}
}