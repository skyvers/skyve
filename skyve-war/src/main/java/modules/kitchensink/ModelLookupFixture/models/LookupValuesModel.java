package modules.kitchensink.ModelLookupFixture.models;

import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;

/**
 * Supplies persisted Kitchen Sink lookup values to form, data-grid and list-grid widgets.
 * Instances are initialised per model request and retain normal query security scoping.
 */
public class LookupValuesModel extends DocumentQueryListModel<Bean> {
	/**
	 * Resolves lookup metadata in both generation and runtime contexts.
	 *
	 * @param customer the active customer
	 * @param runtime whether this is a runtime request
	 */
	@Override
	public void postConstruct(Customer customer, boolean runtime) {
		setQuery(customer.getModule("kitchensink").getNullSafeMetaDataQuery("qModelLookupValues"));
		super.postConstruct(customer, runtime);
	}
}
