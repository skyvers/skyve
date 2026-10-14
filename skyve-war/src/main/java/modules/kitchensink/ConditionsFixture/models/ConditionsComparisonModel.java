package modules.kitchensink.ConditionsFixture.models;

import org.skyve.metadata.view.model.comparison.ComparisonComposite;
import org.skyve.metadata.view.model.comparison.ComparisonModel;

import modules.kitchensink.domain.ConditionsFixture;
import modules.kitchensink.domain.LookupDescription;

/**
 * Supplies an empty comparison, which is all the conditions fixture needs to put a condition on a comparison.
 */
public class ConditionsComparisonModel extends ComparisonModel<ConditionsFixture, LookupDescription> {
	@Override
	public ComparisonComposite getComparisonComposite(LookupDescription toCompareTo) throws Exception {
		return new ComparisonComposite();
	}
}
