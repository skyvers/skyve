package modules.kitchensink.InlineGrid;

import java.util.ArrayList;
import java.util.List;

import org.skyve.metadata.model.document.Bizlet;

import modules.kitchensink.domain.InlineGrid;
import modules.kitchensink.domain.ListAttributes;

public class InlineGridBizlet extends Bizlet<InlineGrid> {
	private static List<DomainValue> dvs = new ArrayList<>(3);
	
	static {
		dvs.add(new DomainValue("one", "One (1)!"));
		dvs.add(new DomainValue("two", "Two (2)!"));
		dvs.add(new DomainValue("three", "Three (3)!"));
	}
	
	@Override
	public List<DomainValue> getConstantDomainValues(String attributeName) throws Exception {
		if (ListAttributes.constantDomainPropertyName.equals(attributeName)) {
			return dvs;
		}
		return null;
	}
	
	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		if (ListAttributes.variantDomainPropertyName.equals(attributeName)) {
			return dvs;
		}
		return null;
	}
	
	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, InlineGrid bean) throws Exception {
		if (ListAttributes.dynamicDomainPropertyName.equals(attributeName)) {
			return dvs;
		}
		return null;
	}

}
