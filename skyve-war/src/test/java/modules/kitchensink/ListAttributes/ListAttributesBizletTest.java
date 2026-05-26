package modules.kitchensink.ListAttributes;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import modules.kitchensink.domain.ListAttributes;

@SuppressWarnings("static-method")
public class ListAttributesBizletTest {

	private static final ListAttributesBizlet bizlet = new ListAttributesBizlet();

	@Test
	void getConstantDomainValuesForKnownAttribute() throws Exception {
		assertNotNull(bizlet.getConstantDomainValues(ListAttributes.constantDomainPropertyName));
	}

	@Test
	void getConstantDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		assertNull(bizlet.getConstantDomainValues("unknown"));
	}

	@Test
	void getVariantDomainValuesForKnownAttribute() throws Exception {
		assertNotNull(bizlet.getVariantDomainValues(ListAttributes.variantDomainPropertyName));
	}

	@Test
	void getVariantDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		assertNull(bizlet.getVariantDomainValues("unknown"));
	}

	@Test
	void getDynamicDomainValuesForKnownAttribute() throws Exception {
		assertNotNull(bizlet.getDynamicDomainValues(ListAttributes.dynamicDomainPropertyName, new ListAttributes()));
	}

	@Test
	void getDynamicDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		assertNull(bizlet.getDynamicDomainValues("unknown", new ListAttributes()));
	}
}
