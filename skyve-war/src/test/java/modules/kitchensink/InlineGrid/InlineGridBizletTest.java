package modules.kitchensink.InlineGrid;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

import modules.kitchensink.domain.ListAttributes;
@SuppressWarnings("static-method")
class InlineGridBizletTest {

	private static final InlineGridBizlet bizlet = new InlineGridBizlet();

	@Test
	void getConstantDomainValuesForKnownAttribute() throws Exception {
		var values = bizlet.getConstantDomainValues(ListAttributes.constantDomainPropertyName);
		assertNotNull(values);
	}

	@Test
	void getConstantDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		assertNull(bizlet.getConstantDomainValues("unknownAttr"));
	}

	@Test
	void getVariantDomainValuesForKnownAttribute() throws Exception {
		var values = bizlet.getVariantDomainValues(ListAttributes.variantDomainPropertyName);
		assertNotNull(values);
	}

	@Test
	void getVariantDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		assertNull(bizlet.getVariantDomainValues("unknownAttr"));
	}

	@Test
	void getDynamicDomainValuesForKnownAttribute() throws Exception {
		@SuppressWarnings("null") // passes null on purpose
		var values = bizlet.getDynamicDomainValues(ListAttributes.dynamicDomainPropertyName, null);
		assertNotNull(values);
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void getDynamicDomainValuesForUnknownAttributeReturnsNull() throws Exception {
		assertNull(bizlet.getDynamicDomainValues("unknownAttr", null));
	}
}
