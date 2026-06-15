package modules.kitchensink.InlineGrid;

import java.util.ArrayList;
import java.util.List;

import org.skyve.metadata.model.document.Bizlet;

import modules.kitchensink.domain.InlineGrid;
import modules.kitchensink.domain.ListAttributes;

/**
 * Supplies static demonstration domain values for the {@link InlineGrid} document.
 */
public class InlineGridBizlet extends Bizlet<InlineGrid> {
	private static List<DomainValue> dvs = new ArrayList<>(3);
	
	static {
		dvs.add(new DomainValue("one", "One (1)!"));
		dvs.add(new DomainValue("two", "Two (2)!"));
		dvs.add(new DomainValue("three", "Three (3)!"));
	}
	
	/**
	 * Returns the shared constant domain values for the inline-grid sample attribute.
	 *
	 * @param attributeName the document attribute requesting values
	 * @return the configured domain values when the constant-domain attribute is requested, otherwise {@code null}
	 * @throws Exception if domain value resolution fails
	 */
	@Override
	public List<DomainValue> getConstantDomainValues(String attributeName) throws Exception {
		if (ListAttributes.constantDomainPropertyName.equals(attributeName)) {
			return dvs;
		}
		return null;
	}
	
	/**
	 * Returns the shared variant domain values for the inline-grid sample attribute.
	 *
	 * @param attributeName the document attribute requesting values
	 * @return the configured domain values when the variant-domain attribute is requested, otherwise {@code null}
	 * @throws Exception if domain value resolution fails
	 */
	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		if (ListAttributes.variantDomainPropertyName.equals(attributeName)) {
			return dvs;
		}
		return null;
	}
	
	/**
	 * Returns the shared dynamic domain values for the inline-grid sample attribute.
	 *
	 * @param attributeName the document attribute requesting values
	 * @param bean the current inline-grid bean
	 * @return the configured domain values when the dynamic-domain attribute is requested, otherwise {@code null}
	 * @throws Exception if domain value resolution fails
	 */
	@Override
	public List<DomainValue> getDynamicDomainValues(String attributeName, InlineGrid bean) throws Exception {
		if (ListAttributes.dynamicDomainPropertyName.equals(attributeName)) {
			return dvs;
		}
		return null;
	}
}
