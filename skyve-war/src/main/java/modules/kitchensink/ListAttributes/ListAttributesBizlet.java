package modules.kitchensink.ListAttributes;

import java.util.ArrayList;
import java.util.List;

import org.skyve.metadata.model.document.Bizlet;

import modules.kitchensink.domain.ListAttributes;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Supplies static demonstration domain values for list-attribute examples.
 */
public class ListAttributesBizlet extends Bizlet<ListAttributes> {
	private static List<DomainValue> dvs = new ArrayList<>(3);
	
	static {
		dvs.add(new DomainValue("one", "One (1)!"));
		dvs.add(new DomainValue("two", "Two (2)!"));
		dvs.add(new DomainValue("three", "Three (3)!"));
	}
	
	/**
	 * Keeps dateTimeAsDate equal to dateTime, so that it has a time that its column (a date-only converter) does not
	 * show.
	 *
	 * @param bean the list attributes being saved
	 * @throws Exception if the bean cannot be updated
	 */
	@Override
	public void preSave(@Nonnull ListAttributes bean) throws Exception {
		bean.setDateTimeAsDate(bean.getDateTime());
	}

	/**
	 * Returns the shared constant domain values for the configured sample attribute.
	 *
	 * @param attributeName the document attribute requesting values
	 * @return the configured domain values when the constant-domain attribute is requested, otherwise {@code null}
	 * @throws Exception if domain value resolution fails
	 */
	@Override
	public @Nullable List<DomainValue> getConstantDomainValues(@Nonnull String attributeName) throws Exception {
		if (ListAttributes.constantDomainPropertyName.equals(attributeName)) {
			return dvs;
		}
		return null;
	}
	
	/**
	 * Returns the shared variant domain values for the configured sample attribute.
	 *
	 * @param attributeName the document attribute requesting values
	 * @return the configured domain values when the variant-domain attribute is requested, otherwise {@code null}
	 * @throws Exception if domain value resolution fails
	 */
	@Override
	public @Nullable List<DomainValue> getVariantDomainValues(@Nonnull String attributeName) throws Exception {
		if (ListAttributes.variantDomainPropertyName.equals(attributeName)) {
			return dvs;
		}
		return null;
	}
	
	/**
	 * Returns the shared dynamic domain values for the configured sample attribute.
	 *
	 * @param attributeName the document attribute requesting values
	 * @param bean the current list-attributes bean
	 * @return the configured domain values when the dynamic-domain attribute is requested, otherwise {@code null}
	 * @throws Exception if domain value resolution fails
	 */
	@Override
	public @Nullable List<DomainValue> getDynamicDomainValues(@Nonnull String attributeName, @Nonnull ListAttributes bean) throws Exception {
		if (ListAttributes.dynamicDomainPropertyName.equals(attributeName)) {
			return dvs;
		}
		return null;
	}
}
