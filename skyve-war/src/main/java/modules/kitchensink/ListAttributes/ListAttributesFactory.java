package modules.kitchensink.ListAttributes;

import org.skyve.util.test.SkyveFactory;

import modules.kitchensink.domain.ListAttributes;

/**
 * Fixture factory for {@code kitchensink.ListAttributes} test instances.
 *
 * <p>ListAttributesBizlet keeps {@code dateTimeAsDate} equal to {@code dateTime} when saved, so the generated update test
 * cannot change it: it is kept from choosing it.
 */
@SkyveFactory(excludedUpdateAttributes = { ListAttributes.dateTimeAsDatePropertyName })
public class ListAttributesFactory {
	// the annotation is all
}
