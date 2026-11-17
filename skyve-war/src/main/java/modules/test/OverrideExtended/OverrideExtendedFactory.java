package modules.test.OverrideExtended;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.test.domain.OverrideExtended;

/**
 * Creates test fixtures for {@link OverrideExtended}, returning the vanilla extension class, which the
 * test customer's own extension extends through its generated override class.
 */
@SuppressWarnings("java:S1118") // DataBuilder instantiates data factories through their public no-argument constructor
public class OverrideExtendedFactory {
	/** The text every CRUD fixture has. */
	public static final String CRUD_TEXT = "factory";

	/**
	 * Builds a CRUD fixture of the customer's bean class with the text {@link #CRUD_TEXT}.
	 *
	 * @return	The fixture.
	 */
	@SkyveFixture(types = FixtureType.crud)
	public static OverrideExtendedExtension crudInstance() {
		OverrideExtendedExtension result = new DataBuilder().factoryBuild(OverrideExtended.MODULE_NAME, OverrideExtended.DOCUMENT_NAME);
		result.setText(CRUD_TEXT);
		return result;
	}
}
