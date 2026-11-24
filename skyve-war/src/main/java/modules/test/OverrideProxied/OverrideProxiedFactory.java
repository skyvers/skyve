package modules.test.OverrideProxied;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.test.domain.OverrideProxied;
import modules.test.domain.TestDocument;

/**
 * Creates test fixtures for {@link OverrideProxied}, returning the extension class as UserFactory does.
 */
@SuppressWarnings("java:S1118") // DataBuilder instantiates data factories through their public no-argument constructor
public class OverrideProxiedFactory {
	/** The text every CRUD fixture has. */
	public static final String CRUD_TEXT = "factory";

	/**
	 * Builds a CRUD fixture with the text {@link #CRUD_TEXT}.
	 *
	 * @return	The fixture.
	 */
	@SkyveFixture(types = FixtureType.crud)
	public static OverrideProxiedExtension crudInstance() {
		OverrideProxiedExtension result = new DataBuilder().factoryBuild(TestDocument.OVERRIDE_PROXIED);
		result.setText(CRUD_TEXT);
		return result;
	}
}
