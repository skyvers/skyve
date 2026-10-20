package modules.kitchensink.DialogsFixture;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.kitchensink.domain.DialogsFixture;

/**
 * Fixture factory for {@code kitchensink.DialogsFixture} test instances.
 *
 * <p>When a fixture asks on save, its preSave also sets the outcome, which the generated update test would
 * find changed. Fixtures for generated tests therefore never ask on save or delete, and the generated update
 * test does not switch either on. Generated action tests run the actions with the answers that the test
 * scaffolding's {@code util.TestDialogs} gives.
 */
@SuppressWarnings("java:S1118") // The repository instantiates factories, so the implicit public constructor must stay
@SkyveFactory(excludedUpdateAttributes = { DialogsFixture.askOnSavePropertyName, DialogsFixture.askOnDeletePropertyName })
public class DialogsFixtureFactory {
	/**
	 * Builds a fixture that neither asks on save nor on delete.
	 *
	 * @return a fixture with random text and no questions on save or delete
	 */
	@SkyveFixture(types = FixtureType.crud)
	public static DialogsFixture crudInstance() {
		DialogsFixture bean = new DataBuilder().fixture(FixtureType.crud).factoryBuild(DialogsFixture.MODULE_NAME, DialogsFixture.DOCUMENT_NAME);
		bean.setAskOnSave(Boolean.FALSE);
		bean.setAskOnDelete(Boolean.FALSE);
		return bean;
	}
}
