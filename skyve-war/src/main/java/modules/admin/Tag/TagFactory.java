package modules.admin.Tag;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Tag.actions.BulkDocumentAction;
import modules.admin.Tag.actions.PerformCombination;
import modules.admin.Tag.actions.PrepareExplanation;
import modules.admin.Tag.actions.TagAll;
import modules.admin.domain.Contact;
import modules.admin.domain.Tag;

@SkyveFactory(excludedActions = { BulkDocumentAction.class, PerformCombination.class, PrepareExplanation.class, TagAll.class })
/**
 * Creates fixture instances for admin {@link Tag} tests.
 */
public class TagFactory {
	/**
	 * Builds a CRUD fixture with upload defaults pointing at {@link Contact}.
	 *
	 * @return A fixture suitable for standard CRUD tests.
	 */
	@SkyveFixture(types = FixtureType.crud)
	public static Tag crudInstance() {
		Tag tag = new DataBuilder().factoryBuild(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);

		tag.setUploadModuleName(Contact.MODULE_NAME);
		tag.setUploadDocumentName(Contact.DOCUMENT_NAME);

		return tag;
	}

	/**
	 * Builds a SAIL fixture with optional action/upload fields cleared.
	 *
	 * @return A fixture tailored for SAIL tests.
	 */
	@SuppressWarnings("static-method")
	@SkyveFixture(types = FixtureType.sail)
	public Tag sail() {
		Tag result = crudInstance();

		result.setOperandTag(null);

		result.setActionModuleName(null);
		result.setActionDocumentName(null);

		result.setUploadModuleName(null);
		result.setUploadDocumentName(null);

		result.setDocumentAction(null);
		result.setDocumentCondition(null);
		result.setAttributeName(null);

		return result;
	}
}
