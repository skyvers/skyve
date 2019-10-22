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
public class TagFactory {

	@SkyveFixture(types = FixtureType.crud)
	public static Tag crudInstance() {
		Tag tag = new DataBuilder().build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);

		tag.setUploadModuleName(Contact.MODULE_NAME);
		tag.setUploadDocumentName(Contact.DOCUMENT_NAME);

		return tag;
	}

	@SuppressWarnings("static-method")
	@SkyveFixture(types = FixtureType.sail)
	public Tag sail() {
		Tag result = new DataBuilder().build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);

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
