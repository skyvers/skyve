package modules.admin.util;

import org.skyve.util.test.SkyveFactory;

import modules.admin.Tag.actions.BulkDocumentAction;
import modules.admin.Tag.actions.PerformCombination;
import modules.admin.Tag.actions.PrepareExplanation;
import modules.admin.domain.Contact;
import modules.admin.domain.Tag;

@SkyveFactory(excludedActions = { BulkDocumentAction.class, PerformCombination.class, PrepareExplanation.class })
public class TagFactoryExtension extends TagFactory {

	@Override
	public Tag getInstance() throws Exception {
		Tag tag = super.getInstance();
		tag.setActionTag(super.getInstance());

		tag.setUploadModuleName(Contact.MODULE_NAME);
		tag.setUploadDocumentName(Contact.DOCUMENT_NAME);

		return tag;
	}

}
