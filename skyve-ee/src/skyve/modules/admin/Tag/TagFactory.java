package modules.admin.Tag;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Tag;

public class TagFactory {
	@SuppressWarnings("static-method")
	@SkyveFixture(types = FixtureType.sail)
	public Tag sail() {
		Tag result = new DataBuilder().build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);

		result.setActionModuleName(null);
		result.setActionDocumentName(null);
		result.setActionTag(null);
		result.setActionTagCount(null);

		result.setUploadModuleName(null);
		result.setUploadDocumentName(null);
		
		result.setDocumentAction(null);
		result.setDocumentCondition(null);
		result.setAttributeName(null);
		
		return result;
	}
}
