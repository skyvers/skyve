package modules.admin.Tag.actions;

import modules.admin.Tag.TagExtension;
import modules.admin.domain.AdminDocument;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class CopyTagToUserTest extends AbstractActionTest<TagExtension, CopyTagToUser> {

	@Override
	protected CopyTagToUser getAction() {
		return new CopyTagToUser();
	}

	@Override
	protected TagExtension getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(AdminDocument.TAG);
	}
}