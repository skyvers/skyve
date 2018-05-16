package modules.admin.Tag.actions;

import modules.admin.domain.Tag;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractActionTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractActionTest} to create your own tests for this action.
 */
public class ExceptTagTest extends AbstractActionTest<Tag, ExceptTag> {

	@Override
	protected ExceptTag getAction() {
		return new ExceptTag();
	}

	@Override
	protected Tag getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
	}
}