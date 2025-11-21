package modules.admin.Tag.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Tag.TagExtension;
import modules.admin.domain.Tag;
import util.AbstractH2TestForJUnit4;

public class CopyTagToUserActionTest extends AbstractH2TestForJUnit4 {
	
	@Test(expected = ValidationException.class)
	@SuppressWarnings("static-method")
	public void testExecuteThrowsValidationExceptionWhenCopyToUserIsNull() throws Exception {
		// setup the test data
		TagExtension tag = new DataBuilder()
			.fixture(FixtureType.crud)
			.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
		tag.setCopyToUser(null);

		// call the method under test - should throw ValidationException
		new CopyTagToUser().execute(tag, null);
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testExecuteSucceedsWhenCopyToUserIsSet() throws Exception {
		// setup the test data
		TagExtension tag = new DataBuilder()
			.fixture(FixtureType.crud)
			.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
		
		// Ensure copyToUser is set by the fixture
		assertThat("copyToUser should be set by fixture", tag.getCopyToUser() != null, is(true));
		
		// call the method under test - should not throw exception
		// Note: This test assumes the fixture sets copyToUser. If it doesn't,
		// we would need to manually create and set a UserProxy
		new CopyTagToUser().execute(tag, null);
	}
}
