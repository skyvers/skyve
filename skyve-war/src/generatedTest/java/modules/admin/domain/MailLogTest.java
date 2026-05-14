package modules.admin.domain;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;
import util.AbstractDomainTest;

/**
 * Generated - local changes will be overwritten.
 * Extend {@link AbstractDomainTest} to create your own tests for this document.
 */
public class MailLogTest extends AbstractDomainTest<MailLog> {

	@Override
	protected MailLog getBean() throws Exception {
		return new DataBuilder()
			.fixture(FixtureType.crud)
			.build(MailLog.MODULE_NAME, MailLog.DOCUMENT_NAME);
	}
}