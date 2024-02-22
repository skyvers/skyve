package modules.admin.Communication;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.Test;
import org.skyve.CORE;
import org.skyve.persistence.Persistence;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture;

import modules.admin.domain.Communication;
import util.AbstractH2TestForJUnit4;

public class CommunicationBizletTest extends AbstractH2TestForJUnit4 {

	@SuppressWarnings({ "deprecation", "static-method" })
	@Test
	public void testAnonymouslyCommunicationExists() {
		// create the test data
		Persistence pers = CORE.getPersistence();
		Communication c = new DataBuilder().fixture(SkyveFixture.FixtureType.crud).build(Communication.MODULE_NAME,
				Communication.DOCUMENT_NAME);
		c = pers.save(c);
		String customer = pers.getUser().getCustomerName();

		// call the method under test
		boolean result = CommunicationBizlet.anonymouslyCommunicationExists(pers, customer, c.getBizId());

		// verify the result
		assertThat(Boolean.valueOf(result), is(notNullValue()));
		assertTrue(result);
	}

}
