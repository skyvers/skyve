package modules.admin.Communication;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.persistence.Persistence;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture;

import modules.admin.domain.Communication;
import util.AbstractH2Test;

public class CommunicationServiceTest extends AbstractH2Test {

	private CommunicationService communicationService;

	@BeforeEach
	public void setup() {
		// Initialise CommunicationService
		communicationService = new CommunicationService();
	}

	@Test
	public void testAnonymouslyCommunicationExists() {
		// create the test data
		Persistence pers = CORE.getPersistence();
		Communication c = new DataBuilder().fixture(SkyveFixture.FixtureType.crud)
				.build(Communication.MODULE_NAME,
						Communication.DOCUMENT_NAME);
		c = pers.save(c);
		String customer = pers.getUser().getCustomerName();

		// call the method under test
		boolean result = communicationService.anonymouslyCommunicationExists(pers, customer, c.getBizId());

		// verify the result
		assertThat(Boolean.valueOf(result), notNullValue());
		assertTrue(result);
	}

}
