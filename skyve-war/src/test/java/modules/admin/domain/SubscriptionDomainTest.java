package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Communication.CommunicationExtension;
import modules.test.AbstractSkyveTest;

public class SubscriptionDomainTest extends AbstractSkyveTest {

	@Test
	@SuppressWarnings("static-method")
	void dataBuilderCreatesSubscription() throws Exception {
		Subscription bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Subscription.MODULE_NAME, Subscription.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	@SuppressWarnings("static-method")
	void moduleAndDocumentNames() throws Exception {
		Subscription bean = new Subscription();
		assertEquals("admin", bean.getBizModule());
		assertEquals("Subscription", bean.getBizDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void receiverIdentifierSetAndGet() throws Exception {
		Subscription bean = new Subscription();
		bean.setReceiverIdentifier("user@example.com");
		assertEquals("user@example.com", bean.getReceiverIdentifier());
	}

	@Test
	@SuppressWarnings("static-method")
	void declinedSetAndGet() throws Exception {
		Subscription bean = new Subscription();
		bean.setDeclined(Boolean.TRUE);
		assertTrue(bean.getDeclined());
	}

	@Test
	@SuppressWarnings("static-method")
	void preferredReceiverIdentifierSetAndGet() throws Exception {
		Subscription bean = new Subscription();
		bean.setPreferredReceiverIdentifier("preferred@example.com");
		assertEquals("preferred@example.com", bean.getPreferredReceiverIdentifier());
	}

	@Test
	@SuppressWarnings("static-method")
	void getBizKeyNotNull() throws Exception {
		Subscription bean = new Subscription();
		assertNotNull(bean.getBizKey());
	}

	@Test
	@SuppressWarnings("static-method")
	void communicationNullByDefault() throws Exception {
		Subscription bean = new Subscription();
		assertNull(bean.getCommunication());
	}

	@Test
	@SuppressWarnings("static-method")
	void communicationSetAndGet() throws Exception {
		Subscription bean = new Subscription();
		CommunicationExtension comm = new CommunicationExtension();
		bean.setCommunication(comm);
		assertEquals(comm, bean.getCommunication());
                // Setting same value again should not fail
                bean.setCommunication(comm);
                assertEquals(comm, bean.getCommunication());
        }
}

