package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Communication.CommunicationExtension;
import modules.test.AbstractSkyveTest;

class SubscriptionDomainTest extends AbstractSkyveTest {

	@Test
	@SuppressWarnings("static-method")
	void dataBuilderCreatesSubscription() {
		Subscription bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Subscription.MODULE_NAME, Subscription.DOCUMENT_NAME);
		assertNotNull(bean);
	}

	@Test
	@SuppressWarnings("static-method")
	void moduleAndDocumentNames() {
		Subscription bean = new Subscription();
		assertEquals("admin", bean.getBizModule());
		assertEquals("Subscription", bean.getBizDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void receiverIdentifierSetAndGet() {
		Subscription bean = new Subscription();
		bean.setReceiverIdentifier("user@example.com");
		assertEquals("user@example.com", bean.getReceiverIdentifier());
	}

	@Test
	@SuppressWarnings("static-method")
	void declinedSetAndGet() {
		Subscription bean = new Subscription();
		bean.setDeclined(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getDeclined());
	}

	@Test
	@SuppressWarnings("static-method")
	void preferredReceiverIdentifierSetAndGet() {
		Subscription bean = new Subscription();
		bean.setPreferredReceiverIdentifier("preferred@example.com");
		assertEquals("preferred@example.com", bean.getPreferredReceiverIdentifier());
	}

	@Test
	@SuppressWarnings("static-method")
	void getBizKeyNotNull() {
		Subscription bean = new Subscription();
		assertNotNull(bean.getBizKey());
	}

	@Test
	@SuppressWarnings("static-method")
	void communicationNullByDefault() {
		Subscription bean = new Subscription();
		assertNull(bean.getCommunication());
	}

	@Test
	@SuppressWarnings("static-method")
	void communicationSetAndGet() {
		Subscription bean = new Subscription();
		CommunicationExtension comm = new CommunicationExtension();
		bean.setCommunication(comm);
		assertEquals(comm, bean.getCommunication());
                // Setting same value again should not fail
                bean.setCommunication(comm);
                assertEquals(comm, bean.getCommunication());
        }
}

