package modules.admin.Subscription;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.RETURNS_DEFAULTS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.DomainException;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;

import modules.admin.domain.Subscription;

/**
 * Unit tests for anonymous subscription SQL orchestration.
 */
class SubscriptionServiceTest {
	private static final String CUSTOMER = "cust";
	private static final String COMMUNICATION_ID = "communication-1";
	private static final String RECEIVER = "person@example.com";

	private final SubscriptionService service = new SubscriptionService();

	@Test
	void anonymouslyCheckSubscriptionPublicUserReturnsTrueWhenUserAlreadyExists() {
		Persistence persistence = mock(Persistence.class);
		SQL lookup = sql();
		when(persistence.newSQL(anyString())).thenReturn(lookup);
		when(lookup.tupleResult()).thenReturn(new Object[] { "SkyveSubscriptionUser", "SkyveSubscriptionUser" });

		assertTrue(service.anonymouslyCheckSubscriptionPublicUser(persistence, CUSTOMER));

		verify(persistence, never()).begin();
		verify(persistence, never()).commit(false);
		verify(lookup).putParameter(Bean.CUSTOMER_NAME, CUSTOMER, false);
		verify(lookup).putParameter(Bean.DOCUMENT_ID, "SkyveSubscriptionUser", false);
	}

	@Test
	void anonymouslyCheckSubscriptionPublicUserCreatesUserWhenMissing() {
		Persistence persistence = mock(Persistence.class);
		SQL lookup = sql();
		SQL insertContact = sql();
		SQL insertUser = sql();
		when(persistence.newSQL(anyString())).thenReturn(lookup, insertContact, insertUser);
		when(lookup.tupleResult()).thenReturn(null);

		assertTrue(service.anonymouslyCheckSubscriptionPublicUser(persistence, CUSTOMER));

		verify(persistence).begin();
		verify(insertContact).execute();
		verify(insertUser).execute();
		verify(persistence).commit(false);
		verify(insertContact).putParameter(Bean.CUSTOMER_NAME, CUSTOMER, false);
		verify(insertUser).putParameter(Bean.CUSTOMER_NAME, CUSTOMER, false);
	}

	@Test
	void anonymouslyCheckSubscriptionPublicUserReturnsFalseWhenSqlThrowsDomainException() {
		Persistence persistence = mock(Persistence.class);
		SQL lookup = sql();
		when(persistence.newSQL(anyString())).thenReturn(lookup);
		when(lookup.tupleResult()).thenThrow(new DomainException("lookup failed"));

		assertFalse(service.anonymouslyCheckSubscriptionPublicUser(persistence, CUSTOMER));

		verify(persistence, never()).begin();
	}

	@Test
	void anonymouslySubscriptionExistsReturnsTrueWhenCountIsPositive() {
		Persistence persistence = mock(Persistence.class);
		SQL sql = sql();
		when(persistence.newSQL(anyString())).thenReturn(sql);
		when(sql.scalarResult(Number.class)).thenReturn(Integer.valueOf(1));

		assertTrue(service.anonymouslySubscriptionExists(persistence, CUSTOMER, COMMUNICATION_ID, RECEIVER));

		verify(sql).putParameter(Bean.CUSTOMER_NAME, CUSTOMER, false);
		verify(sql).putParameter(Subscription.communicationPropertyName, COMMUNICATION_ID, false);
		verify(sql).putParameter(Subscription.receiverIdentifierPropertyName, RECEIVER, false);
	}

	@Test
	void anonymouslySubscriptionExistsReturnsFalseForNullZeroAndDomainExceptionResults() {
		Persistence persistence = mock(Persistence.class);
		SQL nullCount = sql();
		SQL zeroCount = sql();
		SQL failing = sql();
		when(persistence.newSQL(anyString())).thenReturn(nullCount, zeroCount, failing);
		when(nullCount.scalarResult(Number.class)).thenReturn(null);
		when(zeroCount.scalarResult(Number.class)).thenReturn(Integer.valueOf(0));
		when(failing.scalarResult(Number.class)).thenThrow(new DomainException("count failed"));

		assertFalse(service.anonymouslySubscriptionExists(persistence, CUSTOMER, COMMUNICATION_ID, RECEIVER));
		assertFalse(service.anonymouslySubscriptionExists(persistence, CUSTOMER, COMMUNICATION_ID, RECEIVER));
		assertFalse(service.anonymouslySubscriptionExists(persistence, CUSTOMER, COMMUNICATION_ID, RECEIVER));
	}

	@Test
	void anonymouslyUnsubscribeExecutesInsertWhenPublicUserExists() throws Exception {
		Persistence persistence = mock(Persistence.class);
		SQL lookup = sql();
		SQL insert = sql();
		when(persistence.newSQL(anyString())).thenReturn(lookup, insert);
		when(lookup.tupleResult()).thenReturn(new Object[] { "SkyveSubscriptionUser", "SkyveSubscriptionUser" });

		assertFalse(service.anonymouslyUnsubscribe(persistence, CUSTOMER, COMMUNICATION_ID, RECEIVER));

		verify(insert).putParameter(Bean.CUSTOMER_NAME, CUSTOMER, false);
		verify(insert).putParameter(Subscription.communicationPropertyName, COMMUNICATION_ID, false);
		verify(insert).putParameter(Subscription.receiverIdentifierPropertyName, RECEIVER, false);
		verify(insert).putParameter(Subscription.declinedPropertyName, Boolean.TRUE);
		verify(insert).execute();
	}

	@Test
	void anonymouslyUnsubscribeSkipsInsertWhenPublicUserCannotBeCreated() throws Exception {
		Persistence persistence = mock(Persistence.class);
		SQL lookup = sql();
		when(persistence.newSQL(anyString())).thenReturn(lookup);
		when(lookup.tupleResult()).thenThrow(new DomainException("lookup failed"));

		assertFalse(service.anonymouslyUnsubscribe(persistence, CUSTOMER, COMMUNICATION_ID, RECEIVER));

		verify(lookup, never()).execute();
	}

	private static SQL sql() {
		return mock(SQL.class, invocation -> {
			if (SQL.class.equals(invocation.getMethod().getReturnType())) {
				return invocation.getMock();
			}
			return RETURNS_DEFAULTS.answer(invocation);
		});
	}
}
