package org.skyve.impl.metadata.repository.customer;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.Observer;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;

import jakarta.annotation.Nonnull;
import jakarta.servlet.http.HttpSession;

class ObserverMetaDataImplTest {

	/** Concrete no-arg Observer implementation for testing. */
	public static class NoOpObserver implements Observer {
		@Override public void startup(@Nonnull Customer customer) { /* no-op */ }
		@Override public void shutdown(@Nonnull Customer customer) { /* no-op */ }
		@Override public void beforeBackup(@Nonnull Customer customer) { /* no-op */ }
		@Override public void afterBackup(@Nonnull Customer customer) { /* no-op */ }
		@Override public void beforeRestore(@Nonnull Customer customer) { /* no-op */ }
		@Override public void afterRestore(@Nonnull Customer customer) { /* no-op */ }
		@Override public void login(@Nonnull User user, @Nonnull HttpSession session) { /* no-op */ }
		@Override public void logout(@Nonnull User user, @Nonnull HttpSession session) { /* no-op */ }
	}

	@Test
	@SuppressWarnings("static-method")
	void setClassNameRoundtrip() {
		ObserverMetaDataImpl obs = new ObserverMetaDataImpl();
		obs.setClassName("com.example.MyObserver");
		assertThat(obs.getClassName(), is("com.example.MyObserver"));
	}

	@Test
	@SuppressWarnings("static-method")
	void blankClassNameBecomesNull() {
		ObserverMetaDataImpl obs = new ObserverMetaDataImpl();
		obs.setClassName("  ");
		assertNull(obs.getClassName());
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultClassNameIsNull() {
		ObserverMetaDataImpl obs = new ObserverMetaDataImpl();
		assertNull(obs.getClassName());
	}

	@Test
	@SuppressWarnings("static-method")
	void getObserverThrowsForUnknownClass() {
		ObserverMetaDataImpl obs = new ObserverMetaDataImpl();
		obs.setClassName("com.example.DoesNotExist");
		assertThrows(MetaDataException.class, obs::getObserver);
	}

	@Test
	@SuppressWarnings("static-method")
	void getObserverInstantiatesConcreteClass() {
		ObserverMetaDataImpl obs = new ObserverMetaDataImpl();
		obs.setClassName(NoOpObserver.class.getName());
		Observer result = obs.getObserver();
		assertNotNull(result);
		assertSame(NoOpObserver.class, result.getClass());
	}

	@Test
	@SuppressWarnings("static-method")
	void getObserverReturnsSameInstanceOnSecondCall() {
		ObserverMetaDataImpl obs = new ObserverMetaDataImpl();
		obs.setClassName(NoOpObserver.class.getName());
		Observer first = obs.getObserver();
		Observer second = obs.getObserver();
		assertSame(first, second, "Should return the cached singleton observer");
	}
}
