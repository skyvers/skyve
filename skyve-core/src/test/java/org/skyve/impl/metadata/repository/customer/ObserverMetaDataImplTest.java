package org.skyve.impl.metadata.repository.customer;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.MetaDataException;

public class ObserverMetaDataImplTest {

	@Test
	@SuppressWarnings("static-method")
	public void setClassNameRoundtrip() {
		ObserverMetaDataImpl obs = new ObserverMetaDataImpl();
		obs.setClassName("com.example.MyObserver");
		assertThat(obs.getClassName(), is("com.example.MyObserver"));
	}

	@Test
	@SuppressWarnings("static-method")
	public void blankClassNameBecomesNull() {
		ObserverMetaDataImpl obs = new ObserverMetaDataImpl();
		obs.setClassName("  ");
		assertNull(obs.getClassName());
	}

	@Test
	@SuppressWarnings("static-method")
	public void defaultClassNameIsNull() {
		ObserverMetaDataImpl obs = new ObserverMetaDataImpl();
		assertNull(obs.getClassName());
	}

        @Test
        @SuppressWarnings("static-method")
        public void getObserverThrowsForUnknownClass() {
                ObserverMetaDataImpl obs = new ObserverMetaDataImpl();
                obs.setClassName("com.example.DoesNotExist");
                assertThrows(MetaDataException.class, obs::getObserver);
        }
}
