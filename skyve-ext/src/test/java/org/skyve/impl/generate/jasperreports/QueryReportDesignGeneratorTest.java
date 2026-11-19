package org.skyve.impl.generate.jasperreports;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

@SuppressWarnings("static-method")
class QueryReportDesignGeneratorTest {

	@Test
	void getSubreportGeneratorThrowsUnsupportedOperationException() {
		QueryReportDesignGenerator generator = new QueryReportDesignGenerator();
		assertThrows(UnsupportedOperationException.class, generator::getSubreportGenerator);
	}

	@Test
	void addFieldsWithoutAQueryNameThrowsMetaDataException() throws Exception {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Order")).thenReturn(document);
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		DesignSpecification design = new DesignSpecification();
		design.setName("OrderQuery");
		design.setModuleName("sales");
		design.setDocumentName("Order");
		QueryReportDesignGenerator generator = new QueryReportDesignGenerator();

		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		AbstractPersistence previous = threadLocal.get();
		try {
			threadLocal.set(persistence);
			assertThrows(MetaDataException.class, () -> generator.addFields(design));
		}
		finally {
			if (previous == null) {
				threadLocal.remove();
			}
			else {
				threadLocal.set(previous);
			}
		}
	}
}
