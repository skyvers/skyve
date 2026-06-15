package org.skyve.impl.tools.jasperreports;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;

import net.sf.jasperreports.engine.JRDataset;
import net.sf.jasperreports.engine.JRException;
import net.sf.jasperreports.engine.JRQuery;
import net.sf.jasperreports.engine.JasperReportsContext;
import net.sf.jasperreports.engine.query.JRQueryExecuter;

@SuppressWarnings("static-method")
class SkyveDocumentExecuterFactoryTest {
	@Test
	void createQueryExecuterReturnsNullForMissingDataset() throws JRException {
		SkyveDocumentExecuterFactory factory = new SkyveDocumentExecuterFactory();

		assertThat(factory.createQueryExecuter((JasperReportsContext) null, null, null), nullValue());
	}

	@Test
	void createQueryExecuterReturnsNullForMissingQuery() throws JRException {
		JRDataset dataset = mock(JRDataset.class);
		SkyveDocumentExecuterFactory factory = new SkyveDocumentExecuterFactory();

		assertThat(factory.createQueryExecuter((JasperReportsContext) null, dataset, null), nullValue());
	}

	@Test
	void createQueryExecuterCreatesDocumentExecuterForQueryText() throws JRException {
		JRQuery query = mock(JRQuery.class);
		when(query.getText()).thenReturn("admin.Contact");
		JRDataset dataset = mock(JRDataset.class);
		when(dataset.getQuery()).thenReturn(query);

		SkyveDocumentExecuterFactory factory = new SkyveDocumentExecuterFactory();

		JRQueryExecuter executer = factory.createQueryExecuter((JasperReportsContext) null, dataset, null);

		assertThat(executer, instanceOf(SkyveDocumentExecuter.class));
		assertFalse(executer.cancelQuery());
		executer.close();
	}

	@Test
	void supportsAllQueryParameterTypesAndNoBuiltins() {
		SkyveDocumentExecuterFactory factory = new SkyveDocumentExecuterFactory();

		assertTrue(factory.supportsQueryParameterType("java.lang.String"));
		assertEquals(0, factory.getBuiltinParameters().length);
	}
}
