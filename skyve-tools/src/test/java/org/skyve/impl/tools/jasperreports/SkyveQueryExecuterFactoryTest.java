package org.skyve.impl.tools.jasperreports;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Collections;

import org.junit.jupiter.api.Test;

import net.sf.jasperreports.engine.JRDataset;
import net.sf.jasperreports.engine.JRQuery;
import net.sf.jasperreports.engine.JRValueParameter;
import net.sf.jasperreports.engine.JasperReportsContext;
import net.sf.jasperreports.engine.query.JRQueryExecuter;

class SkyveQueryExecuterFactoryTest {

	private final SkyveQueryExecuterFactory factory = new SkyveQueryExecuterFactory();

	@Test
	void createQueryExecuterReturnsNullWhenDatasetIsNull() throws Exception {
		assertNull(factory.createQueryExecuter(mock(JasperReportsContext.class), null, Collections.<String, JRValueParameter>emptyMap()));
	}

	@Test
	void createQueryExecuterReturnsNullWhenDatasetQueryIsNull() throws Exception {
		JRDataset dataset = mock(JRDataset.class);
		when(dataset.getQuery()).thenReturn(null);

		assertNull(factory.createQueryExecuter(mock(JasperReportsContext.class), dataset, Collections.<String, JRValueParameter>emptyMap()));
	}

	@Test
	void createQueryExecuterReturnsSkyveQueryExecuterWhenQueryTextExists() throws Exception {
		JRQuery query = mock(JRQuery.class);
		when(query.getText()).thenReturn("admin.Contact");

		JRDataset dataset = mock(JRDataset.class);
		when(dataset.getQuery()).thenReturn(query);

		JRQueryExecuter executer = factory.createQueryExecuter(mock(JasperReportsContext.class), dataset, Collections.<String, JRValueParameter>emptyMap());
		assertTrue(executer instanceof SkyveQueryExecuter);
	}

	@Test
	void supportsAllParameterTypesAndNoBuiltinParameters() {
		assertTrue(factory.supportsQueryParameterType("anything"));
		assertFalse(factory.getBuiltinParameters().length > 0);
	}
}