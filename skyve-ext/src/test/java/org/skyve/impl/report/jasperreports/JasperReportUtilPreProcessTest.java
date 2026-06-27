package org.skyve.impl.report.jasperreports;

import static org.hamcrest.CoreMatchers.endsWith;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.ReportParameters;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;
import org.skyve.report.ReportFormat;

import net.sf.jasperreports.engine.JRQuery;
import net.sf.jasperreports.engine.JasperPrint;
import net.sf.jasperreports.engine.JasperReport;

@SuppressWarnings("static-method")
class JasperReportUtilPreProcessTest {
	private final ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();

	@AfterEach
	void restoreRepository() {
		if (originalRepository == null) {
			ProvidedRepositoryFactory.clear();
		}
		else {
			ProvidedRepositoryFactory.set(originalRepository);
		}
	}

	@Test
	void preProcessAddsIntrinsicResourceParametersAndReturnsReportFileName() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		Map<String, Object> parameters = new HashMap<>();
		when(customer.getName()).thenReturn("demo");
		when(document.getOwningModuleName()).thenReturn("sales");
		when(repository.getReportFileName(customer, document, "invoice.jasper"))
				.thenReturn("/reports/sales/invoice.jasper");
		ProvidedRepositoryFactory.set(repository);

		String result = invokePreProcess(customer, document, "invoice.jasper", parameters);

		assertThat(result, is("/reports/sales/invoice.jasper"));
		assertThat((String) parameters.get("RESOURCE_DIR"), endsWith("customers/demo/resources/"));
		assertThat((String) parameters.get("MODULE_RESOURCE_DIR"), endsWith("modules/sales/resources/"));
		assertThat(parameters.get("SUBREPORT_DIR"), is("/reports/sales/"));
	}

	@Test
	void preProcessThrowsMetadataExceptionWhenReportCannotBeResolved() {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		when(customer.getName()).thenReturn("demo");
		when(document.getOwningModuleName()).thenReturn("sales");
		when(document.getName()).thenReturn("Order");
		when(repository.getReportFileName(customer, document, "missing.jasper")).thenReturn(null);
		ProvidedRepositoryFactory.set(repository);

		Map<String, Object> emptyParams = new HashMap<>();
		MetaDataException thrown = assertThrows(MetaDataException.class,
				() -> invokePreProcess(customer, document, "missing.jasper", emptyParams));

		assertThat(thrown.getMessage(), is("Report missing.jasper in document sales.Order for customer demo does not exist."));
	}

	@Test
	void preProcessReportParametersDelegatesToDocumentReportAndParameterMap() throws Exception {
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		Map<String, Object> parameters = new HashMap<>();
		ReportParameters reportParameters = new ReportParameters(document, "summary.jasper", parameters);
		when(customer.getName()).thenReturn("demo");
		when(document.getOwningModuleName()).thenReturn("sales");
		when(repository.getReportFileName(customer, document, "summary.jasper"))
				.thenReturn("/reports/sales/summary.jasper");
		ProvidedRepositoryFactory.set(repository);

		String result = invokePreProcess(customer, reportParameters);

		assertThat(result, is("/reports/sales/summary.jasper"));
		assertThat(parameters.get("SUBREPORT_DIR"), is("/reports/sales/"));
	}

	@Test
	void runReportReturnsNullForUnsupportedQueryLanguage() throws Exception {
		JasperReport report = mock(JasperReport.class);
		JRQuery query = mock(JRQuery.class);
		User user = mock(User.class);
		Document document = mock(Document.class);
		when(report.getQuery()).thenReturn(query);
		when(query.getLanguage()).thenReturn("noop");

		JasperPrint result = JasperReportUtil.runReport(report,
															user,
															document,
															new HashMap<>(),
															mock(Bean.class),
															ReportFormat.pdf,
															new ByteArrayOutputStream());

		assertThat(result, is((JasperPrint) null));
	}

	private static String invokePreProcess(Customer customer,
											Document document,
											String reportName,
											Map<String, Object> parameters)
	throws Exception {
		Method method = JasperReportUtil.class.getDeclaredMethod("preProcess", Customer.class, Document.class, String.class, Map.class);
		method.setAccessible(true);
		try {
			return (String) method.invoke(null, customer, document, reportName, parameters);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw new RuntimeException(cause);
		}
	}

	private static String invokePreProcess(Customer customer, ReportParameters reportParameters)
	throws Exception {
		Method method = JasperReportUtil.class.getDeclaredMethod("preProcess", Customer.class, ReportParameters.class);
		method.setAccessible(true);
		try {
			return (String) method.invoke(null, customer, reportParameters);
		}
		catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw new RuntimeException(cause);
		}
	}

}
