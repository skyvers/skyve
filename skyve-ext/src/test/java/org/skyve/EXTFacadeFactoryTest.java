package org.skyve;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.lang.reflect.Constructor;
import java.sql.Connection;
import java.util.List;

import org.jfree.chart.JFreeChart;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.Test;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.content.ContentManager;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.UploadException;
import org.skyve.impl.generate.charts.JFreeChartGenerator;
import org.skyve.impl.generate.charts.JFreeChartPostProcessor;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;
import org.skyve.impl.metadata.view.widget.Chart.ChartType;
import org.skyve.impl.persistence.hibernate.dialect.H2SpatialDialect;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.user.UserAccess;
import org.skyve.metadata.view.model.chart.ChartData;
import org.skyve.persistence.DataStore;
import org.skyve.util.PushMessage;

@SuppressWarnings({"static-method", "boxing"})
class EXTFacadeFactoryTest {
	public static class NoOpPostProcessor implements JFreeChartPostProcessor {
		@Override
		public void process(JFreeChart chart) {
			// no-op for tests
		}
	}

	@Test
	void newBizPortWorkbookFromExistingWorkbookReturnsNonNull() throws Exception {
		try (XSSFWorkbook workbook = new XSSFWorkbook()) {
			org.apache.poi.ss.usermodel.Sheet sheet = workbook.createSheet("Orders");
			org.apache.poi.ss.usermodel.Row row = sheet.createRow(0);
			row.createCell(0).setCellValue("sales");
			row.createCell(1).setCellValue("Order");
			org.apache.poi.ss.usermodel.Row bindingRow = sheet.createRow(1);
			bindingRow.createCell(0).setCellValue("bizId");
			org.apache.poi.ss.usermodel.Row titleRow = sheet.createRow(2);
			titleRow.createCell(0).setCellValue("Id");
			Customer customer = mock(Customer.class);
			Module module = mock(Module.class);
			Document document = mock(Document.class);
			when(customer.getModule("sales")).thenReturn(module);
			when(module.getDocument(customer, "Order")).thenReturn(document);

			BizPortWorkbook wb = EXT.newBizPortWorkbook(customer, workbook, new UploadException());

			assertNotNull(wb);
		}
	}

	@Test
	void newBizPortStandardGeneratorReturnsNonNull() {
		assertNotNull(EXT.newBizPortStandardGenerator(mock(Customer.class), mock(Document.class), "customer"));
	}

	@Test
	void newBizPortStandardLoaderReturnsNonNull() {
		assertNotNull(EXT.newBizPortStandardLoader(mock(BizPortWorkbook.class), new UploadException()));
	}

	@Test
	void newChartGeneratorReturnsNonNull() {
		JFreeChartGenerator generator = EXT.newChartGenerator(mock(ChartData.class), 320, 200);

		assertNotNull(generator);
	}

	@Test
	void chartFacadeReturnsJFreeChart() {
		JFreeChart chart = EXT.chart(ChartType.bar, categoryData(), 320, 200);

		assertNotNull(chart);
	}

	@Test
	void chartImageFacadeReturnsBufferedImage() {
		BufferedImage image = EXT.chartImage(ChartType.bar, categoryData(), 120, 80);

		assertNotNull(image);
	}

	@Test
	void pushSkipsStaleReceiver() {
		PushMessage.RECEIVERS.clear();
		TrackingReceiver stale = new TrackingReceiver(true);
		try {
			PushMessage.RECEIVERS.add(stale);

			EXT.push(new PushMessage().growl(org.skyve.domain.messages.MessageSeverity.info, "hello"));

			assertEquals(0, stale.messagesSent);
		}
		finally {
			PushMessage.RECEIVERS.clear();
		}
	}

	@Test
	void getDataStoreConnectionWrapsInvalidDriverClass() {
		DataStore dataStore = new DataStore("example.DoesNotExistDriver",
												"jdbc:h2:mem:invalid_driver",
												H2SpatialDialect.class.getName());

		assertThrows(DomainException.class, () -> EXT.getDataStoreConnection(dataStore, false));
	}

	@Test
	void getDataStoreConnectionSupportsJdbcWithoutCredentials() throws Exception {
		DataStore dataStore = new DataStore("org.h2.Driver",
												"jdbc:h2:mem:ext_no_credentials;DB_CLOSE_DELAY=-1",
												H2SpatialDialect.class.getName());

		try (Connection connection = EXT.getDataStoreConnection(dataStore, false)) {
			assertNotNull(connection);
		}
	}

	@Test
	void getDataStoreConnectionWrapsMissingJndiDataSource() {
		DataStore dataStore = new DataStore("java:comp/env/jdbc/doesNotExist",
												H2SpatialDialect.class.getName());

		assertThrows(DomainException.class, () -> EXT.getDataStoreConnection(dataStore, false));
	}

	@Test
	void newContentManagerUsesConfiguredImplementationClass() throws Exception {
		Class<? extends AbstractContentManager> original = AbstractContentManager.IMPLEMENTATION_CLASS;
		AbstractContentManager.IMPLEMENTATION_CLASS = NoOpContentManager.class;
		try (ContentManager manager = EXT.newContentManager()) {
			assertSame(NoOpContentManager.class, manager.getClass());
		}
		finally {
			AbstractContentManager.IMPLEMENTATION_CLASS = original;
		}
	}

	@Test
	void checkAccessThrowsForDocumentAggregateAccess() {
		assertAccessDenied(UserAccess.documentAggregate("admin", "User"));
	}

	@Test
	void checkAccessThrowsForQueryAggregateAccess() {
		assertAccessDenied(UserAccess.queryAggregate("admin", "UsersByStatus"));
	}

	@Test
	void checkAccessThrowsForPreviousCompleteAccess() {
		assertAccessDenied(UserAccess.previousComplete("admin", "User", "contact"));
	}

	@Test
	void checkAccessThrowsIllegalStateForUnknownAccessType() throws Exception {
		Constructor<UserAccess> constructor = UserAccess.class.getDeclaredConstructor(char.class, String.class, String.class, String.class);
		constructor.setAccessible(true);
		UserAccess access = constructor.newInstance('X', "admin", "User", "unknown");
		User user = mock(User.class);
		when(user.canAccess(access, "desktop")).thenReturn(false);
		when(user.getName()).thenReturn("tester");

		assertThrows(IllegalStateException.class, () -> EXT.checkAccess(user, access, "desktop"));
	}

	private static void assertAccessDenied(UserAccess access) {
		User user = mock(User.class);
		when(user.canAccess(access, "desktop")).thenReturn(false);
		when(user.getName()).thenReturn("tester");

		assertThrows(IllegalArgumentException.class, () -> EXT.checkAccess(user, access, "desktop"));
	}

	private static ChartData categoryData() {
		ChartData data = new ChartData();
		data.setTitle("Facade Chart");
		data.setLabel("Series");
		data.setLabels(List.of("Alpha", "Beta"));
		data.setValues(List.of(Integer.valueOf(1), Integer.valueOf(2)));
		data.setBackground(Color.BLUE);
		data.setBorder(Color.BLACK);
		data.setJFreeChartPostProcessorClassName(NoOpPostProcessor.class.getName());
		return data;
	}

	private static final class TrackingReceiver implements PushMessage.PushMessageReceiver {
		private final boolean stale;
		private int messagesSent;

		private TrackingReceiver(boolean stale) {
			this.stale = stale;
		}

		@Override
		public String forUserId() {
			return "tester";
		}

		@Override
		public void sendMessage(PushMessage message) {
			messagesSent++;
		}

		@Override
		public boolean isStale() {
			return stale;
		}
	}
}
