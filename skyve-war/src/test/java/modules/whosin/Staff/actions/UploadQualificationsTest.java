package modules.whosin.Staff.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.bizport.AbstractDataFileLoader.LoaderActivityType;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.WebFileInputStream;

import modules.whosin.domain.Staff;
import modules.whosin.domain.StaffQualification;

@SuppressWarnings("static-method")
class UploadQualificationsTest {
	@Test
	void uploadXlsxAddsQualificationsToStaff() throws Exception {
		StaffQualification qualification = new StaffQualification();
		FakeQualificationLoader loader = new FakeQualificationLoader(List.of(qualification));
		TestableUploadQualifications action = new TestableUploadQualifications(loader);
		Staff staff = new Staff();

		Staff returned = action.upload(staff, upload("qualifications.xlsx"), new UploadException(), null);

		assertSame(staff, returned);
		assertEquals(LoaderActivityType.CREATE_ALL, loader.activityType);
		assertEquals(Integer.valueOf(0), loader.sheetIndex);
		assertEquals(Boolean.TRUE, Boolean.valueOf(loader.debugMode));
		assertEquals(1, loader.dataIndex);
		assertEquals(List.of(StaffQualification.namePropertyName,
				StaffQualification.typePropertyName,
				StaffQualification.issuingOrganisationPropertyName,
				StaffQualification.descriptionPropertyName,
				StaffQualification.dateAttainedPropertyName,
				StaffQualification.dateExpiryPropertyName), loader.bindings);
		assertEquals(1, staff.getQualifications().size());
		assertSame(qualification, staff.getQualifications().get(0));
		assertSame(staff, qualification.getParent());
	}

	@Test
	void uploadCsvAddsQualificationsToStaff() throws Exception {
		StaffQualification first = new StaffQualification();
		StaffQualification second = new StaffQualification();
		FakeQualificationLoader loader = new FakeQualificationLoader(List.of(first, second));
		TestableUploadQualifications action = new TestableUploadQualifications(loader);
		Staff staff = new Staff();

		action.upload(staff, upload("qualifications.csv"), new UploadException(), null);

		assertEquals(LoaderActivityType.CREATE_ALL, loader.activityType);
		assertEquals(Boolean.TRUE, Boolean.valueOf(loader.debugMode));
		assertEquals(1, loader.dataIndex);
		assertEquals(2, staff.getQualifications().size());
		assertSame(staff, first.getParent());
		assertSame(staff, second.getParent());
	}

	@Test
	void uploadUnsupportedFileTypeThrowsValidationException() {
		UploadQualifications action = new UploadQualifications();

		ValidationException exception = assertThrows(ValidationException.class,
				() -> action.upload(null, upload("qualifications.txt"), new UploadException(), null));

		assertEquals("Only csv or xlsx file types are supported", exception.getMessages().iterator().next().getText());
	}

	@SuppressWarnings("resource")
	private static Upload upload(String fileName) {
		return new Upload(fileName,
				new WebFileInputStream(new ByteArrayInputStream("content".getBytes(StandardCharsets.UTF_8))),
				MimeType.csv);
	}

	private static class TestableUploadQualifications extends UploadQualifications {
		private final FakeQualificationLoader loader;

		private TestableUploadQualifications(FakeQualificationLoader loader) {
			this.loader = loader;
		}

		@Override
		protected QualificationLoader newPOISheetLoader(LoaderActivityType activityType,
				InputStream is,
				int sheetIndex,
				UploadException exception,
				String moduleName,
				String documentName) {
			loader.activityType = activityType;
			loader.sheetIndex = Integer.valueOf(sheetIndex);
			return loader;
		}

		@Override
		protected QualificationLoader newCSVLoader(LoaderActivityType activityType,
				InputStream is,
				UploadException exception,
				String moduleName,
				String documentName,
				String... bindings) {
			loader.activityType = activityType;
			loader.bindings = List.of(bindings);
			return loader;
		}
	}

	private static class FakeQualificationLoader implements UploadQualifications.QualificationLoader {
		private final List<StaffQualification> results;
		private LoaderActivityType activityType;
		private Integer sheetIndex;
		private List<String> bindings = List.of();
		private boolean debugMode;
		private int dataIndex;

		private FakeQualificationLoader(List<StaffQualification> results) {
			this.results = results;
		}

		@Override
		public void addFields(String... b) {
			this.bindings = Arrays.asList(b);
		}

		@Override
		public void setDebugMode(boolean debugMode) {
			this.debugMode = debugMode;
		}

		@Override
		public void setDataIndex(int dataIndex) {
			this.dataIndex = dataIndex;
		}

		@Override
		public List<StaffQualification> beanResults() {
			return results;
		}
	}
}
