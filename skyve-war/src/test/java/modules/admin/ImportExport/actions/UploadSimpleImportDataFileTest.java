package modules.admin.ImportExport.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.domain.messages.UploadException;

import modules.admin.ImportExport.ImportExportExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.ImportExportColumn;
import util.AbstractH2Test;

/**
 * Tests spreadsheet column inference for simple import uploads.
 */
@SuppressWarnings("static-method")
class UploadSimpleImportDataFileTest extends AbstractH2Test {
	@TempDir
	private Path tempDir;

	@Test
	void loadColumnsFromFileWithHeadersUsesDisplayNameAndBindingNameMatches() throws Exception {
		ImportExportExtension bean = importBean(Boolean.TRUE, "Name", "Email1", "Unknown Header");

		UploadSimpleImportDataFile.loadColumnsFromFile(bean, new UploadException());

		assertEquals(3, bean.getImportExportColumns().size());
		ImportExportColumn name = bean.getImportExportColumns().get(0);
		assertEquals(Integer.valueOf(0), name.getBizOrdinal());
		assertEquals("Name", name.getColumnName());
		assertEquals(Contact.namePropertyName, name.getBindingName());

		ImportExportColumn email = bean.getImportExportColumns().get(1);
		assertEquals(Integer.valueOf(1), email.getBizOrdinal());
		assertEquals("Email1", email.getColumnName());
		assertEquals(Contact.email1PropertyName, email.getBindingName());

		ImportExportColumn unknown = bean.getImportExportColumns().get(2);
		assertEquals(Integer.valueOf(2), unknown.getBizOrdinal());
		assertEquals("Unknown Header", unknown.getColumnName());
		assertNull(unknown.getBindingName());
	}

	@Test
	void loadColumnsFromFileWithoutHeadersUsesWorksheetColumnNames() throws Exception {
		ImportExportExtension bean = importBean(Boolean.FALSE, "ignored one", "ignored two");

		UploadSimpleImportDataFile.loadColumnsFromFile(bean, new UploadException());

		assertEquals(2, bean.getImportExportColumns().size());
		assertEquals("A", bean.getImportExportColumns().get(0).getColumnName());
		assertEquals("B", bean.getImportExportColumns().get(1).getColumnName());
		assertNull(bean.getImportExportColumns().get(0).getBindingName());
		assertNull(bean.getImportExportColumns().get(1).getBindingName());
	}

	@Test
	void loadColumnsFromFileStopsAtFirstBlankHeaderCell() throws Exception {
		ImportExportExtension bean = importBean(Boolean.TRUE, "Name", "", "Email1");

		UploadSimpleImportDataFile.loadColumnsFromFile(bean, new UploadException());

		assertEquals(1, bean.getImportExportColumns().size());
		assertEquals("Name", bean.getImportExportColumns().get(0).getColumnName());
	}

	private ImportExportExtension importBean(Boolean fileContainsHeaders, String... firstRowValues) throws Exception {
		ImportExportExtension bean = new ImportExportExtension();
		bean.setModuleName(Contact.MODULE_NAME);
		bean.setDocumentName(Contact.DOCUMENT_NAME);
		bean.setFileContainsHeaders(fileContainsHeaders);
		bean.setImportFileAbsolutePath(writeWorkbook(firstRowValues).toString());
		return bean;
	}

	private Path writeWorkbook(String... firstRowValues) throws Exception {
		Path path = tempDir.resolve("import.xlsx");
		try (XSSFWorkbook workbook = new XSSFWorkbook();
				OutputStream out = Files.newOutputStream(path)) {
			XSSFRow row = workbook.createSheet("Data").createRow(0);
			for (int i = 0; i < firstRowValues.length; i++) {
				row.createCell(i).setCellValue(firstRowValues[i]);
			}
			workbook.write(out);
		}
		return path;
	}
}
