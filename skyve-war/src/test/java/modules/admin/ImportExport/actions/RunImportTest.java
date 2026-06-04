package modules.admin.ImportExport.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.ImportExport.ImportExportExtension;
import modules.admin.ImportExport.ImportExportUtil;
import modules.admin.domain.ImportExportColumn;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class RunImportTest extends AbstractH2Test {
	@TempDir
	private Path tempDir;

	@Test
	void executeWithNullImportFileReturnsBean() throws Exception {
		RunImport action = new RunImport();
		ImportExportExtension bean = new ImportExportExtension();
		// importFileAbsolutePath is null by default - execute skips import and returns immediately
		ServerSideActionResult<modules.admin.domain.ImportExport> result = action.execute(bean, null);
		assertNotNull(result);
		assertSame(bean, result.getBean());
	}

	@Test
	void executeWithHeaderMismatchThrowsValidationException() throws Exception {
		ImportExportExtension bean = importBeanWithColumn("Expected Header", "userName");
		bean.setImportFileAbsolutePath(writeWorkbook("Actual Header").toString());

		ValidationException exception = assertThrows(ValidationException.class, () -> new RunImport().execute(bean, null));

		assertTrue(exception.getMessages()
				.stream()
				.anyMatch(message -> message.getText().contains("doesn't match the title of the column in the file")));
	}

	@Test
	void executeWithExpressionColumnWithoutBindingExpressionThrowsValidationException() throws Exception {
		ImportExportExtension bean = importBeanWithColumn("Expression Header", ImportExportUtil.EXPRESSION);
		bean.setImportFileAbsolutePath(writeWorkbook("Expression Header").toString());

		ValidationException exception = assertThrows(ValidationException.class, () -> new RunImport().execute(bean, null));

		assertTrue(exception.getMessages()
				.stream()
				.anyMatch(message -> message.getText().contains("but have not provided a binding expression")));
	}

	private ImportExportExtension importBeanWithColumn(String columnName, String bindingName) throws Exception {
		ImportExportExtension bean = new ImportExportExtension();
		bean.setModuleName("admin");
		bean.setDocumentName("User");
		bean.setFileContainsHeaders(Boolean.TRUE);

		ImportExportColumn column = ImportExportColumn.newInstance();
		column.setColumnName(columnName);
		column.setBindingName(bindingName);
		bean.addImportExportColumnsElement(column);

		return bean;
	}

	private Path writeWorkbook(String header) throws Exception {
		Path path = tempDir.resolve(header.replace(' ', '_') + ".xlsx");
		try (XSSFWorkbook workbook = new XSSFWorkbook();
				OutputStream out = Files.newOutputStream(path)) {
			workbook.createSheet("Data").createRow(0).createCell(0).setCellValue(header);
			workbook.write(out);
		}
		return path;
	}
}
