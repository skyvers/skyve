package modules.admin.ImportExport.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ImportExport.ImportExportExtension;
import modules.admin.ImportExport.ImportExportUtil;
import modules.admin.domain.ImportExportColumn;
import modules.admin.domain.ImportExportColumn.LoadAction;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class RunImportTest extends AbstractH2Test {
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
		RunImport action = new RunImport();

		ValidationException exception = assertThrows(ValidationException.class, () -> action.execute(bean, null));

		assertTrue(exception.getMessages()
				.stream()
				.anyMatch(message -> message.getText().contains("doesn't match the title of the column in the file")));
	}

	@Test
	void executeWithExpressionColumnWithoutBindingExpressionThrowsValidationException() throws Exception {
		ImportExportExtension bean = importBeanWithColumn("Expression Header", ImportExportUtil.EXPRESSION);
		bean.setImportFileAbsolutePath(writeWorkbook("Expression Header").toString());
		RunImport action = new RunImport();

		ValidationException exception = assertThrows(ValidationException.class, () -> action.execute(bean, null));

		assertTrue(exception.getMessages()
				.stream()
				.anyMatch(message -> message.getText().contains("but have not provided a binding expression")));
	}

	@Test
	void executeWithHeaderOnlyFileMapsLoadActionsAndReportsUnsuccessfulImport() throws Exception {
		ImportExportExtension bean = new ImportExportExtension();
		bean.setModuleName("admin");
		bean.setDocumentName("User");
		bean.setFileContainsHeaders(Boolean.TRUE);
		bean.setDetailedLogging(Boolean.TRUE);
		bean.setLoadType(ImportExportUtil.CREATE_EVERYTHING_EVEN_IF_THERE_MIGHT_BE_DUPLICATES);
		bean.addImportExportColumnsElement(column("User Name", "userName", null));
		bean.addImportExportColumnsElement(column("Contact Email", "contact.email1", LoadAction.confirmValue));
		bean.addImportExportColumnsElement(column("Lookup Contains", "userName", LoadAction.lookupContains));
		bean.addImportExportColumnsElement(column("Lookup Equals", "userName", LoadAction.lookupEquals));
		bean.addImportExportColumnsElement(column("Lookup Like", "userName", LoadAction.lookupLike));
		bean.addImportExportColumnsElement(column("Set Value", "userName", LoadAction.setValue));
		ImportExportColumn expression = column("Expression", ImportExportUtil.EXPRESSION, null);
		expression.setBindingExpression("{contact.name}");
		bean.addImportExportColumnsElement(expression);
		bean.setImportFileAbsolutePath(writeWorkbook("User Name",
				"Contact Email",
				"Lookup Contains",
				"Lookup Equals",
				"Lookup Like",
				"Set Value",
				"Expression").toString());
		WebContext webContext = mock(WebContext.class);

		ServerSideActionResult<modules.admin.domain.ImportExport> result = new RunImport().execute(bean, webContext);

		assertSame(bean, result.getBean());
		assertTrue(bean.getResults().contains("Import unsuccessful"));
		verify(webContext).growl(MessageSeverity.info, "Import unsuccessful. Try again.");
	}

	private ImportExportExtension importBeanWithColumn(String columnName, String bindingName) {
		ImportExportExtension bean = new ImportExportExtension();
		bean.setModuleName("admin");
		bean.setDocumentName("User");
		bean.setFileContainsHeaders(Boolean.TRUE);

		ImportExportColumn column = column(columnName, bindingName, null);
		bean.addImportExportColumnsElement(column);

		return bean;
	}

	private static ImportExportColumn column(String columnName, String bindingName, LoadAction loadAction) {
		ImportExportColumn column = ImportExportColumn.newInstance();
		column.setColumnName(columnName);
		column.setBindingName(bindingName);
		column.setLoadAction(loadAction);
		return column;
	}

	private Path writeWorkbook(String... headers) throws Exception {
		Path path = tempDir.resolve(headers[0].replace(' ', '_') + ".xlsx");
		try (XSSFWorkbook workbook = new XSSFWorkbook();
				OutputStream out = Files.newOutputStream(path)) {
			var row = workbook.createSheet("Data").createRow(0);
			for (int i = 0; i < headers.length; i++) {
				row.createCell(i).setCellValue(headers[i]);
			}
			workbook.write(out);
		}
		return path;
	}
}
