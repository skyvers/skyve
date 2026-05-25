package modules.admin.ImportExport.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.ImportExport.ImportExportExtension;
import util.AbstractSkyveTest;

@SuppressWarnings("static-method")
public class RunImportTest extends AbstractSkyveTest {

	@Test
	void executeWithNullImportFileReturnsBean() throws Exception {
		RunImport action = new RunImport();
		ImportExportExtension bean = new ImportExportExtension();
		// importFileAbsolutePath is null by default - execute skips import and returns immediately
		ServerSideActionResult<modules.admin.domain.ImportExport> result = action.execute(bean, null);
		assertNotNull(result);
		assertSame(bean, result.getBean());
	}
}
