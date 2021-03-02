package modules.admin.ImportExport;

import org.skyve.util.test.SkyveFactory;

import modules.admin.ImportExport.actions.ResetColumns;
import modules.admin.ImportExport.actions.RunImport;

@SkyveFactory(excludedActions = { RunImport.class, ResetColumns.class })
public class ImportExportFactory {
	// builder defaults
}
