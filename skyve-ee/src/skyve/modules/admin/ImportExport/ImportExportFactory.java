package modules.admin.ImportExport;

import org.skyve.util.test.SkyveFactory;

import modules.admin.ImportExport.actions.RunImport;

@SkyveFactory(excludedActions = { RunImport.class })
public class ImportExportFactory {
	// builder defaults
}
