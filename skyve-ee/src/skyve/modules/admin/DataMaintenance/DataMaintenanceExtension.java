package modules.admin.DataMaintenance;

import org.skyve.impl.backup.RestoreOptions;

import modules.admin.domain.DataMaintenance;

/**
 * Implement the RestoreOptions interface.
 * @author mike
 */
public class DataMaintenanceExtension extends DataMaintenance implements RestoreOptions {
	private static final long serialVersionUID = -838440738587384988L;

	@Override
	public PreProcess getPreProcess() {
		return PreProcess.valueOf(getRestorePreProcess().toCode());
	}

	@Override
	public ContentOption getContentOption() {
		return ContentOption.valueOf(getContentRestoreOption().toCode());
	}

	@Override
	public IndexingOption getIndexingOption() {
		return IndexingOption.valueOf(getRestoreIndexingOption().toCode());
	}
}
