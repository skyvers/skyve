package modules.admin.ImportExport;

import java.io.File;

import org.skyve.CORE;
import org.skyve.util.Util;

import modules.admin.domain.ImportExport;
import modules.admin.domain.ImportExportColumn;

public class ImportExportExtension extends ImportExport {

	/**
	 * 
	 */
	private static final long serialVersionUID = -4914725314222700513L;

	public String baseFolder() {
		return String.format("%simportExport_%s%s%s",
				Util.getContentDirectory(),
				CORE.getUser().getCustomerName(),
				File.separator,
				getBizId());
	}

	public boolean anyColumnHasExpression() {
		for (ImportExportColumn c : this.getImportExportColumns()) {
			if (c.isShowExpression()) {
				return true;
			}
		}
		return false;
	}

	public void cleanupImportFile() {

		// remove any previous file
		if (getImportFileAbsolutePath() != null) {
			File previous = new File(getImportFileAbsolutePath());
			if (previous.exists()) {
				previous.delete();
				File folder = new File(baseFolder());
				folder.delete();

				setImportFileAbsolutePath(null);
			}
		}
	}
}
