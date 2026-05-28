package modules.admin.ImportExport;

import java.io.File;

import org.skyve.CORE;
import org.skyve.util.Util;

import modules.admin.domain.ImportExport;
import modules.admin.domain.ImportExportColumn;

/**
 * Extends {@link ImportExport} with helper operations for file lifecycle and column inspection.
 */
public class ImportExportExtension extends ImportExport {
	private static final long serialVersionUID = -4914725314222700513L;

	/**
	 * Returns the content-folder path used for uploaded import files.
	 *
	 * @return the customer- and record-scoped base folder
	 */
	public String baseFolder() {
		return String.format("%simportExport_%s%s%s",
				Util.getContentDirectory(),
				CORE.getUser().getCustomerName(),
				File.separator,
				getBizId());
	}

	/**
	 * Determines whether any configured column uses expression-based binding.
	 *
	 * @return {@code true} if at least one column is configured for expressions
	 */
	public boolean anyColumnHasExpression() {
		for (ImportExportColumn c : this.getImportExportColumns()) {
			if (c.isShowExpression()) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Deletes any uploaded import file and resets file path state.
	 */
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
