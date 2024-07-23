package modules.admin.Configuration;

import java.io.IOException;
import java.nio.file.FileStore;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.NumberFormat;

import org.skyve.util.Util;

import modules.admin.ModulesUtil;

/**
 * Determine the total file system sizings.
 */
public final class DiskSpaceSummary {
	private long totalAvailable;
	private long totalSpace;
	private long totalAvailableLevel;
	private String htmlSummary;

	/**
	 * Determines disk usage.
	 */
	public DiskSpaceSummary() {
		@SuppressWarnings("resource")
		// doco says never close default file system
		FileSystem fs = FileSystems.getDefault();
		for (Path root : fs.getRootDirectories()) {
			try {
				FileStore store = Files.getFileStore(root);
				long usableSpace = store.getUsableSpace() / ModulesUtil.MEGABYTE;
				long space = store.getTotalSpace() / ModulesUtil.MEGABYTE;

				if (space > 0) {
					totalSpace += space;
					totalAvailable += usableSpace;
				}

			}
			catch (IOException e) {
				Util.LOGGER.severe("Error querying available disk space:");
				e.printStackTrace();
			}
		}
		this.totalAvailableLevel = (100 * totalAvailable / totalSpace);
	}

	/**
	 * Total available space in MB.
	 */
	public long getTotalAvailable() {
		return totalAvailable;
	}

	/**
	 * Total space in MB.
	 */
	public long getTotalSpace() {
		return totalSpace;
	}

	/**
	 * Total disk free as a percentage.
	 */
	public long getTotalAvailableLevel() {
		return totalAvailableLevel;
	}

	/**
	 * Returns a HTML snippit (2 paragraphs describing disk usage)
	 * @return	The HTML.
	 */
	public String getHTMLSummary() {
		if (htmlSummary == null) {
			StringBuilder result = new StringBuilder(128);
			
			result.append("<p><strong>Summary</strong></p>");
			NumberFormat nf = NumberFormat.getNumberInstance();
			result.append("<p>Total=").append(nf.format(totalSpace));
			result.append("MB, Available=").append(nf.format(totalAvailable));
			result.append("MB, Available Percentage=").append(totalAvailableLevel).append("%</p>");
			
			htmlSummary = result.toString();
		}
		return htmlSummary;
	}
}
