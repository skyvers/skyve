package modules.admin.HeapDumpList.util;

import java.io.File;
import java.lang.management.ManagementFactory;

import org.skyve.CORE;
import org.skyve.util.Util;

import com.sun.management.HotSpotDiagnosticMXBean;

/**
 * Utility for generating JVM heap dump files using the HotSpotDiagnosticMXBean.
 * 
 * Provides methods to create heap dumps in a timestamped file within a
 * customer-specific content directory.
 */
public class HeapDumpUtil {

	/**
	 * Generates a live heap dump file and returns its file path.
	 * <p>
	 * <b>Note:</b> This method requires the <code>jdk.management</code> module to be
	 * declared in <code>jboss-deployment-structure.xml</code>.
	 * 
	 * @return the absolute path of the generated heap dump file
	 * @throws Exception if the heap dump operation fails
	 */
	public static String dumpHeap() throws Exception {
		String filePath = buildFilePath();

		HotSpotDiagnosticMXBean mxBean = ManagementFactory.getPlatformMXBean(HotSpotDiagnosticMXBean.class);
		mxBean.dumpHeap(filePath, true);

		return filePath;
	}
	
	/**
     * Constructs the full file path for the heap dump file based on the current timestamp.
     * 
     * @return the heap dump file path as a string
     */
	public static String buildFilePath() {
		String dirPath = getDirectory();

		String dumpDir = String.format("%s/%s.hprof",
				dirPath,
				CORE.getDateFormat("yyyyMMddHHmmss").format(new java.util.Date())); 

		return dumpDir;
	}
	
	/**
     * Returns the directory path where heap dumps are stored, creating it if necessary.
     * 
     * The directory is under the content directory, named with a prefix and the customer name.
     * 
     * @return the heap dump directory path as a string
     * @throws RuntimeException if the directory cannot be created
     */
	public static String getDirectory() {
		String dirPath = String.format("%sheap_%s",
				Util.getContentDirectory(),
				CORE.getCustomer().getName());

		// Create directory if it does not exist
		File dir = new File(dirPath);
		if (!dir.exists()) {
			boolean created = dir.mkdirs();
			if (!created) {
				throw new RuntimeException("Failed to create directory: " + dirPath);
			}
		}
		
		return dirPath;
	}
}