package org.skyve.wildcat.util;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Basic file utilities
 * 
 * @author RB
 * 
 */
public class FileUtil {

	public FileUtil() {
		// no op
	}

	/**
	 * Return a byte array from an input stream
	 * 
	 * WARNING: If the input stream is large, this may have impacts on
	 * performance.
	 * 
	 * @param inputStream
	 * @return
	 * @throws IOException
	 */
	public static final byte[] getFileBytes(InputStream inputStream) throws IOException {
		byte[] bytes = null;

		InputStream stream = inputStream;
		try {
			try (BufferedInputStream bis = new BufferedInputStream(stream)) {
				try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
					byte[] temp = new byte[1024]; // 1K
					int bytesRead = 0;
					while ((bytesRead = bis.read(temp)) > 0) {
						baos.write(temp, 0, bytesRead);
					}
					bytes = baos.toByteArray();
				}
			}
		} finally {
			stream.close();
			stream = null;
		}

		return bytes;
	}

	/**
	 * Returns a safe filename from the unsafeName
	 * 
	 * @param unsafeName
	 * @return
	 */
	public static String safeFileName(String unsafeName) {

		return unsafeName.replace(',', '_').replace('&', '_').replace('/', '_').replace('\\', '_').replace(" ", "");
	}

	/**
	 * Construct a safe file path and name
	 * 
	 * @param safeBasePath
	 * @param unsafeFileName
	 * @param extension
	 * @param makeDirectories
	 * @param unsafeFolderNames
	 * @return the full filepath (and filename with extension if provided)
	 */
	public static String constructFilePath(String safeBasePath, String unsafeFileName, String extension, boolean makeDirectories, String... unsafeFolderNames) {
		
		StringBuilder filePath = new StringBuilder();
		filePath.append(safeBasePath);
		
		//construct subfolders as required
		String separator = System.getProperty("file.separator");
		if (!safeBasePath.endsWith(separator)) {
			filePath.append(separator);
		}
		for (String folder : unsafeFolderNames) {
			filePath.append(FileUtil.safeFileName(folder));
		}
		filePath.append(separator);

		//make directories if requested
		if (makeDirectories) {
			new File(filePath.toString()).mkdirs();
		}

		//append the file name with extension
		filePath.append(safeFileName(unsafeFileName));
		if (extension != null) {
			if (!extension.startsWith(".")) {
				filePath.append('.');
			}
			filePath.append(extension);
		}

		return filePath.toString();
	}

}
