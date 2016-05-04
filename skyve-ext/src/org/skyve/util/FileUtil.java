package org.skyve.util;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.skyve.content.MimeType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.DownloadAction.Download;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

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
	public static String constructSafeFilePath(String safeBasePath, String unsafeFileName, String extension, boolean makeDirectories, String... unsafeFolderNames) {
		
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

	/**
	 * Prepare a zip download for the directory
	 * 
	 * @param directoryPath
	 * @param zipName
	 * @param webContext
	 * @return
	 * @throws Exception
	 */
	public static Download prepareZipDownload(String directoryPath, String zipName, WebContext webContext)
	throws Exception {
		try (ByteArrayOutputStream out = new ByteArrayOutputStream(20480)) {
			File backupDir = new File(directoryPath);
			if (backupDir.exists() && backupDir.isDirectory()) {

				try (ZipOutputStream zip = new ZipOutputStream(out)) {
					List<File> fileList = new ArrayList<>();
		
					getAllFiles(backupDir, fileList);
		
					for (File file : fileList) {
						if (! file.isDirectory()) { // we only zip files, not directories
							addToZip(backupDir, file, zip);
						}
					}
					
					zip.flush();
				}
			}
			
			out.flush();
			return new Download(zipName, new ByteArrayInputStream(out.toByteArray()), MimeType.zip);
		}
	}
	
	/**
	 * Get all files from a directory
	 * 
	 * @param dir
	 * @param fileList
	 */
	private static void getAllFiles(File dir, List<File> fileList) {
		File[] files = dir.listFiles();
		for (File file : files) {
			fileList.add(file);
			if (file.isDirectory()) {
				getAllFiles(file, fileList);
			}
		}
	}

	/**
	 * Add files within a directory to a zip file
	 * @param directoryToZip
	 * @param file
	 * @param zos
	 * @throws FileNotFoundException
	 * @throws IOException
	 */
	public static void addToZip(File directoryToZip, File file, ZipOutputStream zos)
	throws FileNotFoundException, IOException {
		try (FileInputStream fis = new FileInputStream(file)) {
			// we want the zipEntry's path to be a relative path that is relative
			// to the directory being zipped, so chop off the rest of the path
			String zipFilePath = file.getCanonicalPath().substring(directoryToZip.getCanonicalPath().length() + 1,
																	file.getCanonicalPath().length());
			Util.LOGGER.info(String.format("Writing '%s' to zip file", zipFilePath));
			ZipEntry zipEntry = new ZipEntry(zipFilePath);
			zos.putNextEntry(zipEntry);

			byte[] bytes = new byte[1024];
			int length = 0;
			while ((length = fis.read(bytes)) >= 0) {
				zos.write(bytes, 0, length);
			}

			zos.closeEntry();
		}
	}

	
	/**
	 * Delete file with exception if unsuccessful
	 * 
	 * @param f
	 * @throws ValidationException
	 */
	public static void delete(File f) throws ValidationException {
		if (f.isDirectory()) {
			for (File c : f.listFiles()) {
				delete(c);
			}
		}
		
		if (! f.delete()) {
			throw new ValidationException(new Message(f.getAbsolutePath() + " was not deleted"));
		}
	}

}
