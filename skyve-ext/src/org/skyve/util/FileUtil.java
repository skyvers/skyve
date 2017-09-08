package org.skyve.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.comparator.NameFileComparator;
import org.apache.commons.io.filefilter.RegexFileFilter;
import org.skyve.content.MimeType;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.controller.DownloadAction.Download;
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

		return unsafeName.replace(',', '_').replace('&', '_').replace('/', '_').replace('\\', '_').replace(" ", "").replace(":", "_").replace("-","_");
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
		if(unsafeFolderNames!=null){
			for (String folder : unsafeFolderNames) {
				filePath.append(FileUtil.safeFileName(folder));
			}
		}
		if (!filePath.toString().endsWith(separator)) {
			filePath.append(separator);
		}

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
	 * @throws IOException
	 */
	public static Download prepareZipDownload(String directoryPath, String zipName, WebContext webContext)
	throws IOException {
		try (ByteArrayOutputStream out = new ByteArrayOutputStream(20480)) {
			File backupDir = new File(directoryPath);
			createZipArchive(backupDir, out);
			return new Download(zipName, new ByteArrayInputStream(out.toByteArray()), MimeType.zip);
		}
	}
	
	/**
	 * Create a zip archive for the directory
	 * 
	 * @param directory
	 * @param zip	The archive file.
	 * @throws IOException
	 */
	public static void createZipArchive(File directory, File zip)
	throws IOException {
		try (FileOutputStream fos = new FileOutputStream(zip)) {
			createZipArchive(directory, fos);
		}
	}
	
	/**
	 * Create a zip archive for the directory.
	 * 
	 * @param directory
	 * @param out
	 * @throws IOException
	 */
	public static void createZipArchive(File directory, OutputStream out) throws IOException {
		if (directory.exists() && directory.isDirectory()) {
			try (ZipOutputStream zos = new ZipOutputStream(out)) {
				List<File> fileList = new ArrayList<>();
	
				getAllFiles(directory, fileList);
	
				for (File file : fileList) {
					if (! file.isDirectory()) { // we only zip files, not directories
						addToZip(directory, file, zos);
					}
				}
				
				zos.flush();
			}
		}
		
		out.flush();
	}
	
	/**
	 * Get all files from a directory recursively.
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
	 * Add a file within a directory to a zip file.
	 * 
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
			zipFilePath = zipFilePath.replace('\\', '/');
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

	private static void extractFile(ZipInputStream in, File outdir, String name)
	throws IOException {
		File file = new File(outdir, name);
		Util.LOGGER.info(String.format("Writing '%s' from zip file to %s", name, file));
		try (FileOutputStream fos = new FileOutputStream(file)) {
			try (BufferedOutputStream out = new BufferedOutputStream(fos)) {
				byte[] bytes = new byte[1024];
			    int length = 0;
			    while ((length = in.read(bytes)) >= 0) {
		    		out.write(bytes, 0, length);
			    }
			}
		}
	}

	private static void mkdirs(File outdir, String path) {
		File d = new File(outdir, path);
		if (! d.exists()) {
			d.mkdirs();
		}
	}

	private static String dirpart(String name) {
		int s = Math.max(name.lastIndexOf('/'), name.lastIndexOf('\\'));
		return (s == -1) ? null : name.substring( 0, s );
	  }

	/**
	 * Extract zipfile to outdir with complete directory structure
	 * 
	 * @param zipfile Input .zip file
	 * @param outdir Output directory
	 */
	public static void extractZipArchive(File zipfile, File outdir)
	throws IOException {
		outdir.mkdirs();
		try (FileInputStream fis = new FileInputStream(zipfile)) {
			try (ZipInputStream zin = new ZipInputStream(fis)) {
				ZipEntry entry = null;
				while ((entry = zin.getNextEntry()) != null) {
					String name = entry.getName();
					if (entry.isDirectory()) {
						mkdirs(outdir, name);
						Util.LOGGER.info("create dir " + name);
						continue;
					}
					/* this part is necessary because file entry can come before
					 * directory entry where is file located
					 * i.e.:
					 *   /foo/foo.txt
					 *   /foo/
					 */
					String dir = dirpart(name);
					if (dir != null) {
						mkdirs(outdir, dir);
						Util.LOGGER.info("create dir " + name);
					}
					extractFile(zin, outdir, name);
				}
			}
		}
	}

	/**
	 * Delete file with exception if unsuccessful
	 * 
	 * @param f	The file to delete
	 * @throws IOException
	 */
	public static void delete(File f) throws IOException {
		if (f.isDirectory()) {
			for (File c : f.listFiles()) {
				delete(c);
			}
		}
		
		if (! f.delete()) {
			throw new IOException(f.getAbsolutePath() + " was not deleted");
		}
	}

	/**
	 * Copy the src file to the dest file, overwriting if necessary.
	 * @param src
	 * @param dest
	 * @throws IOException
	 */
	public static void copy(File src, File dest) throws IOException {
		FileUtils.copyFile(src, dest);
	}
	
	/**
	 * List files (non-recursive) for a directory.
	 * @param dir	The directory to list for.
	 * @param regexPattern	File pattern to match, or null for all files
	 * @param direction	If defined, perform a case insensitive sort by the file name (not the whole path).
	 * @return	The files.
	 */
	public static File[] listFiles(File dir, String regexPattern, SortDirection direction) {
		File[] files = (regexPattern == null) ? 
							dir.listFiles() :
							dir.listFiles((FilenameFilter) new RegexFileFilter(regexPattern));
		if (files != null) {
			if (SortDirection.ascending.equals(direction)) {
				Arrays.sort(files, NameFileComparator.NAME_INSENSITIVE_COMPARATOR);
			}
			else if (SortDirection.descending.equals(direction)) {
				Arrays.sort(files, NameFileComparator.NAME_INSENSITIVE_REVERSE);
			}
		}
		
		return files;
	}
}
