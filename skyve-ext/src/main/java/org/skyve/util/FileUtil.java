package org.skyve.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.comparator.NameFileComparator;
import org.apache.commons.io.filefilter.RegexFileFilter;
import org.skyve.content.MimeType;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.controller.Download;
import org.skyve.util.logging.Category;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Basic file utilities
 * 
 * @author RB
 * 
 */
public class FileUtil {

    private static final Logger COMMAND_LOGGER = Category.COMMAND.logger();

	public FileUtil() {
		// no op
	}

	/**
	 * Read the bytes from a file.
	 * 
	 * @param file	The file to read.
	 * @return	the bytes.
	 * @throws IOException
	 */
	public static @Nonnull byte[] bytes(@Nonnull File file) throws IOException {
		try (FileInputStream fis = new FileInputStream(file)) {
			return bytes(fis);
		}
	}
	
	/**
	 * Return a byte array from an input stream
	 * 
	 * WARNING: If the input stream is large, this may have impacts on performance.
	 * 
	 * @param inputStream
	 * @return
	 * @throws IOException
	 */
	public static @Nonnull byte[] bytes(@Nonnull InputStream inputStream) throws IOException {
		try (BufferedInputStream bis = new BufferedInputStream(inputStream)) {
			try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
				byte[] bytes = new byte[1024]; // 1K
				int bytesRead = 0;
				while ((bytesRead = bis.read(bytes)) > 0) {
					baos.write(bytes, 0, bytesRead);
				}
				return baos.toByteArray();
			}
		}
	}
	
	/**
	 * Return the contents of a text file as a String.
	 */
	public static final @Nonnull String string(@Nonnull File file) throws IOException {
		StringBuilder result = new StringBuilder(1024);
		
		try (FileReader fr = new FileReader(file)) {
			try (BufferedReader br = new BufferedReader(fr)) {
				String line = null;
				while ((line = br.readLine()) != null) {
					result.append(line).append('\n');
				}
			}
		}
		
		return result.toString();
	}

	/**
	 * Returns a safe filename from the unsafeName
	 * 
	 * @param unsafeName
	 * @return
	 */
	public static @Nonnull String safeFileName(@Nonnull String unsafeName) {
		// suitable for FAT32 - thanks to SharkAlley@Stack Overflow
		final Pattern PATTERN = Pattern.compile("[%\\.\"\\*/:<>\\?\\\\\\|\\+,\\.;=\\[\\]]");

		final int MAX_LENGTH = 127;

	    StringBuffer sb = new StringBuffer();

	    // Apply the regex
	    Matcher m = PATTERN.matcher(unsafeName);

	    while (m.find()) {
	        // Convert matched character to percent-encoded.
	        String replacement = "%"+Integer.toHexString(m.group().charAt(0)).toUpperCase();

	        m.appendReplacement(sb,replacement);
	    }
	    m.appendTail(sb);

	    String encoded = sb.toString();

	    // Truncate the string.
	    int end = Math.min(encoded.length(),MAX_LENGTH);
	    return encoded.substring(0,end);
	}

	/**
	 * Construct a safe file path and name
	 * 
	 * @param safeBasePath
	 * @param unsafeFileName
	 * @param extension
	 * @param makeDirectories
	 * @param unsafeFolderNames
	 * @return the full file path (and filename with extension if provided)
	 */
	public static @Nonnull String constructSafeFilePath(@Nonnull String safeBasePath,
															@Nonnull String unsafeFileName,
															@Nullable String extension,
															boolean makeDirectories,
															@Nonnull String... unsafeFolderNames) {
		StringBuilder result = new StringBuilder();
		result.append(safeBasePath);
		
		//construct subfolders as required
		String separator = System.getProperty("file.separator");
		if (! safeBasePath.endsWith(separator)) {
			result.append(separator);
		}
		for (String folder : unsafeFolderNames) {
			result.append(FileUtil.safeFileName(folder));
		}

		if (! result.toString().endsWith(separator)) {
			result.append(separator);
		}

		//make directories if requested
		if (makeDirectories) {
			new File(result.toString()).mkdirs();
		}

		//append the file name with extension
		result.append(safeFileName(unsafeFileName));
		if (extension != null) {
			if (! extension.startsWith(".")) {
				result.append('.');
			}
			result.append(extension);
		}

		return result.toString();
	}

	/**
	 * Prepare a zip download for the directory
	 * 
	 * @param directoryPath
	 * @param zipName
	 * @return
	 * @throws IOException
	 */
	public static @Nonnull Download prepareZipDownload(@Nonnull String directoryPath,
														@Nonnull String zipName)
	throws IOException {
		try (ByteArrayOutputStream out = new ByteArrayOutputStream(20480)) {
			File backupDir = new File(directoryPath);
			createZipArchive(backupDir, out);
			return new Download(zipName, out.toByteArray(), MimeType.zip);
		}
	}
	
	/**
	 * Create a zip archive for the directory
	 * 
	 * @param directory
	 * @param zip	The archive file.
	 * @throws IOException
	 */
	public static void createZipArchive(@Nonnull File directory,@Nonnull File zip)
	throws IOException {
		try (FileOutputStream fos = new FileOutputStream(zip)) {
			createZipArchive(directory, fos);
		}
	}
	
	/**
	 * Create a jar archive for the directory.
	 * 
	 * @param directory
	 * @param out
	 * @throws IOException
	 */
	public static void createJarArchive(@Nonnull File directory, @Nonnull OutputStream out)
	throws IOException {
		if (directory.exists() && directory.isDirectory()) {
			Manifest manifest = new Manifest();
			manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");
			try (JarOutputStream jos = new JarOutputStream(out, manifest)) {
				List<File> fileList = new ArrayList<>();
	
				getAllFiles(directory, fileList);
	
				for (File file : fileList) {
					if (! file.isDirectory()) { // we only zip files, not directories
						addToZip(directory, file, jos);
					}
				}
				
				jos.flush();
			}
		}
		
		out.flush();
	}

	/**
	 * Create a zip archive for the directory.
	 * 
	 * @param directory
	 * @param out
	 * @throws IOException
	 */
	public static void createZipArchive(@Nonnull File directory, @Nonnull OutputStream out)
	throws IOException {
		if (directory.exists() && directory.isDirectory()) {
			try (ZipOutputStream zos = new ZipOutputStream(out)) {
				Path root = directory.toPath();
				Files.walkFileTree(root, new SimpleFileVisitor<Path>() {
					@Override
					public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
						addPathToZip(root, file, zos);
						return FileVisitResult.CONTINUE;
					}
				});
				
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
	private static void getAllFiles(@Nonnull File dir, @Nonnull List<File> fileList) {
		File[] files = dir.listFiles();
		if (files != null) {
			for (File file : files) {
				fileList.add(file);
				if (file.isDirectory()) {
					getAllFiles(file, fileList);
				}
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
	public static void addToZip(@Nonnull File directoryToZip, @Nonnull File file, @Nonnull ZipOutputStream zos)
	throws FileNotFoundException, IOException {
		addPathToZip(directoryToZip.toPath(), file.toPath(), zos);
	}

	private static void addPathToZip(@Nonnull Path root, @Nonnull Path file, @Nonnull ZipOutputStream zos)
	throws IOException {
		try (InputStream fis = Files.newInputStream(file)) {
			// we want the zipEntry's path to be a relative path that is relative
			// to the directory being zipped, so chop off the rest of the path
			String zipFilePath = root.relativize(file).toString().replace('\\', '/');
			if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info(String.format("Writing '%s' to zip file", zipFilePath));
			ZipEntry zipEntry = new ZipEntry(zipFilePath);
			zos.putNextEntry(zipEntry);

			byte[] bytes = new byte[8192];
			int length = 0;
			while ((length = fis.read(bytes)) >= 0) {
				zos.write(bytes, 0, length);
			}

			zos.closeEntry();
		}
	}

	private static void extractFile(@Nonnull ZipInputStream in, @Nonnull File outdir, @Nonnull String name)
	throws IOException {
		File file = new File(outdir, name);
		if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info(String.format("Writing '%s' from zip file to %s", name, file));
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

	private static void mkdirs(@Nonnull File outdir, @Nonnull String path) {
		File d = new File(outdir, path);
		if (! d.exists()) {
			d.mkdirs();
		}
	}

	private static String dirpart(@Nonnull String name) {
		int s = Math.max(name.lastIndexOf('/'), name.lastIndexOf('\\'));
		return (s == -1) ? null : name.substring( 0, s );
	  }

	/**
	 * Extract zipfile to outdir with complete directory structure
	 * 
	 * @param zipfile Input .zip file
	 * @param outdir Output directory
	 */
	public static void extractZipArchive(@Nonnull File zipfile, @Nonnull File outdir)
	throws IOException {
		outdir.mkdirs();
		try (FileInputStream fis = new FileInputStream(zipfile)) {
			try (ZipInputStream zin = new ZipInputStream(fis)) {
				ZipEntry entry = null;
				while ((entry = zin.getNextEntry()) != null) {
					String name = entry.getName();
					if (entry.isDirectory()) {
						mkdirs(outdir, name);
						if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("create dir " + name);
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
						if (UtilImpl.COMMAND_TRACE) COMMAND_LOGGER.info("create dir " + name);
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
	public static void delete(@Nonnull File f) throws IOException {
		if (f.isDirectory()) {
			File[] cs = f.listFiles();
			if (cs != null) {
				for (File c : cs) {
					delete(c);
				}
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
	public static void copy(@Nonnull File src, @Nonnull File dest) throws IOException {
		FileUtils.copyFile(src, dest);
	}
	
	/**
	 * List files (non-recursive) for a directory.
	 * @param dir	The directory to list for.
	 * @param regexPattern	File pattern to match, or null for all files
	 * @param direction	If defined, perform a case insensitive sort by the file name (not the whole path).
	 * @return	The files.
	 */
	public static @Nullable File[] listFiles(@Nonnull File dir,
												@Nullable String regexPattern,
												@Nullable SortDirection direction) {
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
