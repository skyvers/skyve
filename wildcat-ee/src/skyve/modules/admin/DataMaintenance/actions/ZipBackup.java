package modules.admin.DataMaintenance.actions;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import modules.admin.domain.DataMaintenance;

import org.skyve.CORE;
import org.skyve.content.MimeType;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.util.Util;
import org.skyve.web.WebContext;
import org.skyve.wildcat.util.UtilImpl;

public class ZipBackup extends DownloadAction<DataMaintenance> {
	private static final long serialVersionUID = 4544317770456317465L;

	@Override
	public Download download(DataMaintenance bean, WebContext webContext)
	throws Exception {
		try (ByteArrayOutputStream out = new ByteArrayOutputStream(20480)) {
			String customerName = CORE.getUser().getCustomerName();
			File backupDir = new File(UtilImpl.CONTENT_DIRECTORY + "backup_" + customerName + File.separator + bean.getSelectedBackupTimestampFolderName());
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
			return new Download(String.format("backup_%s_%s.zip", customerName, bean.getSelectedBackupTimestampFolderName()),
									new ByteArrayInputStream(out.toByteArray()),
									MimeType.zip);
		}
	}
	
	private static void getAllFiles(File dir, List<File> fileList) {
		File[] files = dir.listFiles();
		for (File file : files) {
			fileList.add(file);
			if (file.isDirectory()) {
				getAllFiles(file, fileList);
			}
		}
	}

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
}
