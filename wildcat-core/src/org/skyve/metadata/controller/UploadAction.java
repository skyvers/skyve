package org.skyve.metadata.controller;

import java.io.InputStream;

import org.skyve.content.MimeType;
import org.skyve.domain.Bean;
import org.skyve.metadata.MetaData;
import org.skyve.web.WebContext;

/**
 * 
 * @param <T>
 */
public abstract class UploadAction<T extends Bean> implements MetaData {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2843062742565336035L;

	/**
	 * 
	 */
	public static class UploadedFile {
		private String fileName;
		private InputStream is;
		private MimeType mimeType;
		
		/**
		 * 
		 * @param fileName
		 * @param is
		 * @param mimeType
		 */
		public UploadedFile(String fileName, InputStream is, MimeType mimeType) {
			this.fileName = fileName;
			this.is = is;
			this.mimeType = mimeType;
		}

		/**
		 * 
		 * @return
		 */
		public String getFileName() {
			return fileName;
		}

		/**
		 * 
		 * @param fileName
		 */
		public void setFileName(String fileName) {
			this.fileName = fileName;
		}
		
		/**
		 * 
		 * @return
		 */
		public InputStream getInputStream() {
			return is;
		}

		/**
		 * 
		 * @param is
		 */
		public void setInputStream(InputStream is) {
			this.is = is;
		}

		/**
		 * 
		 * @return
		 */
		public MimeType getMimeType() {
			return mimeType;
		}

		/**
		 * 
		 * @param mimeType
		 */
		public void setMimeType(MimeType mimeType) {
			this.mimeType = mimeType;
		}
	}
	
	/**
	 * 
	 * @param bean	The bean to execute on.
	 * @param file	The file to process.
	 * @param webContext	The context to manipulate.
	 * @throws Exception
	 */
	public abstract void upload(T bean, UploadedFile file, WebContext webContext) throws Exception;
}
